// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomUUID} from 'node:crypto';
import {ArchiveAttemptSupersededError} from '@app/api/archive/ArchiveAttemptSupersededError';
import {ArchiveTaskDeferredError, ArchiveTerminalFailureError, isArchiveTask} from '@app/api/archive/ArchiveTask';
import type {IJobLedgerRepository} from '@app/api/jobs/IJobLedgerRepository';
import {Logger} from '@app/api/Logger';
import {getWorkerService} from '@app/api/middleware/ServiceRegistry';
import {isJsonRecord, parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import type {WorkerDeadLetterMetadata} from '@app/api/worker/JetStreamWorkerQueue';
import {
	WORKER_LANE_HEARTBEAT_INTERVAL_MS,
	WORKER_LANE_STALE_AFTER_MS,
	type WorkerHeartbeat,
	type WorkerHeartbeatSignal,
} from '@app/api/worker/WorkerHeartbeat';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {JobCancelledError, type WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type {ConsumerMessages, FetchOptions, JsMsg} from 'nats';

const MAX_DLQ_PUBLISH_ATTEMPTS = 3;
const DLQ_RETRY_DELAY_MS = 250;
const MIN_ACK_HEARTBEAT_MS = 1000;
const MAX_ACK_WAIT_MS = 2 * 2_147_483_647;
const RESUBSCRIBE_DELAY_MS = 5000;
const RETIRED_TASK_REASON = 'task type retired';

interface WorkerRunnerConsumer {
	fetch(options: FetchOptions): Promise<ConsumerMessages>;
}

interface WorkerRunnerJetStreamClient {
	consumers: {
		get(streamName: string, consumerName: string): Promise<WorkerRunnerConsumer>;
	};
}

interface WorkerRunnerConnectionManager {
	getJetStreamClient(): WorkerRunnerJetStreamClient;
}

interface WorkerRunnerQueue {
	getConnectionManager(): WorkerRunnerConnectionManager;
	getStreamName(): string;
	publishToDlq(
		taskType: string,
		originalPayload: Record<string, unknown>,
		meta: WorkerDeadLetterMetadata,
	): Promise<void>;
}

interface WorkerRunnerOptions {
	tasks: Record<string, WorkerTaskHandler>;
	retiredTaskTypes?: ReadonlyArray<string>;
	queue: WorkerRunnerQueue;
	consumerName: string;
	laneName: string;
	ledger: IJobLedgerRepository;
	workerId?: string;
	concurrency?: number;
	maxDeliver?: number;
	ackWaitMs?: number;
	heartbeat?: WorkerHeartbeat;
}

export class WorkerRunner {
	private readonly tasks: Record<string, WorkerTaskHandler>;
	private readonly retiredTaskTypes: Set<string>;
	private readonly queue: WorkerRunnerQueue;
	private readonly consumerName: string;
	private readonly laneName: string;
	private readonly workerId: string;
	private readonly concurrency: number;
	private readonly maxDeliver: number;
	private readonly ackWaitMs: number;
	private readonly workerService: IWorkerService;
	private readonly ledger: IJobLedgerRepository;
	private readonly heartbeat: WorkerHeartbeat | null;
	private heartbeatSignal: WorkerHeartbeatSignal | null = null;
	private heartbeatTimer: NodeJS.Timeout | null = null;
	private running = false;
	private consumerMessages: ConsumerMessages | null = null;
	private startup: Promise<void> | null = null;
	private shutdown: Promise<void> | null = null;
	private processingLoop: Promise<void> | null = null;
	private readonly inFlightJobs = new Set<Promise<void>>();

	constructor(options: WorkerRunnerOptions) {
		this.tasks = options.tasks;
		this.retiredTaskTypes = new Set(options.retiredTaskTypes ?? []);
		this.queue = options.queue;
		this.consumerName = options.consumerName;
		this.laneName = options.laneName;
		this.workerId = options.workerId ?? `worker-${options.laneName}-${randomUUID()}`;
		this.concurrency = options.concurrency ?? 1;
		if (!Number.isSafeInteger(this.concurrency) || this.concurrency < 1) {
			throw new RangeError('Worker concurrency must be a positive safe integer');
		}
		this.maxDeliver = options.maxDeliver ?? 5;
		if (!Number.isSafeInteger(this.maxDeliver) || this.maxDeliver < 1) {
			throw new RangeError('Worker maxDeliver must be a positive safe integer');
		}
		this.ackWaitMs = options.ackWaitMs ?? 60000;
		if (
			!Number.isSafeInteger(this.ackWaitMs) ||
			this.ackWaitMs < MIN_ACK_HEARTBEAT_MS * 2 ||
			this.ackWaitMs > MAX_ACK_WAIT_MS
		) {
			throw new RangeError(
				`Worker ackWaitMs must be a safe integer between ${MIN_ACK_HEARTBEAT_MS * 2} and ${MAX_ACK_WAIT_MS}`,
			);
		}
		this.workerService = getWorkerService();
		this.ledger = options.ledger;
		this.heartbeat = options.heartbeat ?? null;
	}

	async start(): Promise<void> {
		if (this.startup !== null) {
			await this.startup;
			return;
		}
		if (this.running || this.shutdown !== null || this.processingLoop !== null) {
			Logger.warn({workerId: this.workerId}, 'Worker already running or stopping');
			return;
		}
		this.running = true;
		Logger.info({workerId: this.workerId, lane: this.laneName, concurrency: this.concurrency}, 'Worker starting');
		this.startup = this.startConsuming().finally(() => {
			this.startup = null;
		});
		await this.startup;
	}

	private async startConsuming(): Promise<void> {
		try {
			this.startHeartbeat();
			const consumer = await this.openConsumer();
			if (!this.running) {
				return;
			}
			this.processingLoop = this.consumeUntilStopped(consumer).finally(() => {
				this.running = false;
				this.processingLoop = null;
				this.stopHeartbeat();
			});
		} catch (error) {
			this.running = false;
			this.stopHeartbeat();
			throw error;
		}
	}

	async stop(): Promise<void> {
		if (this.shutdown !== null) {
			await this.shutdown;
			return;
		}
		if (!this.running && this.startup === null && this.processingLoop === null) {
			return;
		}
		this.running = false;
		this.shutdown = this.stopConsuming().finally(() => {
			this.shutdown = null;
		});
		await this.shutdown;
	}

	private async stopConsuming(): Promise<void> {
		const startupResult = await Promise.allSettled([this.startup]);
		const messages = this.consumerMessages;
		const stopResults = await Promise.allSettled([
			Promise.resolve().then(() => messages?.close()),
			this.processingLoop,
		]);
		this.stopHeartbeat();
		const errors = [...startupResult, ...stopResults]
			.filter((result) => result.status === 'rejected')
			.map((result) => result.reason);
		if (errors.length > 0) {
			throw new AggregateError(errors, 'Worker shutdown failed');
		}
		Logger.info({workerId: this.workerId}, 'Worker stopped');
	}

	private startHeartbeat(): void {
		if (this.heartbeat === null) {
			return;
		}
		this.heartbeatSignal = this.heartbeat.register(`lane:${this.laneName}`, WORKER_LANE_STALE_AFTER_MS);
		this.heartbeatTimer = setInterval(() => {
			this.heartbeatSignal?.report();
		}, WORKER_LANE_HEARTBEAT_INTERVAL_MS);
	}

	private stopHeartbeat(): void {
		if (this.heartbeatTimer !== null) {
			clearInterval(this.heartbeatTimer);
			this.heartbeatTimer = null;
		}
		this.heartbeatSignal?.release();
		this.heartbeatSignal = null;
	}

	private async openConsumer(): Promise<WorkerRunnerConsumer> {
		const js = this.queue.getConnectionManager().getJetStreamClient();
		return await js.consumers.get(this.queue.getStreamName(), this.consumerName);
	}

	private async consumeUntilStopped(initialConsumer: WorkerRunnerConsumer): Promise<void> {
		let consumer: WorkerRunnerConsumer | null = initialConsumer;
		while (this.running) {
			if (consumer === null) {
				await new Promise((resolve) => setTimeout(resolve, RESUBSCRIBE_DELAY_MS));
				if (!this.running) {
					break;
				}
				try {
					consumer = await this.openConsumer();
				} catch (error) {
					Logger.error(
						{workerId: this.workerId, lane: this.laneName, err: error},
						'Failed to resubscribe the worker consumer',
					);
				}
				continue;
			}
			try {
				await this.processMessages(consumer);
			} catch (error) {
				Logger.error({workerId: this.workerId, err: error}, 'Worker message processing failed unexpectedly');
			}
			consumer = null;
			if (this.running) {
				Logger.error(
					{workerId: this.workerId, lane: this.laneName},
					'Worker message stream ended while running, resubscribing',
				);
			}
		}
		Logger.info({workerId: this.workerId}, 'Worker message iterator ended');
	}

	private async processMessages(consumer: WorkerRunnerConsumer): Promise<void> {
		try {
			while (this.running) {
				while (this.inFlightJobs.size >= this.concurrency) {
					await Promise.race(this.inFlightJobs);
				}
				if (!this.running) {
					break;
				}
				const messages = await consumer.fetch({
					max_messages: this.concurrency - this.inFlightJobs.size,
					idle_heartbeat: 5000,
				});
				this.consumerMessages = messages;
				if (!this.running) {
					messages.stop();
				}
				let admissionError: Error | null = null;
				try {
					for await (const msg of messages) {
						if (!this.running || admissionError !== null) {
							continue;
						}
						try {
							if (this.inFlightJobs.size >= this.concurrency) {
								throw new Error('Worker consumer delivered a message without available capacity');
							}
							this.admitJob(msg);
						} catch (error) {
							admissionError = error instanceof Error ? error : new Error('Worker admission failed', {cause: error});
							messages.stop(admissionError);
						}
					}
					if (admissionError !== null) {
						throw admissionError;
					}
				} finally {
					try {
						await messages.close();
					} finally {
						this.consumerMessages = null;
					}
				}
			}
		} finally {
			await Promise.allSettled(this.inFlightJobs);
		}
	}

	private admitJob(msg: JsMsg): void {
		const taskType = msg.subject.startsWith('jobs.') ? msg.subject.slice(5) : msg.subject;
		Logger.info(
			{
				workerId: this.workerId,
				lane: this.laneName,
				taskType,
				seq: msg.seq,
				redelivered: msg.redelivered,
			},
			'Processing job',
		);
		const jobPromise = this.processJob(taskType, msg)
			.then((succeeded) => {
				if (succeeded) {
					Logger.info({workerId: this.workerId, taskType, seq: msg.seq}, 'Job completed successfully');
				}
			})
			.catch((error) => {
				Logger.error({workerId: this.workerId, taskType, seq: msg.seq, err: error}, 'Job processing crashed');
				if (isArchiveTask(this.tasks[taskType])) {
					return;
				}
				try {
					msg.nak(5000);
				} catch (nakError) {
					Logger.error({workerId: this.workerId, taskType, seq: msg.seq, err: nakError}, 'Failed to NAK crashed job');
				}
			})
			.finally(() => {
				this.inFlightJobs.delete(jobPromise);
			});
		this.inFlightJobs.add(jobPromise);
	}

	protected async processJob(taskType: string, msg: JsMsg): Promise<boolean> {
		const ackHeartbeat = this.startAckHeartbeat(taskType, msg);
		try {
			return await this.executeJob(taskType, msg);
		} finally {
			clearInterval(ackHeartbeat);
		}
	}

	private async executeJob(taskType: string, msg: JsMsg): Promise<boolean> {
		const task = this.tasks[taskType];
		const archiveTask = isArchiveTask(task);
		if (this.retiredTaskTypes.has(taskType)) {
			await this.retireJob(taskType, msg);
			return false;
		}
		if (!task) {
			Logger.error({taskType, seq: msg.seq}, 'Unknown task type, terminating message');
			msg.term(`unknown task type: ${taskType}`);
			return false;
		}
		let jobPayload: Record<string, unknown> = {};
		let runAt: string | undefined;
		let ledgerJobId: bigint | null = null;
		try {
			const decoded = parseJsonRecord(new TextDecoder().decode(msg.data));
			if (!decoded) {
				throw new Error('job envelope must be a JSON object');
			}
			jobPayload = isJsonRecord(decoded.payload) ? decoded.payload : {};
			runAt = typeof decoded.run_at === 'string' ? decoded.run_at : undefined;
			const embedded = jobPayload['__jobId'];
			if (typeof embedded === 'string') {
				try {
					ledgerJobId = BigInt(embedded);
				} catch {
					ledgerJobId = null;
				}
				delete jobPayload['__jobId'];
			}
		} catch (error) {
			if (archiveTask) {
				Logger.error(
					{taskType, seq: msg.seq, err: error},
					'Invalid archive job payload, leaving message unacknowledged',
				);
				return false;
			}
			Logger.error({taskType, seq: msg.seq}, 'Failed to decode job payload, terminating message');
			msg.term('invalid payload');
			return false;
		}
		if (runAt) {
			const runAtMs = new Date(runAt).getTime();
			if (Number.isFinite(runAtMs)) {
				const delayMs = runAtMs - Date.now();
				if (delayMs > 0) {
					Logger.debug(
						{taskType, seq: msg.seq, runAt, delayMs},
						'Job scheduled for future execution, deferring execution',
					);
					if (!archiveTask) msg.nak(delayMs);
					return false;
				}
			}
		}
		if (ledgerJobId !== null && !archiveTask) {
			try {
				await this.ledger.markRunning(ledgerJobId, this.laneName);
			} catch (err) {
				Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markRunning failed');
			}
		}
		const ledger = this.ledger;
		const capturedJobId = ledgerJobId;
		const helpers = {
			logger: Logger.child({taskType, seq: msg.seq, jobId: capturedJobId?.toString()}),
			jobId: capturedJobId ?? 0n,
			attempt: {isLastAttempt: msg.info.deliveryCount >= this.maxDeliver},
			addJob: this.workerService.addJob.bind(this.workerService),
			reportProgress: async (current: number, total: number | null, message?: string | null) => {
				if (capturedJobId === null) return;
				try {
					await ledger.reportProgress(capturedJobId, current, total, message ?? null);
				} catch (err) {
					Logger.warn({err, jobId: capturedJobId.toString()}, 'Ledger reportProgress failed');
				}
			},
			shouldCancel: async () => {
				if (capturedJobId === null) return false;
				try {
					return await ledger.isCancelRequested(capturedJobId);
				} catch (err) {
					Logger.warn({err, jobId: capturedJobId.toString()}, 'Ledger isCancelRequested failed');
					return false;
				}
			},
			setContextLink: async (link: string) => {
				if (capturedJobId === null) return;
				try {
					await ledger.setContextLink(capturedJobId, link);
				} catch (err) {
					Logger.warn({err, jobId: capturedJobId.toString()}, 'Ledger setContextLink failed');
				}
			},
		};
		try {
			const result = await task(jobPayload, helpers);
			if (ledgerJobId !== null) {
				try {
					await this.ledger.markSucceeded(ledgerJobId, result ?? null);
				} catch (err) {
					Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markSucceeded failed');
				}
			}
			msg.ack();
			return true;
		} catch (error) {
			if (archiveTask && !(error instanceof ArchiveTerminalFailureError)) {
				if (error instanceof ArchiveAttemptSupersededError) {
					Logger.info({taskType, seq: msg.seq}, 'Archive attempt was superseded');
				} else if (error instanceof ArchiveTaskDeferredError) {
					Logger.warn(
						{taskType, seq: msg.seq, err: error},
						'Archive attempt deferred without changing queue disposition',
					);
				} else {
					Logger.error(
						{taskType, seq: msg.seq, err: error},
						'Archive attempt failed without a confirmed terminal state',
					);
				}
				return false;
			}
			const isCancelled = error instanceof JobCancelledError;
			if (isCancelled) {
				if (ledgerJobId !== null) {
					try {
						await this.ledger.markCancelled(ledgerJobId);
					} catch (err) {
						Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markCancelled failed');
					}
				}
				Logger.info({taskType, seq: msg.seq, jobId: ledgerJobId?.toString()}, 'Job cancelled by admin');
				msg.ack();
				return false;
			}
			const deliveryCount = msg.info.deliveryCount;
			const isLastDelivery = deliveryCount >= this.maxDeliver;
			const errorMessage = error instanceof Error ? error.message : String(error);
			if (archiveTask || isLastDelivery) {
				Logger.error(
					{taskType, seq: msg.seq, deliveryCount, err: error},
					'Job failed permanently, publishing to dead-letter queue',
				);
				const published = await this.publishToDlqWithRetry(
					taskType,
					jobPayload,
					this.createDeadLetterMetadata(msg, errorMessage, runAt),
				);
				if (!published) return false;
				if (ledgerJobId !== null) {
					try {
						await this.ledger.markDeadletter(ledgerJobId, errorMessage);
					} catch (err) {
						Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markDeadletter failed');
					}
				}
				msg.term('moved to dead-letter queue');
			} else {
				Logger.error({taskType, seq: msg.seq, err: error}, 'Job failed');
				if (ledgerJobId !== null) {
					try {
						await this.ledger.incrementAttempts(ledgerJobId);
					} catch (err) {
						Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger incrementAttempts failed');
					}
				}
				msg.nak(5000);
			}
			return false;
		}
	}

	private createDeadLetterMetadata(msg: JsMsg, errorMessage: string, runAt?: string): WorkerDeadLetterMetadata {
		const identity = [this.queue.getStreamName(), msg.subject, msg.seq, msg.headers?.get('Nats-Msg-Id') ?? null];
		const digest = createHash('sha256').update(JSON.stringify(identity)).update('\0').update(msg.data).digest('hex');
		return {
			messageId: `dlq:${digest}`,
			originalSeq: msg.seq,
			errorMessage,
			deliveryCount: msg.info.deliveryCount,
			lane: this.laneName,
			runAt,
		};
	}

	private async publishToDlqWithRetry(
		taskType: string,
		payload: Record<string, unknown>,
		meta: WorkerDeadLetterMetadata,
		maxAttempts = MAX_DLQ_PUBLISH_ATTEMPTS,
	): Promise<boolean> {
		for (let attempt = 1; attempt <= maxAttempts; attempt++) {
			try {
				await this.queue.publishToDlq(taskType, payload, meta);
				return true;
			} catch (error) {
				if (attempt === maxAttempts) {
					Logger.error(
						{taskType, seq: meta.originalSeq, deliveryCount: meta.deliveryCount, attempts: attempt, err: error},
						'Failed to publish to dead-letter queue, leaving original message unacknowledged',
					);
					return false;
				}
				Logger.warn(
					{taskType, seq: meta.originalSeq, attempt, err: error},
					'Dead-letter publish failed, retrying within this delivery',
				);
				await new Promise((resolve) => setTimeout(resolve, DLQ_RETRY_DELAY_MS * attempt));
			}
		}
		return false;
	}

	private async retireJob(taskType: string, msg: JsMsg): Promise<void> {
		const decoded = parseJsonRecord(new TextDecoder().decode(msg.data));
		const jobPayload = decoded && isJsonRecord(decoded.payload) ? decoded.payload : {};
		const runAt = decoded && typeof decoded.run_at === 'string' ? decoded.run_at : undefined;
		let ledgerJobId: bigint | null = null;
		const embedded = jobPayload['__jobId'];
		if (typeof embedded === 'string') {
			try {
				ledgerJobId = BigInt(embedded);
			} catch {
				ledgerJobId = null;
			}
			delete jobPayload['__jobId'];
		}
		Logger.warn(
			{taskType, seq: msg.seq, jobId: ledgerJobId?.toString()},
			'Retired task type from an older release, moving to dead-letter queue',
		);
		const isLastDelivery = msg.info.deliveryCount >= this.maxDeliver;
		const published = await this.publishToDlqWithRetry(
			taskType,
			jobPayload,
			this.createDeadLetterMetadata(msg, RETIRED_TASK_REASON, runAt),
			isLastDelivery ? MAX_DLQ_PUBLISH_ATTEMPTS : 1,
		);
		if (!published) {
			if (!isLastDelivery) msg.nak(5000);
			return;
		}
		if (ledgerJobId !== null) {
			try {
				await this.ledger.markDeadletter(ledgerJobId, RETIRED_TASK_REASON);
			} catch (err) {
				Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markDeadletter failed');
			}
		}
		msg.term(RETIRED_TASK_REASON);
	}

	private startAckHeartbeat(taskType: string, msg: JsMsg): NodeJS.Timeout {
		const heartbeat = setInterval(
			() => {
				try {
					msg.working();
				} catch (err) {
					Logger.warn({workerId: this.workerId, taskType, seq: msg.seq, err}, 'Failed to extend job ack deadline');
				}
			},
			Math.max(MIN_ACK_HEARTBEAT_MS, Math.floor(this.ackWaitMs / 2)),
		);
		heartbeat.unref();
		return heartbeat;
	}
}
