// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {JobCancelledError, type WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type {ConsumerMessages, JsMsg} from 'nats';
import type {IJobLedgerRepository} from '../jobs/IJobLedgerRepository';
import {Logger} from '../Logger';
import {getWorkerService} from '../middleware/ServiceRegistry';
import {isJsonRecord, parseJsonRecord} from '../utils/JsonBoundaryUtils';
import {
	WORKER_LANE_HEARTBEAT_INTERVAL_MS,
	WORKER_LANE_STALE_AFTER_MS,
	type WorkerHeartbeat,
	type WorkerHeartbeatSignal,
} from './WorkerHeartbeat';

const MAX_DLQ_PUBLISH_ATTEMPTS = 3;
const MIN_ACK_HEARTBEAT_MS = 1000;
const RESUBSCRIBE_DELAY_MS = 5000;
const RETIRED_TASK_REASON = 'task type retired';

interface WorkerRunnerJetStreamClient {
	consumers: {
		get(
			streamName: string,
			consumerName: string,
		): Promise<{
			consume(options: {max_messages: number; idle_heartbeat: number}): Promise<ConsumerMessages>;
		}>;
	};
}

interface WorkerRunnerConnectionManager {
	getJetStreamClient(): WorkerRunnerJetStreamClient;
}

interface WorkerRunnerDlqMeta {
	originalSeq: number;
	errorMessage: string;
	deliveryCount: number;
	lane: string;
	runAt?: string;
}

interface WorkerRunnerQueue {
	getConnectionManager(): WorkerRunnerConnectionManager;
	getStreamName(): string;
	publishToDlq(taskType: string, originalPayload: Record<string, unknown>, meta: WorkerRunnerDlqMeta): Promise<void>;
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
	private heartbeatTimer: ReturnType<typeof setInterval> | null = null;
	private running = false;
	private consumerMessages: ConsumerMessages | null = null;
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
		this.maxDeliver = options.maxDeliver ?? 5;
		this.ackWaitMs = options.ackWaitMs ?? 60000;
		this.workerService = getWorkerService();
		this.ledger = options.ledger;
		this.heartbeat = options.heartbeat ?? null;
	}

	async start(): Promise<void> {
		if (this.running) {
			Logger.warn({workerId: this.workerId}, 'Worker already running');
			return;
		}
		this.running = true;
		Logger.info({workerId: this.workerId, lane: this.laneName, concurrency: this.concurrency}, 'Worker starting');
		this.startHeartbeat();
		this.consumerMessages = await this.openConsumerMessages();
		this.processingLoop = this.consumeUntilStopped(this.consumerMessages).finally(() => {
			this.stopHeartbeatTicker();
		});
	}

	async stop(): Promise<void> {
		if (!this.running) {
			return;
		}
		this.running = false;
		if (this.consumerMessages !== null) {
			await this.consumerMessages.close();
			this.consumerMessages = null;
		}
		if (this.processingLoop !== null) {
			await this.processingLoop;
			this.processingLoop = null;
		}
		this.stopHeartbeatTicker();
		this.heartbeatSignal?.release();
		this.heartbeatSignal = null;
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

	private stopHeartbeatTicker(): void {
		if (this.heartbeatTimer !== null) {
			clearInterval(this.heartbeatTimer);
			this.heartbeatTimer = null;
		}
	}

	private async openConsumerMessages(): Promise<ConsumerMessages> {
		const js = this.queue.getConnectionManager().getJetStreamClient();
		const consumer = await js.consumers.get(this.queue.getStreamName(), this.consumerName);
		const prefetch = Math.max(this.concurrency * 2, 16);
		return await consumer.consume({
			max_messages: prefetch,
			idle_heartbeat: 5000,
		});
	}

	private async consumeUntilStopped(initialMessages: ConsumerMessages): Promise<void> {
		let messages: ConsumerMessages | null = initialMessages;
		while (this.running) {
			if (messages === null) {
				await new Promise((resolve) => setTimeout(resolve, RESUBSCRIBE_DELAY_MS));
				if (!this.running) {
					break;
				}
				try {
					messages = await this.openConsumerMessages();
					this.consumerMessages = messages;
				} catch (error) {
					Logger.error(
						{workerId: this.workerId, lane: this.laneName, err: error},
						'Failed to resubscribe the worker consumer',
					);
				}
				continue;
			}
			try {
				await this.processMessages(messages);
			} catch (error) {
				Logger.error({workerId: this.workerId, err: error}, 'Worker message processing failed unexpectedly');
			}
			messages = null;
			this.consumerMessages = null;
			if (this.running) {
				Logger.error(
					{workerId: this.workerId, lane: this.laneName},
					'Worker message stream ended while running, resubscribing',
				);
			}
		}
		Logger.info({workerId: this.workerId}, 'Worker message iterator ended');
	}

	private async processMessages(consumerMessages: ConsumerMessages): Promise<void> {
		for await (const msg of consumerMessages) {
			if (!this.running) {
				break;
			}
			while (this.inFlightJobs.size >= this.concurrency) {
				await Promise.race(this.inFlightJobs);
			}
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
		await Promise.allSettled(this.inFlightJobs);
	}

	protected async processJob(taskType: string, msg: JsMsg): Promise<boolean> {
		if (this.retiredTaskTypes.has(taskType)) {
			await this.retireJob(taskType, msg);
			return false;
		}
		const task = this.tasks[taskType];
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
		} catch {
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
						'Job scheduled for future execution, redelivering with delay',
					);
					msg.nak(delayMs);
					return false;
				}
			}
		}
		if (ledgerJobId !== null) {
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
		const ackHeartbeat = this.startAckHeartbeat(taskType, msg);
		try {
			await task(jobPayload, helpers);
			if (ledgerJobId !== null) {
				try {
					await this.ledger.markSucceeded(ledgerJobId, null);
				} catch (err) {
					Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markSucceeded failed');
				}
			}
			msg.ack();
			return true;
		} catch (error) {
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
			if (isLastDelivery) {
				Logger.error(
					{taskType, seq: msg.seq, deliveryCount, err: error},
					'Job failed on final delivery attempt, moving to dead-letter queue',
				);
				if (ledgerJobId !== null) {
					try {
						await this.ledger.markDeadletter(ledgerJobId, errorMessage);
					} catch (err) {
						Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markDeadletter failed');
					}
				}
				try {
					await this.queue.publishToDlq(taskType, jobPayload, {
						originalSeq: msg.seq,
						errorMessage,
						deliveryCount,
						lane: this.laneName,
						runAt,
					});
					msg.term('moved to dead-letter queue');
				} catch (dlqError) {
					const dlqPublishAttempts = deliveryCount - this.maxDeliver;
					if (dlqPublishAttempts >= MAX_DLQ_PUBLISH_ATTEMPTS) {
						Logger.error(
							{taskType, seq: msg.seq, deliveryCount, err: dlqError},
							'Failed to publish to dead-letter queue after repeated attempts, dropping message to avoid poison loop',
						);
						msg.term('dead-letter publish failed repeatedly');
					} else {
						Logger.error(
							{taskType, seq: msg.seq, deliveryCount, err: dlqError},
							'Failed to publish to dead-letter queue, will retry on redelivery',
						);
						msg.nak(5000);
					}
				}
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
		} finally {
			clearInterval(ackHeartbeat);
		}
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
		try {
			await this.queue.publishToDlq(taskType, jobPayload, {
				originalSeq: msg.seq,
				errorMessage: RETIRED_TASK_REASON,
				deliveryCount: msg.info.deliveryCount,
				lane: this.laneName,
				runAt,
			});
		} catch (error) {
			Logger.error({taskType, seq: msg.seq, err: error}, 'Failed to dead-letter a retired job');
			msg.nak(5000);
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

	private startAckHeartbeat(taskType: string, msg: JsMsg): ReturnType<typeof setInterval> {
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
		if (typeof heartbeat === 'object' && heartbeat && 'unref' in heartbeat) {
			(heartbeat as {unref(): void}).unref();
		}
		return heartbeat;
	}
}
