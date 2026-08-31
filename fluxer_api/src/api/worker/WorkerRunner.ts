// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {JobCancelledError, type WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type {ConsumerMessages, JsMsg} from 'nats';
import type {IJobLedgerRepository} from '../jobs/IJobLedgerRepository';
import {Logger} from '../Logger';
import {getWorkerService} from '../middleware/ServiceRegistry';
import {isJsonRecord, parseJsonRecord} from '../utils/JsonBoundaryUtils';

const MAX_DLQ_PUBLISH_ATTEMPTS = 3;
const MIN_ACK_HEARTBEAT_MS = 1000;

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
	queue: WorkerRunnerQueue;
	consumerName: string;
	laneName: string;
	ledger: IJobLedgerRepository;
	workerId?: string;
	concurrency?: number;
	maxDeliver?: number;
	ackWaitMs?: number;
}

export class WorkerRunner {
	private readonly tasks: Record<string, WorkerTaskHandler>;
	private readonly queue: WorkerRunnerQueue;
	private readonly consumerName: string;
	private readonly laneName: string;
	private readonly workerId: string;
	private readonly concurrency: number;
	private readonly maxDeliver: number;
	private readonly ackWaitMs: number;
	private readonly workerService: IWorkerService;
	private readonly ledger: IJobLedgerRepository;
	private running = false;
	private consumerMessages: ConsumerMessages | null = null;
	private readonly inFlightJobs = new Set<Promise<void>>();

	constructor(options: WorkerRunnerOptions) {
		this.tasks = options.tasks;
		this.queue = options.queue;
		this.consumerName = options.consumerName;
		this.laneName = options.laneName;
		this.workerId = options.workerId ?? `worker-${options.laneName}-${randomUUID()}`;
		this.concurrency = options.concurrency ?? 1;
		this.maxDeliver = options.maxDeliver ?? 5;
		this.ackWaitMs = options.ackWaitMs ?? 60000;
		this.workerService = getWorkerService();
		this.ledger = options.ledger;
	}

	async start(): Promise<void> {
		if (this.running) {
			Logger.warn({workerId: this.workerId}, 'Worker already running');
			return;
		}
		this.running = true;
		Logger.info({workerId: this.workerId, lane: this.laneName, concurrency: this.concurrency}, 'Worker starting');
		const js = this.queue.getConnectionManager().getJetStreamClient();
		const consumer = await js.consumers.get(this.queue.getStreamName(), this.consumerName);
		const prefetch = Math.max(this.concurrency * 2, 16);
		this.consumerMessages = await consumer.consume({
			max_messages: prefetch,
			idle_heartbeat: 5000,
		});
		this.processMessages().catch((error) => {
			Logger.error({workerId: this.workerId, err: error}, 'Worker message processing failed unexpectedly');
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
		Logger.info({workerId: this.workerId}, 'Worker stopped');
	}

	private async processMessages(): Promise<void> {
		if (this.consumerMessages === null) {
			return;
		}
		for await (const msg of this.consumerMessages) {
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
		Logger.info({workerId: this.workerId}, 'Worker message iterator ended');
	}

	protected async processJob(taskType: string, msg: JsMsg): Promise<boolean> {
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
