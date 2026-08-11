// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {JobCancelledError, type WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import type {ConsumerMessages, JsMsg} from 'nats';
import type {JobByIdRow} from '../database/types/JobLedgerTypes';
import type {IJobLedgerRepository} from '../jobs/IJobLedgerRepository';
import {Logger} from '../Logger';
import {getWorkerService} from '../middleware/ServiceRegistry';
import {isJsonRecord, parseJsonRecord} from '../utils/JsonBoundaryUtils';

import {WORKER_DLQ_PUBLISH_ATTEMPTS} from './WorkerLaneConfig';

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
	enqueue(
		taskType: string,
		payload: WorkerJobPayload,
		options?: {
			runAt?: Date;
			maxAttempts?: number;
			priority?: number;
			jobKey?: string;
		},
	): Promise<string>;
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
	private startPromise: Promise<void> | null = null;
	private stopPromise: Promise<void> | null = null;
	private processLoop: Promise<void> | null = null;
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
		if (this.processLoop !== null) {
			throw new Error('Cannot restart worker while its previous message loop is still active');
		}
		this.running = true;
		Logger.info({workerId: this.workerId, lane: this.laneName, concurrency: this.concurrency}, 'Worker starting');
		const startPromise = (async () => {
			const js = this.queue.getConnectionManager().getJetStreamClient();
			const consumer = await js.consumers.get(this.queue.getStreamName(), this.consumerName);
			const prefetch = Math.max(this.concurrency * 2, 16);
			const messages = await consumer.consume({
				max_messages: prefetch,
				idle_heartbeat: 5000,
			});
			if (!this.running) {
				await messages.close();
				return;
			}
			this.consumerMessages = messages;
			const processLoop = this.processMessages()
				.catch((error) => {
					Logger.error({workerId: this.workerId, err: error}, 'Worker message processing failed unexpectedly');
				})
				.finally(() => {
					if (this.processLoop === processLoop) this.processLoop = null;
				});
			this.processLoop = processLoop;
		})();
		this.startPromise = startPromise;
		try {
			await startPromise;
		} catch (error) {
			this.running = false;
			throw error;
		} finally {
			if (this.startPromise === startPromise) this.startPromise = null;
		}
	}

	async stop(): Promise<void> {
		if (this.stopPromise !== null) return this.stopPromise;
		const stopping = this.stopInternal();
		this.stopPromise = stopping;
		await stopping;
		if (this.stopPromise === stopping) this.stopPromise = null;
	}

	private async stopInternal(): Promise<void> {
		if (!this.running && this.startPromise === null && this.consumerMessages === null && this.processLoop === null) {
			return;
		}
		this.running = false;
		let shutdownError: unknown = null;
		const starting = this.startPromise;
		if (starting !== null) {
			try {
				await starting;
			} catch (error) {
				shutdownError = error;
			}
		}
		const messages = this.consumerMessages;
		this.consumerMessages = null;
		let consumerClosed = messages === null;
		if (messages !== null) {
			try {
				await messages.close();
				consumerClosed = true;
			} catch (error) {
				shutdownError ??= error;
			}
		}
		if (consumerClosed) {
			await this.processLoop;
		} else {
			await Promise.allSettled([...this.inFlightJobs]);
		}
		Logger.info({workerId: this.workerId}, 'Worker stopped');
		if (shutdownError !== null) throw shutdownError;
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
			if (!this.running) {
				try {
					msg.nak(0);
				} catch (error) {
					Logger.warn({workerId: this.workerId, err: error}, 'Failed to NAK prefetched job during shutdown');
				}
				break;
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

	private startOwnershipHeartbeat(
		jobId: bigint,
		msg: JsMsg,
		leaseDurationMs: number,
		renew: (now: Date) => Promise<boolean>,
		failureMessage: string,
	): () => void {
		const interval = setInterval(
			() => {
				void renew(new Date())
					.then((renewed) => {
						if (renewed) msg.working();
						else clearInterval(interval);
					})
					.catch((err) => Logger.warn({err, jobId: jobId.toString()}, failureMessage));
			},
			Math.max(1000, Math.floor(leaseDurationMs / 3)),
		);
		return () => clearInterval(interval);
	}

	private startLeaseHeartbeat(jobId: bigint, leaseToken: string, msg: JsMsg, leaseDurationMs: number): () => void {
		return this.startOwnershipHeartbeat(
			jobId,
			msg,
			leaseDurationMs,
			(now) => this.ledger.renewLease(jobId, leaseToken, now, leaseDurationMs),
			'Failed to renew worker lease',
		);
	}

	private startDeadletterPublicationHeartbeat(
		jobId: bigint,
		leaseToken: string,
		msg: JsMsg,
		leaseDurationMs: number,
	): () => void {
		return this.startOwnershipHeartbeat(
			jobId,
			msg,
			leaseDurationMs,
			(now) => this.ledger.renewDeadletterPublicationLease(jobId, leaseToken, now, leaseDurationMs),
			'Failed to renew dead-letter publication lease',
		);
	}

	private async retainAfterTerminalizationFailure(
		jobId: bigint,
		msg: JsMsg,
		reason: string,
		leaseToken: string,
	): Promise<boolean> {
		try {
			const released = await this.ledger.releaseForRetry(jobId, reason, false, leaseToken);
			if (released) {
				msg.nak(5000);
				return false;
			}
			const latest = await this.ledger.getJob(jobId);
			if (latest && ['succeeded', 'failed', 'cancelled', 'deadletter'].includes(latest.status)) {
				msg.ack();
				return true;
			}
		} catch (err) {
			Logger.warn({err, jobId: jobId.toString()}, 'Failed to release job after terminalization failure');
		}
		msg.nak(5000);
		return false;
	}

	private async publishDlqAndFinalize(
		taskType: string,
		msg: JsMsg,
		jobPayload: Record<string, unknown>,
		runAt: string | undefined,
		ledgerJobId: bigint | null,
		errorMessage: string,
		leaseToken: string | null = null,
	): Promise<void> {
		const deliveryCount = msg.info.deliveryCount;
		let durableErrorMessage = errorMessage;
		let publicationLeaseToken: string | null = null;
		if (ledgerJobId !== null) {
			try {
				const prepared = await this.ledger.markDeadletterPending(
					ledgerJobId,
					errorMessage,
					leaseToken,
					new Date(),
					this.ackWaitMs,
				);
				if (prepared) {
					publicationLeaseToken = prepared.leaseToken;
					durableErrorMessage = prepared.errorMessage;
				} else {
					const latest = await this.ledger.getJob(ledgerJobId);
					if (latest?.status === 'deadletter_pending') {
						msg.nak(5000);
						return;
					}
					if (latest && ['succeeded', 'failed', 'cancelled', 'deadletter'].includes(latest.status)) {
						msg.ack();
						return;
					}
					if (leaseToken !== null) {
						await this.ledger.releaseForRetry(ledgerJobId, errorMessage, false, leaseToken);
					}
					msg.nak(5000);
					return;
				}
			} catch (ledgerError) {
				Logger.warn({err: ledgerError, jobId: ledgerJobId.toString()}, 'Failed to claim dead-letter publication');
				msg.nak(5000);
				return;
			}
		}

		const stopHeartbeat =
			ledgerJobId !== null && publicationLeaseToken !== null
				? this.startDeadletterPublicationHeartbeat(ledgerJobId, publicationLeaseToken, msg, this.ackWaitMs)
				: () => undefined;
		let published = false;
		try {
			await this.queue.publishToDlq(taskType, jobPayload, {
				originalSeq: msg.seq,
				errorMessage: durableErrorMessage,
				deliveryCount,
				lane: this.laneName,
				runAt,
			});
			published = true;
			if (ledgerJobId !== null && publicationLeaseToken !== null) {
				const marked = await this.ledger.markDeadletter(ledgerJobId, durableErrorMessage, publicationLeaseToken);
				if (!marked) {
					const latest = await this.ledger.getJob(ledgerJobId);
					if (latest?.status !== 'deadletter') {
						throw new Error(`Unable to terminalize ledger job from ${latest?.status ?? 'missing'} state`);
					}
				}
			}
			msg.term('moved to dead-letter queue');
		} catch (dlqError) {
			if (published) {
				Logger.error(
					{taskType, seq: msg.seq, deliveryCount, err: dlqError},
					'Dead-letter message was published but ledger terminalization failed; retaining source delivery',
				);
				msg.nak(5000);
				return;
			}
			let dlqPublishAttempts: number | null = deliveryCount - this.maxDeliver;
			if (ledgerJobId !== null && publicationLeaseToken !== null) {
				try {
					dlqPublishAttempts = await this.ledger.recordDlqPublishFailure(ledgerJobId, publicationLeaseToken);
				} catch (ledgerError) {
					Logger.warn(
						{err: ledgerError, jobId: ledgerJobId.toString()},
						'Failed to persist DLQ publication attempt; retaining broker message',
					);
					msg.nak(5000);
					return;
				}
				if (dlqPublishAttempts === null) {
					const latest = await this.ledger.getJob(ledgerJobId);
					if (latest && ['succeeded', 'failed', 'cancelled', 'deadletter'].includes(latest.status)) {
						msg.term('dead-letter handling already terminal');
						return;
					}
					msg.nak(5000);
					return;
				}
			}
			if (dlqPublishAttempts >= WORKER_DLQ_PUBLISH_ATTEMPTS) {
				Logger.error(
					{taskType, seq: msg.seq, deliveryCount, dlqPublishAttempts, err: dlqError},
					'Dead-letter publication failed repeatedly',
				);
				if (ledgerJobId === null) {
					msg.term('dead-letter publish failed repeatedly');
					return;
				}
				Logger.warn(
					{jobId: ledgerJobId.toString()},
					'Retaining ledgered source delivery until DLQ publication succeeds or stream retention expires',
				);
			}
			Logger.error(
				{taskType, seq: msg.seq, deliveryCount, dlqPublishAttempts, err: dlqError},
				'Failed to publish dead-letter message, will retry on redelivery',
			);
			msg.nak(5000);
		} finally {
			stopHeartbeat();
		}
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
			if (decoded.run_at !== undefined) {
				if (typeof decoded.run_at !== 'string' || !Number.isFinite(new Date(decoded.run_at).getTime())) {
					throw new Error('run_at must be a valid timestamp string');
				}
				runAt = decoded.run_at;
			}
			const embedded = jobPayload['__jobId'];
			if (embedded !== undefined) {
				if (typeof embedded !== 'string' || !/^[1-9]\d*$/.test(embedded)) {
					throw new Error('__jobId must be a positive decimal string');
				}
				ledgerJobId = BigInt(embedded);
				delete jobPayload['__jobId'];
			}
		} catch {
			Logger.error({taskType, seq: msg.seq}, 'Failed to decode job payload, terminating message');
			msg.term('invalid payload');
			return false;
		}
		if (runAt) {
			const runAtMs = new Date(runAt).getTime();
			const delayMs = runAtMs - Date.now();
			if (delayMs > 0) {
				Logger.debug(
					{taskType, seq: msg.seq, runAt, delayMs},
					'Job scheduled for future execution, redelivering with broker delay',
				);
				msg.nak(delayMs);
				return false;
			}
		}
		let claimedJob: JobByIdRow | null = null;
		let leaseToken: string | null = null;
		const leaseDurationMs = Math.max(this.ackWaitMs * 2, 60_000);
		if (ledgerJobId !== null) {
			try {
				leaseToken = `${this.workerId}:${msg.seq}:${msg.info.deliveryCount}:${randomUUID()}`;
				claimedJob = await this.ledger.claimJob(ledgerJobId, this.laneName, leaseToken, new Date(), leaseDurationMs);
				const claimed = claimedJob !== null;
				if (!claimed) {
					leaseToken = null;
					claimedJob = await this.ledger.getJob(ledgerJobId);
					if (claimedJob?.status === 'deadletter_pending' && claimedJob.error_message) {
						await this.publishDlqAndFinalize(taskType, msg, jobPayload, runAt, ledgerJobId, claimedJob.error_message);
						return false;
					}
					if (claimedJob && ['succeeded', 'failed', 'cancelled', 'deadletter'].includes(claimedJob.status)) {
						Logger.warn(
							{jobId: ledgerJobId.toString(), taskType, seq: msg.seq, status: claimedJob.status},
							'Ledger job is terminal; acknowledging duplicate delivery',
						);
						msg.ack();
					} else {
						Logger.warn(
							{jobId: ledgerJobId.toString(), taskType, seq: msg.seq, status: claimedJob?.status},
							'Ledger job is currently owned; retrying duplicate delivery',
						);
						msg.nak(5000);
					}
					return false;
				}
			} catch (err) {
				Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger claim failed; retrying delivery');
				msg.nak();
				return false;
			}
		}
		if (ledgerJobId !== null && leaseToken === null) {
			Logger.error({jobId: ledgerJobId.toString()}, 'Claimed ledger job is missing its lease token');
			msg.nak();
			return false;
		}
		const activeLeaseToken = leaseToken;
		const ledger = this.ledger;
		const capturedJobId = ledgerJobId;
		const helpers = {
			logger: Logger.child({taskType, seq: msg.seq, jobId: capturedJobId?.toString()}),
			jobId: capturedJobId ?? 0n,
			addJob: this.workerService.addJob.bind(this.workerService),
			reportProgress: async (current: number, total: number | null, message?: string | null) => {
				if (capturedJobId === null || activeLeaseToken === null) return;
				try {
					await ledger.reportProgress(capturedJobId, current, total, message ?? null, activeLeaseToken);
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
				if (capturedJobId === null || activeLeaseToken === null) return;
				try {
					await ledger.setContextLink(capturedJobId, link, activeLeaseToken);
				} catch (err) {
					Logger.warn({err, jobId: capturedJobId.toString()}, 'Ledger setContextLink failed');
				}
			},
		};
		const stopLeaseHeartbeat =
			ledgerJobId !== null && activeLeaseToken !== null
				? this.startLeaseHeartbeat(ledgerJobId, activeLeaseToken, msg, leaseDurationMs)
				: () => undefined;
		try {
			try {
				await task(jobPayload, helpers);
				if (ledgerJobId !== null && activeLeaseToken !== null) {
					try {
						const marked = await this.ledger.markSucceeded(ledgerJobId, null, activeLeaseToken);
						if (!marked) {
							Logger.warn({jobId: ledgerJobId.toString()}, 'Ledger markSucceeded was not applied');
							return this.retainAfterTerminalizationFailure(
								ledgerJobId,
								msg,
								'Terminalization failed after successful execution',
								activeLeaseToken,
							);
						}
					} catch (err) {
						Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markSucceeded failed');
						return this.retainAfterTerminalizationFailure(
							ledgerJobId,
							msg,
							'Terminalization failed after successful execution',
							activeLeaseToken,
						);
					}
				}
				msg.ack();
				return true;
			} catch (error) {
				const isCancelled = error instanceof JobCancelledError;
				if (isCancelled) {
					if (ledgerJobId !== null && activeLeaseToken !== null) {
						try {
							const marked = await this.ledger.markCancelled(ledgerJobId, activeLeaseToken);
							if (!marked) {
								Logger.warn({jobId: ledgerJobId.toString()}, 'Ledger markCancelled was not applied');
								await this.retainAfterTerminalizationFailure(
									ledgerJobId,
									msg,
									'Terminalization failed after cancellation',
									activeLeaseToken,
								);
								return false;
							}
						} catch (err) {
							Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger markCancelled failed');
							await this.retainAfterTerminalizationFailure(
								ledgerJobId,
								msg,
								'Terminalization failed after cancellation',
								activeLeaseToken,
							);
							return false;
						}
					}
					Logger.info({taskType, seq: msg.seq, jobId: ledgerJobId?.toString()}, 'Job cancelled by admin');
					msg.ack();
					return false;
				}
				const deliveryCount = msg.info.deliveryCount;
				const ledgerAttempts = claimedJob?.attempts;
				const ledgerMaxAttempts = claimedJob?.max_attempts;
				const hasLedgerAttemptBudget =
					ledgerJobId !== null && typeof ledgerAttempts === 'number' && typeof ledgerMaxAttempts === 'number';
				const isLastDelivery = hasLedgerAttemptBudget
					? ledgerAttempts + 1 >= ledgerMaxAttempts
					: deliveryCount >= this.maxDeliver;
				const errorMessage = error instanceof Error ? error.message : String(error);
				if (isLastDelivery) {
					Logger.error(
						{taskType, seq: msg.seq, deliveryCount, err: error},
						'Job failed on final delivery attempt, moving to dead-letter queue',
					);
					await this.publishDlqAndFinalize(
						taskType,
						msg,
						jobPayload,
						runAt,
						ledgerJobId,
						errorMessage,
						activeLeaseToken,
					);
				} else {
					Logger.error({taskType, seq: msg.seq, err: error}, 'Job failed');
					if (ledgerJobId !== null && activeLeaseToken !== null) {
						try {
							const released = await this.ledger.releaseForRetry(ledgerJobId, errorMessage, true, activeLeaseToken);
							if (!released) {
								const latest = await this.ledger.getJob(ledgerJobId);
								if (latest && ['succeeded', 'failed', 'cancelled', 'deadletter'].includes(latest.status)) {
									msg.ack();
									return false;
								}
							}
						} catch (err) {
							Logger.warn({err, jobId: ledgerJobId.toString()}, 'Ledger releaseForRetry failed');
						}
					}
					msg.nak(5000);
				}
				return false;
			}
		} finally {
			stopLeaseHeartbeat();
		}
	}
}
