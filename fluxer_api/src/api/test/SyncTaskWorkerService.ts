// SPDX-License-Identifier: AGPL-3.0-or-later

import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

type TaskHandlerMap = Record<string, WorkerTaskHandler>;

let nextSyntheticJobId = 1n;

export interface SyncTaskWorkerServiceOptions {
	deferred?: boolean;
}

export interface QueuedWorkerJob {
	jobId: bigint;
	taskType: string;
	payload: WorkerJobPayload;
	jobKey: string | undefined;
	runAt: Date | undefined;
	options: WorkerJobOptions | undefined;
}

export interface DeadLetterWorkerJob extends QueuedWorkerJob {
	attempts: number;
	error: unknown;
}

export interface DrainOptions {
	maxJobs?: number;
	maxAttempts?: number;
}

export interface DrainResult {
	ran: number;
	failed: number;
	remaining: number;
}

interface PendingJob {
	job: QueuedWorkerJob;
	attempts: number;
}

export class SyncTaskWorkerService implements IWorkerService {
	private handlers: TaskHandlerMap;
	private readonly deferred: boolean;
	private queue: Array<PendingJob> = [];
	private readonly activeJobKeys = new Set<string>();
	readonly recorded: Array<QueuedWorkerJob> = [];
	readonly deduped: Array<QueuedWorkerJob> = [];
	readonly deadLetters: Array<DeadLetterWorkerJob> = [];
	failure: ((taskType: string, payload: WorkerJobPayload) => Error | null) | null = null;

	constructor(handlers: TaskHandlerMap, options: SyncTaskWorkerServiceOptions = {}) {
		this.handlers = handlers;
		this.deferred = options.deferred ?? false;
	}

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint> {
		const jobId = nextSyntheticJobId++;
		if (this.deferred) {
			this.enqueue({
				jobId,
				taskType,
				payload,
				jobKey: options?.jobKey,
				runAt: options?.runAt,
				options,
			});
			return jobId;
		}
		const handler = this.handlers[taskType];
		if (handler) {
			await handler(payload, {
				logger: new NoopLogger(),
				jobId,
				addJob: async () => 0n,
				reportProgress: async () => {},
				shouldCancel: async () => false,
				setContextLink: async () => {},
			});
		}
		return jobId;
	}

	async cancelJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	get pending(): ReadonlyArray<QueuedWorkerJob> {
		return this.queue.map((entry) => entry.job);
	}

	byTask(taskType: string): Array<QueuedWorkerJob> {
		return this.recorded.filter((job) => job.taskType === taskType);
	}

	async drain({maxJobs = 10_000, maxAttempts = 3}: DrainOptions = {}): Promise<DrainResult> {
		let ran = 0;
		let failed = 0;
		while (this.queue.length > 0 && ran < maxJobs) {
			const entry = this.queue.shift()!;
			ran++;
			entry.attempts++;
			try {
				await this.runJob(entry, maxAttempts);
			} catch (error) {
				failed++;
				if (entry.attempts < maxAttempts) {
					this.queue.push(entry);
				} else {
					this.deadLetters.push({...entry.job, attempts: entry.attempts, error});
				}
			}
		}
		if (this.queue.length === 0) {
			this.activeJobKeys.clear();
		}
		return {ran, failed, remaining: this.queue.length};
	}

	clear(): void {
		this.queue = [];
		this.activeJobKeys.clear();
		this.recorded.length = 0;
		this.deduped.length = 0;
		this.deadLetters.length = 0;
	}

	private enqueue(job: QueuedWorkerJob): void {
		const error = this.failure?.(job.taskType, job.payload) ?? null;
		if (error) {
			throw error;
		}
		if (job.jobKey !== undefined) {
			if (this.activeJobKeys.has(job.jobKey)) {
				this.deduped.push(job);
				return;
			}
			this.activeJobKeys.add(job.jobKey);
		}
		this.recorded.push(job);
		this.queue.push({job, attempts: 0});
	}

	private async runJob(entry: PendingJob, maxAttempts: number): Promise<void> {
		const handler = this.handlers[entry.job.taskType];
		if (!handler) {
			return;
		}
		const helpers: WorkerTaskHelpers = {
			logger: new NoopLogger(),
			jobId: entry.job.jobId,
			attempt: {isLastAttempt: entry.attempts >= maxAttempts},
			addJob: async (taskType, payload, options) => {
				const jobId = nextSyntheticJobId++;
				this.enqueue({jobId, taskType, payload, jobKey: options?.jobKey, runAt: options?.runAt, options});
				return jobId;
			},
			reportProgress: async () => {},
			shouldCancel: async () => false,
			setContextLink: async () => {},
		};
		await handler(entry.job.payload, helpers);
	}
}
