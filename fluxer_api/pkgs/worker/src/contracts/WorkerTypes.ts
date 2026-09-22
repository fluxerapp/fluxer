// SPDX-License-Identifier: AGPL-3.0-or-later

export type WorkerJobPayload = Record<string, unknown>;

export interface WorkerRuntimeConfig {
	workerId?: string | undefined;
	concurrency?: number | undefined;
	taskTypes?: Array<string> | undefined;
}

export interface WorkerQueueConfig {
	queueBaseUrl: string;
	requestTimeoutMs?: number | undefined;
}

export interface WorkerConfig extends WorkerRuntimeConfig, WorkerQueueConfig {}

export interface TracingInterface {
	withSpan<T>(
		options: {
			name: string;
			attributes?: Record<string, unknown>;
		},
		fn: () => Promise<T>,
	): Promise<T>;
	addSpanEvent(name: string, attributes?: Record<string, unknown>): void;
	setSpanAttributes(attributes: Record<string, unknown>): void;
}

export interface QueueJob {
	id: string;
	task_type: string;
	payload: WorkerJobPayload;
	priority: number;
	run_at: string;
	created_at: string;
	attempts: number;
	max_attempts: number;
	error?: string | null;
	deduplication_id?: string | null;
}

export interface LeasedQueueJob {
	receipt: string;
	visibility_deadline: string;
	job: QueueJob;
}

export interface EnqueueOptions {
	runAt?: Date | undefined;
	maxAttempts?: number | undefined;
	priority?: number | undefined;
}

export interface WorkerJobOptions {
	queueName?: string | undefined;
	runAt?: Date | undefined;
	maxAttempts?: number | undefined;
	jobKey?: string | undefined;
	priority?: number | undefined;
	flags?: Array<string> | undefined;
	requestedByUserId?: bigint | undefined;
	auditLogReason?: string | undefined;
	skipLedger?: boolean | undefined;
	requireLedger?: boolean | undefined;
}
