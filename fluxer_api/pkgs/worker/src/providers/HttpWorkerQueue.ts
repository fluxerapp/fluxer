// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_HTTP_WORKER_TIMEOUT_MS} from '@fluxer/constants/src/Timeouts';
import type {
	EnqueueOptions,
	LeasedQueueJob,
	TracingInterface,
	WorkerJobPayload,
} from '@pkgs/worker/src/contracts/WorkerTypes';
import type {IQueueProvider} from '@pkgs/worker/src/providers/IQueueProvider';

export class HttpWorkerQueue implements IQueueProvider {
	private readonly baseUrl: string;
	private readonly timeoutMs: number;
	private readonly tracing: TracingInterface | undefined;

	constructor(options: {
		baseUrl: string;
		timeoutMs?: number | undefined;
		tracing?: TracingInterface | undefined;
	}) {
		this.baseUrl = options.baseUrl;
		this.timeoutMs = options.timeoutMs ?? DEFAULT_HTTP_WORKER_TIMEOUT_MS;
		this.tracing = options.tracing;
	}

	private async withResponse<T>(
		input: string | URL,
		init: RequestInit,
		consume: (response: Response) => Promise<T>,
	): Promise<T> {
		const controller = new AbortController();
		const timeoutId = setTimeout(() => controller.abort(), this.timeoutMs);
		try {
			const response = await fetch(input, {
				...init,
				signal: controller.signal,
			});
			return await consume(response);
		} finally {
			clearTimeout(timeoutId);
			controller.abort();
		}
	}

	private async requireSuccess(response: Response, action: string): Promise<void> {
		if (response.ok) {
			return;
		}
		const text = await response.text();
		throw new Error(`Failed to ${action}: ${response.status} ${text}`);
	}

	private async withOptionalSpan<T>(
		options: {
			name: string;
			attributes?: Record<string, unknown>;
		},
		fn: () => Promise<T>,
	): Promise<T> {
		if (this.tracing) {
			return this.tracing.withSpan(options, fn);
		}
		return fn();
	}

	private addSpanEvent(name: string, attributes?: Record<string, unknown>): void {
		if (this.tracing) {
			this.tracing.addSpanEvent(name, attributes);
		}
	}

	private setSpanAttributes(attributes: Record<string, unknown>): void {
		if (this.tracing) {
			this.tracing.setSpanAttributes(attributes);
		}
	}

	async enqueue(taskType: string, payload: WorkerJobPayload, options?: EnqueueOptions): Promise<string> {
		return await this.withOptionalSpan(
			{
				name: 'queue.enqueue',
				attributes: {
					'queue.task_type': taskType,
					'queue.priority': options?.priority ?? 0,
					'queue.max_attempts': options?.maxAttempts ?? 5,
					'queue.scheduled': options?.runAt !== undefined,
					'net.peer.name': new URL(this.baseUrl).hostname,
				},
			},
			async () => {
				const body = {
					task_type: taskType,
					payload,
					priority: options?.priority ?? 0,
					run_at: options?.runAt?.toISOString(),
					max_attempts: options?.maxAttempts ?? 5,
				};
				return this.withResponse(
					`${this.baseUrl}/enqueue`,
					{
						method: 'POST',
						headers: {'Content-Type': 'application/json'},
						body: JSON.stringify(body),
					},
					async (response) => {
						await this.requireSuccess(response, 'enqueue job');
						const jobIdResult = (await response.json()) as {
							job_id: string;
						};
						this.setSpanAttributes({'queue.job_id': jobIdResult.job_id});
						return jobIdResult.job_id;
					},
				);
			},
		);
	}

	async dequeue(taskTypes: Array<string>, limit = 1): Promise<Array<LeasedQueueJob>> {
		return await this.withOptionalSpan(
			{
				name: 'queue.dequeue',
				attributes: {
					'queue.task_types': taskTypes.join(','),
					'queue.limit': limit,
					'queue.service': 'fluxer-queue',
				},
			},
			async () => {
				this.addSpanEvent('dequeue.start');
				const url = new URL(`${this.baseUrl}/dequeue`);
				url.searchParams.set('task_types', taskTypes.join(','));
				url.searchParams.set('limit', limit.toString());
				url.searchParams.set('wait_time_ms', '0');
				return this.withResponse(url, {method: 'GET'}, async (response) => {
					await this.requireSuccess(response, 'dequeue job');
					this.addSpanEvent('dequeue.parse_response');
					const jobs = (await response.json()) as Array<LeasedQueueJob>;
					const jobCount = jobs?.length ?? 0;
					this.setSpanAttributes({
						'queue.jobs_returned': jobCount,
						'queue.empty': jobCount === 0,
					});
					this.addSpanEvent('dequeue.complete');
					return jobs ?? [];
				});
			},
		);
	}

	async upsertCron(id: string, taskType: string, payload: WorkerJobPayload, cronExpression: string): Promise<void> {
		await this.withResponse(
			`${this.baseUrl}/cron`,
			{
				method: 'POST',
				headers: {'Content-Type': 'application/json'},
				body: JSON.stringify({id, task_type: taskType, payload, cron_expression: cronExpression}),
			},
			(response) => this.requireSuccess(response, 'upsert cron job'),
		);
	}

	async complete(receipt: string): Promise<void> {
		return await this.withOptionalSpan(
			{
				name: 'queue.complete',
				attributes: {
					'queue.receipt': receipt,
				},
			},
			async () =>
				this.withResponse(
					`${this.baseUrl}/ack`,
					{
						method: 'POST',
						headers: {'Content-Type': 'application/json'},
						body: JSON.stringify({receipt}),
					},
					(response) => this.requireSuccess(response, 'complete job'),
				),
		);
	}

	async fail(receipt: string, error: string): Promise<void> {
		return await this.withOptionalSpan(
			{
				name: 'queue.fail',
				attributes: {
					'queue.receipt': receipt,
					'queue.error_message': error,
				},
			},
			async () =>
				this.withResponse(
					`${this.baseUrl}/nack`,
					{
						method: 'POST',
						headers: {'Content-Type': 'application/json'},
						body: JSON.stringify({receipt, error}),
					},
					(response) => this.requireSuccess(response, 'fail job'),
				),
		);
	}

	async cancelJob(jobId: string): Promise<boolean> {
		return this.withResponse(`${this.baseUrl}/job/${jobId}`, {method: 'DELETE'}, async (response) => {
			if (!response.ok) {
				const text = await response.text();
				if (response.status === 404) {
					return false;
				}
				throw new Error(`Failed to cancel job: ${response.status} ${text}`);
			}
			const result = (await response.json()) as {
				success: boolean;
			};
			return result.success ?? true;
		});
	}

	async retryDeadLetterJob(jobId: string): Promise<boolean> {
		return this.withResponse(`${this.baseUrl}/retry/${jobId}`, {method: 'POST'}, async (response) => {
			if (!response.ok) {
				const text = await response.text();
				if (response.status === 404) {
					return false;
				}
				throw new Error(`Failed to retry job: ${response.status} ${text}`);
			}
			return true;
		});
	}
}
