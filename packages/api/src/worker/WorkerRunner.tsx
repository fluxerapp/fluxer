/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {randomUUID} from 'node:crypto';
import {Logger} from '@fluxer/api/src/Logger';
import {getWorkerService} from '@fluxer/api/src/middleware/ServiceRegistry';
import {addSpanEvent, setSpanAttributes, withSpan} from '@fluxer/api/src/telemetry/Tracing';
import type {JetStreamWorkerQueue} from '@fluxer/api/src/worker/JetStreamWorkerQueue';
import type {IWorkerService} from '@fluxer/worker/src/contracts/IWorkerService';
import type {WorkerTaskHandler} from '@fluxer/worker/src/contracts/WorkerTask';
import type {ConsumerMessages, JsMsg} from 'nats';

interface WorkerRunnerOptions {
	tasks: Record<string, WorkerTaskHandler>;
	queue: JetStreamWorkerQueue;
	workerId?: string;
	concurrency?: number;
}

export class WorkerRunner {
	private readonly tasks: Record<string, WorkerTaskHandler>;
	private readonly queue: JetStreamWorkerQueue;
	private readonly workerId: string;
	private readonly concurrency: number;
	private readonly workerService: IWorkerService;
	private running = false;
	private consumerMessages: ConsumerMessages | null = null;

	constructor(options: WorkerRunnerOptions) {
		this.tasks = options.tasks;
		this.queue = options.queue;
		this.workerId = options.workerId ?? `worker-${randomUUID()}`;
		this.concurrency = options.concurrency ?? 1;
		this.workerService = getWorkerService();
	}

	async start(): Promise<void> {
		if (this.running) {
			Logger.warn({workerId: this.workerId}, 'Worker already running');
			return;
		}

		this.running = true;

		Logger.info({workerId: this.workerId, concurrency: this.concurrency}, 'Worker starting');

		const js = this.queue.getConnectionManager().getJetStreamClient();
		const consumer = await js.consumers.get(this.queue.getStreamName(), this.queue.getConsumerName());

		this.consumerMessages = await consumer.consume({
			max_messages: this.concurrency,
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

			const taskType = msg.subject.startsWith('jobs.') ? msg.subject.slice(5) : msg.subject;

			Logger.info(
				{
					workerId: this.workerId,
					taskType,
					seq: msg.seq,
					redelivered: msg.redelivered,
				},
				'Processing job',
			);

			const succeeded = await this.processJob(taskType, msg);
			if (succeeded) {
				Logger.info({workerId: this.workerId, taskType, seq: msg.seq}, 'Job completed successfully');
			}
		}

		Logger.info({workerId: this.workerId}, 'Worker message iterator ended');
	}

	private async processJob(taskType: string, msg: JsMsg): Promise<boolean> {
		return await withSpan(
			{
				name: 'worker.process_job',
				attributes: {
					'worker.id': this.workerId,
					'job.seq': msg.seq,
					'job.task_type': taskType,
					'job.redelivered': msg.redelivered,
				},
			},
			async () => {
				const task = this.tasks[taskType];
				if (!task) {
					Logger.error({taskType, seq: msg.seq}, 'Unknown task type, terminating message');
					msg.term(`unknown task type: ${taskType}`);
					return false;
				}

				let jobPayload: Record<string, unknown> = {};
				try {
					const decoded = JSON.parse(new TextDecoder().decode(msg.data)) as {payload?: Record<string, unknown>};
					jobPayload = decoded.payload ?? {};
				} catch {
					Logger.error({taskType, seq: msg.seq}, 'Failed to decode job payload, terminating message');
					msg.term('invalid payload');
					return false;
				}

				addSpanEvent('job.execution.start');

				try {
					await task(jobPayload as never, {
						logger: Logger.child({taskType, seq: msg.seq}),
						addJob: this.workerService.addJob.bind(this.workerService),
					});

					addSpanEvent('job.execution.success');
					setSpanAttributes({'job.status': 'success'});

					msg.ack();
					return true;
				} catch (error) {
					Logger.error({taskType, seq: msg.seq, err: error}, 'Job failed');

					setSpanAttributes({
						'job.status': 'failed',
						'job.error': error instanceof Error ? error.message : String(error),
					});
					addSpanEvent('job.execution.failed', {
						error: error instanceof Error ? error.message : String(error),
					});

					msg.nak(5000);
					return false;
				}
			},
		);
	}
}
