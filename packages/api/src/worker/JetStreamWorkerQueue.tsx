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
import {addSpanEvent, setSpanAttributes, withSpan} from '@fluxer/api/src/telemetry/Tracing';
import type {JetStreamConnectionManager} from '@fluxer/nats/src/JetStreamConnectionManager';
import type {WorkerJobPayload} from '@fluxer/worker/src/contracts/WorkerTypes';
import {AckPolicy, nanos, RetentionPolicy, StorageType} from 'nats';

const STREAM_NAME = 'JOBS';
const CONSUMER_NAME = 'workers';
const SUBJECT_PREFIX = 'jobs.';
const MAX_AGE_MS = 7 * 24 * 60 * 60 * 1000;
const MAX_DELIVER = 5;
const ACK_WAIT_MS = 30_000;
const MAX_ACK_PENDING = 100;

export class JetStreamWorkerQueue {
	private readonly connectionManager: JetStreamConnectionManager;
	private streamReady = false;
	private consumerReady = false;

	constructor(connectionManager: JetStreamConnectionManager) {
		this.connectionManager = connectionManager;
	}

	async ensureStream(): Promise<void> {
		if (this.streamReady) {
			return;
		}
		const jsm = await this.connectionManager.getJetStreamManager();
		try {
			await jsm.streams.info(STREAM_NAME);
		} catch {
			await jsm.streams.add({
				name: STREAM_NAME,
				subjects: [`${SUBJECT_PREFIX}>`],
				retention: RetentionPolicy.Workqueue,
				storage: StorageType.File,
				max_age: nanos(MAX_AGE_MS),
				num_replicas: 1,
			});
		}
		this.streamReady = true;
	}

	async ensureConsumer(): Promise<void> {
		if (this.consumerReady) {
			return;
		}
		const jsm = await this.connectionManager.getJetStreamManager();
		try {
			await jsm.consumers.info(STREAM_NAME, CONSUMER_NAME);
		} catch {
			await jsm.consumers.add(STREAM_NAME, {
				durable_name: CONSUMER_NAME,
				ack_policy: AckPolicy.Explicit,
				max_deliver: MAX_DELIVER,
				ack_wait: nanos(ACK_WAIT_MS),
				max_ack_pending: MAX_ACK_PENDING,
			});
		}
		this.consumerReady = true;
	}

	async ensureInfrastructure(): Promise<void> {
		await this.ensureStream();
		await this.ensureConsumer();
	}

	async enqueue(
		taskType: string,
		payload: WorkerJobPayload,
		options?: {runAt?: Date; maxAttempts?: number; priority?: number},
	): Promise<string> {
		return await withSpan(
			{
				name: 'queue.enqueue',
				attributes: {
					'queue.task_type': taskType,
					'queue.priority': options?.priority ?? 0,
					'queue.max_attempts': options?.maxAttempts ?? 5,
					'queue.scheduled': options?.runAt !== undefined,
				},
			},
			async () => {
				const js = this.connectionManager.getJetStreamClient();
				const subject = `${SUBJECT_PREFIX}${taskType}`;
				const body = JSON.stringify({
					payload,
					run_at: options?.runAt?.toISOString(),
					max_attempts: options?.maxAttempts ?? 5,
					priority: options?.priority ?? 0,
					created_at: new Date().toISOString(),
				});

				const ack = await js.publish(subject, body, {
					msgID: randomUUID(),
				});

				const jobId = `${ack.seq}`;
				setSpanAttributes({'queue.job_id': jobId});
				addSpanEvent('enqueue.complete');
				return jobId;
			},
		);
	}

	getStreamName(): string {
		return STREAM_NAME;
	}

	getConsumerName(): string {
		return CONSUMER_NAME;
	}

	getConnectionManager(): JetStreamConnectionManager {
		return this.connectionManager;
	}
}
