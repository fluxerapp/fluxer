// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {AckPolicy, nanos, RetentionPolicy, StorageType} from 'nats';
import {WORKER_QUEUE_MAX_AGE_MS} from '../jobs/JobSchedulingPolicy';
import {Logger} from '../Logger';
import type {WorkerLaneDefinition} from './WorkerLaneConfig';

const STREAM_NAME = 'JOBS';
const SUBJECT_PREFIX = 'jobs.';

export {WORKER_QUEUE_MAX_AGE_MS, WORKER_QUEUE_RECOVERY_WINDOW_MS} from '../jobs/JobSchedulingPolicy';

const LEGACY_CONSUMER_NAME = 'workers';
const DLQ_STREAM_NAME = 'JOBS_DLQ';
const DLQ_SUBJECT_PREFIX = 'dlq.';
const DLQ_MAX_AGE_MS = 30 * 24 * 60 * 60 * 1000;

export class JetStreamWorkerQueue {
	private readonly connectionManager: JetStreamConnectionManager;
	private streamReady = false;
	private dlqStreamReady = false;
	private consumersReady = false;

	constructor(connectionManager: JetStreamConnectionManager) {
		this.connectionManager = connectionManager;
	}

	async ensureStream(): Promise<void> {
		if (this.streamReady) {
			return;
		}
		const jsm = await this.connectionManager.getJetStreamManager();
		let info = null;
		try {
			info = await jsm.streams.info(STREAM_NAME);
		} catch {
			await jsm.streams.add({
				name: STREAM_NAME,
				subjects: [`${SUBJECT_PREFIX}>`],
				retention: RetentionPolicy.Workqueue,
				storage: StorageType.File,
				max_age: nanos(WORKER_QUEUE_MAX_AGE_MS),
				duplicate_window: nanos(2 * 60 * 1000),
				num_replicas: 1,
			});
		}
		if (info) {
			await jsm.streams.update(STREAM_NAME, {
				...info.config,
				max_age: nanos(WORKER_QUEUE_MAX_AGE_MS),
			});
		}
		this.streamReady = true;
	}

	async ensureDlqStream(): Promise<void> {
		if (this.dlqStreamReady) {
			return;
		}
		const jsm = await this.connectionManager.getJetStreamManager();
		let info = null;
		try {
			info = await jsm.streams.info(DLQ_STREAM_NAME);
		} catch {
			await jsm.streams.add({
				name: DLQ_STREAM_NAME,
				subjects: [`${DLQ_SUBJECT_PREFIX}>`],
				retention: RetentionPolicy.Limits,
				storage: StorageType.File,
				max_age: nanos(DLQ_MAX_AGE_MS),
				duplicate_window: nanos(DLQ_MAX_AGE_MS),
				num_replicas: 1,
			});
		}
		if (info) {
			await jsm.streams.update(DLQ_STREAM_NAME, {
				...info.config,
				max_age: nanos(DLQ_MAX_AGE_MS),
				duplicate_window: nanos(DLQ_MAX_AGE_MS),
			});
		}
		this.dlqStreamReady = true;
	}

	async ensureConsumers(lanes: ReadonlyArray<WorkerLaneDefinition>): Promise<void> {
		if (this.consumersReady) {
			return;
		}
		const jsm = await this.connectionManager.getJetStreamManager();
		for (const lane of lanes) {
			const filterSubjects = lane.taskTypes.map((t) => `${SUBJECT_PREFIX}${t}`);
			const config = {
				durable_name: lane.consumerName,
				ack_policy: AckPolicy.Explicit,
				max_deliver: -1,
				ack_wait: nanos(lane.ackWaitMs),
				max_ack_pending: lane.maxAckPending,
				filter_subjects: filterSubjects,
			};
			try {
				await jsm.consumers.add(STREAM_NAME, config);
				Logger.info({lane: lane.name, consumer: lane.consumerName}, 'Consumer created');
			} catch (error) {
				const message = error instanceof Error ? error.message : String(error);
				if (message.includes('consumer already exists') || message.includes('consumer name already')) {
					Logger.info(
						{lane: lane.name, consumer: lane.consumerName},
						'Consumer already exists, deleting and recreating with updated config',
					);
					try {
						await jsm.consumers.delete(STREAM_NAME, lane.consumerName);
						await jsm.consumers.add(STREAM_NAME, config);
						Logger.info({lane: lane.name, consumer: lane.consumerName}, 'Consumer recreated');
					} catch (recreateError) {
						Logger.error(
							{lane: lane.name, consumer: lane.consumerName, err: recreateError},
							'Failed to recreate consumer',
						);
						throw recreateError;
					}
				} else {
					throw error;
				}
			}
		}
		this.consumersReady = true;
	}

	async migrateOldConsumer(): Promise<void> {
		const jsm = await this.connectionManager.getJetStreamManager();
		try {
			await jsm.consumers.info(STREAM_NAME, LEGACY_CONSUMER_NAME);
			await jsm.consumers.delete(STREAM_NAME, LEGACY_CONSUMER_NAME);
			Logger.info('Legacy consumer deleted, lane consumers will handle any unacked messages');
		} catch {
			Logger.debug('Legacy consumer does not exist, nothing to migrate');
		}
	}

	async ensureInfrastructure(lanes: ReadonlyArray<WorkerLaneDefinition>): Promise<void> {
		await this.ensureStream();
		await this.ensureDlqStream();
		await this.migrateOldConsumer();
		await this.ensureConsumers(lanes);
	}

	async enqueue(
		taskType: string,
		payload: WorkerJobPayload,
		options?: {
			runAt?: Date;
			maxAttempts?: number;
			priority?: number;
			jobKey?: string;
		},
	): Promise<string> {
		const js = this.connectionManager.getJetStreamClient();
		const subject = `${SUBJECT_PREFIX}${taskType}`;
		const body = JSON.stringify({
			payload,
			run_at: options?.runAt?.toISOString(),
			max_attempts: options?.maxAttempts ?? 5,
			priority: options?.priority ?? 0,
			created_at: new Date().toISOString(),
		});
		const msgID = options?.jobKey ? `${taskType}:${options.jobKey}` : randomUUID();
		const ack = await js.publish(subject, body, {
			msgID,
		});
		const jobId = `${ack.seq}`;
		return jobId;
	}

	async publishToDlq(
		taskType: string,
		originalPayload: Record<string, unknown>,
		meta: {
			originalSeq: number;
			errorMessage: string;
			deliveryCount: number;
			lane: string;
			runAt?: string;
		},
	): Promise<void> {
		const js = this.connectionManager.getJetStreamClient();
		const subject = `${DLQ_SUBJECT_PREFIX}${taskType}`;
		const body = JSON.stringify({
			original_subject: `${SUBJECT_PREFIX}${taskType}`,
			original_seq: meta.originalSeq,
			payload: originalPayload,
			error_message: meta.errorMessage,
			delivery_count: meta.deliveryCount,
			lane: meta.lane,
			run_at: meta.runAt,
			failed_at: new Date().toISOString(),
		});
		await js.publish(subject, body, {
			msgID: `dlq:${meta.lane}:${taskType}:${meta.originalSeq}`,
		});
	}

	getStreamName(): string {
		return STREAM_NAME;
	}

	getConnectionManager(): JetStreamConnectionManager {
		return this.connectionManager;
	}
}
