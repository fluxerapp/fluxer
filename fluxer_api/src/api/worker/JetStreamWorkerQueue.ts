// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {
	AckPolicy,
	DiscardPolicy,
	type JetStreamManager,
	NatsError,
	nanos,
	RetentionPolicy,
	StorageType,
	type StreamConfig,
} from 'nats';
import {Logger} from '../Logger';
import type {WorkerLaneDefinition} from './WorkerLaneConfig';
import {WorkerQueueOverflowError} from './WorkerQueueOverflowError';

const STREAM_NAME = 'JOBS';
const SUBJECT_PREFIX = 'jobs.';
const MAX_AGE_MS = 7 * 24 * 60 * 60 * 1000;
const LEGACY_CONSUMER_NAME = 'workers';
const DLQ_STREAM_NAME = 'JOBS_DLQ';
const DLQ_SUBJECT_PREFIX = 'dlq.';
const DLQ_MAX_AGE_MS = 30 * 24 * 60 * 60 * 1000;
const STREAM_MAX_MSGS = 2_000_000;
const STREAM_MAX_BYTES = 8 * 1024 * 1024 * 1024;
const STREAM_MIN_BYTES = 64 * 1024 * 1024;
const STREAM_MAX_MSGS_PER_SUBJECT = 250_000;
const DLQ_MAX_BYTES = 64 * 1024 * 1024;
const DLQ_MIN_BYTES = 8 * 1024 * 1024;
const STREAM_FULL_ERR_CODES = new Set([10023, 10077]);
const STREAM_NO_STORAGE_ERR_CODE = 10047;
const STREAM_NAME_IN_USE_ERR_CODE = 10058;

const STREAM_LIMITS = {
	max_msgs: STREAM_MAX_MSGS,
	max_bytes: STREAM_MAX_BYTES,
	max_msgs_per_subject: STREAM_MAX_MSGS_PER_SUBJECT,
	discard: DiscardPolicy.New,
	discard_new_per_subject: true,
} satisfies Partial<StreamConfig>;

function jsErrorCode(error: unknown): number | null {
	if (!(error instanceof NatsError)) {
		return null;
	}
	return error.jsError()?.err_code ?? null;
}

function describeStreamRejection(error: unknown): string | null {
	if (!(error instanceof NatsError)) {
		return null;
	}
	const apiError = error.jsError();
	if (apiError === null || !STREAM_FULL_ERR_CODES.has(apiError.err_code ?? 0)) {
		return null;
	}
	return apiError.description ?? 'stream rejected the publish';
}

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
		const existingConfig = await this.readStreamConfig(jsm, STREAM_NAME);
		if (existingConfig === null) {
			await this.addStream(jsm);
		} else if (!this.hasStreamLimits(existingConfig)) {
			await this.applyStreamLimits(jsm, existingConfig);
		}
		this.streamReady = true;
	}

	private async readStreamConfig(jsm: JetStreamManager, name: string): Promise<StreamConfig | null> {
		try {
			return (await jsm.streams.info(name)).config;
		} catch {
			return null;
		}
	}

	private async fitToStorageBudget(
		startBytes: number,
		minBytes: number,
		apply: (maxBytes: number) => Promise<unknown>,
	): Promise<number> {
		let maxBytes = startBytes;
		for (;;) {
			try {
				await apply(maxBytes);
				return maxBytes;
			} catch (error) {
				if (jsErrorCode(error) !== STREAM_NO_STORAGE_ERR_CODE || maxBytes <= minBytes) {
					throw error;
				}
				maxBytes = Math.floor(maxBytes / 2);
			}
		}
	}

	private async adoptConcurrentStream(jsm: JetStreamManager, name: string, error: unknown): Promise<boolean> {
		if (jsErrorCode(error) !== STREAM_NAME_IN_USE_ERR_CODE) {
			return false;
		}
		const config = await this.readStreamConfig(jsm, name);
		if (config === null) {
			return false;
		}
		Logger.info({stream: name, max_bytes: config.max_bytes}, 'Stream was created concurrently, adopting it');
		return true;
	}

	private async addStream(jsm: JetStreamManager): Promise<void> {
		let maxBytes: number;
		try {
			maxBytes = await this.fitToStorageBudget(STREAM_MAX_BYTES, STREAM_MIN_BYTES, (bytes) =>
				jsm.streams.add({
					name: STREAM_NAME,
					subjects: [`${SUBJECT_PREFIX}>`],
					retention: RetentionPolicy.Workqueue,
					storage: StorageType.File,
					max_age: nanos(MAX_AGE_MS),
					duplicate_window: nanos(2 * 60 * 1000),
					num_replicas: 1,
					...STREAM_LIMITS,
					max_bytes: bytes,
				}),
			);
		} catch (error) {
			if (await this.adoptConcurrentStream(jsm, STREAM_NAME, error)) {
				return;
			}
			if (jsErrorCode(error) === STREAM_NO_STORAGE_ERR_CODE) {
				Logger.error(
					{err: error, stream: STREAM_NAME, max_bytes: STREAM_MIN_BYTES},
					'Jobs stream does not fit the JetStream storage budget at its smallest size, free disk space on the NATS store',
				);
			}
			throw error;
		}
		if (maxBytes !== STREAM_MAX_BYTES) {
			Logger.warn(
				{stream: STREAM_NAME, max_bytes: maxBytes},
				'Created the jobs stream below its target size to fit the JetStream storage budget',
			);
		}
	}

	private hasStreamLimits(config: StreamConfig): boolean {
		return (
			config.max_msgs === STREAM_MAX_MSGS &&
			config.max_bytes > 0 &&
			config.max_msgs_per_subject === STREAM_MAX_MSGS_PER_SUBJECT &&
			config.discard === DiscardPolicy.New &&
			config.discard_new_per_subject
		);
	}

	private async applyStreamLimits(jsm: JetStreamManager, existingConfig: StreamConfig): Promise<void> {
		const startBytes = existingConfig.max_bytes > 0 ? existingConfig.max_bytes : STREAM_MAX_BYTES;
		try {
			const maxBytes = await this.fitToStorageBudget(startBytes, STREAM_MIN_BYTES, (bytes) =>
				jsm.streams.update(STREAM_NAME, {...STREAM_LIMITS, max_bytes: bytes}),
			);
			Logger.info({stream: STREAM_NAME, ...STREAM_LIMITS, max_bytes: maxBytes}, 'Applied jobs stream limits');
		} catch (error) {
			if (jsErrorCode(error) !== STREAM_NO_STORAGE_ERR_CODE) {
				Logger.error({err: error, stream: STREAM_NAME}, 'Failed to apply jobs stream limits');
				return;
			}
			Logger.warn(
				{err: error, stream: STREAM_NAME, max_bytes: existingConfig.max_bytes},
				'JetStream has no room to bound the jobs stream, it keeps the limits it already has',
			);
		}
	}

	async ensureDlqStream(): Promise<void> {
		if (this.dlqStreamReady) {
			return;
		}
		const jsm = await this.connectionManager.getJetStreamManager();
		if ((await this.readStreamConfig(jsm, DLQ_STREAM_NAME)) === null) {
			if (!(await this.addDlqStream(jsm))) {
				return;
			}
		}
		this.dlqStreamReady = true;
	}

	private async addDlqStream(jsm: JetStreamManager): Promise<boolean> {
		try {
			const maxBytes = await this.fitToStorageBudget(DLQ_MAX_BYTES, DLQ_MIN_BYTES, (bytes) =>
				jsm.streams.add({
					name: DLQ_STREAM_NAME,
					subjects: [`${DLQ_SUBJECT_PREFIX}>`],
					retention: RetentionPolicy.Limits,
					storage: StorageType.File,
					max_age: nanos(DLQ_MAX_AGE_MS),
					num_replicas: 1,
					max_bytes: bytes,
					discard: DiscardPolicy.Old,
				}),
			);
			Logger.info({stream: DLQ_STREAM_NAME, max_bytes: maxBytes}, 'Dead-letter stream created');
			return true;
		} catch (error) {
			if (await this.adoptConcurrentStream(jsm, DLQ_STREAM_NAME, error)) {
				return true;
			}
			if (jsErrorCode(error) !== STREAM_NO_STORAGE_ERR_CODE) {
				throw error;
			}
			Logger.warn(
				{err: error, stream: DLQ_STREAM_NAME},
				'JetStream has no room for the dead-letter stream, failed jobs stay in the jobs stream until they expire',
			);
			return false;
		}
	}

	async ensureConsumers(lanes: ReadonlyArray<WorkerLaneDefinition>): Promise<void> {
		if (this.consumersReady) {
			return;
		}
		const jsm = await this.connectionManager.getJetStreamManager();
		for (const lane of lanes) {
			const filterSubjects = [...lane.taskTypes, ...lane.retiredTaskTypes].map((t) => `${SUBJECT_PREFIX}${t}`);
			const config = {
				durable_name: lane.consumerName,
				ack_policy: AckPolicy.Explicit,
				max_deliver: lane.maxDeliver,
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
		try {
			const ack = await js.publish(subject, body, {
				msgID,
			});
			const jobId = `${ack.seq}`;
			return jobId;
		} catch (error) {
			const rejection = describeStreamRejection(error);
			if (rejection === null) {
				throw error;
			}
			throw new WorkerQueueOverflowError(taskType, rejection);
		}
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
			msgID: randomUUID(),
		});
	}

	getStreamName(): string {
		return STREAM_NAME;
	}

	getConnectionManager(): JetStreamConnectionManager {
		return this.connectionManager;
	}
}
