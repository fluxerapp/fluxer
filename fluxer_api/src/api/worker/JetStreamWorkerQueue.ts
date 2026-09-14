// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {Logger} from '@app/api/Logger';
import type {WorkerLaneDefinition} from '@app/api/worker/WorkerLaneConfig';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {
	AckPolicy,
	type ConsumerInfo,
	type ConsumerUpdateConfig,
	DeliverPolicy,
	DiscardPolicy,
	type JetStreamManager,
	NatsError,
	nanos,
	ReplayPolicy,
	RetentionPolicy,
	StorageType,
	type StreamConfig,
} from 'nats';

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
const DLQ_PUBLISH_TIMEOUT_MS = 5000;
const DUPLICATE_WINDOW_MS = 2 * 60 * 1000;
const STREAM_FULL_ERR_CODES = new Set([10023, 10077]);
const STREAM_NO_STORAGE_ERR_CODE = 10047;
const STREAM_NAME_IN_USE_ERR_CODE = 10058;
const STREAM_NOT_FOUND_ERR_CODE = 10059;
const CONSUMER_NOT_FOUND_ERR_CODE = 10014;
const CONSUMER_EXISTS_ERR_CODES = new Set([10013, 10148]);

interface WorkerStreamDefinition {
	name: string;
	subject: string;
	retention: RetentionPolicy;
	maxAgeMs: number;
	minBytes: number;
	maxMessages: number;
	maxMessagesPerSubject: number;
	discard: DiscardPolicy;
	discardNewPerSubject: boolean;
}

interface WorkerStreamConfiguration extends StreamConfig {
	persist_mode?: string;
	allow_msg_ttl?: boolean;
	allow_msg_counter?: boolean;
}

const JOBS_STREAM: WorkerStreamDefinition = {
	name: STREAM_NAME,
	subject: `${SUBJECT_PREFIX}>`,
	retention: RetentionPolicy.Workqueue,
	maxAgeMs: MAX_AGE_MS,
	minBytes: STREAM_MIN_BYTES,
	maxMessages: STREAM_MAX_MSGS,
	maxMessagesPerSubject: STREAM_MAX_MSGS_PER_SUBJECT,
	discard: DiscardPolicy.New,
	discardNewPerSubject: true,
};

const DEAD_LETTER_STREAM: WorkerStreamDefinition = {
	name: DLQ_STREAM_NAME,
	subject: `${DLQ_SUBJECT_PREFIX}>`,
	retention: RetentionPolicy.Limits,
	maxAgeMs: DLQ_MAX_AGE_MS,
	minBytes: DLQ_MIN_BYTES,
	maxMessages: -1,
	maxMessagesPerSubject: -1,
	discard: DiscardPolicy.Old,
	discardNewPerSubject: false,
};

export interface WorkerDeadLetterMetadata {
	messageId: string;
	originalSeq: number;
	errorMessage: string;
	deliveryCount: number;
	lane: string;
	runAt?: string;
}

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
	private streamSetup: Promise<void> | null = null;
	private dlqStreamSetup: Promise<void> | null = null;

	constructor(connectionManager: JetStreamConnectionManager) {
		this.connectionManager = connectionManager;
	}

	async ensureStream(): Promise<void> {
		if (this.streamReady) return;
		this.streamSetup ??= this.initializeStream().finally(() => {
			this.streamSetup = null;
		});
		await this.streamSetup;
	}

	private async initializeStream(): Promise<void> {
		const jsm = await this.connectionManager.getJetStreamManager();
		const existingConfig = await this.readStreamConfig(jsm, JOBS_STREAM);
		if (existingConfig === null) {
			await this.addStream(jsm);
		} else {
			await this.applyStreamLimits(jsm, existingConfig);
		}
		this.streamReady = true;
	}

	private async oversizedSubjects(jsm: JetStreamManager): Promise<Array<[string, number]> | null> {
		try {
			const info = await jsm.streams.info(STREAM_NAME, {subjects_filter: JOBS_STREAM.subject});
			const subjects = info.state.subjects;
			if (subjects === undefined) return [];
			return Object.entries(subjects).filter(([, count]) => count > STREAM_MAX_MSGS_PER_SUBJECT);
		} catch (error) {
			Logger.warn({err: error, stream: STREAM_NAME}, 'Could not read jobs stream subject counts, leaving limits alone');
			return null;
		}
	}

	private async applyStreamLimits(jsm: JetStreamManager, existing: WorkerStreamConfiguration): Promise<void> {
		const unmanaged = this.unmanagedStreamSettings(existing, JOBS_STREAM);
		if (unmanaged.length > 0) {
			Logger.warn(
				{stream: STREAM_NAME, unmanaged},
				'Jobs stream has settings this service does not own, change them explicitly with publishers and workers stopped',
			);
		}
		const drifted = this.diffStreamLimits(existing, JOBS_STREAM);
		if (drifted.length === 0) return;
		const oversized = await this.oversizedSubjects(jsm);
		if (oversized === null) return;
		if (oversized.length > 0) {
			Logger.error(
				{stream: STREAM_NAME, drifted, oversized, limit: STREAM_MAX_MSGS_PER_SUBJECT},
				'Jobs stream limits are out of date but tightening them would drop queued jobs, drain these subjects then migrate the stream explicitly',
			);
			return;
		}
		const startBytes = existing.max_bytes > 0 ? existing.max_bytes : STREAM_MAX_BYTES;
		try {
			const maxBytes = await this.fitToStorageBudget(startBytes, STREAM_MIN_BYTES, (bytes) =>
				jsm.streams.update(STREAM_NAME, {
					max_msgs: JOBS_STREAM.maxMessages,
					max_bytes: bytes,
					max_msgs_per_subject: JOBS_STREAM.maxMessagesPerSubject,
					discard: JOBS_STREAM.discard,
					discard_new_per_subject: JOBS_STREAM.discardNewPerSubject,
				}),
			);
			Logger.info({stream: STREAM_NAME, drifted, max_bytes: maxBytes}, 'Applied jobs stream limits');
		} catch (error) {
			Logger.error(
				{err: error, stream: STREAM_NAME, drifted},
				'Could not apply jobs stream limits, continuing with the limits the stream already has',
			);
		}
	}

	private async readStreamConfig(jsm: JetStreamManager, stream: WorkerStreamDefinition): Promise<StreamConfig | null> {
		try {
			const {config} = await jsm.streams.info(stream.name);
			this.assertStreamIdentity(config, stream);
			return config;
		} catch (error) {
			if (jsErrorCode(error) === STREAM_NOT_FOUND_ERR_CODE) return null;
			throw error;
		}
	}

	private assertStreamIdentity(config: WorkerStreamConfiguration, stream: WorkerStreamDefinition): void {
		const incompatible: Array<string> = [];
		if (config.name !== stream.name) incompatible.push('name');
		if (config.subjects?.length !== 1 || config.subjects[0] !== stream.subject) incompatible.push('subjects');
		if (config.retention !== stream.retention) incompatible.push('retention');
		if (config.storage !== StorageType.File) incompatible.push('storage');
		for (const field of ['sealed', 'no_ack', 'mirror', 'republish', 'subject_transform'] as const) {
			if (config[field]) incompatible.push(field);
		}
		if (config.sources?.length) incompatible.push('sources');
		if (incompatible.length > 0) {
			throw new Error(
				`Worker stream ${stream.name} has incompatible ${incompatible.join(', ')} configuration, stop publishers and workers and migrate it explicitly before startup`,
			);
		}
	}

	private diffStreamLimits(config: WorkerStreamConfiguration, stream: WorkerStreamDefinition): Array<string> {
		const drifted: Array<string> = [];
		if (!Number.isSafeInteger(config.max_bytes) || config.max_bytes < stream.minBytes) drifted.push('max_bytes');
		if (config.max_msgs !== stream.maxMessages) drifted.push('max_msgs');
		if (config.max_msgs_per_subject !== stream.maxMessagesPerSubject) drifted.push('max_msgs_per_subject');
		if (config.discard !== stream.discard) drifted.push('discard');
		if ((config.discard_new_per_subject ?? false) !== stream.discardNewPerSubject) {
			drifted.push('discard_new_per_subject');
		}
		return drifted;
	}

	private unmanagedStreamSettings(config: WorkerStreamConfiguration, stream: WorkerStreamDefinition): Array<string> {
		const unmanaged: Array<string> = [];
		if (config.max_age !== nanos(stream.maxAgeMs)) unmanaged.push('max_age');
		if (config.duplicate_window !== nanos(DUPLICATE_WINDOW_MS)) unmanaged.push('duplicate_window');
		if (config.max_msg_size !== undefined && config.max_msg_size !== -1) unmanaged.push('max_msg_size');
		if (config.persist_mode !== undefined && config.persist_mode !== 'default') unmanaged.push('persist_mode');
		for (const field of ['allow_rollup_hdrs', 'allow_msg_ttl', 'allow_msg_counter'] as const) {
			if (config[field]) unmanaged.push(field);
		}
		return unmanaged;
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

	private async adoptConcurrentStream(
		jsm: JetStreamManager,
		stream: WorkerStreamDefinition,
		error: unknown,
	): Promise<StreamConfig | null> {
		if (jsErrorCode(error) !== STREAM_NAME_IN_USE_ERR_CODE) {
			return null;
		}
		const config = await this.readStreamConfig(jsm, stream);
		if (config !== null) {
			Logger.info({stream: stream.name, max_bytes: config.max_bytes}, 'Stream was created concurrently, adopting it');
		}
		return config;
	}

	private async createStream(jsm: JetStreamManager, stream: WorkerStreamDefinition, maxBytes: number): Promise<void> {
		const {config} = await jsm.streams.add({
			name: stream.name,
			subjects: [stream.subject],
			retention: stream.retention,
			storage: StorageType.File,
			max_age: nanos(stream.maxAgeMs),
			duplicate_window: nanos(DUPLICATE_WINDOW_MS),
			num_replicas: 1,
			max_msgs: stream.maxMessages,
			max_bytes: maxBytes,
			max_msgs_per_subject: stream.maxMessagesPerSubject,
			discard: stream.discard,
			discard_new_per_subject: stream.discardNewPerSubject,
		});
		this.assertStreamIdentity(config, stream);
	}

	private async addStream(jsm: JetStreamManager): Promise<void> {
		let maxBytes: number;
		try {
			maxBytes = await this.fitToStorageBudget(STREAM_MAX_BYTES, STREAM_MIN_BYTES, (bytes) =>
				this.createStream(jsm, JOBS_STREAM, bytes),
			);
		} catch (error) {
			const adopted = await this.adoptConcurrentStream(jsm, JOBS_STREAM, error);
			if (adopted !== null) {
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

	async ensureDlqStream(): Promise<void> {
		if (this.dlqStreamReady) return;
		this.dlqStreamSetup ??= this.initializeDlqStream().finally(() => {
			this.dlqStreamSetup = null;
		});
		await this.dlqStreamSetup;
	}

	private async initializeDlqStream(): Promise<void> {
		const jsm = await this.connectionManager.getJetStreamManager();
		if ((await this.readStreamConfig(jsm, DEAD_LETTER_STREAM)) === null) {
			if (!(await this.addDlqStream(jsm))) {
				return;
			}
		}
		this.dlqStreamReady = true;
	}

	private async addDlqStream(jsm: JetStreamManager): Promise<boolean> {
		try {
			const maxBytes = await this.fitToStorageBudget(DLQ_MAX_BYTES, DLQ_MIN_BYTES, (bytes) =>
				this.createStream(jsm, DEAD_LETTER_STREAM, bytes),
			);
			Logger.info({stream: DLQ_STREAM_NAME, max_bytes: maxBytes}, 'Dead-letter stream created');
			return true;
		} catch (error) {
			if ((await this.adoptConcurrentStream(jsm, DEAD_LETTER_STREAM, error)) !== null) {
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
			await this.ensureConsumer(jsm, lane);
		}
		this.consumersReady = true;
	}

	private async readConsumer(jsm: JetStreamManager, name: string): Promise<ConsumerInfo | null> {
		try {
			return await jsm.consumers.info(STREAM_NAME, name);
		} catch (error) {
			if (jsErrorCode(error) === CONSUMER_NOT_FOUND_ERR_CODE) {
				return null;
			}
			throw error;
		}
	}

	private async ensureConsumer(jsm: JetStreamManager, lane: WorkerLaneDefinition): Promise<void> {
		const config = {
			max_deliver: lane.maxDeliver,
			ack_wait: nanos(lane.ackWaitMs),
			max_ack_pending: lane.maxAckPending,
			filter_subject: undefined,
			filter_subjects: [...lane.taskTypes, ...lane.retiredTaskTypes].map((task) => `${SUBJECT_PREFIX}${task}`),
		} satisfies ConsumerUpdateConfig;
		let existing = await this.readConsumer(jsm, lane.consumerName);
		if (existing === null) {
			try {
				const created = await jsm.consumers.add(STREAM_NAME, {
					...config,
					durable_name: lane.consumerName,
					ack_policy: AckPolicy.Explicit,
					deliver_policy: DeliverPolicy.All,
					replay_policy: ReplayPolicy.Instant,
				});
				this.requireConsumerConfiguration(created, lane.consumerName);
				Logger.info({lane: lane.name, consumer: lane.consumerName}, 'Consumer created');
				return;
			} catch (error) {
				if (!CONSUMER_EXISTS_ERR_CODES.has(jsErrorCode(error) ?? 0)) {
					throw error;
				}
				existing = await this.readConsumer(jsm, lane.consumerName);
				if (existing === null) {
					throw error;
				}
			}
		}
		this.requireConsumerConfiguration(existing, lane.consumerName);
		const updated = await jsm.consumers.update(STREAM_NAME, lane.consumerName, config);
		this.requireConsumerConfiguration(updated, lane.consumerName);
		if (updated.created !== existing.created) {
			throw new Error(`Worker consumer ${lane.consumerName} was replaced during startup`);
		}
		Logger.info({lane: lane.name, consumer: lane.consumerName}, 'Consumer updated without resetting delivery state');
	}

	private requireConsumerConfiguration(consumer: ConsumerInfo, name: string): void {
		const current = consumer.config;
		if (
			current.durable_name !== name ||
			current.ack_policy !== AckPolicy.Explicit ||
			current.deliver_policy !== DeliverPolicy.All ||
			current.replay_policy !== ReplayPolicy.Instant ||
			current.deliver_subject ||
			current.headers_only ||
			(current.inactive_threshold ?? 0) > 0 ||
			(current.backoff?.length ?? 0) > 0
		) {
			throw new Error(
				`Worker consumer ${name} has an incompatible configuration, migrate it explicitly with workers stopped`,
			);
		}
	}

	private async migrateLegacyConsumer(): Promise<void> {
		const jsm = await this.connectionManager.getJetStreamManager();
		if ((await this.readConsumer(jsm, LEGACY_CONSUMER_NAME)) === null) return;
		try {
			await jsm.consumers.delete(STREAM_NAME, LEGACY_CONSUMER_NAME);
			Logger.info({consumer: LEGACY_CONSUMER_NAME}, 'Legacy consumer deleted, lane consumers will handle its messages');
		} catch (error) {
			Logger.error(
				{err: error, consumer: LEGACY_CONSUMER_NAME},
				'Could not delete the legacy worker consumer, delete it manually once legacy workers are stopped',
			);
		}
	}

	async ensureInfrastructure(lanes: ReadonlyArray<WorkerLaneDefinition>): Promise<void> {
		await this.ensureStream();
		await this.ensureDlqStream();
		await this.migrateLegacyConsumer();
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
		meta: WorkerDeadLetterMetadata,
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
			msgID: meta.messageId,
			timeout: DLQ_PUBLISH_TIMEOUT_MS,
		});
	}

	getStreamName(): string {
		return STREAM_NAME;
	}

	getConnectionManager(): JetStreamConnectionManager {
		return this.connectionManager;
	}
}
