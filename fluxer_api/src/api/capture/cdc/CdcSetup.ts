// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CdcConfig} from '@app/api/capture/cdc/CdcConfig';
import {CDC_SUBJECTS} from '@app/api/database/capture/CdcRecords';
import {
	AckPolicy,
	type ConsumerConfig,
	DeliverPolicy,
	DiscardPolicy,
	JetStreamApiCodes,
	JetStreamApiError,
	type JetStreamManager,
	ReplayPolicy,
	RetentionPolicy,
	StorageType,
	StoreCompression,
	type StreamConfig,
} from '@nats-io/jetstream';

const NANOS_PER_MS = 1_000_000;

const MAX_ACK_PENDING = 20_000;

function wantedStream(config: CdcConfig['nats']): Partial<StreamConfig> & {name: string} {
	return {
		name: config.stream,
		subjects: [...CDC_SUBJECTS],
		storage: StorageType.File,
		num_replicas: config.replicas,
		retention: RetentionPolicy.Limits,
		max_age: 3 * 86_400_000 * NANOS_PER_MS,
		max_bytes: config.maxBytes,
		discard: DiscardPolicy.Old,
		duplicate_window: 120_000 * NANOS_PER_MS,
		max_msg_size: 1024 * 1024,
		compression: StoreCompression.S2,
		deny_delete: true,
		deny_purge: true,
	};
}

function wantedConsumer(config: CdcConfig['nats']): Partial<ConsumerConfig> {
	return {
		durable_name: config.consumer,
		deliver_policy: DeliverPolicy.All,
		ack_policy: AckPolicy.Explicit,
		ack_wait: 300_000 * NANOS_PER_MS,
		max_ack_pending: MAX_ACK_PENDING,
		max_deliver: -1,
		replay_policy: ReplayPolicy.Instant,
		filter_subjects: [...CDC_SUBJECTS],
	};
}

function comparable(value: unknown): string {
	return JSON.stringify(Array.isArray(value) ? [...value].sort() : (value ?? null));
}

export function settingDifferences(what: string, wanted: object, actual: object): Array<string> {
	const current = actual as Readonly<Record<string, unknown>>;
	return Object.entries(wanted)
		.filter(([field, value]) => comparable(current[field]) !== comparable(value))
		.map(([field, value]) => `${what} ${field} is ${comparable(current[field])}, wanted ${comparable(value)}`);
}

async function existing<T>(load: () => Promise<T>): Promise<T | null> {
	try {
		return await load();
	} catch (error) {
		const missing = [JetStreamApiCodes.StreamNotFound, JetStreamApiCodes.ConsumerNotFound] as ReadonlyArray<number>;
		if (error instanceof JetStreamApiError && missing.includes(error.code)) return null;
		throw error;
	}
}

export async function checkSetup(
	jsm: JetStreamManager,
	config: CdcConfig['nats'],
	create: boolean,
): Promise<Array<string>> {
	const stream = wantedStream(config);
	const consumer = wantedConsumer(config);
	const differences: Array<string> = [];
	const streamInfo = await existing(() => jsm.streams.info(config.stream));
	if (streamInfo === null) {
		if (!create) return [`stream ${config.stream} does not exist, run cdc setup`];
		await jsm.streams.add(stream);
	} else {
		differences.push(...settingDifferences(`stream ${config.stream}`, stream, streamInfo.config));
	}
	const consumerInfo = await existing(() => jsm.consumers.info(config.stream, config.consumer));
	if (consumerInfo === null) {
		if (!create) return [...differences, `consumer ${config.consumer} does not exist, run cdc setup`];
		await jsm.consumers.add(config.stream, consumer);
	} else {
		differences.push(...settingDifferences(`consumer ${config.consumer}`, consumer, consumerInfo.config));
	}
	return differences;
}
