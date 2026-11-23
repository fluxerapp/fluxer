// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KvQueryMeta} from '@app/api/database/CassandraTypes';
import {registerCounter, registerHistogram} from '@fluxer/hono/src/middleware/Metrics';
import type {JetStreamClient} from '@nats-io/jetstream';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';

export const PUBLISH_TIMEOUT_MS = 2000;

export const recordsTotal = registerCounter('fluxer_cassandra_capture_records_total', 'Capture records built, by kind');
export const bytesTotal = registerCounter('fluxer_cassandra_capture_bytes_total', 'Bytes of capture records built');
export const recordBytes = registerHistogram(
	'fluxer_cassandra_capture_record_bytes',
	'Size of each capture record in bytes',
	[256, 1024, 4096, 16_384, 65_536, 262_144, 1_048_576],
);
export const unkeyedTotal = registerCounter(
	'fluxer_cassandra_capture_unkeyed_total',
	'Captured writes whose rows capture cannot key, by table',
);
export const publishFailuresTotal = registerCounter(
	'fluxer_cassandra_capture_publish_failures_total',
	'Capture publishes that failed, by kind',
);

export interface CaptureSink {
	publish(subject: string, body: string, msgID: string, timeoutMs: number): Promise<void>;
	close(): Promise<void>;
}

export interface WriteCaptureHook {
	plain<T>(
		statements: ReadonlyArray<CapturedStatement>,
		logged: boolean,
		send: (timestamp: number) => Promise<T>,
	): Promise<T>;
	conditional<T>(
		statement: CapturedStatement,
		send: () => Promise<T>,
		appliedOf: (result: T) => boolean | undefined,
	): Promise<T>;
	close(timeoutMs: number): Promise<void>;
}

export interface CapturedStatement {
	cql: string;
	type: string;
	params: Readonly<Record<string, unknown>>;
	meta: KvQueryMeta | undefined;
}

export function tableOf(statement: CapturedStatement): string {
	return (
		statement.meta?.table.name ??
		/^\s*(?:INSERT\s+INTO|UPDATE|DELETE\s+FROM)\s+([A-Za-z0-9_]+)/i.exec(statement.cql)?.[1] ??
		'unknown'
	);
}

export class JetStreamCaptureSink implements CaptureSink {
	private readonly connection: JetStreamConnectionManager;
	private readonly stream: string;
	private client: JetStreamClient | null = null;
	private connecting: Promise<JetStreamClient> | null = null;

	constructor(options: {url: string; token: string; stream: string}) {
		this.connection = new JetStreamConnectionManager({
			url: options.url,
			token: options.token || undefined,
			name: 'cassandra-capture',
		});
		this.stream = options.stream;
	}

	async connect(): Promise<void> {
		await this.jetStream();
	}

	async publish(subject: string, body: string, msgID: string, timeoutMs: number): Promise<void> {
		const client = await this.jetStream();
		await client.publish(subject, body, {msgID, timeout: timeoutMs, expect: {streamName: this.stream}});
	}

	private jetStream(): Promise<JetStreamClient> {
		if (this.client !== null) return Promise.resolve(this.client);
		this.connecting ??= this.openClient().finally(() => {
			this.connecting = null;
		});
		return this.connecting;
	}

	private async openClient(): Promise<JetStreamClient> {
		await this.connection.connect();
		const jsm = await this.connection.getJetStreamManager();
		await jsm.streams.info(this.stream);
		this.client = this.connection.getJetStreamClient();
		return this.client;
	}

	async close(): Promise<void> {
		await this.connection.drain();
	}
}
