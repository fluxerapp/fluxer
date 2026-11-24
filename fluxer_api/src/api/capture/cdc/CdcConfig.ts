// SPDX-License-Identifier: AGPL-3.0-or-later

import {MARKED_ROWS_PER_SECOND, ROUTINE_ROWS_PER_SECOND} from '@app/api/capture/cdc/CdcRepair';

export interface CdcNatsConfig {
	url: string;
	token: string;
	stream: string;
	consumer: string;
	replicas: number;
	maxBytes: number;
}

export interface CdcConfig {
	cassandra: {
		hosts: Array<string>;
		port: number;
		keyspace: string;
		localDc: string;
		username: string;
		password: string;
	};
	nats: CdcNatsConfig;
	clickhouse: {url: string; user: string; password: string};
	viewDatabase: string;
	webhookUrl: string;
	port: number;
	flushSeconds: number;
	markedRowsPerSecond: number;
	routineRowsPerSecond: number;
}

type Env = Readonly<Record<string, string | undefined>>;

function required(env: Env, name: string): string {
	const value = env[name]?.trim();
	if (!value) throw new Error(`${name} is not set`);
	return value;
}

function optional(env: Env, name: string, fallback: string): string {
	return env[name]?.trim() || fallback;
}

function positive(env: Env, name: string, fallback: number): number {
	const value = Number(optional(env, name, String(fallback)));
	if (!Number.isSafeInteger(value) || value <= 0) throw new Error(`${name} must be a positive integer`);
	return value;
}

export function loadCdcNatsConfig(env: Env): CdcNatsConfig {
	return {
		url: required(env, 'CDC_NATS_URL'),
		token: optional(env, 'CDC_NATS_TOKEN', ''),
		stream: optional(env, 'CDC_STREAM', 'CASSANDRA_CDC'),
		consumer: optional(env, 'CDC_CONSUMER', 'clickhouse-cdc'),
		replicas: positive(env, 'CDC_STREAM_REPLICAS', 3),
		maxBytes: positive(env, 'CDC_STREAM_MAX_BYTES', 8 * 1024 ** 3),
	};
}

export function loadCdcConfig(env: Env): CdcConfig {
	return {
		cassandra: {
			hosts: required(env, 'CDC_CASSANDRA_HOSTS')
				.split(',')
				.map((host) => host.trim())
				.filter(Boolean),
			port: positive(env, 'CDC_CASSANDRA_PORT', 9042),
			keyspace: optional(env, 'CDC_CASSANDRA_KEYSPACE', 'fluxer'),
			localDc: required(env, 'CDC_CASSANDRA_DC'),
			username: optional(env, 'CDC_CASSANDRA_USER', ''),
			password: optional(env, 'CDC_CASSANDRA_PASSWORD', ''),
		},
		nats: loadCdcNatsConfig(env),
		clickhouse: {
			url: required(env, 'CDC_CLICKHOUSE_URL'),
			user: optional(env, 'CDC_CLICKHOUSE_USER', 'cdc'),
			password: optional(env, 'CDC_CLICKHOUSE_PASSWORD', ''),
		},
		viewDatabase: optional(env, 'CDC_VIEW_DATABASE', 'fluxer'),
		webhookUrl: optional(env, 'CDC_WEBHOOK_URL', ''),
		port: positive(env, 'CDC_PORT', 9102),
		flushSeconds: positive(env, 'CDC_FLUSH_SECONDS', 10),
		markedRowsPerSecond: positive(env, 'CDC_MARKED_ROWS_PER_SECOND', MARKED_ROWS_PER_SECOND),
		routineRowsPerSecond: positive(env, 'CDC_ROUTINE_ROWS_PER_SECOND', ROUTINE_ROWS_PER_SECOND),
	};
}
