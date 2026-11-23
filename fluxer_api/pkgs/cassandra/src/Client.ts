// SPDX-License-Identifier: AGPL-3.0-or-later

import type cassandra from 'cassandra-driver';

type CassandraDriver = typeof cassandra;

const MAX_REQUESTS_PER_CONNECTION = 2048;
const CONNECT_TIMEOUT_MS = 5000;
const DEFAULT_READ_TIMEOUT_MS = 5000;

export const BACKGROUND_READ_TIMEOUT_MS = 12000;

export type CassandraConsistencyName = 'one' | 'localOne' | 'quorum' | 'localQuorum' | 'eachQuorum' | 'all';
export type CassandraSerialConsistencyName = 'serial' | 'localSerial';

const CONSISTENCY_NAMES: Record<string, CassandraConsistencyName> = {
	one: 'one',
	local_one: 'localOne',
	quorum: 'quorum',
	local_quorum: 'localQuorum',
	each_quorum: 'eachQuorum',
	all: 'all',
};

const SERIAL_CONSISTENCY_NAMES: Record<string, CassandraSerialConsistencyName> = {
	serial: 'serial',
	local_serial: 'localSerial',
};

function normalizeConsistencyKey(value: string): string {
	return value
		.trim()
		.replace(/([a-z])([A-Z])/g, '$1_$2')
		.toLowerCase();
}

export function parseCassandraConsistency(value: string | undefined): CassandraConsistencyName {
	if (!value?.trim()) return 'localQuorum';
	const name = CONSISTENCY_NAMES[normalizeConsistencyKey(value)];
	if (!name) {
		throw new Error(
			`Unsupported Cassandra consistency "${value}". Use one of: ${Object.keys(CONSISTENCY_NAMES).join(', ')}`,
		);
	}
	return name;
}

export function parseCassandraSerialConsistency(value: string | undefined): CassandraSerialConsistencyName {
	if (!value?.trim()) return 'localSerial';
	const name = SERIAL_CONSISTENCY_NAMES[normalizeConsistencyKey(value)];
	if (!name) {
		throw new Error(
			`Unsupported Cassandra serial consistency "${value}". Use one of: ${Object.keys(SERIAL_CONSISTENCY_NAMES).join(', ')}`,
		);
	}
	return name;
}

interface CassandraConfig {
	hosts: Array<string>;
	port?: number | undefined;
	keyspace: string;
	localDc: string;
	consistency?: CassandraConsistencyName | undefined;
	serialConsistency?: CassandraSerialConsistencyName | undefined;
	username?: string | undefined;
	password?: string | undefined;
	readTimeoutMs?: number | undefined;
}

interface DefaultClientState {
	client: CassandraClient | null;
}

const defaultClientState: DefaultClientState = {
	client: null,
};

let loadedDriver: CassandraDriver | null = null;

export function getLoadedCassandraDriver(): CassandraDriver | null {
	return loadedDriver;
}

class CassandraClient {
	private readonly config: CassandraConfig;
	private client: cassandra.Client | null;
	private timestampGenerator: cassandra.policies.timestampGeneration.TimestampGenerator | null = null;

	public constructor(config: CassandraConfig) {
		this.config = {
			hosts: [...config.hosts],
			port: config.port,
			keyspace: config.keyspace,
			localDc: config.localDc,
			consistency: config.consistency ?? 'localQuorum',
			serialConsistency: config.serialConsistency ?? 'localSerial',
			username: config.username,
			password: config.password,
			readTimeoutMs: config.readTimeoutMs,
		};
		this.client = null;
	}

	public async connect(): Promise<void> {
		if (this.client !== null) {
			return;
		}
		const {default: driver} = await import('cassandra-driver');
		loadedDriver = driver;
		const distance = driver.types.distance;
		const timestampGenerator = driver.policies.defaultTimestampGenerator();
		const authProvider = this.config.username
			? new driver.auth.PlainTextAuthProvider(this.config.username, this.config.password ?? '')
			: undefined;
		const client = new driver.Client({
			contactPoints: this.config.hosts,
			keyspace: this.config.keyspace,
			localDataCenter: this.config.localDc,
			queryOptions: {
				consistency: driver.types.consistencies[this.config.consistency ?? 'localQuorum'],
				serialConsistency: driver.types.consistencies[this.config.serialConsistency ?? 'localSerial'],
			},
			protocolOptions: {
				port: this.config.port ?? 9042,
			},
			policies: {timestampGeneration: timestampGenerator},
			pooling: {
				maxRequestsPerConnection: MAX_REQUESTS_PER_CONNECTION,
				coreConnectionsPerHost: {
					[distance.local]: 4,
					[distance.remote]: 2,
				},
			},
			socketOptions: {
				connectTimeout: CONNECT_TIMEOUT_MS,
				readTimeout: this.config.readTimeoutMs ?? DEFAULT_READ_TIMEOUT_MS,
			},
			encoding: {
				map: Map,
				set: Set,
				useBigIntAsLong: true,
				useBigIntAsVarint: true,
			},
			...(authProvider ? {authProvider} : {}),
		});
		await client.connect();
		this.client = client;
		this.timestampGenerator = timestampGenerator;
	}

	public async shutdown(): Promise<void> {
		if (this.client === null) {
			return;
		}
		const activeClient = this.client;
		this.client = null;
		await activeClient.shutdown();
	}

	public getSerialConsistency(): CassandraSerialConsistencyName {
		return this.config.serialConsistency ?? 'localSerial';
	}

	public nextWriteTimestamp(): number {
		const client = this.getNativeClient();
		const timestamp = this.timestampGenerator?.next(client);
		if (typeof timestamp !== 'number') {
			throw new Error('Cassandra timestamp generator returned no microsecond timestamp');
		}
		return timestamp;
	}

	public getNativeClient(): cassandra.Client {
		if (this.client === null) {
			throw new Error('Cassandra client is not connected. Call connect() first.');
		}
		return this.client;
	}
}

export async function initCassandra(config: CassandraConfig): Promise<void> {
	if (defaultClientState.client !== null) {
		await defaultClientState.client.shutdown();
	}
	const client = new CassandraClient(config);
	await client.connect();
	defaultClientState.client = client;
}

export async function shutdownCassandra(): Promise<void> {
	if (defaultClientState.client === null) {
		return;
	}
	await defaultClientState.client.shutdown();
	defaultClientState.client = null;
}

function getDefaultCassandraClient(): CassandraClient {
	if (defaultClientState.client === null) {
		throw new Error('Cassandra client is not initialized. Call initCassandra() first.');
	}
	return defaultClientState.client;
}

export function getClient(): cassandra.Client {
	return getDefaultCassandraClient().getNativeClient();
}

export function getSerialConsistency(): CassandraSerialConsistencyName {
	return defaultClientState.client?.getSerialConsistency() ?? 'localSerial';
}

export function nextWriteTimestamp(): number {
	return getDefaultCassandraClient().nextWriteTimestamp();
}
