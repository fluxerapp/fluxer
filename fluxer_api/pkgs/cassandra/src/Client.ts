// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CassandraParams, PreparedQuery} from '@pkgs/cassandra/src/CassandraTypes';
import {type Logger, NoopLogger} from '@pkgs/cassandra/src/Logger';
import cassandra from 'cassandra-driver';

const distance = cassandra.types.distance;

const MAX_REQUESTS_PER_CONNECTION = 2048;
const CONNECT_TIMEOUT_MS = 5000;
const DEFAULT_READ_TIMEOUT_MS = 5000;

export const BACKGROUND_READ_TIMEOUT_MS = 12000;

interface CassandraConfig {
	hosts: Array<string>;
	port?: number | undefined;
	keyspace: string;
	localDc: string;
	username?: string | undefined;
	password?: string | undefined;
	readTimeoutMs?: number | undefined;
}

interface CassandraClientOptions {
	logger?: Logger | undefined;
}

export interface CassandraExecuteOptions {
	prepare?: boolean | undefined;
}

export interface CassandraBatchOptions {
	prepare?: boolean | undefined;
}

export interface ICassandraClient {
	connect(): Promise<void>;
	shutdown(): Promise<void>;
	isConnected(): boolean;
	execute<P extends CassandraParams>(
		query: PreparedQuery<P>,
		options?: CassandraExecuteOptions,
	): Promise<cassandra.types.ResultSet>;
	batch(queries: Array<PreparedQuery>, options?: CassandraBatchOptions): Promise<void>;
	getNativeClient(): cassandra.Client;
	setLogger(logger: Logger): void;
}

interface DefaultClientState {
	client: CassandraClient | null;
	logger: Logger;
}

const defaultClientState: DefaultClientState = {
	client: null,
	logger: NoopLogger,
};

class CassandraClient implements ICassandraClient {
	private readonly config: CassandraConfig;
	private logger: Logger;
	private client: cassandra.Client | null;

	public constructor(config: CassandraConfig, options: CassandraClientOptions = {}) {
		this.config = {
			hosts: [...config.hosts],
			port: config.port,
			keyspace: config.keyspace,
			localDc: config.localDc,
			username: config.username,
			password: config.password,
			readTimeoutMs: config.readTimeoutMs,
		};
		this.logger = options.logger ?? NoopLogger;
		this.client = null;
	}

	public async connect(): Promise<void> {
		if (this.client !== null) {
			return;
		}
		const authProvider = this.config.username
			? new cassandra.auth.PlainTextAuthProvider(this.config.username, this.config.password ?? '')
			: undefined;
		const client = new cassandra.Client({
			contactPoints: this.config.hosts,
			keyspace: this.config.keyspace,
			localDataCenter: this.config.localDc,
			protocolOptions: {
				port: this.config.port ?? 9042,
			},
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
		this.logger.info(
			{
				hosts: this.config.hosts,
				keyspace: this.config.keyspace,
				local_dc: this.config.localDc,
			},
			'Connected to Cassandra',
		);
	}

	public async shutdown(): Promise<void> {
		if (this.client === null) {
			return;
		}
		const activeClient = this.client;
		this.client = null;
		await activeClient.shutdown();
		this.logger.info({}, 'Cassandra connection closed');
	}

	public isConnected(): boolean {
		return this.client !== null;
	}

	public async execute<P extends CassandraParams>(
		query: PreparedQuery<P>,
		options: CassandraExecuteOptions = {},
	): Promise<cassandra.types.ResultSet> {
		return this.getNativeClient().execute(query.cql, query.params, {
			prepare: options.prepare ?? true,
		});
	}

	public async batch(queries: Array<PreparedQuery>, options: CassandraBatchOptions = {}): Promise<void> {
		if (queries.length === 0) {
			return;
		}
		const batch = queries.map((query) => ({query: query.cql, params: query.params}));
		await this.getNativeClient().batch(batch, {
			prepare: options.prepare ?? true,
		});
	}

	public getNativeClient(): cassandra.Client {
		if (this.client === null) {
			throw new Error('Cassandra client is not connected. Call connect() first.');
		}
		return this.client;
	}

	public setLogger(logger: Logger): void {
		this.logger = logger;
	}
}

export async function initCassandra(config: CassandraConfig): Promise<void> {
	if (defaultClientState.client !== null) {
		await defaultClientState.client.shutdown();
	}
	const client = new CassandraClient(config, {logger: defaultClientState.logger});
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

export function getDefaultCassandraClient(): ICassandraClient {
	if (defaultClientState.client === null) {
		throw new Error('Cassandra client is not initialized. Call initCassandra() first.');
	}
	return defaultClientState.client;
}

export function getClient(): cassandra.Client {
	return getDefaultCassandraClient().getNativeClient();
}
