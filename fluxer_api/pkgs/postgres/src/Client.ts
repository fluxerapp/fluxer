// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Pool, PoolClient, QueryResult, QueryResultRow} from 'pg';
import pg from 'pg';

interface PostgresConfig {
	url?: string;
	host?: string;
	port?: number;
	database?: string;
	username?: string;
	password?: string;
	ssl?: boolean;
	sslCa?: string;
	maxConnections?: number;
	kvTable?: string;
	preparedStatements?: boolean;
}

export interface PostgresQueryable {
	query<T extends QueryResultRow = QueryResultRow>(
		text: string,
		values?: Array<unknown>,
		name?: string,
	): Promise<QueryResult<T>>;
}

export interface IPostgresClient extends PostgresQueryable {
	connect(): Promise<void>;
	shutdown(): Promise<void>;
	isConnected(): boolean;
	transaction<T>(fn: (client: PostgresQueryable) => Promise<T>): Promise<T>;
	kvTable(): string;
}

interface DefaultClientState {
	client: PostgresClient | null;
}

const defaultClientState: DefaultClientState = {
	client: null,
};

function assertIdentifier(identifier: string): string {
	if (!/^[A-Za-z_][A-Za-z0-9_]*$/u.test(identifier)) {
		throw new Error(`Unsafe Postgres identifier: ${JSON.stringify(identifier)}`);
	}
	return identifier;
}

function normalizePem(pem: string | undefined): string | undefined {
	if (!pem) return undefined;
	return pem.replaceAll('\\n', '\n');
}

export function quoteIdentifier(identifier: string): string {
	return `"${assertIdentifier(identifier)}"`;
}

class PostgresClient implements IPostgresClient {
	private readonly config: PostgresConfig;
	private pool: Pool | null;

	constructor(config: PostgresConfig) {
		this.config = {...config};
		this.pool = null;
	}

	async connect(): Promise<void> {
		if (this.pool !== null) return;
		const pool = new pg.Pool({
			connectionString: this.config.url || undefined,
			host: this.config.url ? undefined : (this.config.host ?? '127.0.0.1'),
			port: this.config.url ? undefined : (this.config.port ?? 5432),
			database: this.config.url ? undefined : (this.config.database ?? 'fluxer'),
			user: this.config.url ? undefined : (this.config.username ?? 'fluxer'),
			password: this.config.url ? undefined : (this.config.password ?? 'fluxer'),
			ssl: this.config.ssl ? {rejectUnauthorized: true, ca: normalizePem(this.config.sslCa)} : undefined,
			max: this.config.maxConnections ?? 20,
		});
		const client = await pool.connect();
		client.release();
		this.pool = pool;
	}

	async shutdown(): Promise<void> {
		const pool = this.pool;
		if (pool === null) return;
		this.pool = null;
		await pool.end();
	}

	isConnected(): boolean {
		return this.pool !== null;
	}

	async query<T extends QueryResultRow = QueryResultRow>(
		text: string,
		values: Array<unknown> = [],
		name?: string,
	): Promise<QueryResult<T>> {
		return this.getPool().query<T>({text, values, name: this.statementName(name)});
	}

	private statementName(name: string | undefined): string | undefined {
		return this.config.preparedStatements === false ? undefined : name;
	}

	async transaction<T>(fn: (client: PostgresQueryable) => Promise<T>): Promise<T> {
		const client = await this.getPool().connect();
		try {
			await client.query('BEGIN');
			const result = await fn(poolClientQueryable(client, this.config.preparedStatements !== false));
			await client.query('COMMIT');
			return result;
		} catch (error) {
			await rollback(client);
			throw error;
		} finally {
			client.release();
		}
	}

	kvTable(): string {
		return this.config.kvTable ?? 'fluxer_kv';
	}

	private getPool(): Pool {
		if (this.pool === null) {
			throw new Error('Postgres client is not connected. Call connect() first.');
		}
		return this.pool;
	}
}

function poolClientQueryable(client: PoolClient, preparedStatements: boolean): PostgresQueryable {
	return {
		query: <T extends QueryResultRow = QueryResultRow>(text: string, values: Array<unknown> = [], name?: string) =>
			client.query<T>({text, values, name: preparedStatements ? name : undefined}),
	};
}

async function rollback(client: PoolClient): Promise<void> {
	try {
		await client.query('ROLLBACK');
	} catch {}
}

export async function initPostgres(config: PostgresConfig): Promise<void> {
	if (defaultClientState.client !== null) {
		await defaultClientState.client.shutdown();
	}
	const client = new PostgresClient(config);
	await client.connect();
	defaultClientState.client = client;
}

export async function shutdownPostgres(): Promise<void> {
	if (defaultClientState.client === null) return;
	await defaultClientState.client.shutdown();
	defaultClientState.client = null;
}

export function getDefaultPostgresClient(): IPostgresClient {
	if (defaultClientState.client === null) {
		throw new Error('Postgres client is not initialized. Call initPostgres() first.');
	}
	return defaultClientState.client;
}
