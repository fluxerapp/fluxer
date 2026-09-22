// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {Pool, PoolClient, PoolConfig, QueryResult, QueryResultRow} from 'pg';
import pg from 'pg';

const MAX_DIAGNOSTIC_FIELD_LENGTH = 128;

interface PostgresConnectionDiagnostic {
	phase: 'idle' | 'checked_out';
	errorName: string;
	code?: string;
	severity?: string;
	routine?: string;
}

type PostgresConnectionErrorReporter = (diagnostic: PostgresConnectionDiagnostic) => void;

interface AcquiredPostgresConnection {
	error: Error | null;
	onError: (error: Error) => void;
}

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
	initialization: Promise<void> | null;
	shutdown: Promise<void> | null;
}

const defaultClientState: DefaultClientState = {
	client: null,
	initialization: null,
	shutdown: null,
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

function connectionDiagnostic(
	error: Error,
	phase: PostgresConnectionDiagnostic['phase'],
): PostgresConnectionDiagnostic {
	return {
		phase,
		errorName: error.name.slice(0, MAX_DIAGNOSTIC_FIELD_LENGTH),
		...('code' in error && typeof error.code === 'string'
			? {code: error.code.slice(0, MAX_DIAGNOSTIC_FIELD_LENGTH)}
			: {}),
		...('severity' in error && typeof error.severity === 'string'
			? {severity: error.severity.slice(0, MAX_DIAGNOSTIC_FIELD_LENGTH)}
			: {}),
		...('routine' in error && typeof error.routine === 'string'
			? {routine: error.routine.slice(0, MAX_DIAGNOSTIC_FIELD_LENGTH)}
			: {}),
	};
}

function reportConnectionError(diagnostic: PostgresConnectionDiagnostic): void {
	console.error('Postgres connection error', diagnostic);
}

export function quoteIdentifier(identifier: string): string {
	return `"${assertIdentifier(identifier)}"`;
}

class PostgresClient implements IPostgresClient {
	private readonly config: PostgresConfig;
	private pool: Pool | null;
	private connection: Promise<void> | null = null;
	private disconnection: Promise<void> | null = null;
	private activeOperations = 0;
	private resolveOperationsDrained: (() => void) | null = null;
	private readonly acquiredConnections = new WeakMap<PoolClient, AcquiredPostgresConnection>();

	constructor(
		config: PostgresConfig,
		private readonly onConnectionError: PostgresConnectionErrorReporter,
	) {
		this.config = {...config};
		this.pool = null;
	}

	async connect(): Promise<void> {
		if (this.disconnection !== null) {
			throw new Error('Cannot connect Postgres while it is shutting down');
		}
		if (this.pool !== null) return;
		this.connection ??= this.openPool().finally(() => {
			this.connection = null;
		});
		await this.connection;
	}

	private async openPool(): Promise<void> {
		const poolConfig: PoolConfig & {scramMaxIterations: number} = {
			connectionString: this.config.url || undefined,
			host: this.config.url ? undefined : (this.config.host ?? '127.0.0.1'),
			port: this.config.url ? undefined : (this.config.port ?? 5432),
			database: this.config.url ? undefined : (this.config.database ?? 'fluxer'),
			user: this.config.url ? undefined : (this.config.username ?? 'fluxer'),
			password: this.config.url ? undefined : (this.config.password ?? 'fluxer'),
			ssl: this.config.ssl ? {rejectUnauthorized: true, ca: normalizePem(this.config.sslCa)} : undefined,
			max: this.config.maxConnections ?? 20,
			scramMaxIterations: 0,
		};
		const pool = new pg.Pool(poolConfig);
		this.observePoolConnections(pool);
		try {
			const client = await pool.connect();
			let discardClient = true;
			try {
				const acquired = this.getAcquiredConnection(client);
				if (acquired.error) throw acquired.error;
				discardClient = false;
			} finally {
				client.release(discardClient);
			}
		} catch (error) {
			try {
				await pool.end();
			} catch (cleanupError) {
				throw new AggregateError([error, cleanupError], 'Postgres connection and pool cleanup failed');
			}
			throw error;
		}
		this.pool = pool;
	}

	private observePoolConnections(pool: Pool): void {
		pool.on('error', (error) => this.onConnectionError(connectionDiagnostic(error, 'idle')));
		pool.on('acquire', (client) => {
			assert(!this.acquiredConnections.has(client), 'Postgres connection was acquired twice without release');
			const acquired: AcquiredPostgresConnection = {
				error: null,
				onError: (error) => {
					if (acquired.error !== null) return;
					acquired.error = error;
					this.onConnectionError(connectionDiagnostic(error, 'checked_out'));
				},
			};
			this.acquiredConnections.set(client, acquired);
			client.on('error', acquired.onError);
		});
		pool.on('release', (_error, client) => {
			const acquired = this.acquiredConnections.get(client);
			if (acquired) client.removeListener('error', acquired.onError);
			this.acquiredConnections.delete(client);
		});
	}

	private getAcquiredConnection(client: PoolClient): AcquiredPostgresConnection {
		const acquired = this.acquiredConnections.get(client);
		assert(acquired, 'Postgres connection has no active acquisition');
		return acquired;
	}

	async shutdown(): Promise<void> {
		if (this.disconnection !== null) return this.disconnection;
		if (this.pool === null && this.connection === null) return;
		this.disconnection = this.closePool().finally(() => {
			this.disconnection = null;
		});
		await this.disconnection;
	}

	private async closePool(): Promise<void> {
		await this.connection;
		if (this.activeOperations > 0) {
			await new Promise<void>((resolve) => {
				this.resolveOperationsDrained = resolve;
			});
		}
		const pool = this.pool;
		if (pool === null) return;
		this.pool = null;
		await pool.end();
	}

	isConnected(): boolean {
		return this.pool !== null && this.disconnection === null;
	}

	async query<T extends QueryResultRow = QueryResultRow>(
		text: string,
		values: Array<unknown> = [],
		name?: string,
	): Promise<QueryResult<T>> {
		return this.withPool((pool) => pool.query<T>({text, values, name: this.statementName(name)}));
	}

	private statementName(name: string | undefined): string | undefined {
		return this.config.preparedStatements === false ? undefined : name;
	}

	async transaction<T>(fn: (client: PostgresQueryable) => Promise<T>): Promise<T> {
		return this.withPool((pool) => this.runTransaction(pool, fn));
	}

	private async runTransaction<T>(pool: Pool, fn: (client: PostgresQueryable) => Promise<T>): Promise<T> {
		const client = await pool.connect();
		let acquired: AcquiredPostgresConnection | null = null;
		let discardClient = false;
		try {
			acquired = this.getAcquiredConnection(client);
			if (acquired.error) throw acquired.error;
			await client.query('BEGIN');
			if (acquired.error) throw acquired.error;
			const result = await runTransactionCallback(client, acquired, this.config.preparedStatements !== false, fn);
			if (acquired.error) throw acquired.error;
			await client.query('COMMIT');
			return result;
		} catch (error) {
			if (acquired === null) throw error;
			if (!acquired.error) {
				try {
					await client.query('ROLLBACK');
				} catch (rollbackError) {
					discardClient = true;
					throw new AggregateError([error, rollbackError], 'Postgres transaction and rollback failed');
				}
			}
			if (acquired.error && acquired.error !== error) {
				throw new AggregateError([error, acquired.error], 'Postgres transaction and connection failed');
			}
			throw error;
		} finally {
			client.release(discardClient || acquired === null || acquired.error !== null);
		}
	}

	kvTable(): string {
		return this.config.kvTable ?? 'fluxer_kv';
	}

	private async withPool<T>(operation: (pool: Pool) => Promise<T>): Promise<T> {
		const pool = this.getPool();
		this.activeOperations += 1;
		try {
			return await operation(pool);
		} finally {
			this.activeOperations -= 1;
			if (this.activeOperations === 0) {
				this.resolveOperationsDrained?.();
				this.resolveOperationsDrained = null;
			}
		}
	}

	private getPool(): Pool {
		if (this.disconnection !== null) {
			throw new Error('Postgres client is shutting down');
		}
		if (this.pool === null) {
			throw new Error('Postgres client is not connected. Call connect() first.');
		}
		return this.pool;
	}
}

async function runTransactionCallback<T>(
	client: PoolClient,
	acquired: AcquiredPostgresConnection,
	preparedStatements: boolean,
	fn: (client: PostgresQueryable) => Promise<T>,
): Promise<T> {
	let active = true;
	const queryable: PostgresQueryable = {
		query: async <TRow extends QueryResultRow = QueryResultRow>(
			text: string,
			values: Array<unknown> = [],
			name?: string,
		) => {
			if (!active) throw new Error('Postgres transaction callback has already completed');
			if (acquired.error) throw acquired.error;
			return client.query<TRow>({text, values, name: preparedStatements ? name : undefined});
		},
	};
	try {
		return await fn(queryable);
	} finally {
		active = false;
	}
}

export async function initPostgres(
	config: PostgresConfig,
	onConnectionError: PostgresConnectionErrorReporter = reportConnectionError,
): Promise<void> {
	if (defaultClientState.initialization !== null) {
		throw new Error('Postgres initialization is already in progress');
	}
	if (defaultClientState.shutdown !== null) {
		throw new Error('Cannot initialize Postgres while it is shutting down');
	}
	const client = new PostgresClient(config, onConnectionError);
	defaultClientState.initialization = replaceDefaultClient(client).finally(() => {
		defaultClientState.initialization = null;
	});
	await defaultClientState.initialization;
}

async function replaceDefaultClient(client: PostgresClient): Promise<void> {
	const previousClient = defaultClientState.client;
	defaultClientState.client = null;
	await previousClient?.shutdown();
	await client.connect();
	defaultClientState.client = client;
}

export async function shutdownPostgres(): Promise<void> {
	if (defaultClientState.shutdown !== null) return defaultClientState.shutdown;
	if (defaultClientState.client === null && defaultClientState.initialization === null) return;
	defaultClientState.shutdown = closeDefaultClient().finally(() => {
		defaultClientState.shutdown = null;
	});
	await defaultClientState.shutdown;
}

async function closeDefaultClient(): Promise<void> {
	await defaultClientState.initialization;
	const client = defaultClientState.client;
	defaultClientState.client = null;
	await client?.shutdown();
}

export function getDefaultPostgresClient(): IPostgresClient {
	if (defaultClientState.shutdown !== null) {
		throw new Error('Default Postgres client is shutting down');
	}
	if (defaultClientState.client === null) {
		throw new Error('Postgres client is not initialized. Call initPostgres() first.');
	}
	return defaultClientState.client;
}
