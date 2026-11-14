// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {setTimeout as sleep} from 'node:timers/promises';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import TARGET_SCHEMA from '../../../../tools/dev/cassandra_target_schema.json' with {type: 'json'};

const LOCK_KEY = 'cassandra_schema_startup';
const LOCK_TTL_SECONDS = 60;
const LOCK_RENEW_MS = 20_000;
const CASSANDRA_UNAUTHORIZED = 0x2100;
const CASSANDRA_INVALID = 0x2200;
const LOCK_RETRY_MS = 1000;
const SCHEMA_PAGE_SIZE = 1000;
const CQL_IDENTIFIER_PATTERN = /^[A-Za-z_][A-Za-z0-9_]*$/;
const TYPE_TOKEN_PATTERN = /[A-Za-z_][A-Za-z0-9_]*/g;

interface SchemaField {
	name: string;
	type: string;
}

interface UserType {
	name: string;
	fields: Array<SchemaField>;
}

export interface CassandraSchemaClient {
	execute(
		query: string,
		params?: Array<unknown>,
		options?: {fetchSize?: number; pageState?: string},
	): Promise<{rows: Array<Record<string, unknown>>; pageState?: string | null}>;
}

interface ExistingSchemaRows {
	types: ReadonlyArray<Record<string, unknown>>;
	columns: ReadonlyArray<Record<string, unknown>>;
	indexes: ReadonlyArray<Record<string, unknown>>;
}

function errorCode(error: unknown): unknown {
	return typeof error === 'object' && error !== null && 'code' in error ? error.code : undefined;
}

function cqlKeyspace(keyspace: string): string {
	return /^[a-z_][a-z0-9_]*$/.test(keyspace) ? keyspace : `"${keyspace.replaceAll('"', '""')}"`;
}

function lower(value: unknown): string {
	return String(value).toLowerCase();
}

function sortedUserTypes(): Array<UserType> {
	const names = new Set(TARGET_SCHEMA.user_types.map((type) => type.name));
	const done = new Set<string>();
	const result: Array<UserType> = [];
	let remaining = [...TARGET_SCHEMA.user_types].sort((a, b) => a.name.localeCompare(b.name));
	while (remaining.length > 0) {
		const ready = remaining.filter((type) =>
			type.fields.every((field) =>
				[...field.type.matchAll(TYPE_TOKEN_PATTERN)].every(([token]) => {
					const name = token.toLowerCase();
					return name === type.name || !names.has(name) || done.has(name);
				}),
			),
		);
		const batch = ready.length > 0 ? ready : remaining;
		for (const type of batch) {
			result.push(type);
			done.add(type.name);
		}
		remaining = remaining.filter((type) => !done.has(type.name));
	}
	return result;
}

function columnList(fields: ReadonlyArray<SchemaField>): Array<string> {
	return fields.map((field) => `${field.name} ${field.type}`);
}

export function logMissingCassandraKeyspace(
	error: unknown,
	keyspace: string,
	localDc: string,
	logger: LoggerInterface,
): void {
	if (errorCode(error) !== CASSANDRA_INVALID || !(error instanceof Error)) {
		return;
	}
	if (error.message.toLowerCase() !== `keyspace '${keyspace.toLowerCase()}' does not exist`) {
		return;
	}
	logger.error(
		{keyspace},
		`The Cassandra keyspace "${keyspace}" does not exist. Create it before you start Fluxer, for example with CREATE KEYSPACE ${cqlKeyspace(keyspace)} WITH replication = {'class': 'NetworkTopologyStrategy', '${localDc}': 3}, and use 1 instead of 3 on a single node. On Scylla, add AND tablets = {'enabled': false} to that statement, because Fluxer needs lightweight transactions and secondary indexes, which tablets support only on Scylla 2025.4 or later with rf_rack_valid_keyspaces turned on. Fluxer creates the tables in it.`,
	);
}

export function planCassandraSchema(existing: ExistingSchemaRows, keyspace: string): Array<string> {
	if (!CQL_IDENTIFIER_PATTERN.test(keyspace)) {
		throw new Error(`Unsafe Cassandra keyspace name: ${JSON.stringify(keyspace)}`);
	}
	const quotedKeyspace = cqlKeyspace(keyspace);
	const typeFields = new Map<string, Set<string>>();
	for (const row of existing.types) {
		typeFields.set(lower(row.type_name), new Set(((row.field_names as Array<string> | null) ?? []).map(lower)));
	}
	const tableColumns = new Map<string, Set<string>>();
	for (const row of existing.columns) {
		const table = lower(row.table_name);
		const columns = tableColumns.get(table) ?? new Set<string>();
		columns.add(lower(row.column_name));
		tableColumns.set(table, columns);
	}
	const indexes = new Set(existing.indexes.map((row) => lower(row.index_name)));
	const statements: Array<string> = [];
	for (const type of sortedUserTypes()) {
		const fields = typeFields.get(type.name);
		if (!fields) {
			statements.push(
				`CREATE TYPE IF NOT EXISTS ${quotedKeyspace}.${type.name} (\n    ${columnList(type.fields).join(',\n    ')}\n)`,
			);
			continue;
		}
		for (const field of type.fields) {
			if (!fields.has(field.name)) {
				statements.push(`ALTER TYPE ${quotedKeyspace}.${type.name} ADD ${field.name} ${field.type}`);
			}
		}
	}
	for (const table of [...TARGET_SCHEMA.tables].sort((a, b) => a.name.localeCompare(b.name))) {
		const columns = tableColumns.get(table.name);
		if (!columns) {
			const body = [...columnList(table.columns), `PRIMARY KEY ${table.primary_key}`].join(',\n    ');
			const options = table.options ? ` WITH ${table.options}` : '';
			statements.push(`CREATE TABLE IF NOT EXISTS ${quotedKeyspace}.${table.name} (\n    ${body}\n)${options}`);
			continue;
		}
		for (const column of table.columns) {
			if (!columns.has(column.name)) {
				statements.push(`ALTER TABLE ${quotedKeyspace}.${table.name} ADD ${column.name} ${column.type}`);
			}
		}
	}
	for (const index of [...TARGET_SCHEMA.indexes].sort((a, b) => a.name.localeCompare(b.name))) {
		if (!indexes.has(index.name)) {
			statements.push(
				`CREATE INDEX IF NOT EXISTS ${index.name} ON ${quotedKeyspace}.${index.table} (${index.expression})`,
			);
		}
	}
	return statements;
}

async function readAllRows(
	client: CassandraSchemaClient,
	query: string,
	keyspace: string,
	pageSize: number,
): Promise<Array<Record<string, unknown>>> {
	const rows: Array<Record<string, unknown>> = [];
	let pageState: string | undefined;
	do {
		const page = await client.execute(query, [keyspace], {fetchSize: pageSize, ...(pageState ? {pageState} : {})});
		rows.push(...page.rows);
		pageState = page.pageState ?? undefined;
	} while (pageState);
	return rows;
}

async function readExistingSchema(
	client: CassandraSchemaClient,
	keyspace: string,
	pageSize: number,
): Promise<ExistingSchemaRows> {
	const [types, columns, indexes] = await Promise.all([
		readAllRows(
			client,
			'SELECT type_name, field_names FROM system_schema.types WHERE keyspace_name = ?',
			keyspace,
			pageSize,
		),
		readAllRows(
			client,
			'SELECT table_name, column_name FROM system_schema.columns WHERE keyspace_name = ?',
			keyspace,
			pageSize,
		),
		readAllRows(client, 'SELECT index_name FROM system_schema.indexes WHERE keyspace_name = ?', keyspace, pageSize),
	]);
	return {types, columns, indexes};
}

async function scyllaTabletsEnabled(client: CassandraSchemaClient, keyspace: string): Promise<boolean> {
	const {rows: tabletColumns} = await client.execute(
		"SELECT column_name FROM system_schema.columns WHERE keyspace_name = 'system_schema' AND table_name = 'scylla_keyspaces' AND column_name = 'initial_tablets'",
	);
	if (tabletColumns.length === 0) {
		return false;
	}
	const {rows} = await client.execute(
		'SELECT initial_tablets FROM system_schema.scylla_keyspaces WHERE keyspace_name = ?',
		[keyspace],
	);
	return rows.some((row) => row.initial_tablets !== null && row.initial_tablets !== undefined);
}

async function applySchemaStatement(
	client: CassandraSchemaClient,
	keyspace: string,
	statement: string,
	logger: LoggerInterface,
): Promise<void> {
	try {
		await client.execute(statement);
	} catch (error) {
		logger.error(
			{statement, reason: error instanceof Error ? error.message : String(error)},
			errorCode(error) === CASSANDRA_UNAUTHORIZED
				? `The Cassandra login cannot change the schema of keyspace "${keyspace}". Grant it CREATE and ALTER on the keyspace, or run this statement by hand, then start again.`
				: 'A Cassandra schema change failed. Fix the cause shown here, or run this statement by hand, then start again.',
		);
		throw error;
	}
}

async function scyllaTabletsRefusal(
	client: CassandraSchemaClient,
	keyspace: string,
	logger: LoggerInterface,
): Promise<string | null> {
	const table = `${cqlKeyspace(keyspace)}.fluxer_tablets_probe`;
	await applySchemaStatement(
		client,
		keyspace,
		`CREATE TABLE IF NOT EXISTS ${table} (id int PRIMARY KEY, value int)`,
		logger,
	);
	try {
		await client.execute(`CREATE INDEX IF NOT EXISTS fluxer_tablets_probe_value ON ${table} (value)`);
		await client.execute(`INSERT INTO ${table} (id, value) VALUES (0, 0) IF NOT EXISTS`);
		return null;
	} catch (error) {
		if (errorCode(error) !== CASSANDRA_INVALID) {
			throw error;
		}
		return error instanceof Error ? error.message : String(error);
	} finally {
		await applySchemaStatement(client, keyspace, `DROP TABLE IF EXISTS ${table}`, logger);
	}
}

export async function ensureCassandraSchema({
	client,
	keyspace,
	kv,
	logger,
	wait = () => sleep(LOCK_RETRY_MS),
	pageSize = SCHEMA_PAGE_SIZE,
}: {
	client: CassandraSchemaClient;
	keyspace: string;
	kv: Pick<IKVProvider, 'acquireLock' | 'extendLock' | 'releaseLock'>;
	logger: LoggerInterface;
	wait?: () => Promise<unknown>;
	pageSize?: number;
}): Promise<void> {
	const token = randomUUID();
	while (!(await kv.acquireLock(LOCK_KEY, token, LOCK_TTL_SECONDS))) {
		await wait();
	}
	let lockLost = false;
	const renewal = setInterval(() => {
		kv.extendLock(LOCK_KEY, token, LOCK_TTL_SECONDS).then(
			(held) => {
				lockLost ||= !held;
			},
			(error: unknown) => {
				lockLost = true;
				logger.warn({error}, 'Could not renew the Cassandra schema lock');
			},
		);
	}, LOCK_RENEW_MS);
	try {
		const statements = planCassandraSchema(await readExistingSchema(client, keyspace, pageSize), keyspace);
		if (statements.length > 0 && (await scyllaTabletsEnabled(client, keyspace))) {
			const reason = await scyllaTabletsRefusal(client, keyspace, logger);
			if (reason) {
				const message = `The Scylla keyspace "${keyspace}" has tablets turned on, and this Scylla server refused the lightweight transactions or secondary indexes Fluxer needs on them, so Fluxer stopped before changing its schema. Drop the keyspace, create it again with AND tablets = {'enabled': false} at the end of the CREATE KEYSPACE statement, then start again. On Scylla 2025.4 or later you can keep tablets and turn on rf_rack_valid_keyspaces instead.`;
				logger.error({keyspace, reason}, message);
				throw new Error(message);
			}
		}
		for (const statement of statements) {
			if (lockLost) {
				throw new Error('Lost the Cassandra schema lock before the schema was complete. Start again to finish.');
			}
			logger.info({statement}, 'Applying Cassandra schema change');
			await applySchemaStatement(client, keyspace, statement, logger);
		}
		logger.info({applied: statements.length}, 'Cassandra schema is up to date');
	} finally {
		clearInterval(renewal);
		await kv.releaseLock(LOCK_KEY, token).catch((error: unknown) => {
			logger.warn({error}, 'Could not release the Cassandra schema lock');
		});
	}
}
