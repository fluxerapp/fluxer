// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {
	type CassandraSchemaClient,
	ensureCassandraSchema,
	logMissingCassandraKeyspace,
	planCassandraSchema,
} from '@app/api/database/CassandraSchemaStartup';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {describe, expect, it} from 'vitest';

const THIS_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(THIS_DIR, '../../../..');

interface SchemaField {
	name: string;
	type: string;
}

const SCHEMA = JSON.parse(fs.readFileSync(path.join(REPO_ROOT, 'tools/dev/cassandra_target_schema.json'), 'utf8')) as {
	user_types: Array<{name: string; fields: Array<SchemaField>}>;
	tables: Array<{name: string; columns: Array<SchemaField>}>;
	indexes: Array<{name: string}>;
};

interface ExistingSchemaRows {
	types: Array<{type_name: string; field_names: Array<string>}>;
	columns: Array<{table_name: string; column_name: string}>;
	indexes: Array<{index_name: string}>;
}

function fullSchemaRows(): ExistingSchemaRows {
	return {
		types: SCHEMA.user_types.map((type) => ({
			type_name: type.name,
			field_names: type.fields.map((field) => field.name),
		})),
		columns: SCHEMA.tables.flatMap((table) =>
			table.columns.map((column) => ({table_name: table.name, column_name: column.name})),
		),
		indexes: SCHEMA.indexes.map((index) => ({index_name: index.name})),
	};
}

function withoutTables(rows: ExistingSchemaRows, names: ReadonlyArray<string>): ExistingSchemaRows {
	return {...rows, columns: rows.columns.filter((row) => !names.includes(row.table_name))};
}

const DRIVER_PAGE_SIZE = 5000;

class RecordingLogger implements LoggerInterface {
	readonly errors: Array<{fields: object; message?: string}> = [];
	trace(): void {}
	debug(): void {}
	info(): void {}
	warn(): void {}
	fatal(): void {}
	error(fields: object | string, message?: string): void {
		this.errors.push(typeof fields === 'string' ? {fields: {}, message: fields} : {fields, message});
	}
	child(): LoggerInterface {
		return this;
	}
}

function cassandraError(code: number, message: string): Error {
	return Object.assign(new Error(message), {code});
}

class FakeCassandra implements CassandraSchemaClient {
	readonly executed: Array<string> = [];
	readonly fetchSizes: Array<number> = [];
	beforeStatement: (query: string) => Promise<void> = async () => {};
	scyllaKeyspaces: Array<{keyspace_name: string; initial_tablets: number | null}> | null = null;
	tabletsProbeError: Error | null = null;
	readonly probes: Array<string> = [];

	constructor(private rows: ExistingSchemaRows) {}

	private page(
		rows: ReadonlyArray<Record<string, unknown>>,
		options?: {fetchSize?: number; pageState?: string},
	): {rows: Array<Record<string, unknown>>; pageState?: string} {
		const size = options?.fetchSize ?? DRIVER_PAGE_SIZE;
		this.fetchSizes.push(size);
		const start = options?.pageState ? Number(options.pageState) : 0;
		const end = start + size;
		return {rows: rows.slice(start, end), ...(end < rows.length ? {pageState: String(end)} : {})};
	}

	async execute(
		query: string,
		params?: Array<unknown>,
		options?: {fetchSize?: number; pageState?: string},
	): Promise<{rows: Array<Record<string, unknown>>; pageState?: string}> {
		if (query.includes('system_schema.columns') && query.includes("'scylla_keyspaces'")) {
			return {rows: this.scyllaKeyspaces ? [{column_name: 'initial_tablets'}] : []};
		}
		if (query.includes('system_schema.scylla_keyspaces')) {
			if (!this.scyllaKeyspaces) {
				throw cassandraError(0x2200, 'unconfigured table scylla_keyspaces');
			}
			expect(params).toEqual(['fluxer']);
			return {rows: this.scyllaKeyspaces.filter((row) => row.keyspace_name === params?.[0])};
		}
		if (query.includes('system_schema.types')) {
			expect(params).toEqual(['fluxer']);
			return this.page(this.rows.types, options);
		}
		if (query.includes('system_schema.columns')) {
			expect(params).toEqual(['fluxer']);
			return this.page(this.rows.columns, options);
		}
		if (query.includes('system_schema.indexes')) {
			expect(params).toEqual(['fluxer']);
			return this.page(this.rows.indexes, options);
		}
		if (query.includes('fluxer_tablets_probe')) {
			this.probes.push(query);
			if (this.tabletsProbeError && /^(CREATE INDEX|INSERT)/.test(query)) {
				throw this.tabletsProbeError;
			}
			return {rows: []};
		}
		await this.beforeStatement(query);
		this.executed.push(query);
		this.rows = fullSchemaRows();
		return {rows: []};
	}
}

function withExtraColumnsFirst(rows: ExistingSchemaRows, count: number): ExistingSchemaRows {
	const extra = Array.from({length: count}, (_, index) => ({
		table_name: 'aaa_operator_table',
		column_name: `c${index}`,
	}));
	return {...rows, columns: [...extra, ...rows.columns]};
}

describe('planCassandraSchema', () => {
	it('plans nothing for a keyspace that already has the target schema', () => {
		expect(planCassandraSchema(fullSchemaRows(), 'fluxer')).toEqual([]);
	});

	it('creates the tables an older release did not have', () => {
		const statements = planCassandraSchema(
			withoutTables(fullSchemaRows(), ['users_by_acl', 'user_sso_identities_by_user']),
			'fluxer',
		);
		expect(statements).toEqual([
			'CREATE TABLE IF NOT EXISTS fluxer.user_sso_identities_by_user (\n    user_id bigint,\n    provider_id text,\n    subject text,\n    claimed_at timestamp,\n    PRIMARY KEY ((user_id), provider_id, subject)\n)',
			'CREATE TABLE IF NOT EXISTS fluxer.users_by_acl (\n    acl text,\n    user_id bigint,\n    PRIMARY KEY ((acl), user_id)\n)',
		]);
	});

	it('adds missing columns, type fields, types and indexes', () => {
		const rows = fullSchemaRows();
		const statements = planCassandraSchema(
			{
				types: rows.types
					.filter((type) => type.type_name !== 'mute_config')
					.map((type) =>
						type.type_name === 'channel_override'
							? {...type, field_names: type.field_names.filter((name) => name !== 'unread_badges')}
							: type,
					),
				columns: rows.columns.filter(
					(row) => !(row.table_name === 'webauthn_credentials' && row.column_name === 'rp_id'),
				),
				indexes: [],
			},
			'fluxer',
		);
		const muteConfig = SCHEMA.user_types.find((type) => type.name === 'mute_config');
		expect(statements[0]).toBe(
			`CREATE TYPE IF NOT EXISTS fluxer.mute_config (\n    ${muteConfig?.fields.map((field) => `${field.name} ${field.type}`).join(',\n    ')}\n)`,
		);
		expect(statements).toContain('ALTER TYPE fluxer.channel_override ADD unread_badges int');
		expect(statements).toContain('ALTER TABLE fluxer.webauthn_credentials ADD rp_id text');
		expect(statements.at(-1)).toBe(
			'CREATE INDEX IF NOT EXISTS billing_action_intents_by_status ON fluxer.billing_action_intents (status)',
		);
	});

	it('keeps clustering order and other table options', () => {
		const statements = planCassandraSchema(withoutTables(fullSchemaRows(), ['admin_api_keys_by_creator']), 'fluxer');
		expect(statements).toHaveLength(1);
		expect(statements[0]).toMatch(/\) WITH CLUSTERING ORDER BY \(key_id DESC\)$/);
	});

	it('quotes a mixed-case keyspace the way the driver connects to it', () => {
		const statements = planCassandraSchema(withoutTables(fullSchemaRows(), ['users_by_acl']), 'Fluxer');
		expect(statements).toHaveLength(1);
		expect(statements[0]).toMatch(/^CREATE TABLE IF NOT EXISTS "Fluxer"\.users_by_acl \(/);
	});

	it('refuses a keyspace name that is not a plain identifier', () => {
		expect(() => planCassandraSchema(fullSchemaRows(), 'fluxer; DROP KEYSPACE fluxer')).toThrow();
	});
});

describe('ensureCassandraSchema', () => {
	it('creates the missing tables under the schema lock and releases it', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl', 'user_sso_identities_by_user']));
		const kv = new MockKVProvider();
		await ensureCassandraSchema({client, keyspace: 'fluxer', kv, logger: new NoopLogger()});
		expect(client.executed).toHaveLength(2);
		expect(client.executed[0]).toContain('fluxer.user_sso_identities_by_user');
		expect(client.executed[1]).toContain('fluxer.users_by_acl');
		expect(await kv.acquireLock('cassandra_schema_startup', 'probe', 10)).toBe(true);
	});

	it('runs nothing when the schema is complete', async () => {
		const client = new FakeCassandra(fullSchemaRows());
		await ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger: new NoopLogger()});
		expect(client.executed).toEqual([]);
	});

	it('waits for another process to finish, then finds nothing left to do', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		const kv = new MockKVProvider();
		await kv.acquireLock('cassandra_schema_startup', 'other', 60);
		let waits = 0;
		await ensureCassandraSchema({
			client,
			keyspace: 'fluxer',
			kv,
			logger: new NoopLogger(),
			wait: async () => {
				waits += 1;
				if (waits === 2) {
					await client.execute('CREATE TABLE IF NOT EXISTS fluxer.users_by_acl (acl text PRIMARY KEY)');
					await kv.releaseLock('cassandra_schema_startup', 'other');
				}
			},
		});
		expect(waits).toBe(2);
		expect(client.executed).toEqual(['CREATE TABLE IF NOT EXISTS fluxer.users_by_acl (acl text PRIMARY KEY)']);
	});

	it('reads every page of a keyspace with more columns than one driver page', async () => {
		const client = new FakeCassandra(withExtraColumnsFirst(fullSchemaRows(), DRIVER_PAGE_SIZE));
		await ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger: new NoopLogger()});
		expect(client.executed).toEqual([]);
	});

	it('pages through the schema with a tiny page size', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		await ensureCassandraSchema({
			client,
			keyspace: 'fluxer',
			kv: new MockKVProvider(),
			logger: new NoopLogger(),
			pageSize: 3,
		});
		expect(client.fetchSizes.length).toBeGreaterThan(SCHEMA.tables.length);
		expect(new Set(client.fetchSizes)).toEqual(new Set([3]));
		expect(client.executed).toHaveLength(1);
		expect(client.executed[0]).toContain('fluxer.users_by_acl');
	});
});

describe('ensureCassandraSchema lock', () => {
	const LOCK_KEY = 'cassandra_schema_startup';

	it('releases the lock when a schema change fails', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		client.beforeStatement = async () => {
			throw new Error('boom');
		};
		const kv = new MockKVProvider();
		await expect(ensureCassandraSchema({client, keyspace: 'fluxer', kv, logger: new NoopLogger()})).rejects.toThrow(
			'boom',
		);
		expect(await kv.acquireLock(LOCK_KEY, 'other', 10)).toBe(true);
	});

	it('releases the lock when reading the schema fails', async () => {
		const kv = new MockKVProvider();
		const client: CassandraSchemaClient = {
			execute: async () => {
				throw new Error('read failed');
			},
		};
		await expect(ensureCassandraSchema({client, keyspace: 'fluxer', kv, logger: new NoopLogger()})).rejects.toThrow(
			'read failed',
		);
		expect(await kv.acquireLock(LOCK_KEY, 'other', 10)).toBe(true);
	});
});

describe('ensureCassandraSchema failures', () => {
	it('names the statement and the missing permission when the login cannot change the schema', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		const denied = cassandraError(
			0x2100,
			'User limited has no CREATE permission on <all tables in fluxer> or any of its parents',
		);
		client.beforeStatement = async () => {
			throw denied;
		};
		const logger = new RecordingLogger();
		await expect(ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger})).rejects.toBe(
			denied,
		);
		expect(logger.errors).toHaveLength(1);
		const [entry] = logger.errors;
		expect(entry?.fields).toMatchObject({
			statement: expect.stringContaining('CREATE TABLE IF NOT EXISTS fluxer.users_by_acl'),
			reason: denied.message,
		});
		expect(entry?.message).toContain('CREATE and ALTER');
		expect(entry?.message).toContain('"fluxer"');
	});

	it('names the statement and the reason when a change fails for another reason', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		client.beforeStatement = async () => {
			throw cassandraError(0x2200, 'Unknown type fluxer.mute_config');
		};
		const logger = new RecordingLogger();
		await expect(ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger})).rejects.toThrow(
			'Unknown type',
		);
		expect(logger.errors[0]?.fields).toMatchObject({
			statement: expect.stringContaining('fluxer.users_by_acl'),
			reason: 'Unknown type fluxer.mute_config',
		});
		expect(logger.errors[0]?.message).not.toContain('CREATE and ALTER');
	});
});

describe('ensureCassandraSchema on Scylla', () => {
	const INDEX_REFUSED = cassandraError(
		0x2200,
		"Secondary indexes are not supported on base tables with tablets (keyspace 'fluxer')",
	);
	const LWT_REFUSED = cassandraError(
		0x2200,
		'Cannot use LightWeight Transactions for table fluxer.fluxer_tablets_probe: LWT is not yet supported with tablets',
	);

	it('stops before creating anything when this Scylla cannot index tables with tablets', async () => {
		const client = new FakeCassandra({types: [], columns: [], indexes: []});
		client.scyllaKeyspaces = [{keyspace_name: 'fluxer', initial_tablets: 0}];
		client.tabletsProbeError = INDEX_REFUSED;
		const logger = new RecordingLogger();
		const kv = new MockKVProvider();
		await expect(ensureCassandraSchema({client, keyspace: 'fluxer', kv, logger})).rejects.toThrow(/tablets/);
		expect(client.executed).toEqual([]);
		expect(client.probes.at(-1)).toBe('DROP TABLE IF EXISTS fluxer.fluxer_tablets_probe');
		expect(logger.errors).toHaveLength(1);
		expect(logger.errors[0]?.fields).toMatchObject({reason: INDEX_REFUSED.message});
		expect(logger.errors[0]?.message).toContain('"fluxer" has tablets turned on');
		expect(logger.errors[0]?.message).toContain("AND tablets = {'enabled': false}");
		expect(logger.errors[0]?.message).toContain('rf_rack_valid_keyspaces');
		expect(await kv.acquireLock('cassandra_schema_startup', 'other', 10)).toBe(true);
	});

	it('stops when this Scylla cannot run lightweight transactions with tablets', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		client.scyllaKeyspaces = [{keyspace_name: 'fluxer', initial_tablets: 8}];
		client.tabletsProbeError = LWT_REFUSED;
		await expect(
			ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger: new RecordingLogger()}),
		).rejects.toThrow(/tablets/);
		expect(client.executed).toEqual([]);
		expect(client.probes.at(-1)).toBe('DROP TABLE IF EXISTS fluxer.fluxer_tablets_probe');
	});

	it('creates the schema when this Scylla supports indexes and transactions with tablets', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		client.scyllaKeyspaces = [{keyspace_name: 'fluxer', initial_tablets: 0}];
		await ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger: new NoopLogger()});
		expect(client.executed).toHaveLength(1);
		expect(client.executed[0]).toContain('fluxer.users_by_acl');
		expect(client.probes.at(-1)).toBe('DROP TABLE IF EXISTS fluxer.fluxer_tablets_probe');
	});

	it('leaves a complete keyspace with tablets alone', async () => {
		const client = new FakeCassandra(fullSchemaRows());
		client.scyllaKeyspaces = [{keyspace_name: 'fluxer', initial_tablets: 0}];
		client.tabletsProbeError = INDEX_REFUSED;
		await ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger: new NoopLogger()});
		expect(client.executed).toEqual([]);
		expect(client.probes).toEqual([]);
	});

	it('creates the schema when the keyspace has tablets turned off', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		client.scyllaKeyspaces = [
			{keyspace_name: 'other', initial_tablets: 0},
			{keyspace_name: 'fluxer', initial_tablets: null},
		];
		client.tabletsProbeError = INDEX_REFUSED;
		await ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger: new NoopLogger()});
		expect(client.executed).toHaveLength(1);
		expect(client.executed[0]).toContain('fluxer.users_by_acl');
		expect(client.probes).toEqual([]);
	});

	it('creates the schema when the keyspace has no Scylla row at all', async () => {
		const client = new FakeCassandra(withoutTables(fullSchemaRows(), ['users_by_acl']));
		client.scyllaKeyspaces = [];
		client.tabletsProbeError = INDEX_REFUSED;
		await ensureCassandraSchema({client, keyspace: 'fluxer', kv: new MockKVProvider(), logger: new NoopLogger()});
		expect(client.executed).toHaveLength(1);
		expect(client.probes).toEqual([]);
	});
});

describe('logMissingCassandraKeyspace', () => {
	it('tells the admin to create the keyspace when it does not exist', () => {
		const logger = new RecordingLogger();
		logMissingCassandraKeyspace(
			cassandraError(0x2200, "Keyspace 'fluxer' does not exist"),
			'fluxer',
			'datacenter1',
			logger,
		);
		expect(logger.errors).toHaveLength(1);
		expect(logger.errors[0]?.message).toContain(
			"CREATE KEYSPACE fluxer WITH replication = {'class': 'NetworkTopologyStrategy', 'datacenter1': 3}",
		);
	});

	it('tells Scylla admins to create the keyspace with tablets off', () => {
		const logger = new RecordingLogger();
		logMissingCassandraKeyspace(
			cassandraError(0x2200, "Keyspace 'fluxer' does not exist"),
			'fluxer',
			'datacenter1',
			logger,
		);
		expect(logger.errors[0]?.message).toContain("AND tablets = {'enabled': false}");
	});

	it('quotes a mixed-case keyspace in the example statement', () => {
		const logger = new RecordingLogger();
		logMissingCassandraKeyspace(
			cassandraError(0x2200, "Keyspace 'Fluxer' does not exist"),
			'Fluxer',
			'datacenter1',
			logger,
		);
		expect(logger.errors[0]?.message).toContain('CREATE KEYSPACE "Fluxer" WITH');
	});

	it('stays quiet for other connection errors', () => {
		const logger = new RecordingLogger();
		logMissingCassandraKeyspace(new Error('All host(s) tried for query failed'), 'fluxer', 'datacenter1', logger);
		logMissingCassandraKeyspace(
			cassandraError(0x2200, "Keyspace 'other' does not exist"),
			'fluxer',
			'datacenter1',
			logger,
		);
		expect(logger.errors).toEqual([]);
	});
});
