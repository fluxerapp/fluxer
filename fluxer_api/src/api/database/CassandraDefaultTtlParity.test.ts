// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {DEFAULT_TTL_TABLES} from '@app/api/database/PostgresKvDefaultTtlExpiry';
import * as DonationTables from '@app/api/donation/DonationTables';
import * as Tables from '@app/api/Tables';
import {IPINFO_CACHE_TTL_SECONDS, IPINFO_REQUEST_AUDIT_TTL_SECONDS} from '@pkgs/geoip/src/PostgresIpInfoKv';
import {describe, expect, it} from 'vitest';

const THIS_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(THIS_DIR, '../../../..');

interface SchemaTable {
	name: string;
	options: string;
}

const SCHEMA = JSON.parse(fs.readFileSync(path.join(REPO_ROOT, 'tools/dev/cassandra_target_schema.json'), 'utf8')) as {
	tables: Array<SchemaTable>;
};

const SCHEMA_DEFAULTS = new Map<string, number>(
	SCHEMA.tables.flatMap((table): Array<[string, number]> => {
		const match = /default_time_to_live = (\d+)/.exec(table.options);
		return match ? [[table.name, Number(match[1])]] : [];
	}),
);

const DSL_TABLES = [...Object.values(Tables), ...Object.values(DonationTables)];
const DSL_NAMES = new Set<string>(DSL_TABLES.map((table) => table.name));

const NON_DSL_DEFAULTS: Record<string, number | null> = {
	ipinfo_cache: IPINFO_CACHE_TTL_SECONDS,
	ipinfo_requests_by_hour: IPINFO_REQUEST_AUDIT_TTL_SECONDS,
	billing_webhook_events: null,
	forensic_identifier_by_key_day: null,
	forensic_identifier_by_request: null,
	forensic_request_meta_by_actor_day: null,
	forensic_request_meta_by_id: null,
	forensic_request_meta_by_route_day_shard: null,
	forensic_resource_exposure_by_request: null,
	forensic_resource_exposure_by_route_day_shard: null,
	forensic_resource_exposure_by_subject_day: null,
};

const OWN_EXPIRY_PASS = new Set(['jobs_by_id', 'jobs_by_day_bucket']);

function schemaDefault(name: string): number {
	return SCHEMA_DEFAULTS.get(name) ?? 0;
}

function byName(left: {name: string}, right: {name: string}): number {
	return left.name.localeCompare(right.name);
}

describe('Cassandra default TTL parity', () => {
	it('declares every Cassandra default TTL on the matching table', () => {
		const mismatches = DSL_TABLES.flatMap((table) => {
			const declared = table.defaultTtlSeconds ?? 0;
			return declared === schemaDefault(table.name)
				? []
				: [{table: table.name, declared, schema: schemaDefault(table.name)}];
		});
		expect(mismatches).toEqual([]);
	});

	it('declares a writer or no writer for every other table with a default', () => {
		const undeclared = [...SCHEMA_DEFAULTS]
			.filter(([name, ttl]) => ttl > 0 && !DSL_NAMES.has(name) && !Object.hasOwn(NON_DSL_DEFAULTS, name))
			.map(([name]) => name);
		expect(undeclared).toEqual([]);
		const stale = Object.keys(NON_DSL_DEFAULTS).filter((name) => schemaDefault(name) === 0 || DSL_NAMES.has(name));
		expect(stale).toEqual([]);
		const mismatched = Object.entries(NON_DSL_DEFAULTS)
			.filter(([name, ttl]) => ttl !== null && ttl !== schemaDefault(name))
			.map(([name]) => name);
		expect(mismatched).toEqual([]);
	});

	it('the Postgres expiry pass covers every table with a default except the job ledger', () => {
		const expected = [...SCHEMA_DEFAULTS]
			.filter(([name, ttl]) => ttl > 0 && NON_DSL_DEFAULTS[name] !== null && !OWN_EXPIRY_PASS.has(name))
			.map(([name, ttl]) => ({name, defaultTtlSeconds: ttl}))
			.sort(byName);
		expect([...DEFAULT_TTL_TABLES].sort(byName)).toEqual(expected);
	});
});
