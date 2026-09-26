// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	POSTGRES_KV_MIGRATION_TABLE,
	postgresKvPassIsFresh,
	recordPostgresKvCleanPass,
} from '@app/api/database/PostgresKvQueryExecutor';
import * as DonationTables from '@app/api/donation/DonationTables';
import * as Tables from '@app/api/Tables';
import {IPINFO_CACHE_TTL_SECONDS, IPINFO_REQUEST_AUDIT_TTL_SECONDS} from '@pkgs/geoip/src/PostgresIpInfoKv';
import {type IPostgresClient, quoteIdentifier} from '@pkgs/postgres/src/Client';
import {ms} from 'itty-time';

const DEFAULT_TTL_EXPIRY_MARKER = 'default_ttl_expiry_v1';
export const DEFAULT_TTL_EXPIRY_RESUME = 'default_ttl_expiry_v1_resume';
const PAGE_SIZE = 2000;
const CLEAN_PASS_INTERVAL_MS = ms('1 day');
const OWN_EXPIRY_PASS = new Set<string>([Tables.JobsById.name, Tables.JobsByDayBucket.name]);

export const DEFAULT_TTL_TABLES: ReadonlyArray<{name: string; defaultTtlSeconds: number}> = [
	...[...Object.values(Tables), ...Object.values(DonationTables)].flatMap((table) =>
		table.defaultTtlSeconds === undefined || OWN_EXPIRY_PASS.has(table.name)
			? []
			: [{name: table.name, defaultTtlSeconds: table.defaultTtlSeconds}],
	),
	{name: 'ipinfo_cache', defaultTtlSeconds: IPINFO_CACHE_TTL_SECONDS},
	{name: 'ipinfo_requests_by_hour', defaultTtlSeconds: IPINFO_REQUEST_AUDIT_TTL_SECONDS},
];

export interface LegacyDefaultTtlExpiryResult {
	deleted: number;
	expiring: number;
	complete: boolean;
}

interface ResumePoint {
	table: string;
	rowKey: string;
	unset: number;
}

async function readResumePoint(client: IPostgresClient, kvTable: string): Promise<ResumePoint | null> {
	const result = await client.query<{row_data: Record<string, unknown>}>(
		`SELECT row_data FROM ${kvTable} WHERE table_name = $1 AND row_key = $2`,
		[POSTGRES_KV_MIGRATION_TABLE, DEFAULT_TTL_EXPIRY_RESUME],
	);
	const data = result.rows[0]?.row_data;
	if (typeof data?.table !== 'string' || typeof data.row_key !== 'string' || typeof data.unset !== 'number') {
		return null;
	}
	return {table: data.table, rowKey: data.row_key, unset: data.unset};
}

async function writeResumePoint(client: IPostgresClient, kvTable: string, point: ResumePoint | null): Promise<void> {
	if (point === null) {
		await client.query(`DELETE FROM ${kvTable} WHERE table_name = $1 AND row_key = $2`, [
			POSTGRES_KV_MIGRATION_TABLE,
			DEFAULT_TTL_EXPIRY_RESUME,
		]);
		return;
	}
	await client.query(
		`INSERT INTO ${kvTable} (table_name, partition_key, row_key, row_data)
VALUES ($1, $2, $2, jsonb_build_object('table', $3::text, 'row_key', $4::text, 'unset', $5::bigint))
ON CONFLICT (table_name, row_key) DO UPDATE SET row_data = EXCLUDED.row_data, updated_at = now()`,
		[POSTGRES_KV_MIGRATION_TABLE, DEFAULT_TTL_EXPIRY_RESUME, point.table, point.rowKey, point.unset],
	);
}

function pageSql(table: string): string {
	return `
WITH page AS (
	SELECT kv.row_key, kv.expires_at IS NULL AS unset
	FROM ${table} kv
	WHERE kv.table_name = $1 AND kv.row_key > $2
	ORDER BY kv.row_key
	LIMIT $3
), removed AS (
	DELETE FROM ${table} kv
	USING page
	WHERE kv.table_name = $1 AND kv.row_key = page.row_key AND kv.expires_at IS NULL
		AND kv.updated_at + make_interval(secs => $4::double precision) <= now()
	RETURNING 1
), expiring AS (
	UPDATE ${table} kv
	SET expires_at = kv.updated_at + make_interval(secs => $4::double precision)
	FROM page
	WHERE kv.table_name = $1 AND kv.row_key = page.row_key AND kv.expires_at IS NULL
		AND kv.updated_at + make_interval(secs => $4::double precision) > now()
	RETURNING 1
)
SELECT
	(SELECT max(row_key) FROM page) AS last_row_key,
	(SELECT count(*) FROM page WHERE unset) AS unset,
	(SELECT count(*) FROM removed) AS deleted,
	(SELECT count(*) FROM expiring) AS expiring`;
}

export async function expireLegacyDefaultTtlRows(
	client: IPostgresClient,
	deadlineMs: number,
): Promise<LegacyDefaultTtlExpiryResult | null> {
	if (await postgresKvPassIsFresh(client, DEFAULT_TTL_EXPIRY_MARKER, CLEAN_PASS_INTERVAL_MS)) {
		return null;
	}
	const kvTable = quoteIdentifier(client.kvTable());
	const sql = pageSql(kvTable);
	const resume = await readResumePoint(client, kvTable);
	const resumeIndex = resume === null ? -1 : DEFAULT_TTL_TABLES.findIndex((target) => target.name === resume.table);
	let unset = resumeIndex < 0 ? 0 : resume!.unset;
	let deleted = 0;
	let expiring = 0;
	for (let index = Math.max(resumeIndex, 0); index < DEFAULT_TTL_TABLES.length; index += 1) {
		const target = DEFAULT_TTL_TABLES[index]!;
		let cursor = index === resumeIndex ? resume!.rowKey : '';
		for (;;) {
			if (Date.now() >= deadlineMs) {
				await writeResumePoint(client, kvTable, {table: target.name, rowKey: cursor, unset});
				return {deleted, expiring, complete: false};
			}
			const result = await client.query<{
				last_row_key: string | null;
				unset: string;
				deleted: string;
				expiring: string;
			}>(sql, [target.name, cursor, PAGE_SIZE, target.defaultTtlSeconds]);
			const page = result.rows[0];
			if (!page || page.last_row_key === null) {
				break;
			}
			unset += Number(page.unset);
			deleted += Number(page.deleted);
			expiring += Number(page.expiring);
			cursor = page.last_row_key;
		}
	}
	await writeResumePoint(client, kvTable, null);
	if (unset === 0) {
		await recordPostgresKvCleanPass(client, DEFAULT_TTL_EXPIRY_MARKER);
	}
	return {deleted, expiring, complete: true};
}
