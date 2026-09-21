// SPDX-License-Identifier: AGPL-3.0-or-later

import {postgresKvPassIsFresh, recordPostgresKvCleanPass} from '@app/api/database/PostgresKvQueryExecutor';
import {EXPIRED_JOB_ERROR, JOB_LEDGER_TTL_SECONDS, JOB_STALE_AFTER_MS} from '@app/api/jobs/JobLedgerRepository';
import {FLUXER_EPOCH} from '@fluxer/constants/src/Core';
import {TIMESTAMP_SHIFT} from '@fluxer/snowflake/src/Snowflake';
import {type IPostgresClient, quoteIdentifier} from '@pkgs/postgres/src/Client';
import {ms} from 'itty-time';

const LEGACY_JOB_LEDGER_MARKER = 'job_ledger_expiry_v1';
const PAGE_SIZE = 2000;
const CLEAN_PASS_INTERVAL_MS = ms('1 day');
const BIGINT_KEY_PREFIX = '{"__fluxer_type":"bigint","value":"';
const BIGINT_KEY_SUFFIX = '"}';

function bigintKeyExpr(key: string): string {
	const prefixLength = BIGINT_KEY_PREFIX.length;
	const affixLength = prefixLength + BIGINT_KEY_SUFFIX.length;
	return `(CASE WHEN left(${key}, ${prefixLength}) = '${BIGINT_KEY_PREFIX}' AND right(${key}, ${BIGINT_KEY_SUFFIX.length}) = '${BIGINT_KEY_SUFFIX}' THEN substr(${key}, ${prefixLength + 1}, length(${key}) - ${affixLength})::numeric END)`;
}

interface LedgerTable {
	name: string;
	jobIdExpr: string;
	deleteStale: boolean;
	markStaleDeadletter: boolean;
}

const LEDGER_TABLES: ReadonlyArray<LedgerTable> = [
	{
		name: 'jobs_active',
		jobIdExpr: bigintKeyExpr('kv.row_key'),
		deleteStale: true,
		markStaleDeadletter: false,
	},
	{
		name: 'jobs_by_id',
		jobIdExpr: bigintKeyExpr('kv.row_key'),
		deleteStale: false,
		markStaleDeadletter: true,
	},
	{
		name: 'jobs_by_day_bucket',
		jobIdExpr: bigintKeyExpr('split_part(kv.row_key, chr(31), 3)'),
		deleteStale: false,
		markStaleDeadletter: false,
	},
];

export interface LegacyJobLedgerExpiryResult {
	deleted: number;
	expiring: number;
	complete: boolean;
}

function pageSql(table: string, target: LedgerTable): string {
	const staleRemovable = target.deleteStale ? 'c.created_at < $7' : '(c.created_at < $7 AND c.system_job)';
	const removable = `c.job_id IS NULL OR c.created_at < $6 OR ${staleRemovable}`;
	const rowData = target.markStaleDeadletter
		? `CASE WHEN c.unfinished THEN kv.row_data || jsonb_build_object('status', 'deadletter', 'error_message', $9::text, 'completed_at', jsonb_build_object('__fluxer_type', 'date', 'value', $10::text)) ELSE kv.row_data END`
		: 'kv.row_data';
	return `
WITH page AS (
	SELECT kv.row_key, kv.row_data, ${target.jobIdExpr} AS job_id
	FROM ${table} kv
	WHERE kv.table_name = $1 AND kv.expires_at IS NULL AND kv.row_key > $2
	ORDER BY kv.row_key
	LIMIT $3
), classified AS (
	SELECT
		page.row_key,
		page.job_id,
		to_timestamp(((div(page.job_id, $4::numeric) + $5::numeric) / 1000)::double precision) AS created_at,
		COALESCE(page.row_data -> 'requested_by_user_id', 'null'::jsonb) = 'null'::jsonb AS system_job,
		COALESCE(page.row_data ->> 'status' IN ('queued', 'running'), false) AS unfinished
	FROM page
), removed AS (
	DELETE FROM ${table} kv
	USING classified c
	WHERE kv.table_name = $1 AND kv.row_key = c.row_key AND kv.expires_at IS NULL AND (${removable})
	RETURNING kv.row_key
), expiring AS (
	UPDATE ${table} kv
	SET expires_at = c.created_at + make_interval(secs => $8::double precision), updated_at = now(), row_data = ${rowData}
	FROM classified c
	WHERE kv.table_name = $1 AND kv.row_key = c.row_key AND kv.expires_at IS NULL AND c.created_at < $7 AND NOT (${removable})
	RETURNING kv.row_key
)
SELECT
	(SELECT max(row_key) FROM page) AS last_row_key,
	(SELECT count(*) FROM page) AS scanned,
	(SELECT count(*) FROM removed) AS deleted,
	(SELECT count(*) FROM expiring) AS expiring`;
}

export async function expireLegacyJobLedgerRows(
	client: IPostgresClient,
	deadlineMs: number,
): Promise<LegacyJobLedgerExpiryResult | null> {
	const table = quoteIdentifier(client.kvTable());
	if (await postgresKvPassIsFresh(client, LEGACY_JOB_LEDGER_MARKER, CLEAN_PASS_INTERVAL_MS)) {
		return null;
	}
	const now = Date.now();
	const retentionCutoff = new Date(now - JOB_LEDGER_TTL_SECONDS * 1000);
	const staleCutoff = new Date(now - JOB_STALE_AFTER_MS);
	const completedAt = new Date(now).toISOString();
	let scanned = 0;
	let deleted = 0;
	let expiring = 0;
	for (const target of LEDGER_TABLES) {
		const sql = pageSql(table, target);
		const deadletterValues = target.markStaleDeadletter ? [EXPIRED_JOB_ERROR, completedAt] : [];
		let cursor = '';
		for (;;) {
			if (Date.now() >= deadlineMs) {
				return {deleted, expiring, complete: false};
			}
			const result = await client.query<{
				last_row_key: string | null;
				scanned: string;
				deleted: string;
				expiring: string;
			}>(sql, [
				target.name,
				cursor,
				PAGE_SIZE,
				(1n << TIMESTAMP_SHIFT).toString(),
				FLUXER_EPOCH.toString(),
				retentionCutoff,
				staleCutoff,
				JOB_LEDGER_TTL_SECONDS,
				...deadletterValues,
			]);
			const page = result.rows[0];
			if (!page || page.last_row_key === null) {
				break;
			}
			scanned += Number(page.scanned);
			deleted += Number(page.deleted);
			expiring += Number(page.expiring);
			cursor = page.last_row_key;
		}
	}
	if (scanned === 0) {
		await recordPostgresKvCleanPass(client, LEGACY_JOB_LEDGER_MARKER);
	}
	return {deleted, expiring, complete: true};
}
