// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

const THIS_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(THIS_DIR, '../../../..');

const TYPESCRIPT_SOURCE = fs.readFileSync(path.join(THIS_DIR, 'PostgresKvQueryExecutor.ts'), 'utf8');
const RUST_SOURCE = fs.readFileSync(path.join(REPO_ROOT, 'fluxer_svc/src/postgres.rs'), 'utf8');

const RUST_INDEX_SUFFIXES = ['expires_idx', 'partition_idx', 'row_key_c_idx', 'sort_id_fill'];

const API_ONLY_INDEX_SUFFIXES = [
	'message_reactions_message_idx',
	'messages_message_idx',
	'partition_row_idx',
	'row_key_numeric_idx',
];

const SHARED_LITERALS = [
	'120s',
	'30min',
	'10s',
	'__fluxer_schema_migrations',
	'messages_partition_key_v1',
	'sort_id_v1',
	'sort_id bigint,',
	'ADD COLUMN sort_id bigint',
	"attname = 'sort_id'",
	"'lz4' = ANY(enumvals)",
	"att.attcompression <> 'l'",
	'ALTER COLUMN row_data SET COMPRESSION lz4',
	"IF NEW.row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN",
	"NEW.sort_id := (NEW.row_data -> 'message_id' ->> 'value')::bigint;",
	"FOR EACH ROW WHEN (NEW.sort_id IS NULL AND NEW.table_name IN ('messages', 'message_reactions')) EXECUTE FUNCTION",
];

const BACKFILL_FRAGMENTS = [
	"split_part(row_key, chr(31), 3) <> ''",
	'split_part(row_key, chr(31), 1) || chr(31) || split_part(row_key, chr(31), 2)',
];

function indexSuffixes(source: string, pattern: RegExp): Array<string> {
	return [...new Set([...source.matchAll(pattern)].map((match) => match[1]!))].sort();
}

function lockNamespace(source: string, pattern: RegExp): number {
	const match = pattern.exec(source);
	expect(match, 'missing Postgres KV schema lock namespace').not.toBeNull();
	return Number.parseInt(match![1]!.replaceAll('_', ''), 16);
}

describe('Postgres KV schema parity between the API and fluxer_svc', () => {
	it('creates the same indexes apart from the ones the API builds or retires on its own', () => {
		const typescript = indexSuffixes(TYPESCRIPT_SOURCE, /\$\{kvTable\}_([a-z_]+)/g);
		const rust = indexSuffixes(RUST_SOURCE, /\{kv_table\}_([a-z_]+)/g);
		expect(rust).toEqual(RUST_INDEX_SUFFIXES);
		expect(typescript.filter((suffix) => !rust.includes(suffix))).toEqual(API_ONLY_INDEX_SUFFIXES);
		expect(rust.filter((suffix) => !typescript.includes(suffix))).toEqual([]);
	});

	it('takes the same advisory lock namespace', () => {
		expect(lockNamespace(RUST_SOURCE, /POSTGRES_KV_SCHEMA_LOCK_NAMESPACE: i32 = (0x[0-9a-fA-F_]+)/)).toBe(
			lockNamespace(TYPESCRIPT_SOURCE, /POSTGRES_KV_SCHEMA_LOCK_NAMESPACE = (0x[0-9a-fA-F_]+)/),
		);
	});

	it('shares the schema timeouts and the migration marker identity', () => {
		for (const literal of SHARED_LITERALS) {
			expect(TYPESCRIPT_SOURCE, literal).toContain(literal);
			expect(RUST_SOURCE, literal).toContain(literal);
		}
	});

	it('sets the same table storage options', () => {
		const typescript = /POSTGRES_KV_STORAGE_OPTIONS = \[([^\]]*)\]/.exec(TYPESCRIPT_SOURCE)?.[1] ?? '';
		const rust = /POSTGRES_KV_STORAGE_OPTIONS: &\[&str\] = &\[([^\]]*)\]/.exec(RUST_SOURCE)?.[1] ?? '';
		const options = (list: string) => [...list.matchAll(/['"]([a-z_]+=[0-9.]+)['"]/g)].map((match) => match[1]);
		expect(options(typescript).length).toBeGreaterThan(0);
		expect(options(rust)).toEqual(options(typescript));
	});

	it('fills and reads sort ids for the same tables', () => {
		expect(TYPESCRIPT_SOURCE).toContain("POSTGRES_KV_SORT_ID_TABLES = ['messages', 'message_reactions']");
		expect(RUST_SOURCE).toContain('POSTGRES_KV_SORT_ID_TABLES: &[&str] = &["messages", "message_reactions"]');
	});

	it('never disables the statement timeout in fluxer_svc', () => {
		expect(RUST_SOURCE).not.toContain("set_config('statement_timeout', '0'");
	});

	it('backfills message partition keys with byte-identical SQL', () => {
		for (const fragment of BACKFILL_FRAGMENTS) {
			expect(TYPESCRIPT_SOURCE, fragment).toContain(fragment);
			expect(RUST_SOURCE, fragment).toContain(fragment);
		}
	});
});
