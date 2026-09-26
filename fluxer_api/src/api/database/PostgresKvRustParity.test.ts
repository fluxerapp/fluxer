// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

const THIS_DIR = path.dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = path.resolve(THIS_DIR, '../../../..');

const TYPESCRIPT_SOURCE = fs.readFileSync(path.join(THIS_DIR, 'PostgresKvQueryExecutor.ts'), 'utf8');
const RUST_SOURCE = fs.readFileSync(path.join(REPO_ROOT, 'fluxer_svc/src/postgres.rs'), 'utf8');

const RUST_INDEX_SUFFIXES = [
	'expires_idx',
	'message_reactions_message_idx',
	'messages_message_idx',
	'partition_idx',
	'partition_row_idx',
	'row_key_c_idx',
];

const SHARED_LITERALS = ['120s', '30min', '__fluxer_schema_migrations', 'messages_partition_key_v1'];

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
	it('creates the same indexes apart from the API-only numeric index', () => {
		const typescript = indexSuffixes(TYPESCRIPT_SOURCE, /\$\{kvTable\}_([a-z_]+)/g);
		const rust = indexSuffixes(RUST_SOURCE, /\{kv_table\}_([a-z_]+)/g);
		expect(rust).toEqual(RUST_INDEX_SUFFIXES);
		expect(typescript.filter((suffix) => !rust.includes(suffix))).toEqual(['row_key_numeric_idx']);
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
