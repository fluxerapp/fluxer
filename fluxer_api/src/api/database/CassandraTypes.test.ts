// SPDX-License-Identifier: AGPL-3.0-or-later

import {BatchBuilder, executeQuery, fetchPage} from '@app/api/database/CassandraQueryExecution';
import {getStatementMeta, normalizeInParams} from '@app/api/database/CassandraTypes';
import {describe, expect, it} from 'vitest';

describe('getStatementMeta', () => {
	it('flags SELECT * statements as unsafe', () => {
		expect(getStatementMeta('SELECT * FROM messages WHERE channel_id = :channel_id').unsafe).toBe(true);
		expect(getStatementMeta('  select   *   FROM messages').unsafe).toBe(true);
		expect(getStatementMeta('SELECT\n\t*\nFROM messages').unsafe).toBe(true);
	});

	it('does not flag projected selects or other statements as unsafe', () => {
		expect(getStatementMeta('SELECT id, content FROM messages WHERE id = :id').unsafe).toBe(false);
		expect(getStatementMeta('SELECT count(*) FROM messages').unsafe).toBe(false);
		expect(getStatementMeta('UPDATE messages SET content = :content WHERE id = :id').unsafe).toBe(false);
		expect(getStatementMeta('SELECT').unsafe).toBe(false);
		expect(getStatementMeta('').unsafe).toBe(false);
	});
});

describe('normalizeInParams', () => {
	it('expands Set values bound to IN parameters into arrays', () => {
		const meta = getStatementMeta('SELECT id FROM messages WHERE id IN :ids');
		const params = {ids: new Set(['a', 'b'])};
		const normalized = normalizeInParams(meta, params);
		expect(normalized).not.toBe(params);
		expect(normalized.ids).toEqual(['a', 'b']);
	});

	it('returns the original params when there is nothing to expand', () => {
		const withoutIn = getStatementMeta('SELECT id FROM messages WHERE id = :id');
		const params = {id: 'a'};
		expect(normalizeInParams(withoutIn, params)).toBe(params);
		const withIn = getStatementMeta('SELECT id FROM messages WHERE id IN :ids');
		const arrayParams = {ids: ['a', 'b']};
		expect(normalizeInParams(withIn, arrayParams)).toBe(arrayParams);
	});
});

describe('unsafe statement guard', () => {
	it('rejects SELECT * through every execution entrypoint', async () => {
		const message = 'Cannot prepare a statement that looks like `SELECT *`';
		await expect(executeQuery('SELECT * FROM messages', {})).rejects.toThrow(message);
		await expect(fetchPage('SELECT * FROM messages', {}, {pageSize: 1})).rejects.toThrow(message);
		await expect(new BatchBuilder().add('SELECT * FROM messages', {}).execute()).rejects.toThrow(message);
	});
});
