// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineTable} from '@app/api/database/CassandraTableDsl';
import {Db, getStatementMeta, type PreparedQuery} from '@app/api/database/CassandraTypes';
import {lwtRows, writeKeys} from '@app/api/database/capture/CaptureKeys';
import {describe, expect, it} from 'vitest';

interface ReactionRow {
	channel_id: bigint;
	message_id: bigint;
	emoji: string;
	user_id: bigint;
	added_at: Date | null;
}

const Reactions = defineTable<ReactionRow, 'channel_id' | 'message_id' | 'emoji' | 'user_id', 'channel_id'>({
	name: 'capture_reactions',
	columns: ['channel_id', 'message_id', 'emoji', 'user_id', 'added_at'],
	primaryKey: ['channel_id', 'message_id', 'emoji', 'user_id'],
	partitionKey: ['channel_id'],
});

interface CodeRow {
	code: string;
	user_id: bigint;
	uses: number;
}

const Codes = defineTable<CodeRow, 'code'>({
	name: 'capture_codes',
	columns: ['code', 'user_id', 'uses'],
	primaryKey: ['code'],
	defaultTtlSeconds: 600,
});

const reaction: ReactionRow = {channel_id: 1n, message_id: 2n, emoji: 'x', user_id: 3n, added_at: null};

function keys(query: PreparedQuery) {
	return writeKeys(query.kvMeta, getStatementMeta(query.cql).type, query.params);
}

describe('write keys', () => {
	it('keys every row write by its full primary key', () => {
		const row = {s: 'row', c: {channel_id: 1n, message_id: 2n, emoji: 'x', user_id: 3n}};
		expect(keys(Reactions.upsertAll(reaction))).toEqual([row]);
		expect(keys(Reactions.insert(reaction))).toEqual([row]);
		expect(keys(Reactions.insertWithTtl(reaction, 60))).toEqual([row]);
		expect(keys(Reactions.upsertAllWithTtl(reaction, 60))).toEqual([row]);
		expect(keys(Reactions.patchByPk(row.c, {added_at: Db.set(new Date(0))}))).toEqual([row]);
		expect(keys(Reactions.patchByPkWithTtl(row.c, {added_at: Db.clear()}, 60))).toEqual([row]);
		expect(keys(Reactions.deleteByPk(row.c))).toEqual([row]);
	});

	it('keys partition and prefix deletes by the columns they name', () => {
		expect(keys(Reactions.deletePartition({channel_id: 1n}))).toEqual([{s: 'partition', c: {channel_id: 1n}}]);
		const prefix = Reactions.delete({
			where: [Reactions.where.eq('channel_id'), Reactions.where.eq('message_id'), Reactions.where.eq('emoji')],
		}).bind({channel_id: 1n, message_id: 2n, emoji: 'x'});
		expect(keys(prefix)).toEqual([{s: 'prefix', c: {channel_id: 1n, message_id: 2n, emoji: 'x'}}]);
	});

	it('expands an IN list into one key per value', () => {
		const query = Reactions.delete({
			where: [Reactions.where.eq('channel_id'), Reactions.where.in('message_id', 'message_ids')],
		}).bind({channel_id: 1n, message_ids: [2n, 4n]});
		expect(keys(query)).toEqual([
			{s: 'prefix', c: {channel_id: 1n, message_id: 2n}},
			{s: 'prefix', c: {channel_id: 1n, message_id: 4n}},
		]);
	});

	it('cannot key a delete that skips the leading key column or has no metadata', () => {
		const query = Reactions.delete({where: Reactions.where.eq('user_id')}).bind({user_id: 3n});
		expect(keys(query)).toBeUndefined();
		expect(writeKeys(undefined, 'DELETE', {channel_id: 1n})).toBeUndefined();
	});
});

describe('conditional rows', () => {
	it('records the row of each conditional constructor and marks inserts', () => {
		const pk = {code: 'abc'};
		const row = {code: 'abc', user_id: 1n, uses: 0};
		const rowsOf = (query: PreparedQuery) => lwtRows(query.kvMeta, query.params);
		expect(rowsOf(Codes.insertIfNotExists(row))).toEqual([{t: 'capture_codes', c: pk, insert: true}]);
		expect(rowsOf(Codes.insertIfNotExistsWithTtl(row, 60))).toEqual([{t: 'capture_codes', c: pk, insert: true}]);
		expect(rowsOf(Codes.conditionalPatchByPk(pk, {uses: Db.set(1)}, {uses: 0}))).toEqual([
			{t: 'capture_codes', c: pk, insert: false},
		]);
		expect(rowsOf(Codes.conditionalPatchByPkWithTtl(pk, {uses: Db.set(1)}, {uses: 0}, 60))).toEqual([
			{t: 'capture_codes', c: pk, insert: false},
		]);
		expect(rowsOf(Codes.conditionalDeleteByPk(pk, {user_id: 1n}))).toEqual([
			{t: 'capture_codes', c: pk, insert: false},
		]);
	});

	it('records every row of a conditional batch', () => {
		const query = Reactions.conditionalBatch([
			{action: 'insert', row: reaction},
			{
				action: 'patch',
				pk: {...reaction, user_id: 4n},
				patch: {added_at: Db.set(new Date(0))},
				expected: {added_at: new Date(1)},
			},
		]);
		const key = {channel_id: 1n, message_id: 2n, emoji: 'x'};
		expect(lwtRows(query.kvMeta, query.params)).toEqual([
			{t: 'capture_reactions', c: {...key, user_id: 3n}, insert: true},
			{t: 'capture_reactions', c: {...key, user_id: 4n}, insert: false},
		]);
	});
});
