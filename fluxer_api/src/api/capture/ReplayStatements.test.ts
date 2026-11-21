// SPDX-License-Identifier: AGPL-3.0-or-later

import {ReplayRefusal, remainingTtl, replayStatement} from '@app/api/capture/ReplayStatements';
import {defineTable} from '@app/api/database/CassandraTableDsl';
import {Db, type PreparedQuery} from '@app/api/database/CassandraTypes';
import {describe, expect, it} from 'vitest';

interface TokenRow {
	token_: string;
	user_id: bigint;
	note: string | null;
}

const Tokens = defineTable<TokenRow, 'token_'>({
	name: 'replay_tokens',
	columns: ['token_', 'user_id', 'note'],
	primaryKey: ['token_'],
	defaultTtlSeconds: 600,
});

const Plain = defineTable<TokenRow, 'token_'>({
	name: 'replay_plain',
	columns: ['token_', 'user_id', 'note'],
	primaryKey: ['token_'],
});

const row: TokenRow = {token_: 't', user_id: 1n, note: null};
const ts = 1_763_750_000_000_000;
const tsMs = ts / 1000;

function replay(query: PreparedQuery, nowMs: number, tableDefaultTtl: number, capturedTtl?: number) {
	return replayStatement(
		{t: 'replay', q: query.cql, p: {}, ...(capturedTtl === undefined ? {} : {ttl: capturedTtl})},
		query.params,
		ts,
		nowMs,
		tableDefaultTtl,
	);
}

describe('replay statements', () => {
	it('counts the remaining ttl from the write timestamp, rounding the age up', () => {
		expect(remainingTtl(600, ts, tsMs)).toBe(600);
		expect(remainingTtl(600, ts, tsMs + 1)).toBe(599);
		expect(remainingTtl(600, ts, tsMs + 590_000)).toBe(10);
		expect(remainingTtl(600, ts, tsMs + 610_000)).toBe(1);
	});

	it('lowers a bound ttl and keeps an expired write as a cell with a one second ttl', () => {
		for (const query of [
			Plain.insertWithTtl(row, 600),
			Plain.upsertAllWithTtl(row, 600),
			Plain.patchByPkWithTtl({token_: 't'}, {note: Db.set('x')}, 600),
		]) {
			expect(replay(query, tsMs + 100_000, 0).params.ttl_seconds_bind).toBe(500);
			expect(replay(query, tsMs + 700_000, 0).params.ttl_seconds_bind).toBe(1);
			expect(replay(query, tsMs + 700_000, 0).cql).toBe(query.cql);
		}
	});

	it('leaves a write that asked for no ttl alone', () => {
		const query = Plain.insertWithTtl(row, 0);
		expect(replay(query, tsMs + 700_000, 0)).toEqual({cql: query.cql, params: query.params});
	});

	it('adds the remaining default ttl to inserts and updates of a default-ttl table', () => {
		const insert = replay(Tokens.insert(row), tsMs + 100_000, 600, 600);
		expect(insert.cql).toBe(
			'INSERT INTO replay_tokens (token_, user_id, note) VALUES (:token_, :user_id, :note) USING TTL :replay_ttl;',
		);
		expect(insert.params.replay_ttl).toBe(500);
		const update = replay(Tokens.patchByPk({token_: 't'}, {note: Db.set('x')}), tsMs + 100_000, 600, 600);
		expect(update.cql).toMatch(
			/^UPDATE replay_tokens USING TTL :replay_ttl\s+SET note = :note\s+WHERE token_ = :token_;/,
		);
		expect(update.params.replay_ttl).toBe(500);
		const upsert = replay(Tokens.upsertAll(row), tsMs + 700_000, 600, 600);
		expect(upsert.params.replay_ttl).toBe(1);
	});

	it('never adds a ttl to a delete', () => {
		const query = Tokens.deleteByPk({token_: 't'});
		expect(replay(query, tsMs + 100_000, 600)).toEqual({cql: query.cql, params: query.params});
	});

	it('refuses a default ttl that differs from the cluster and layouts it cannot replay', () => {
		expect(() => replay(Tokens.insert(row), tsMs, 300, 600)).toThrow(ReplayRefusal);
		expect(() => replay(Tokens.insertIfNotExists(row), tsMs, 600, 600)).toThrow(ReplayRefusal);
		const refused = [
			'BEGIN BATCH INSERT INTO replay_tokens (token_) VALUES (:token_); APPLY BATCH;',
			'INSERT INTO replay_tokens (token_, user_id) VALUES (:token_, now());',
			'UPDATE replay_tokens USING TIMESTAMP 1 SET note = :note WHERE token_ = :token_;',
			'UPDATE replay_tokens SET uses = uses + 1 WHERE token_ = :token_;',
		];
		for (const cql of refused) {
			expect(() => replayStatement({t: 'replay_tokens', q: cql, p: {}, ttl: 600}, {}, ts, tsMs, 600), cql).toThrow(
				ReplayRefusal,
			);
		}
	});
});
