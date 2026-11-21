// SPDX-License-Identifier: AGPL-3.0-or-later

import {ReplayLedger} from '@app/api/capture/ReplayLedger';
import type {
	CaptureOutcome,
	IntentRecord,
	LwtIntentRecord,
	OutcomeRecord,
} from '@app/api/database/capture/CaptureRecords';
import {describe, expect, it} from 'vitest';

const producer = 'api-0:boot';

function intent(seq: number, keyed = true): IntentRecord {
	return {
		kind: 'i',
		producer,
		seq,
		ts: 1_000_000 + seq,
		logged: false,
		statements: [
			{
				t: 'codes',
				q: 'DELETE FROM codes WHERE code = :code;',
				p: {code: `c${seq}`},
				...(keyed ? {keys: [{s: 'row' as const, c: {code: `c${seq}`}}]} : {}),
			},
		],
	};
}

function lwt(seq: number, from = producer): LwtIntentRecord {
	return {kind: 'l', producer: from, seq, ts: 2_000_000 + seq, rows: [{t: 'codes', c: {code: 'x'}, insert: true}]};
}

function outcome(...outcomes: Array<[number, CaptureOutcome]>) {
	return {kind: 'o' as const, producer, outcomes};
}

describe('replay ledger', () => {
	it('applies applied writes, drops failed ones and settles unknown ones', () => {
		const ledger = new ReplayLedger<string>();
		for (const seq of [1, 2, 3, 4]) ledger.intent(seq === 4 ? lwt(seq) : intent(seq), 0, `m${seq}`);
		const actions = ledger.outcome(outcome([1, 'applied'], [2, 'failed'], [3, 'unknown'], [4, 'not_applied']));
		expect(actions).toEqual([
			{kind: 'apply', intent: intent(1), handle: 'm1'},
			{kind: 'drop', handle: 'm2', reason: 'failed'},
			{
				kind: 'settle',
				handle: 'm3',
				keys: [{kind: 'settle', t: 'codes', s: 'row', c: {code: 'c3'}, reason: 'unknown', ts: 1_000_003}],
			},
			{kind: 'drop', handle: 'm4', reason: 'not_applied'},
		]);
		expect(ledger.awaitingOutcome()).toBe(0);
	});

	it('settles the rows of applied and unknown conditional writes, marking unknown ones for a serial read', () => {
		const ledger = new ReplayLedger<string>();
		ledger.intent(lwt(1), 0, 'm1');
		ledger.intent(lwt(2), 0, 'm2');
		const actions = ledger.outcome(outcome([1, 'applied'], [2, 'unknown']));
		expect(actions.map((action) => (action.kind === 'settle' ? action.keys[0] : action))).toEqual([
			{kind: 'settle', t: 'codes', s: 'row', c: {code: 'x'}, reason: 'lwt', ts: 2_000_001, insert: true},
			{kind: 'settle', t: 'codes', s: 'row', c: {code: 'x'}, reason: 'lwt_unknown', ts: 2_000_002, insert: true},
		]);
	});

	it('settles App Attest intents from the outcomes the Rust producer publishes, without waiting 60 s', () => {
		const users = 'users-0:4242-1763750000000000';
		const ledger = new ReplayLedger<string>();
		for (const seq of [1, 2, 3, 4]) {
			const record = JSON.parse(
				`{"kind":"l","producer":"${users}","seq":${seq},"ts":${1_763_750_000_000_000 + seq},"rows":[{"t":"app_attest_keys","c":{"user_id":{"$t":"i64","v":"1309547563201765376"},"key_id":"a2V5"}}]}`,
			) as LwtIntentRecord;
			ledger.intent(record, 0, `m${seq}`);
		}
		const outcomes = JSON.parse(
			`{"kind":"o","producer":"${users}","outcomes":[[1,"applied"],[2,"not_applied"],[3,"unknown"],[4,"failed"]]}`,
		) as OutcomeRecord;
		const key = {user_id: {$t: 'i64', v: '1309547563201765376'}, key_id: 'a2V5'};
		expect(ledger.outcome(outcomes)).toEqual([
			{
				kind: 'settle',
				handle: 'm1',
				keys: [{kind: 'settle', t: 'app_attest_keys', s: 'row', c: key, reason: 'lwt', ts: 1_763_750_000_000_001}],
			},
			{kind: 'drop', handle: 'm2', reason: 'not_applied'},
			{
				kind: 'settle',
				handle: 'm3',
				keys: [
					{kind: 'settle', t: 'app_attest_keys', s: 'row', c: key, reason: 'lwt_unknown', ts: 1_763_750_000_000_003},
				],
			},
			{kind: 'drop', handle: 'm4', reason: 'failed'},
		]);
		expect(ledger.awaitingOutcome()).toBe(0);
		expect(ledger.orphans(60_001)).toEqual([]);
	});

	it('settles an intent with no outcome after 60 s of stream time and ignores its late outcome', () => {
		const ledger = new ReplayLedger<string>();
		ledger.intent(intent(1), 0, 'm1');
		ledger.intent(lwt(2, 'users-0:boot'), 0, 'm2');
		expect(ledger.orphans(60_000)).toEqual([]);
		const orphans = ledger.orphans(60_001);
		expect(orphans.map((action) => action.kind === 'settle' && action.keys[0]?.reason)).toEqual([
			'unknown',
			'lwt_unknown',
		]);
		expect(ledger.outcome(outcome([1, 'applied']))).toEqual([]);
	});

	it('settles what a producer left without an outcome as soon as it closes or is sealed', () => {
		const ledger = new ReplayLedger<string>();
		ledger.intent(lwt(1, 'users-0:boot'), 0, 'm1');
		ledger.intent(intent(2), 0, 'm2');
		const closed = ledger.life({kind: 'life', producer: 'users-0:boot', state: 'closed', last_seq: 1});
		expect(closed.map((action) => action.kind === 'settle' && action.keys[0]?.reason)).toEqual(['lwt_unknown']);
		const sealed = ledger.life({kind: 'life', producer, state: 'sealed'});
		expect(sealed.map((action) => action.kind === 'settle' && action.keys[0]?.reason)).toEqual(['unknown']);
		expect(ledger.awaitingOutcome()).toBe(0);
	});

	it('stops on an unknown outcome it cannot key, a disabled producer and a write from a sealed producer', () => {
		const ledger = new ReplayLedger<string>();
		ledger.intent(intent(1, false), 0, 'm1');
		expect(ledger.outcome(outcome([1, 'unknown']))[0]?.kind).toBe('broken');
		expect(ledger.life({kind: 'life', producer: 'worker-1:boot', state: 'disabled'})[0]?.kind).toBe('broken');
		ledger.life({kind: 'life', producer, state: 'sealed'});
		expect(ledger.intent(intent(2), 0, 'm2')[0]?.kind).toBe('broken');
		expect(ledger.outcome(outcome([2, 'applied']))[0]?.kind).toBe('broken');
	});

	it('counts producers as finished only when every one has closed or been sealed', () => {
		const ledger = new ReplayLedger<string>();
		expect(ledger.allProducersFinished()).toBe(false);
		ledger.life({kind: 'life', producer: 'a', state: 'open'});
		ledger.life({kind: 'life', producer: 'b', state: 'open'});
		ledger.life({kind: 'life', producer: 'a', state: 'closed', last_seq: 4});
		expect(ledger.allProducersFinished()).toBe(false);
		ledger.life({kind: 'life', producer: 'b', state: 'sealed'});
		expect(ledger.allProducersFinished()).toBe(true);
	});
});
