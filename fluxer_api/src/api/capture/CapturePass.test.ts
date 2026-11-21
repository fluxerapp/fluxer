// SPDX-License-Identifier: AGPL-3.0-or-later

import {replayStatus} from '@app/api/capture/ReplayStatus';
import {collectSettleKeys, passHorizon, passStart} from '@app/api/capture/SettleMain';
import type {SettleRecord} from '@app/api/database/capture/CaptureRecords';
import {describe, expect, it} from 'vitest';

const MINUTE_MS = 60_000;
const nowMs = 1_763_750_000_000;

function status(overrides: Partial<Parameters<typeof replayStatus>[0]> = {}) {
	return replayStatus({
		progress: {pending: 0, ackPending: 0, oldestUnackedMs: null, latestStreamMs: nowMs},
		nowMs,
		awaitingOutcome: 0,
		producersFinished: true,
		broken: null,
		...overrides,
	});
}

describe('replay status', () => {
	it('sets the watermark five minutes below the oldest unresolved record', () => {
		expect(status().watermark).toBe((nowMs - 5 * MINUTE_MS) * 1000);
		const held = status({
			progress: {pending: 3, ackPending: 1, oldestUnackedMs: nowMs - 2 * MINUTE_MS, latestStreamMs: nowMs},
		});
		expect(held.watermark).toBe((nowMs - 7 * MINUTE_MS) * 1000);
		expect(held.lag_seconds).toBe(120);
		expect(held.pending_records).toBe(4);
		expect(
			status({progress: {pending: 0, ackPending: 0, oldestUnackedMs: null, latestStreamMs: null}}).watermark,
		).toBeNull();
	});

	it('reports drained only when producers are done, nothing is pending and nothing is broken', () => {
		expect(status().drained).toBe(true);
		expect(status({producersFinished: false}).drained).toBe(false);
		expect(status({awaitingOutcome: 1}).drained).toBe(false);
		expect(status({broken: 'x'}).drained).toBe(false);
		expect(status({progress: {pending: 0, ackPending: 1, oldestUnackedMs: nowMs, latestStreamMs: nowMs}}).drained).toBe(
			false,
		);
		expect(status({progress: {pending: 1, ackPending: 0, oldestUnackedMs: nowMs, latestStreamMs: nowMs}}).drained).toBe(
			false,
		);
	});
});

describe('settle pass arithmetic', () => {
	it('keeps the horizon fifteen minutes back and never past the watermark', () => {
		const nowUs = nowMs * 1000;
		expect(passHorizon(nowUs, {kind: 'mark', watermark: nowUs, drained: false})).toBe(nowUs - 15 * MINUTE_MS * 1000);
		expect(passHorizon(nowUs, {kind: 'mark', watermark: nowUs - 60 * MINUTE_MS * 1000, drained: false})).toBe(
			nowUs - 60 * MINUTE_MS * 1000,
		);
		expect(passHorizon(nowUs, {kind: 'mark', watermark: null, drained: false})).toBeNull();
	});

	it('starts after the last finished pass and repeats a pass that never finished', () => {
		expect(passStart(null)).toEqual({prevH: 0, scanFrom: 1});
		const begin = {kind: 'pass' as const, pass: 'p', state: 'begin' as const, h: 50, prev_h: 20, scan_from: 7};
		expect(passStart(begin)).toEqual({prevH: 20, scanFrom: 7});
		expect(passStart({...begin, state: 'done', scan_from: 9})).toEqual({prevH: 50, scanFrom: 9});
	});

	it('takes the keys of one slice, merges repeats and resumes at the first key of the next slice', () => {
		const record = (ts: number, code: string, reason: SettleRecord['reason'], insert = false): SettleRecord => ({
			kind: 'settle',
			t: 'codes',
			s: 'row',
			c: {code},
			reason,
			ts,
			...(insert ? {insert: true as const} : {}),
		});
		const {keys, nextScanFrom} = collectSettleKeys(
			[
				{seq: 10, record: record(5, 'old', 'unknown')},
				{seq: 11, record: record(20, 'a', 'lwt', true)},
				{seq: 12, record: record(60, 'later', 'unknown')},
				{seq: 13, record: record(30, 'a', 'lwt_unknown')},
				{seq: 14, record: record(40, 'b', 'unknown')},
			],
			10,
			50,
			14,
		);
		expect(keys).toEqual([
			{t: 'codes', s: 'row', c: {code: 'a'}, serial: true, insert: true},
			{t: 'codes', s: 'row', c: {code: 'b'}, serial: false, insert: false},
		]);
		expect(nextScanFrom).toBe(12);
		expect(collectSettleKeys([], 0, 50, 14).nextScanFrom).toBe(15);
	});
});
