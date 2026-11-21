// SPDX-License-Identifier: AGPL-3.0-or-later

import {LossTracker} from '@app/api/capture/cdc/CdcLoss';
import {describe, expect, it} from 'vitest';

const MINUTE = 60_000;

describe('loss tracker', () => {
	it('raises nothing for dense seqs from several producers, in any interleaving', () => {
		const tracker = new LossTracker();
		tracker.life({kind: 'life', producer: 'api-0:a', state: 'open', pod: 'api-0'}, 0);
		tracker.life({kind: 'life', producer: 'api-1:b', state: 'open', pod: 'api-1'}, 0);
		for (const [producer, seq] of [
			['api-0:a', 1],
			['api-1:b', 1],
			['api-0:a', 3],
			['api-1:b', 2],
			['api-0:a', 2],
			['api-0:a', 2],
		] as const) {
			tracker.key(producer, seq, MINUTE);
		}
		expect(tracker.check(30 * MINUTE)).toEqual([
			{kind: 'unclosed', producer: 'api-0:a', detail: 'no record for 15 minutes and no closed record'},
			{kind: 'unclosed', producer: 'api-1:b', detail: 'no record for 15 minutes and no closed record'},
		]);
	});

	it('reports a gap that stays open for 5 minutes of stream time, once', () => {
		const tracker = new LossTracker();
		tracker.key('p', 1, 0);
		tracker.key('p', 3, MINUTE);
		expect(tracker.check(5 * MINUTE)).toEqual([]);
		expect(tracker.check(6 * MINUTE)).toEqual([{kind: 'gap', producer: 'p', detail: 'seq 2 missing below 3'}]);
		tracker.key('p', 4, 6 * MINUTE);
		expect(tracker.check(12 * MINUTE)).toEqual([]);
	});

	it('forgets a gap that a late record fills', () => {
		const tracker = new LossTracker();
		tracker.key('p', 1, 0);
		tracker.key('p', 3, 0);
		tracker.key('p', 2, MINUTE);
		expect(tracker.check(10 * MINUTE)).toEqual([]);
	});

	it('treats an alive last_seq above what arrived as a gap', () => {
		const tracker = new LossTracker();
		tracker.key('p', 1, 0);
		tracker.life({kind: 'life', producer: 'p', state: 'alive', last_seq: 4}, MINUTE);
		expect(tracker.check(7 * MINUTE).map((loss) => loss.kind)).toEqual(['gap']);
	});

	it('reports a lossy producer and a producer that closed with drops', () => {
		const tracker = new LossTracker();
		expect(tracker.life({kind: 'life', producer: 'p', state: 'lossy', from_seq: 3, to_seq: 9}, 0)).toEqual([
			{kind: 'lossy', producer: 'p', detail: 'lost seq 3-9'},
		]);
		expect(
			tracker.life({kind: 'life', producer: 'q', state: 'closed', last_seq: 0, dropped: 2}, 0).map((l) => l.kind),
		).toEqual(['lossy']);
	});

	it('reports a producer that closed above the highest seq that arrived', () => {
		const tracker = new LossTracker();
		tracker.key('p', 1, 0);
		tracker.life({kind: 'life', producer: 'p', state: 'closed', last_seq: 2, dropped: 0}, 0);
		expect(tracker.check(5 * MINUTE).map((loss) => loss.kind)).toEqual(['gap']);
		expect(tracker.unclosed()).toBe(0);
	});

	it('forgets a cleanly closed producer without a loss', () => {
		const tracker = new LossTracker();
		tracker.key('p', 1, 0);
		tracker.life({kind: 'life', producer: 'p', state: 'closed', last_seq: 1, dropped: 0}, 0);
		expect(tracker.check(30 * MINUTE)).toEqual([]);
		expect(tracker.takeChanges().get('p')).toBeNull();
	});

	it('reports the earlier producer of a pod that opened again without closing', () => {
		const tracker = new LossTracker();
		tracker.life({kind: 'life', producer: 'api-0:a', state: 'open', pod: 'api-0'}, 0);
		tracker.key('api-0:a', 1, 0);
		expect(tracker.life({kind: 'life', producer: 'api-0:b', state: 'open', pod: 'api-0'}, MINUTE)).toEqual([
			{kind: 'unclosed', producer: 'api-0:a', detail: 'api-0 opened again without a closed record'},
		]);
	});

	it('keeps no progress for a producer that runs with capture off', () => {
		const tracker = new LossTracker();
		expect(tracker.life({kind: 'life', producer: 'api-0:a', state: 'disabled'}, 0)).toEqual([]);
		expect(tracker.check(30 * MINUTE)).toEqual([]);
		expect(tracker.takeChanges().size).toBe(0);
	});

	it('keeps its progress across a restart', () => {
		const first = new LossTracker();
		first.key('p', 1, 0);
		first.key('p', 3, 0);
		const restored = new LossTracker(
			new Map([...first.takeChanges()].flatMap(([id, progress]) => (progress === null ? [] : [[id, progress]]))),
		);
		restored.key('p', 2, MINUTE);
		expect(restored.check(10 * MINUTE)).toEqual([]);
	});
});
