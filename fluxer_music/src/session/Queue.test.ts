// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {MusicError} from '../Errors';
import type {ResolvedTrack} from '../Types';
import {Queue} from './Queue';

function tracks(n: number, prefix = 't'): Array<ResolvedTrack> {
	return Array.from({length: n}, (_, i) => ({
		title: `${prefix}${i}`,
		author: 'a',
		durationMs: 1000,
		thumbnailUrl: null,
		url: `https://youtu.be/${prefix}${i}`,
		source: 'youtube' as const,
	}));
}

describe('Queue', () => {
	it('add appends and assigns ids and addedBy', () => {
		const q = new Queue();
		expect(q.add(tracks(2), 'u1')).toBe(2);
		const first = q.advance();
		expect(first?.addedBy).toBe('u1');
		expect(first?.id).toBeTruthy();
		expect(first?.title).toBe('t0');
		expect(q.upcoming.map((t) => t.title)).toEqual(['t1']);
	});

	it('add next inserts at the front of upcoming', () => {
		const q = new Queue();
		q.add(tracks(2), 'u1');
		q.add(tracks(2, 'n'), 'u2', {next: true});
		expect(q.upcoming.map((t) => t.title)).toEqual(['n0', 'n1', 't0', 't1']);
	});

	it('add caps at 500 and throws QUEUE_FULL when full', () => {
		const q = new Queue();
		expect(q.add(tracks(600), 'u1')).toBe(500);
		expect(() => q.add(tracks(1), 'u1')).toThrow(MusicError);
		try {
			q.add(tracks(1), 'u1');
		} catch (e) {
			expect((e as MusicError).code).toBe('QUEUE_FULL');
		}
	});

	it('capacity counts the current track', () => {
		const q = new Queue({maxLength: 3});
		q.add(tracks(1), 'u1');
		q.advance();
		expect(q.add(tracks(5), 'u1')).toBe(2);
	});

	it('loop track repeats, skip does not', () => {
		const q = new Queue();
		q.add(tracks(2), 'u1');
		q.advance();
		q.loop = 'track';
		const a = q.advance();
		const b = q.advance();
		expect(a?.id).toBe(b?.id);
		expect(q.skip()?.title).toBe('t1');
	});

	it('loop queue rotates current to the end', () => {
		const q = new Queue();
		q.add(tracks(3), 'u1');
		q.advance();
		q.loop = 'queue';
		expect(q.advance()?.title).toBe('t1');
		expect(q.upcoming.map((t) => t.title)).toEqual(['t2', 't0']);
	});

	it('loop off drops current and ends with null', () => {
		const q = new Queue();
		q.add(tracks(1), 'u1');
		q.advance();
		expect(q.advance()).toBeNull();
		expect(q.current).toBeNull();
	});

	it('remove and move work on upcoming only', () => {
		const q = new Queue();
		q.add(tracks(4), 'u1');
		const current = q.advance()!;
		expect(q.remove(current.id)).toBe(false);
		expect(q.move(current.id, 0)).toBe(false);
		const [a, b, c] = q.upcoming;
		expect(q.remove(a!.id)).toBe(true);
		expect(q.move(c!.id, 0)).toBe(true);
		expect(q.upcoming.map((t) => t.id)).toEqual([c!.id, b!.id]);
		expect(q.move(c!.id, 99)).toBe(true);
		expect(q.upcoming.map((t) => t.id)).toEqual([b!.id, c!.id]);
	});

	it('shuffle keeps the same set', () => {
		let seed = 1;
		const q = new Queue({random: () => (seed = (seed * 16807) % 2147483647) / 2147483647});
		q.add(tracks(20), 'u1');
		const before = q.upcoming.map((t) => t.id);
		q.shuffle();
		const after = q.upcoming.map((t) => t.id);
		expect([...after].sort()).toEqual([...before].sort());
		expect(after).not.toEqual(before);
	});

	it('page returns slices and the total', () => {
		const q = new Queue();
		q.add(tracks(5), 'u1');
		const p = q.page(1, 2);
		expect(p.total).toBe(5);
		expect(p.tracks.map((t) => t.title)).toEqual(['t1', 't2']);
	});

	it('clear and reset', () => {
		const q = new Queue();
		q.add(tracks(3), 'u1');
		q.advance();
		q.clear();
		expect(q.upcoming).toHaveLength(0);
		expect(q.current).not.toBeNull();
		q.reset();
		expect(q.current).toBeNull();
	});
});
