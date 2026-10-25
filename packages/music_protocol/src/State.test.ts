// SPDX-License-Identifier: AGPL-3.0-or-later

import {decodeState, encodeState, livePositionMs} from '@fluxer/music_protocol/src/State';
import {MUSIC_STATE_MAX_BYTES, type MusicState, type Track} from '@fluxer/music_protocol/src/Types';
import {describe, expect, test} from 'vitest';

const track = (i: number, title = `Song ${i}`): Track => ({
	id: `t${i}`,
	title,
	author: 'A',
	durationMs: 180000,
	thumbnailUrl: null,
	url: `https://youtu.be/x${i}`,
	source: 'youtube',
	addedBy: '1',
});
const base: MusicState = {
	v: 1,
	status: 'playing',
	current: track(0),
	positionMs: 1000,
	positionAt: 5000,
	loop: 'off',
	upcoming: [],
	queueLength: 0,
	lastAction: null,
};

describe('State', () => {
	test('round-trips a state', () => {
		expect(decodeState(encodeState(base))).toEqual(base);
	});
	test('stays under 14 KiB with 500 long-titled tracks', () => {
		const upcoming = Array.from({length: 500}, (_, i) => track(i, 'x'.repeat(400)));
		const bytes = encodeState({...base, upcoming, queueLength: 500});
		expect(bytes.byteLength).toBeLessThanOrEqual(MUSIC_STATE_MAX_BYTES);
		const out = decodeState(bytes)!;
		expect(out.queueLength).toBe(500);
		expect(out.upcoming.length).toBeGreaterThan(0);
		expect(out.upcoming.length).toBeLessThanOrEqual(25);
		expect(out.upcoming[0].title.length).toBeLessThanOrEqual(200);
	});
	test('drops trailing upcoming entries when multibyte titles overflow', () => {
		const upcoming = Array.from({length: 25}, (_, i) => track(i, '日'.repeat(200)));
		const bytes = encodeState({...base, upcoming, queueLength: 25});
		expect(bytes.byteLength).toBeLessThanOrEqual(MUSIC_STATE_MAX_BYTES);
		const out = decodeState(bytes)!;
		expect(out.upcoming.length).toBeLessThan(25);
		expect(out.upcoming.length).toBeGreaterThan(0);
	});
	test('decodeState rejects garbage and wrong version', () => {
		expect(decodeState(new TextEncoder().encode('nope'))).toBeNull();
		expect(decodeState(new TextEncoder().encode(JSON.stringify({...base, v: 2})))).toBeNull();
	});
	test('livePositionMs advances only while playing and clamps to duration', () => {
		expect(livePositionMs(base, 7000)).toBe(3000);
		expect(livePositionMs({...base, status: 'paused'}, 7000)).toBe(1000);
		expect(livePositionMs(base, 10_000_000)).toBe(180000);
	});
});
