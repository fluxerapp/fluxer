// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import type {ResolvedTrack} from '../Types';
import {pickMatch} from './Match';

const yt = (title: string, author: string, durationMs: number): ResolvedTrack => ({
	title,
	author,
	durationMs,
	thumbnailUrl: null,
	url: `https://www.youtube.com/watch?v=${title}`,
	source: 'youtube',
});

describe('pickMatch', () => {
	test('prefers Topic within window', () => {
		const upload = yt('upload', 'Someone', 203_000);
		const topic = yt('topic', 'X - Topic', 206_000);
		expect(pickMatch({durationMs: 200_000}, [upload, topic])).toBe(topic);
	});

	test('rejects outside ±10s', () => {
		expect(pickMatch({durationMs: 200_000}, [yt('far', 'Someone', 215_000)])).toBeNull();
	});

	test('ties go to the earlier candidate', () => {
		const a = yt('a', 'One', 201_000);
		const b = yt('b', 'Two', 199_000);
		expect(pickMatch({durationMs: 200_000}, [a, b])).toBe(a);
	});
});
