// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type MessageFocusCandidate,
	resolveBottommostFocusableMessageId,
} from '@app/features/channel/components/ChannelMessageFocusTarget';
import {describe, expect, it} from 'vitest';

function candidate(messageId: string, top: number, height: number): MessageFocusCandidate {
	return {messageId, top, bottom: top + height, height};
}

const VIEWPORT_TOP = 0;
const VIEWPORT_BOTTOM = 600;

describe('resolveBottommostFocusableMessageId', () => {
	it('picks the bottom-most mostly visible message', () => {
		const result = resolveBottommostFocusableMessageId(
			[candidate('a', 10, 100), candidate('b', 150, 100), candidate('c', 300, 100)],
			VIEWPORT_TOP,
			VIEWPORT_BOTTOM,
		);
		expect(result).toBe('c');
	});

	it('skips a message that is mostly scrolled past the bottom edge', () => {
		const result = resolveBottommostFocusableMessageId(
			[candidate('a', 10, 100), candidate('b', 150, 100), candidate('c', 550, 100)],
			VIEWPORT_TOP,
			VIEWPORT_BOTTOM,
		);
		expect(result).toBe('b');
	});

	it('falls back to the most visible message when nothing clears the visibility threshold', () => {
		const result = resolveBottommostFocusableMessageId(
			[candidate('a', -900, 1000), candidate('b', 200, 1000)],
			VIEWPORT_TOP,
			VIEWPORT_BOTTOM,
		);
		expect(result).toBe('b');
	});

	it('prefers the most visible message over the last one in the DOM below the viewport', () => {
		const result = resolveBottommostFocusableMessageId(
			[candidate('tall', -100, 1000), candidate('offscreen', 2000, 50)],
			VIEWPORT_TOP,
			VIEWPORT_BOTTOM,
		);
		expect(result).toBe('tall');
	});

	it('breaks a tie on visible height by taking the bottom-most message', () => {
		const result = resolveBottommostFocusableMessageId(
			[candidate('a', -800, 900), candidate('b', 500, 900)],
			VIEWPORT_TOP,
			VIEWPORT_BOTTOM,
		);
		expect(result).toBe('b');
	});

	it('falls back to the last message when nothing overlaps the viewport at all', () => {
		const result = resolveBottommostFocusableMessageId(
			[candidate('a', 900, 100), candidate('b', 1200, 100)],
			VIEWPORT_TOP,
			VIEWPORT_BOTTOM,
		);
		expect(result).toBe('b');
	});

	it('falls back to the last message when the newest one is hidden behind the composer', () => {
		const result = resolveBottommostFocusableMessageId([candidate('only', 400, 400)], VIEWPORT_TOP, VIEWPORT_BOTTOM);
		expect(result).toBe('only');
	});

	it('ignores zero-height rows when scoring visibility', () => {
		expect(
			resolveBottommostFocusableMessageId(
				[candidate('a', 10, 100), candidate('b', 200, 0)],
				VIEWPORT_TOP,
				VIEWPORT_BOTTOM,
			),
		).toBe('a');
	});

	it('returns null when there are no candidates', () => {
		expect(resolveBottommostFocusableMessageId([], VIEWPORT_TOP, VIEWPORT_BOTTOM)).toBeNull();
	});
});
