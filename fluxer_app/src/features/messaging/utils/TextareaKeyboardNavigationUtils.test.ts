// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getPreviousVisualLineCaretPositionFromPoints,
	getPreviousWordBoundaryAcrossLineStart,
} from '@app/features/messaging/utils/TextareaKeyboardNavigationUtils';
import {describe, expect, it} from 'vitest';

describe('TextareaKeyboardNavigationUtils', () => {
	it('moves a stuck Shift+ArrowUp focus edge to the previous visual row', () => {
		const points = [
			{position: 0, left: 0, top: 0},
			{position: 40, left: 340, top: 0},
			{position: 42, left: 356, top: 0},
			{position: 43, left: 370, top: 0},
			{position: 45, left: 0, top: 22},
			{position: 86, left: 315, top: 22},
			{position: 87, left: 331, top: 22},
			{position: 88, left: 341, top: 22},
			{position: 89, left: 346, top: 22},
			{position: 90, left: 0, top: 44},
			{position: 131, left: 357, top: 44},
		];

		expect(getPreviousVisualLineCaretPositionFromPoints(points, 89, 357)).toBe(42);
		expect(getPreviousVisualLineCaretPositionFromPoints(points, 42, 357)).toBe(0);
	});

	it('uses the previous visual row start for a left-edge goal column', () => {
		const points = [
			{position: 0, left: 0, top: 0},
			{position: 10, left: 100, top: 0},
			{position: 11, left: 0, top: 22},
			{position: 20, left: 90, top: 22},
			{position: 21, left: 0, top: 44},
		];

		expect(getPreviousVisualLineCaretPositionFromPoints(points, 21, 0)).toBe(11);
	});

	it('returns null when the focus point is unavailable', () => {
		expect(getPreviousVisualLineCaretPositionFromPoints([{position: 0, left: 0, top: 0}], 5, 0)).toBeNull();
	});

	it('skips whitespace before a hard line break for Ctrl+ArrowLeft fallback', () => {
		expect(getPreviousWordBoundaryAcrossLineStart('hello\n   ', 9)).toBe(5);
	});
});
