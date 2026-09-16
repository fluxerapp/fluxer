// SPDX-License-Identifier: AGPL-3.0-or-later

import {isPopoutDragClick} from '@app/features/ui/popover/PopoverDragClickUtils';
import {describe, expect, it} from 'vitest';

const PRESS = {x: 120, y: 240, button: 0};

describe('isPopoutDragClick', () => {
	it('treats a click far from the press as a drag', () => {
		expect(isPopoutDragClick(PRESS, {detail: 1, button: 0, clientX: 160, clientY: 240})).toBe(true);
	});

	it('allows a click within the tolerance of the press', () => {
		expect(isPopoutDragClick(PRESS, {detail: 1, button: 0, clientX: 122, clientY: 242})).toBe(false);
	});

	it('allows a click from a different button than the press', () => {
		expect(isPopoutDragClick(PRESS, {detail: 1, button: 2, clientX: 900, clientY: 900})).toBe(false);
	});

	it('allows a click with no recorded press', () => {
		expect(isPopoutDragClick(null, {detail: 1, button: 0, clientX: 900, clientY: 900})).toBe(false);
	});

	it('allows a keyboard click even when a stale press is recorded', () => {
		expect(isPopoutDragClick(PRESS, {detail: 0, button: 0, clientX: 0, clientY: 0})).toBe(false);
	});
});
