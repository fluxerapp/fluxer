// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeybindCommand, KeyCombo} from '@app/features/input/state/InputKeybind';
import {isWebReservedZoomShortcut} from '@app/features/ui/utils/AppZoomKeybindUtils';
import {describe, expect, it} from 'vitest';

const zoomShortcut = (action: KeybindCommand, combo: KeyCombo, isDesktop = false): boolean =>
	isWebReservedZoomShortcut(action, combo, {isDesktop});

describe('AppZoomKeybindUtils', () => {
	it('reserves standard browser zoom shortcuts on web', () => {
		expect(zoomShortcut('system_zoom_in', {key: '=', ctrlOrMeta: true})).toBe(true);
		expect(zoomShortcut('system_zoom_out', {key: '-', ctrlOrMeta: true})).toBe(true);
		expect(zoomShortcut('system_zoom_reset', {key: '0', ctrlOrMeta: true})).toBe(true);
	});

	it('allows app zoom shortcuts on desktop', () => {
		expect(zoomShortcut('system_zoom_in', {key: '=', ctrlOrMeta: true}, true)).toBe(false);
		expect(zoomShortcut('system_zoom_out', {key: '-', ctrlOrMeta: true}, true)).toBe(false);
		expect(zoomShortcut('system_zoom_reset', {key: '0', ctrlOrMeta: true}, true)).toBe(false);
	});

	it('does not reserve unrelated shortcuts', () => {
		expect(zoomShortcut('misc_help', {key: '=', ctrlOrMeta: true})).toBe(false);
		expect(zoomShortcut('system_zoom_in', {key: '=', alt: true, ctrlOrMeta: true})).toBe(false);
		expect(zoomShortcut('system_zoom_in', {key: '='})).toBe(false);
	});
});
