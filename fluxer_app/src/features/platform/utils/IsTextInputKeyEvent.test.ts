// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test, vi} from 'vitest';

let nativeMacOS = false;

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	isNativeMacOS: () => nativeMacOS,
}));

const {isTextInputKeyEvent} = await import('@app/features/platform/utils/IsTextInputKeyEvent');

const keyEvent = (init: Partial<KeyboardEvent>): KeyboardEvent =>
	({key: '', ctrlKey: false, metaKey: false, altKey: false, shiftKey: false, ...init}) as KeyboardEvent;

beforeEach(() => {
	nativeMacOS = false;
});

describe('isTextInputKeyEvent', () => {
	test('Alt+digit is a shortcut chord, not typing, off macOS', () => {
		for (const key of ['1', '2', '3', '4', '5', '6', '7', '8', '9']) {
			expect(isTextInputKeyEvent(keyEvent({key, altKey: true}))).toBe(false);
		}
	});

	test('Alt+letter is a shortcut chord, not typing, off macOS', () => {
		expect(isTextInputKeyEvent(keyEvent({key: 'a', altKey: true}))).toBe(false);
	});

	test('Option-composed characters stay typing on macOS', () => {
		nativeMacOS = true;
		expect(isTextInputKeyEvent(keyEvent({key: '¡', altKey: true}))).toBe(true);
		expect(isTextInputKeyEvent(keyEvent({key: '@', altKey: true}))).toBe(true);
	});

	test('a bare printable key is still typing', () => {
		expect(isTextInputKeyEvent(keyEvent({key: 'a'}))).toBe(true);
		expect(isTextInputKeyEvent(keyEvent({key: '1'}))).toBe(true);
	});

	test('ctrl and meta chords and named keys are still not typing', () => {
		expect(isTextInputKeyEvent(keyEvent({key: '1', ctrlKey: true}))).toBe(false);
		expect(isTextInputKeyEvent(keyEvent({key: '1', metaKey: true}))).toBe(false);
		expect(isTextInputKeyEvent(keyEvent({key: 'ArrowUp'}))).toBe(false);
		expect(isTextInputKeyEvent(keyEvent({key: 'ArrowUp', altKey: true}))).toBe(false);
	});

	test('a dead key still counts as typing while Alt is held', () => {
		expect(isTextInputKeyEvent(keyEvent({key: 'Dead', altKey: true}))).toBe(true);
	});
});
