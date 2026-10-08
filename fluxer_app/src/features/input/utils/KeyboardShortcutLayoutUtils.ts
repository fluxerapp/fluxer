// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyCombo} from '@app/features/input/state/InputKeybind';

interface KeyboardLayoutMapReader {
	get(code: string): string | undefined;
}

interface KeyboardLayoutProvider {
	getLayoutMap?: () => Promise<KeyboardLayoutMapReader>;
}

export interface KeyboardShortcutsOverlayFallbackEvent {
	key: string;
	code?: string;
	shiftKey: boolean;
}

export const DEFAULT_KEYBOARD_SHORTCUTS_OVERLAY_COMBO: KeyCombo = {key: '/', ctrlOrMeta: true};
export const SHIFTED_SLASH_FALLBACK_KEYBOARD_SHORTCUTS_OVERLAY_COMBO: KeyCombo = {key: '.', ctrlOrMeta: true};

const NON_NUMPAD_PRINTABLE_LAYOUT_CODES = [
	'Backquote',
	'Digit1',
	'Digit2',
	'Digit3',
	'Digit4',
	'Digit5',
	'Digit6',
	'Digit7',
	'Digit8',
	'Digit9',
	'Digit0',
	'Minus',
	'Equal',
	'KeyQ',
	'KeyW',
	'KeyE',
	'KeyR',
	'KeyT',
	'KeyY',
	'KeyU',
	'KeyI',
	'KeyO',
	'KeyP',
	'BracketLeft',
	'BracketRight',
	'Backslash',
	'KeyA',
	'KeyS',
	'KeyD',
	'KeyF',
	'KeyG',
	'KeyH',
	'KeyJ',
	'KeyK',
	'KeyL',
	'Semicolon',
	'Quote',
	'KeyZ',
	'KeyX',
	'KeyC',
	'KeyV',
	'KeyB',
	'KeyN',
	'KeyM',
	'Comma',
	'Period',
	'Slash',
	'IntlBackslash',
	'IntlRo',
	'IntlYen',
];

interface NavigatorWithKeyboardLayout {
	keyboard?: KeyboardLayoutProvider;
}

const copyKeyCombo = (combo: KeyCombo): KeyCombo => ({...combo});

export function keyCombosEqual(a: KeyCombo, b: KeyCombo): boolean {
	return (
		a.key === b.key &&
		a.code === b.code &&
		!!a.ctrlOrMeta === !!b.ctrlOrMeta &&
		!!a.ctrl === !!b.ctrl &&
		!!a.alt === !!b.alt &&
		!!a.shift === !!b.shift &&
		!!a.meta === !!b.meta &&
		!!a.global === !!b.global &&
		!!a.enabled === !!b.enabled &&
		(a.mouseButton ?? null) === (b.mouseButton ?? null) &&
		!!a.modifierOnly === !!b.modifierOnly &&
		!!a.bothSides === !!b.bothSides &&
		(a.gamepadButton ?? null) === (b.gamepadButton ?? null)
	);
}

function keyboardLayoutHasDirectSlashKey(layoutMap: KeyboardLayoutMapReader): boolean {
	for (const code of NON_NUMPAD_PRINTABLE_LAYOUT_CODES) {
		if (layoutMap.get(code) === '/') return true;
	}
	return false;
}

function getKeyboardShortcutsOverlayComboForLayoutMap(layoutMap: KeyboardLayoutMapReader): KeyCombo {
	if (keyboardLayoutHasDirectSlashKey(layoutMap)) {
		return copyKeyCombo(DEFAULT_KEYBOARD_SHORTCUTS_OVERLAY_COMBO);
	}
	return copyKeyCombo(SHIFTED_SLASH_FALLBACK_KEYBOARD_SHORTCUTS_OVERLAY_COMBO);
}

function getNavigatorKeyboardLayoutProvider(): KeyboardLayoutProvider | null {
	if (typeof navigator === 'undefined') return null;
	return (navigator as NavigatorWithKeyboardLayout).keyboard ?? null;
}

export async function getKeyboardShortcutsOverlayComboForCurrentLayout(): Promise<KeyCombo | null> {
	const keyboard = getNavigatorKeyboardLayoutProvider();
	if (typeof keyboard?.getLayoutMap !== 'function') return null;
	const layoutMap = await keyboard.getLayoutMap();
	return getKeyboardShortcutsOverlayComboForLayoutMap(layoutMap);
}

export function shouldUseKeyboardShortcutsOverlayFallbackFromEvent(
	event: KeyboardShortcutsOverlayFallbackEvent,
): boolean {
	const code = event.code ?? '';
	if (!code) return false;
	if (code === 'Slash' || code === 'NumpadDivide') return false;
	return event.key === '/' && event.shiftKey;
}
