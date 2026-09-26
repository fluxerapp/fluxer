// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyCombo} from '@app/features/input/state/InputKeybind';
import {SHIFT_KEY_DESCRIPTOR} from '@app/features/input/utils/KeyboardUtils';

import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const isMac = () => /Mac|iPod|iPhone|iPad/.test(navigator.platform);
const CONTROL_KEY_SYMBOL = '⌃';

const isCodeForDisplayNumpadKey = (code: string | undefined): boolean => !!code && /^Numpad/.test(code);
const isShiftKeyName = (key: string): boolean => key === 'Shift' || key === 'ShiftLeft' || key === 'ShiftRight';
const isControlKeyName = (key: string): boolean => key === 'Control' || key === 'ControlLeft' || key === 'ControlRight';
const isAltKeyName = (key: string): boolean => key === 'Alt' || key === 'AltLeft' || key === 'AltRight';
const isMetaKeyName = (key: string): boolean => key === 'Meta' || key === 'MetaLeft' || key === 'MetaRight';
const isModifierKeyName = (key: string): boolean =>
	isShiftKeyName(key) || isControlKeyName(key) || isAltKeyName(key) || isMetaKeyName(key);

export function getPrimaryKeyForComboDisplay(combo: Pick<KeyCombo, 'key' | 'code'>): string {
	if (isCodeForDisplayNumpadKey(combo.code)) return combo.code ?? '';
	return combo.key || combo.code || '';
}

export function isPrimaryKeyAlreadyRepresentedByModifier(
	combo: Pick<KeyCombo, 'key' | 'code' | 'ctrlOrMeta' | 'ctrl' | 'alt' | 'shift' | 'meta'>,
): boolean {
	const rawKey = getPrimaryKeyForComboDisplay(combo);
	if (!rawKey) return false;
	if (isShiftKeyName(rawKey)) return !!combo.shift;
	if (isControlKeyName(rawKey)) return !!combo.ctrl || !!combo.ctrlOrMeta;
	if (isAltKeyName(rawKey)) return !!combo.alt;
	if (isMetaKeyName(rawKey)) return !!combo.meta || !!combo.ctrlOrMeta;
	return false;
}

const CTRL_DESCRIPTOR = msg({message: 'Ctrl'});
const WIN_DESCRIPTOR = msg({message: 'Win'});
const ALT_DESCRIPTOR = msg({message: 'Alt'});
const SPACE_DESCRIPTOR = msg({message: 'Space'});
const ENTER_DESCRIPTOR = msg({message: 'Enter'});
const ESC_DESCRIPTOR = msg({message: 'Esc'});
const TAB_DESCRIPTOR = msg({message: 'Tab'});
const MOUSE_BUTTON_DESCRIPTOR = msg({message: 'Mouse button {button}', context: 'mouse-button-name'});
const GAMEPAD_BUTTON_DESCRIPTOR = msg({message: 'Gamepad button {button}', context: 'gamepad-button-name'});
const LEFT_KEY_DESCRIPTOR = msg({message: 'Left {keyName}', context: 'keyboard-key-name'});
const RIGHT_KEY_DESCRIPTOR = msg({message: 'Right {keyName}', context: 'keyboard-key-name'});
const NUMPAD_KEY_DESCRIPTOR = msg({message: 'Numpad {keyName}', context: 'keyboard-key-name'});
const LAUNCH_APP_DESCRIPTOR = msg({message: 'Launch app {appNumber}', context: 'keyboard-key-name'});
const MOUSE_BUTTON_DESCRIPTORS: Partial<Record<number, MessageDescriptor>> = {
	0: msg({message: 'Left mouse button', context: 'mouse-button-name'}),
	1: msg({message: 'Middle mouse button', context: 'mouse-button-name'}),
	2: msg({message: 'Right mouse button', context: 'mouse-button-name'}),
	3: msg({message: 'Mouse back button', context: 'mouse-button-name'}),
	4: msg({message: 'Mouse forward button', context: 'mouse-button-name'}),
};
const GAMEPAD_BUTTON_DESCRIPTORS: Partial<Record<number, MessageDescriptor>> = {
	0: msg({message: 'Gamepad A / Cross', context: 'gamepad-button-name'}),
	1: msg({message: 'Gamepad B / Circle', context: 'gamepad-button-name'}),
	2: msg({message: 'Gamepad X / Square', context: 'gamepad-button-name'}),
	3: msg({message: 'Gamepad Y / Triangle', context: 'gamepad-button-name'}),
	4: msg({message: 'Gamepad left bumper', context: 'gamepad-button-name'}),
	5: msg({message: 'Gamepad right bumper', context: 'gamepad-button-name'}),
	6: msg({message: 'Gamepad left trigger', context: 'gamepad-button-name'}),
	7: msg({message: 'Gamepad right trigger', context: 'gamepad-button-name'}),
	8: msg({message: 'Gamepad Back / Select', context: 'gamepad-button-name'}),
	9: msg({message: 'Gamepad Start / Options', context: 'gamepad-button-name'}),
	10: msg({message: 'Gamepad left stick click', context: 'gamepad-button-name'}),
	11: msg({message: 'Gamepad right stick click', context: 'gamepad-button-name'}),
	12: msg({message: 'Gamepad D-pad up', context: 'gamepad-button-name'}),
	13: msg({message: 'Gamepad D-pad down', context: 'gamepad-button-name'}),
	14: msg({message: 'Gamepad D-pad left', context: 'gamepad-button-name'}),
	15: msg({message: 'Gamepad D-pad right', context: 'gamepad-button-name'}),
	16: msg({message: 'Gamepad Guide', context: 'gamepad-button-name'}),
};
const KEY_DESCRIPTORS: Partial<Record<string, MessageDescriptor>> = {
	PageUp: msg({message: 'Page up', context: 'keyboard-key-name'}),
	PageDown: msg({message: 'Page down', context: 'keyboard-key-name'}),
	PrintScreen: msg({message: 'Print screen', context: 'keyboard-key-name'}),
	ScrollLock: msg({message: 'Scroll lock', context: 'keyboard-key-name'}),
	Pause: msg({message: 'Pause / Break', context: 'keyboard-key-name'}),
	Break: msg({message: 'Pause / Break', context: 'keyboard-key-name'}),
	NumLock: msg({message: 'Num lock', context: 'keyboard-key-name'}),
	ContextMenu: msg({message: 'Menu', context: 'keyboard-key-name'}),
	Home: msg({message: 'Home', context: 'keyboard-key-name'}),
	End: msg({message: 'End', context: 'keyboard-key-name'}),
	Insert: msg({message: 'Insert', context: 'keyboard-key-name'}),
	Delete: msg({message: 'Delete', context: 'keyboard-key-name'}),
	AudioVolumeMute: msg({message: 'Volume mute', context: 'keyboard-key-name'}),
	AudioVolumeDown: msg({message: 'Volume down', context: 'keyboard-key-name'}),
	AudioVolumeUp: msg({message: 'Volume up', context: 'keyboard-key-name'}),
	MediaTrackNext: msg({message: 'Media next', context: 'keyboard-key-name'}),
	MediaTrackPrevious: msg({message: 'Media previous', context: 'keyboard-key-name'}),
	MediaStop: msg({message: 'Media stop', context: 'keyboard-key-name'}),
	MediaPlayPause: msg({message: 'Media play/pause', context: 'keyboard-key-name'}),
	BrowserBack: msg({message: 'Browser back', context: 'keyboard-key-name'}),
	BrowserForward: msg({message: 'Browser forward', context: 'keyboard-key-name'}),
	BrowserRefresh: msg({message: 'Browser refresh', context: 'keyboard-key-name'}),
	BrowserStop: msg({message: 'Browser stop', context: 'keyboard-key-name'}),
	BrowserSearch: msg({message: 'Browser search', context: 'keyboard-key-name'}),
	BrowserFavorites: msg({message: 'Browser favorites', context: 'keyboard-key-name'}),
	BrowserHome: msg({message: 'Browser home', context: 'keyboard-key-name'}),
	LaunchMail: msg({message: 'Launch mail', context: 'keyboard-key-name'}),
	LaunchMediaPlayer: msg({message: 'Launch media', context: 'keyboard-key-name'}),
	Power: msg({message: 'Power', context: 'keyboard-key-name'}),
	Sleep: msg({message: 'Sleep', context: 'keyboard-key-name'}),
	WakeUp: msg({message: 'Wake up', context: 'keyboard-key-name'}),
	Convert: msg({message: 'Convert', context: 'keyboard-key-name'}),
	NonConvert: msg({message: 'Non-convert', context: 'keyboard-key-name'}),
	KanaMode: msg({message: 'Kana', context: 'keyboard-key-name'}),
};

export function formatMouseButton(i18n: I18n, button: number): string {
	const descriptor = MOUSE_BUTTON_DESCRIPTORS[button];
	return descriptor ? i18n._(descriptor) : i18n._(MOUSE_BUTTON_DESCRIPTOR, {button});
}

export function formatGamepadButton(i18n: I18n, button: number): string {
	const descriptor = GAMEPAD_BUTTON_DESCRIPTORS[button];
	return descriptor ? i18n._(descriptor) : i18n._(GAMEPAD_BUTTON_DESCRIPTOR, {button});
}

function formatBothSidesModifier(i18n: I18n, combo: KeyCombo): Array<string> | null {
	let keyName: string | null = null;
	if (combo.shift) keyName = i18n._(SHIFT_KEY_DESCRIPTOR);
	else if (combo.ctrl) keyName = isMac() ? CONTROL_KEY_SYMBOL : i18n._(CTRL_DESCRIPTOR);
	else if (combo.ctrlOrMeta) keyName = isMac() ? '⌘' : i18n._(CTRL_DESCRIPTOR);
	else if (combo.alt) keyName = isMac() ? '⌥' : i18n._(ALT_DESCRIPTOR);
	else if (combo.meta) keyName = isMac() ? '⌘' : i18n._(WIN_DESCRIPTOR);
	if (!keyName) return null;
	return [i18n._(LEFT_KEY_DESCRIPTOR, {keyName}), i18n._(RIGHT_KEY_DESCRIPTOR, {keyName})];
}

export function formatKeyCombo(i18n: I18n, combo: KeyCombo): string {
	return formatKeyComboParts(i18n, combo).join(' + ');
}

export function formatKeyComboParts(i18n: I18n, combo: KeyCombo): Array<string> {
	const parts: Array<string> = [];
	if (combo.modifierOnly && combo.bothSides) {
		const formatted = formatBothSidesModifier(i18n, combo);
		if (formatted) return formatted;
	}
	if (combo.ctrl) {
		parts.push(isMac() ? CONTROL_KEY_SYMBOL : i18n._(CTRL_DESCRIPTOR));
	} else if (combo.ctrlOrMeta) {
		parts.push(isMac() ? '⌘' : i18n._(CTRL_DESCRIPTOR));
	}
	if (combo.meta && !combo.ctrlOrMeta) {
		parts.push(isMac() ? '⌘' : i18n._(WIN_DESCRIPTOR));
	}
	if (combo.shift) {
		parts.push(i18n._(SHIFT_KEY_DESCRIPTOR));
	}
	if (combo.alt) parts.push(isMac() ? '⌥' : i18n._(ALT_DESCRIPTOR));
	if (combo.mouseButton !== undefined && combo.mouseButton !== null) {
		parts.push(formatMouseButton(i18n, combo.mouseButton));
		return parts;
	}
	if (combo.gamepadButton !== undefined && combo.gamepadButton !== null) {
		parts.push(formatGamepadButton(i18n, combo.gamepadButton));
		return parts;
	}
	const rawKey = getPrimaryKeyForComboDisplay(combo);
	if (
		isPrimaryKeyAlreadyRepresentedByModifier(combo) ||
		(combo.modifierOnly && isModifierKeyName(rawKey) && parts.length > 0)
	) {
		return parts;
	}
	const primarySymbol = formatPrimaryKeySymbol(i18n, rawKey);
	if (primarySymbol) parts.push(primarySymbol);
	return parts;
}

function formatNumpadKey(i18n: I18n, rawKey: string): string | null {
	let keyName: string;
	if (/^Numpad[0-9]$/.test(rawKey)) {
		keyName = rawKey.slice(6);
	} else if (rawKey === 'NumpadEnter') {
		keyName = isMac() ? '⏎' : i18n._(ENTER_DESCRIPTOR);
	} else {
		const symbols: Partial<Record<string, string>> = {
			NumpadDecimal: '.',
			NumpadAdd: '+',
			NumpadSubtract: '-',
			NumpadMultiply: '*',
			NumpadDivide: '/',
			NumpadEqual: '=',
			NumpadComma: ',',
		};
		const symbol = Object.hasOwn(symbols, rawKey) ? symbols[rawKey] : undefined;
		if (!symbol) return null;
		keyName = symbol;
	}
	return i18n._(NUMPAD_KEY_DESCRIPTOR, {keyName});
}

export function formatPrimaryKeySymbol(i18n: I18n, rawKey: string): string {
	if (rawKey === ' ' || rawKey === 'Spacebar' || rawKey === 'Space') return i18n._(SPACE_DESCRIPTOR);
	if (isShiftKeyName(rawKey)) return i18n._(SHIFT_KEY_DESCRIPTOR);
	if (isControlKeyName(rawKey)) return isMac() ? CONTROL_KEY_SYMBOL : i18n._(CTRL_DESCRIPTOR);
	if (isAltKeyName(rawKey)) return isMac() ? '⌥' : i18n._(ALT_DESCRIPTOR);
	if (isMetaKeyName(rawKey)) return isMac() ? '⌘' : i18n._(WIN_DESCRIPTOR);
	if (rawKey === 'ArrowUp') return '▲';
	if (rawKey === 'ArrowDown') return '▼';
	if (rawKey === 'ArrowLeft') return '◀';
	if (rawKey === 'ArrowRight') return '▶';
	if (rawKey === 'Escape') return i18n._(ESC_DESCRIPTOR);
	if (rawKey === 'Backspace') return '⌫';
	if (rawKey === 'Enter' || rawKey === 'Return') return isMac() ? '⏎' : i18n._(ENTER_DESCRIPTOR);
	if (rawKey === 'Tab') return i18n._(TAB_DESCRIPTOR);
	if (rawKey === 'Backquote' || rawKey === '`') return '`';
	if (rawKey === '[' || rawKey === ']') return rawKey;
	if (/^Key[A-Z]$/.test(rawKey)) return rawKey.slice(3);
	if (/^Digit[0-9]$/.test(rawKey)) return rawKey.slice(5);
	const numpadKey = formatNumpadKey(i18n, rawKey);
	if (numpadKey) return numpadKey;
	if (rawKey === 'LaunchApp1' || rawKey === 'LaunchApp2') {
		return i18n._(LAUNCH_APP_DESCRIPTOR, {appNumber: rawKey.slice(-1)});
	}
	const descriptor = Object.hasOwn(KEY_DESCRIPTORS, rawKey) ? KEY_DESCRIPTORS[rawKey] : undefined;
	if (descriptor) return i18n._(descriptor);
	if (rawKey === 'IntlBackslash') return 'Intl \\';
	if (rawKey === 'IntlRo') return 'Intl Ro';
	if (rawKey === 'IntlYen') return 'Intl Yen';
	if (rawKey.length === 1) return rawKey.toUpperCase();
	return rawKey;
}
