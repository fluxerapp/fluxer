// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyCombo} from '@app/features/input/state/InputKeybind';
import {hookKeyNameForLayoutKey, shouldPreferLayoutKey} from '@fluxer/desktop_ipc/src/GlobalShortcutKeyNames';

const globalHookCodeKeyMap: Record<string, string> = {
	Space: 'Space',
	Spacebar: 'Space',
	Tab: 'Tab',
	CapsLock: 'CapsLock',
	Backspace: 'Backspace',
	Delete: 'Delete',
	Insert: 'Insert',
	Enter: 'Enter',
	Return: 'Enter',
	ArrowUp: 'ArrowUp',
	ArrowDown: 'ArrowDown',
	ArrowLeft: 'ArrowLeft',
	ArrowRight: 'ArrowRight',
	Home: 'Home',
	End: 'End',
	PageUp: 'PageUp',
	PageDown: 'PageDown',
	Escape: 'Escape',
	Esc: 'Escape',
	PrintScreen: 'PrintScreen',
	ScrollLock: 'ScrollLock',
	Pause: 'Pause',
	Break: 'Pause',
	NumLock: 'NumLock',
	Clear: 'NumLock',
	ContextMenu: 'ContextMenu',
	Numpad0: 'Numpad0',
	Numpad1: 'Numpad1',
	Numpad2: 'Numpad2',
	Numpad3: 'Numpad3',
	Numpad4: 'Numpad4',
	Numpad5: 'Numpad5',
	Numpad6: 'Numpad6',
	Numpad7: 'Numpad7',
	Numpad8: 'Numpad8',
	Numpad9: 'Numpad9',
	NumpadDecimal: 'NumpadDecimal',
	NumpadAdd: 'NumpadAdd',
	NumpadSubtract: 'NumpadSubtract',
	NumpadMultiply: 'NumpadMultiply',
	NumpadDivide: 'NumpadDivide',
	NumpadEnter: 'NumpadEnter',
	NumpadEqual: 'NumpadEqual',
	NumpadComma: 'NumpadComma',
	AudioVolumeMute: 'AudioVolumeMute',
	AudioVolumeDown: 'AudioVolumeDown',
	AudioVolumeUp: 'AudioVolumeUp',
	MediaTrackNext: 'MediaTrackNext',
	MediaTrackPrevious: 'MediaTrackPrevious',
	MediaStop: 'MediaStop',
	MediaPlayPause: 'MediaPlayPause',
	BrowserBack: 'BrowserBack',
	BrowserForward: 'BrowserForward',
	BrowserRefresh: 'BrowserRefresh',
	BrowserStop: 'BrowserStop',
	BrowserSearch: 'BrowserSearch',
	BrowserFavorites: 'BrowserFavorites',
	BrowserHome: 'BrowserHome',
	LaunchMail: 'LaunchMail',
	LaunchMediaPlayer: 'LaunchMediaPlayer',
	LaunchApp1: 'LaunchApp1',
	LaunchApp2: 'LaunchApp2',
	Power: 'Power',
	Sleep: 'Sleep',
	WakeUp: 'WakeUp',
	Convert: 'Convert',
	NonConvert: 'NonConvert',
	KanaMode: 'KanaMode',
	Lang1: 'Lang1',
	Lang2: 'Lang2',
	Lang3: 'Lang3',
	Lang4: 'Lang4',
	Lang5: 'Lang5',
	IntlBackslash: 'IntlBackslash',
	IntlRo: 'IntlRo',
	IntlYen: 'IntlYen',
	Minus: 'Minus',
	Equal: 'Equal',
	Comma: 'Comma',
	Period: 'Period',
	Semicolon: 'Semicolon',
	Quote: 'Quote',
	Slash: 'Slash',
	Backslash: 'Backslash',
	BracketLeft: 'BracketLeft',
	BracketRight: 'BracketRight',
	Backquote: 'Backquote',
	ControlLeft: 'ControlLeft',
	ControlRight: 'ControlRight',
	ShiftLeft: 'ShiftLeft',
	ShiftRight: 'ShiftRight',
	AltLeft: 'AltLeft',
	AltRight: 'AltRight',
	MetaLeft: 'MetaLeft',
	MetaRight: 'MetaRight',
};
export const keyNameForGlobalHook = (combo: KeyCombo): string | null => {
	if (combo.mouseButton != null || combo.gamepadButton != null || combo.modifierOnly) return null;
	const code = combo.code;
	if (shouldPreferLayoutKey(combo)) {
		const keyName = keyNameForGlobalHookKey(combo.key);
		if (keyName) return keyName;
	}
	return keyNameForGlobalHookCode(code) ?? keyNameForGlobalHookKey(combo.key || code);
};
export const physicalKeyNameForGlobalHook = (combo: KeyCombo): string | null => {
	if (combo.mouseButton != null || combo.gamepadButton != null || combo.modifierOnly) return null;
	return keyNameForGlobalHookCode(combo.code);
};
const keyNameForGlobalHookCode = (code: string | undefined): string | null => {
	if (!code) return null;
	const mapped = globalHookCodeKeyMap[code];
	if (mapped) return mapped;
	const keyCodeMatch = /^Key([A-Z])$/.exec(code);
	if (keyCodeMatch) return keyCodeMatch[1] ?? null;
	const digitCodeMatch = /^Digit([0-9])$/.exec(code);
	if (digitCodeMatch) return digitCodeMatch[1] ?? null;
	if (/^F([1-9]|1[0-9]|2[0-4])$/.test(code)) return code.toUpperCase();
	return null;
};
const keyNameForGlobalHookKey = (raw: string | undefined): string | null => {
	if (!raw) return null;
	if (raw.length === 1) {
		return hookKeyNameForLayoutKey(raw);
	}
	const mapped = globalHookCodeKeyMap[raw];
	if (mapped) return mapped;
	if (/^F([1-9]|1[0-9]|2[0-4])$/.test(raw)) return raw.toUpperCase();
	return null;
};
