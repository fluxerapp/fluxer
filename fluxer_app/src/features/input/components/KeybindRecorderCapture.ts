// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyCombo} from '@app/features/input/state/InputKeybind';
import type {GlobalKeyEvent, GlobalShortcutCaptureEvent, GlobalShortcutsApi} from '@app/types/electron.d';

const GLOBAL_KEY_NAME_ALIASES: Readonly<Record<string, string>> = {
	Esc: 'Escape',
	Return: 'Enter',
	Spacebar: 'Space',
	CapsLock: 'CapsLock',
	Capslock: 'CapsLock',
	Caps_Lock: 'CapsLock',
	Control: 'ControlLeft',
	Ctrl: 'ControlLeft',
	Shift: 'ShiftLeft',
	Alt: 'AltLeft',
	Option: 'AltLeft',
	Command: 'MetaLeft',
	Cmd: 'MetaLeft',
	Super: 'MetaLeft',
	Win: 'MetaLeft',
	Windows: 'MetaLeft',
	Left: 'ArrowLeft',
	Right: 'ArrowRight',
	Up: 'ArrowUp',
	Down: 'ArrowDown',
};

const COMPACT_GLOBAL_KEY_NAME_ALIASES: Readonly<Record<string, string>> = {
	capslock: 'CapsLock',
	printscreen: 'PrintScreen',
	scrolllock: 'ScrollLock',
	numlock: 'NumLock',
	pageup: 'PageUp',
	pagedown: 'PageDown',
	contextmenu: 'ContextMenu',
	arrowup: 'ArrowUp',
	arrowdown: 'ArrowDown',
	arrowleft: 'ArrowLeft',
	arrowright: 'ArrowRight',
};

const GLOBAL_CODE_TO_KEY: Readonly<Record<string, string>> = {
	Space: ' ',
	Minus: '-',
	Equal: '=',
	Comma: ',',
	Period: '.',
	Semicolon: ';',
	Quote: "'",
	Slash: '/',
	Backslash: '\\',
	BracketLeft: '[',
	BracketRight: ']',
	Backquote: '`',
	ControlLeft: 'Control',
	ControlRight: 'Control',
	ShiftLeft: 'Shift',
	ShiftRight: 'Shift',
	AltLeft: 'Alt',
	AltRight: 'Alt',
	MetaLeft: 'Meta',
	MetaRight: 'Meta',
};

const NORMALIZED_CODE_NAMES = new Set([
	'Space',
	'Tab',
	'CapsLock',
	'Backspace',
	'Delete',
	'Insert',
	'Enter',
	'ArrowUp',
	'ArrowDown',
	'ArrowLeft',
	'ArrowRight',
	'Home',
	'End',
	'PageUp',
	'PageDown',
	'Escape',
	'PrintScreen',
	'ScrollLock',
	'Pause',
	'NumLock',
	'ContextMenu',
	'Numpad0',
	'Numpad1',
	'Numpad2',
	'Numpad3',
	'Numpad4',
	'Numpad5',
	'Numpad6',
	'Numpad7',
	'Numpad8',
	'Numpad9',
	'NumpadDecimal',
	'NumpadAdd',
	'NumpadSubtract',
	'NumpadMultiply',
	'NumpadDivide',
	'NumpadEnter',
	'NumpadEqual',
	'NumpadComma',
	'AudioVolumeMute',
	'AudioVolumeDown',
	'AudioVolumeUp',
	'MediaTrackNext',
	'MediaTrackPrevious',
	'MediaStop',
	'MediaPlayPause',
	'BrowserBack',
	'BrowserForward',
	'BrowserRefresh',
	'BrowserStop',
	'BrowserSearch',
	'BrowserFavorites',
	'BrowserHome',
	'LaunchMail',
	'LaunchMediaPlayer',
	'LaunchApp1',
	'LaunchApp2',
	'Power',
	'Sleep',
	'WakeUp',
	'Convert',
	'NonConvert',
	'KanaMode',
	'Lang1',
	'Lang2',
	'Lang3',
	'Lang4',
	'Lang5',
	'IntlBackslash',
	'IntlRo',
	'IntlYen',
	'ControlLeft',
	'ControlRight',
	'ShiftLeft',
	'ShiftRight',
	'AltLeft',
	'AltRight',
	'MetaLeft',
	'MetaRight',
]);

const isMacPlatform = (): boolean => /Mac|iPod|iPhone|iPad/.test(navigator.platform);

const normalizeGlobalKeyName = (rawKeyName: string): string | null => {
	const raw = rawKeyName.trim();
	if (!raw) return null;
	const alias = GLOBAL_KEY_NAME_ALIASES[raw];
	if (alias) return alias;
	const compact = raw.replace(/[\s_-]+/g, '').toLowerCase();
	const compactAlias = COMPACT_GLOBAL_KEY_NAME_ALIASES[compact];
	if (compactAlias) return compactAlias;
	if (/^[a-z]$/.test(raw)) return raw.toUpperCase();
	if (/^[A-Z]$/.test(raw)) return raw;
	if (/^[0-9]$/.test(raw)) return raw;
	if (/^Key[A-Z]$/.test(raw)) return raw;
	if (/^Digit[0-9]$/.test(raw)) return raw;
	if (/^F([1-9]|1[0-9]|2[0-4])$/i.test(raw)) return raw.toUpperCase();
	if (NORMALIZED_CODE_NAMES.has(raw)) return raw;
	return raw;
};

const codeForGlobalKeyName = (keyName: string): string | null => {
	const normalized = normalizeGlobalKeyName(keyName);
	if (!normalized) return null;
	if (/^[A-Z]$/.test(normalized)) return `Key${normalized}`;
	if (/^[0-9]$/.test(normalized)) return `Digit${normalized}`;
	return normalized;
};

const keyForGlobalCode = (code: string, shiftKey: boolean): string => {
	const letterMatch = /^Key([A-Z])$/.exec(code);
	if (letterMatch) {
		const letter = letterMatch[1] ?? '';
		return shiftKey ? letter : letter.toLowerCase();
	}
	const digitMatch = /^Digit([0-9])$/.exec(code);
	if (digitMatch) return digitMatch[1] ?? '';
	return GLOBAL_CODE_TO_KEY[code] ?? code;
};

const modifierStateToCombo = (
	event: Pick<GlobalKeyEvent, 'altKey' | 'ctrlKey' | 'metaKey' | 'shiftKey'>,
): Pick<KeyCombo, 'ctrlOrMeta' | 'ctrl' | 'alt' | 'shift' | 'meta'> => {
	const mac = isMacPlatform();
	const primaryModifierDown = mac ? event.metaKey : event.ctrlKey;
	return {
		ctrlOrMeta: primaryModifierDown || undefined,
		ctrl: (mac ? event.ctrlKey : false) || undefined,
		alt: event.altKey || undefined,
		shift: event.shiftKey || undefined,
		meta: (mac ? false : event.metaKey) || undefined,
	};
};

const MODIFIER_CODES: ReadonlySet<string> = new Set([
	'ShiftLeft',
	'ShiftRight',
	'ControlLeft',
	'ControlRight',
	'AltLeft',
	'AltRight',
	'MetaLeft',
	'MetaRight',
]);

export const isModifierKeyCode = (code: string | undefined): boolean => code !== undefined && MODIFIER_CODES.has(code);

export const globalKeyEventToCombo = (
	event: Pick<GlobalKeyEvent, 'altKey' | 'ctrlKey' | 'keyName' | 'metaKey' | 'shiftKey'>,
): KeyCombo | null => {
	const code = codeForGlobalKeyName(event.keyName);
	if (!code) return null;
	return {
		key: keyForGlobalCode(code, event.shiftKey),
		code,
		...modifierStateToCombo(event),
	};
};

const captureModifierState = (
	event: GlobalShortcutCaptureEvent,
): Pick<GlobalKeyEvent, 'altKey' | 'ctrlKey' | 'metaKey' | 'shiftKey'> => ({
	altKey: event.alt,
	ctrlKey: event.ctrl,
	metaKey: event.meta,
	shiftKey: event.shift,
});

export const shortcutCaptureKeyToCombo = (event: GlobalShortcutCaptureEvent): KeyCombo | null => {
	const code = event.code ?? (event.key ? codeForGlobalKeyName(event.key) : null);
	if (!code) return null;
	const key = event.key !== null && event.key.length === 1 ? event.key : keyForGlobalCode(code, event.shift);
	return {key, code, ...modifierStateToCombo(captureModifierState(event))};
};

export const shortcutCaptureMouseToCombo = (event: GlobalShortcutCaptureEvent): KeyCombo | null => {
	if (event.button === null) return null;
	return {key: '', mouseButton: event.button, ...modifierStateToCombo(captureModifierState(event))};
};

const beginCapture = <E, T>(
	start: () => Promise<T | null>,
	stop: (token: T) => Promise<void>,
	subscribe: (callback: (event: E) => void) => () => void,
	onEvent: (event: E) => void,
): (() => void) => {
	let cancelled = false;
	let unsubscribe: (() => void) | null = subscribe(onEvent);
	const releaseSubscription = (): void => {
		unsubscribe?.();
		unsubscribe = null;
	};
	const started = start().then(
		(token) => token,
		() => null,
	);
	void started.then((token) => {
		if (token === null) releaseSubscription();
	});
	return () => {
		if (cancelled) return;
		cancelled = true;
		releaseSubscription();
		void started.then((token) => {
			if (token !== null) void stop(token);
		});
	};
};

export interface GlobalKeyCaptureApi {
	globalKeyHookStart?: () => Promise<boolean>;
	globalKeyHookStop?: () => Promise<void>;
	onGlobalKeyEvent?: (callback: (event: GlobalKeyEvent) => void) => () => void;
}

export const beginGlobalKeyCapture = (
	api: GlobalKeyCaptureApi | null | undefined,
	onEvent: (event: GlobalKeyEvent) => void,
): (() => void) => {
	const {globalKeyHookStart, globalKeyHookStop, onGlobalKeyEvent} = api ?? {};
	if (!globalKeyHookStart || !globalKeyHookStop || !onGlobalKeyEvent) {
		return () => {};
	}
	return beginCapture(
		async () => ((await globalKeyHookStart()) ? true : null),
		() => globalKeyHookStop(),
		onGlobalKeyEvent,
		onEvent,
	);
};

export const beginShortcutCapture = (
	api: Pick<GlobalShortcutsApi, 'startCapture' | 'stopCapture' | 'onCapture'>,
	onEvent: (event: GlobalShortcutCaptureEvent) => void,
): (() => void) =>
	beginCapture(
		() => api.startCapture(),
		(captureId) => api.stopCapture(captureId),
		(callback) => api.onCapture(callback),
		onEvent,
	);
