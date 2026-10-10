// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeybindConfig, KeyCombo} from '@app/features/input/state/InputKeybind';
import {shouldPreferLayoutKeyForShortcut} from '@app/features/input/utils/KeybindComboUtils';

export type KeyboardShortcutEvent = Pick<KeyboardEvent, 'key' | 'ctrlKey' | 'altKey' | 'shiftKey' | 'metaKey'> &
	Partial<Pick<KeyboardEvent, 'code'>>;
export type KeyboardShortcutPressEvent = KeyboardShortcutEvent & Pick<KeyboardEvent, 'repeat'>;

export const normalizeKeyboardShortcutKey = (key: string): string => {
	if (key === ' ') return 'space';
	if (key === 'Break') return 'pause';
	return key.toLowerCase();
};
const isPrintableNonLetterShortcutKey = (key: string | undefined | null): boolean =>
	!!key && key.length === 1 && !/^[a-z]$/i.test(key);
const hasTriggerKey = (combo: KeyCombo): boolean => (combo.key ?? '') !== '' || (combo.code ?? '') !== '';
const keyboardEventIsModifierKey = (
	event: Pick<KeyboardShortcutEvent, 'key' | 'code'>,
	modifier: 'ctrl' | 'alt' | 'shift' | 'meta',
): boolean => {
	switch (modifier) {
		case 'ctrl':
			return event.key === 'Control' || event.code === 'ControlLeft' || event.code === 'ControlRight';
		case 'alt':
			return event.key === 'Alt' || event.code === 'AltLeft' || event.code === 'AltRight';
		case 'shift':
			return event.key === 'Shift' || event.code === 'ShiftLeft' || event.code === 'ShiftRight';
		case 'meta':
			return event.key === 'Meta' || event.key === 'OS' || event.code === 'MetaLeft' || event.code === 'MetaRight';
	}
};
export const keyboardEventTriggerMatchesCombo = (combo: KeyCombo, event: KeyboardShortcutEvent): boolean => {
	const expectedLayoutKey = combo.key || combo.code;
	if (shouldPreferLayoutKeyForShortcut(combo) && expectedLayoutKey) {
		if (normalizeKeyboardShortcutKey(event.key) === normalizeKeyboardShortcutKey(expectedLayoutKey)) return true;
		return Boolean(combo.code && event.code === combo.code && combo.ctrlOrMeta && combo.alt);
	}
	const expectedCode = combo.code;
	if (expectedCode) {
		return event.code === expectedCode;
	}
	const expectedKey = expectedLayoutKey;
	if (!expectedKey) return false;
	return normalizeKeyboardShortcutKey(event.key) === normalizeKeyboardShortcutKey(expectedKey);
};
export const keyboardEventMatchesCombo = (
	combo: KeyCombo,
	event: KeyboardShortcutEvent,
	options: {isMacOS?: boolean} = {},
): boolean => {
	if (combo.modifierOnly || !hasTriggerKey(combo)) return false;
	const isMacOS = options.isMacOS ?? false;
	const expectsCtrl = Boolean(combo.ctrl || (combo.ctrlOrMeta && !isMacOS));
	const expectsMeta = Boolean(combo.meta || (combo.ctrlOrMeta && isMacOS));
	if (event.ctrlKey !== expectsCtrl) return false;
	if (event.metaKey !== expectsMeta) return false;
	if (event.altKey !== Boolean(combo.alt)) return false;
	const triggerMatches = keyboardEventTriggerMatchesCombo(combo, event);
	if (!triggerMatches) return false;
	if (event.shiftKey !== Boolean(combo.shift)) {
		if (combo.shift || !event.shiftKey || !isPrintableNonLetterShortcutKey(combo.key)) return false;
	}
	return true;
};
export const keyboardEventStartsComboPress = (
	combo: KeyCombo,
	event: KeyboardShortcutPressEvent,
	options: {isMacOS?: boolean} = {},
): boolean => !event.repeat && keyboardEventMatchesCombo(combo, event, options);
export const keyboardEventCanRecoverStaleMacMetaPress = (
	combo: KeyCombo,
	event: KeyboardShortcutPressEvent,
	options: {isMacOS?: boolean} = {},
): boolean => {
	const isMacOS = options.isMacOS ?? false;
	return isMacOS && Boolean(combo.ctrlOrMeta) && event.metaKey && keyboardEventStartsComboPress(combo, event, options);
};
export const keyboardEventReleasesComboModifier = (
	combo: KeyCombo,
	event: KeyboardShortcutEvent,
	options: {isMacOS?: boolean} = {},
): boolean => {
	if (combo.modifierOnly || !hasTriggerKey(combo)) return false;
	const isMacOS = options.isMacOS ?? false;
	const expectsCtrl = Boolean(combo.ctrl || (combo.ctrlOrMeta && !isMacOS));
	const expectsMeta = Boolean(combo.meta || (combo.ctrlOrMeta && isMacOS));
	if (expectsCtrl && keyboardEventIsModifierKey(event, 'ctrl')) return true;
	if (expectsMeta && keyboardEventIsModifierKey(event, 'meta')) return true;
	if (combo.alt && keyboardEventIsModifierKey(event, 'alt')) return true;
	if (combo.shift && keyboardEventIsModifierKey(event, 'shift')) return true;
	return false;
};
export const shouldAllowLocalShortcutForChannelTextarea = (
	entry: Pick<KeybindConfig, 'editableFocusBehavior'>,
	textareaValue: string,
): boolean => {
	if (entry.editableFocusBehavior === 'allow') return true;
	if (entry.editableFocusBehavior === 'allow_when_empty') return textareaValue.trim().length === 0;
	return false;
};
