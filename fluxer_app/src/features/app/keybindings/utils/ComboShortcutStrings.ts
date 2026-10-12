// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyCombo} from '@app/features/input/state/InputKeybind';
import {shouldPreferLayoutKey} from '@fluxer/desktop_ipc/src/GlobalShortcutKeyNames';

const codeKeyForCombokeys: Record<string, string> = {
	Space: 'space',
	Spacebar: 'space',
	Escape: 'esc',
	Esc: 'esc',
	Enter: 'enter',
	Tab: 'tab',
	Backspace: 'backspace',
	ArrowUp: 'up',
	ArrowDown: 'down',
	ArrowLeft: 'left',
	ArrowRight: 'right',
	PrintScreen: 'printscreen',
	ScrollLock: 'scrolllock',
	Pause: 'pause',
	Break: 'pause',
	NumLock: 'numlock',
	Clear: 'numlock',
	ContextMenu: 'menu',
	Slash: '/',
	Semicolon: ';',
	Period: '.',
	Comma: ',',
	Quote: "'",
	Minus: '-',
	Equal: '=',
	BracketLeft: '[',
	BracketRight: ']',
	Backquote: '`',
	Backslash: '\\',
};
const keyFromComboForCombokeys = (combo: KeyCombo, source: 'key' | 'code' = 'key'): string | null => {
	if (combo.modifierOnly) return null;
	const raw = source === 'code' ? combo.code : combo.key || combo.code;
	if (!raw) return null;
	if (raw === ' ') return 'space';
	if (raw.length === 1) {
		return raw.toLowerCase();
	}
	if (/^Key[A-Z]$/.test(raw)) {
		return raw.slice(3).toLowerCase();
	}
	if (/^Digit[0-9]$/.test(raw)) {
		return raw.slice(5);
	}
	return codeKeyForCombokeys[raw] ?? raw.toLowerCase();
};
const comboToCombokeysString = (combo: KeyCombo, source: 'key' | 'code' = 'key'): string | null => {
	const parts: Array<string> = [];
	if (combo.ctrl) {
		parts.push('ctrl');
	} else if (combo.ctrlOrMeta) {
		parts.push('mod');
	}
	if (combo.meta && !combo.ctrlOrMeta) parts.push('meta');
	if (combo.shift) parts.push('shift');
	if (combo.alt) parts.push('alt');
	const key = keyFromComboForCombokeys(combo, source);
	if (!key) return null;
	parts.push(key);
	return parts.join('+');
};
export const comboToCombokeysStrings = (combo: KeyCombo): Array<string> => {
	const shortcuts = [comboToCombokeysString(combo)];
	if (combo.code && !shouldPreferLayoutKey(combo)) {
		shortcuts.push(comboToCombokeysString(combo, 'code'));
	}
	return [...new Set(shortcuts.filter((shortcut): shortcut is string => !!shortcut))];
};
