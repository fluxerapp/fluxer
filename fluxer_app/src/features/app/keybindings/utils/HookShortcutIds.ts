// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeybindCommand, KeyCombo} from '@app/features/input/state/InputKeybind';

function comboModifierSignature(combo: KeyCombo): string {
	return [
		combo.ctrlOrMeta ? 'mod' : '',
		combo.ctrl ? 'ctrl' : '',
		combo.alt ? 'alt' : '',
		combo.shift ? 'shift' : '',
		combo.meta ? 'meta' : '',
	]
		.filter(Boolean)
		.join('+');
}

function comboKeyboardTriggerSignature(combo: KeyCombo): string {
	const trigger = combo.code || combo.key;
	if (!trigger) return '';
	return `${trigger}:${combo.key || ''}`;
}

export function hookShortcutIdForAction(action: KeybindCommand, combo: KeyCombo): string | null {
	const modifiers = comboModifierSignature(combo);
	if (combo.mouseButton != null) {
		return `mouse:${action}:${modifiers}:${combo.mouseButton}`;
	}
	const trigger = comboKeyboardTriggerSignature(combo);
	if (!trigger) return null;
	return `key:${action}:${modifiers}:${trigger}`;
}

export function hookShortcutIdForKeybind(keybind: {action: KeybindCommand; combo: KeyCombo}): string | null {
	return hookShortcutIdForAction(keybind.action, keybind.combo);
}
