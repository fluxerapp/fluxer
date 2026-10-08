// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeyCombo} from '@app/features/input/state/InputKeybind';

function isPrintableShortcutKey(key: string | undefined | null): key is string {
	if (!key) return false;
	if (key === 'Dead' || key === 'Unidentified' || key === 'Process') return false;
	return key.length === 1;
}

export function shouldPreferLayoutKeyForShortcut(combo: Pick<KeyCombo, 'key' | 'code'>): boolean {
	if (!isPrintableShortcutKey(combo.key)) return false;
	if (combo.code && /^Numpad/.test(combo.code)) return false;
	return true;
}

export function isKeybindModifierKey(key: string | undefined | null): boolean {
	if (!key) return false;
	return key === 'Shift' || key === 'Control' || key === 'Alt' || key === 'AltGraph' || key === 'Meta';
}
