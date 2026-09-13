// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CustomKeybindEntry, KeybindConfig, KeyCombo} from '@app/features/input/state/InputKeybind';

export const keyComboHasTriggerInput = (combo: KeyCombo): boolean =>
	(combo.key ?? '') !== '' || (combo.code ?? '') !== '' || combo.mouseButton != null || combo.gamepadButton != null;

export function getActiveCombosForResolvedAction(
	fallback: KeybindConfig | null,
	customBindings: ReadonlyArray<CustomKeybindEntry>,
): Array<KeyCombo> {
	const result: Array<KeyCombo> = [];
	if (customBindings.length === 0) {
		if (fallback && !fallback.informationalOnly && keyComboHasTriggerInput(fallback.combo)) {
			result.push(fallback.combo);
		}
		return result;
	}
	for (const entry of customBindings) {
		if (!entry.enabled || !keyComboHasTriggerInput(entry.combo)) continue;
		result.push(entry.combo);
	}
	return result;
}

export function getDisplayKeybindForResolvedAction(
	base: KeybindConfig,
	customBindings: ReadonlyArray<CustomKeybindEntry>,
): KeybindConfig & {combo: KeyCombo} {
	const firstCustom = customBindings.find((entry) => entry.enabled && keyComboHasTriggerInput(entry.combo));
	if (firstCustom) return {...base, combo: firstCustom.combo};
	if (customBindings.length > 0) return {...base, combo: {key: ''}};
	return {...base};
}
