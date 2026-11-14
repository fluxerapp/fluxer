// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CustomKeybindEntry, KeybindCommand, KeybindConfig, KeyCombo} from '@app/features/input/state/InputKeybind';
import {resolveKeybindCommand} from '@app/features/input/state/input_keybind/KeybindCommands';

export const keyComboHasTriggerInput = (combo: KeyCombo): boolean =>
	(combo.key ?? '') !== '' || (combo.code ?? '') !== '' || combo.mouseButton != null || combo.gamepadButton != null;

export const isBuiltinDisableMarker = (entry: CustomKeybindEntry): boolean =>
	entry.action != null && entry.combo.enabled === false;

export const isActiveCustomKeybind = (entry: CustomKeybindEntry): boolean =>
	entry.action != null && entry.enabled && !isBuiltinDisableMarker(entry) && keyComboHasTriggerInput(entry.combo);

export function getSuppressedBuiltinActions(customs: ReadonlyArray<CustomKeybindEntry>): Set<KeybindCommand> {
	const actions = new Set<KeybindCommand>();
	for (const entry of customs) {
		const action = resolveKeybindCommand(entry.action);
		if (action !== null && (isBuiltinDisableMarker(entry) || isActiveCustomKeybind(entry))) actions.add(action);
	}
	return actions;
}

export function getActiveCombosForResolvedAction(
	fallback: KeybindConfig | null,
	customBindings: ReadonlyArray<CustomKeybindEntry>,
): Array<KeyCombo> {
	const active = customBindings.filter(isActiveCustomKeybind);
	if (active.length > 0) return active.map((entry) => entry.combo);
	if (customBindings.some(isBuiltinDisableMarker)) return [];
	if (fallback && !fallback.informationalOnly && keyComboHasTriggerInput(fallback.combo)) return [fallback.combo];
	return [];
}

export function getDisplayKeybindForResolvedAction(
	base: KeybindConfig,
	customBindings: ReadonlyArray<CustomKeybindEntry>,
): KeybindConfig & {combo: KeyCombo} {
	const firstCustom = customBindings.find(isActiveCustomKeybind);
	if (firstCustom) return {...base, combo: firstCustom.combo};
	if (customBindings.some(isBuiltinDisableMarker)) return {...base, combo: {key: ''}};
	return {...base};
}
