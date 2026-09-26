// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CustomKeybindEntry, KeybindCommand, KeybindConfig, KeyCombo} from '@app/features/input/state/InputKeybind';
import {keyComboHasTriggerInput} from '@app/features/input/state/KeybindResolution';

export {
	comboModifierSignature,
	hookShortcutIdForAction,
	hookShortcutIdForKeybind,
} from '@app/features/app/keybindings/utils/HookShortcutIds';

export type RuntimeKeybind = KeybindConfig & {
	combo: KeyCombo;
};
export type RuntimeKeybindBaseResolver = (action: KeybindCommand) => KeybindConfig | null;
export type HoldAction =
	| 'voice_push_to_talk'
	| 'voice_push_to_talk_priority'
	| 'voice_push_to_mute'
	| 'voice_priority_vad';

export const HOLD_ACTIONS: ReadonlyArray<HoldAction> = [
	'voice_push_to_talk',
	'voice_push_to_talk_priority',
	'voice_push_to_mute',
	'voice_priority_vad',
];
export const HOLD_ACTIONS_FOR_PTT_MODE: ReadonlyArray<HoldAction> = [
	'voice_push_to_talk',
	'voice_push_to_talk_priority',
];
export const HOLD_ACTIONS_FOR_VOICE_ACTIVITY_MODE: ReadonlyArray<HoldAction> = [
	'voice_push_to_mute',
	'voice_priority_vad',
];

export function hasTriggerKey(combo: KeyCombo): boolean {
	return (combo.key ?? '') !== '' || (combo.code ?? '') !== '';
}

export function hasTriggerInput(combo: KeyCombo): boolean {
	return keyComboHasTriggerInput(combo);
}

export function isEnabledDefaultCombo(combo: KeyCombo): boolean {
	return (combo.enabled ?? true) !== false && hasTriggerInput(combo);
}

export function getCustomActionOverrides(customs: ReadonlyArray<CustomKeybindEntry>): Set<KeybindCommand> {
	const overriddenActions = new Set<KeybindCommand>();
	for (const custom of customs) {
		if (!custom.action) continue;
		overriddenActions.add(custom.action);
	}
	return overriddenActions;
}

export function buildDefaultRuntimeKeybinds(
	defaults: ReadonlyArray<KeybindConfig>,
	overriddenActions: Set<KeybindCommand>,
): Array<RuntimeKeybind> {
	const result: Array<RuntimeKeybind> = [];
	for (const entry of defaults) {
		if (overriddenActions.has(entry.action)) continue;
		const combo = entry.combo;
		if (!isEnabledDefaultCombo(combo)) continue;
		result.push({...entry, combo});
	}
	return result;
}

export function buildCustomRuntimeKeybinds(
	customs: ReadonlyArray<CustomKeybindEntry>,
	getBaseByAction: RuntimeKeybindBaseResolver,
): Array<RuntimeKeybind> {
	const result: Array<RuntimeKeybind> = [];
	for (const custom of customs) {
		if (!custom.action || !custom.enabled) continue;
		const base = getBaseByAction(custom.action);
		if (!base) continue;
		const combo = custom.combo;
		if (!hasTriggerInput(combo)) continue;
		result.push({...base, combo});
	}
	return result;
}
