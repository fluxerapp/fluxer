// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CustomKeybindEntry, KeybindCommand, KeybindConfig, KeyCombo} from '@app/features/input/state/InputKeybind';
import {resolveKeybindCommand} from '@app/features/input/state/input_keybind/KeybindCommands';
import {isActiveCustomKeybind, keyComboHasTriggerInput} from '@app/features/input/state/KeybindResolution';

export {
	hookShortcutIdForAction,
	hookShortcutIdForKeybind,
} from '@app/features/app/keybindings/utils/HookShortcutIds';

export type RuntimeKeybind = KeybindConfig & {
	id: string | null;
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

export function sourceIdForKeybind(keybind: {id: string | null; action: KeybindCommand}): string {
	if (keybind.id === null) return `default:${keybind.action}`;
	return `custom:${keybind.id}`;
}

export function gamepadSourceIdForKeybind(keybind: {id: string | null; action: KeybindCommand}): string {
	if (keybind.id === null) return `gamepad:default:${keybind.action}`;
	return `gamepad:${keybind.id}`;
}

function hasTriggerInput(combo: KeyCombo): boolean {
	return keyComboHasTriggerInput(combo);
}

function isEnabledDefaultCombo(combo: KeyCombo): boolean {
	return (combo.enabled ?? true) !== false && hasTriggerInput(combo);
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
		result.push({...entry, id: null, combo});
	}
	return result;
}

export function buildCustomRuntimeKeybinds(
	customs: ReadonlyArray<CustomKeybindEntry>,
	getBaseByAction: RuntimeKeybindBaseResolver,
): Array<RuntimeKeybind> {
	const result: Array<RuntimeKeybind> = [];
	for (const custom of customs) {
		const action = resolveKeybindCommand(custom.action);
		if (action === null || !isActiveCustomKeybind(custom)) continue;
		const base = getBaseByAction(action);
		if (!base) continue;
		result.push({...base, id: custom.id, combo: custom.combo});
	}
	return result;
}
