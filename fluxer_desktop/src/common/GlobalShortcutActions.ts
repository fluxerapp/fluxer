// SPDX-License-Identifier: AGPL-3.0-or-later

export const GLOBAL_SHORTCUT_ACTIONS = [
	'voice_toggle_mute',
	'voice_toggle_deafen',
	'voice_push_to_talk',
	'voice_push_to_talk_priority',
	'voice_push_to_mute',
	'voice_priority_vad',
	'voice_toggle_vad',
	'voice_toggle_camera',
	'voice_switch_channel',
	'voice_disconnect',
] as const;

export type GlobalShortcutAction = (typeof GLOBAL_SHORTCUT_ACTIONS)[number];

export const GLOBAL_SHORTCUT_HOLD_ACTIONS: ReadonlySet<GlobalShortcutAction> = new Set([
	'voice_push_to_talk',
	'voice_push_to_talk_priority',
	'voice_push_to_mute',
	'voice_priority_vad',
]);

export const GLOBAL_SHORTCUT_DEFAULT_DESCRIPTIONS: Readonly<Record<GlobalShortcutAction, string>> = {
	voice_toggle_mute: 'Toggle mute',
	voice_toggle_deafen: 'Toggle deafen',
	voice_push_to_talk: 'Push to talk',
	voice_push_to_talk_priority: 'Push to talk (priority)',
	voice_push_to_mute: 'Push to mute',
	voice_priority_vad: 'Voice activity priority',
	voice_toggle_vad: 'Toggle voice activity',
	voice_toggle_camera: 'Toggle camera',
	voice_switch_channel: 'Switch voice channel',
	voice_disconnect: 'Disconnect from voice',
};

export const GLOBAL_SHORTCUT_DESCRIPTION_MAX_LENGTH = 200;

export function isGlobalShortcutAction(value: unknown): value is GlobalShortcutAction {
	return typeof value === 'string' && (GLOBAL_SHORTCUT_ACTIONS as ReadonlyArray<string>).includes(value);
}
