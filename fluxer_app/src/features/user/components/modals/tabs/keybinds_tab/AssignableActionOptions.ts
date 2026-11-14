// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeybindCommand, KeybindConfig} from '@app/features/input/state/InputKeybind';
import {
	type ActionOption,
	FEATURED_CUSTOM_KEYBIND_ACTIONS,
	UNASSIGNED,
} from '@app/features/user/components/modals/tabs/keybinds_tab/shared';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const UNASSIGNED_DESCRIPTOR = msg({
	message: 'Unassigned',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});

const CUSTOM_KEYBIND_ACTION_LABEL_DESCRIPTORS: Partial<Record<KeybindCommand, MessageDescriptor>> = {
	nav_guild_prev: msg({
		message: 'Switch to previous community',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes previous from next.',
	}),
	nav_guild_next: msg({
		message: 'Switch to next community',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes previous from next.',
	}),
	nav_channel_prev: msg({
		message: 'Switch to previous channel',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes previous from next.',
	}),
	nav_channel_next: msg({
		message: 'Switch to next channel',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes previous from next.',
	}),
	nav_history_back: msg({
		message: 'Move back through page history',
		comment: 'Label in the keybinds tab.',
	}),
	nav_history_forward: msg({
		message: 'Move forward through page history',
		comment: 'Label in the keybinds tab.',
	}),
	nav_unread_prev: msg({
		message: 'Jump to previous unread channel',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes previous from next.',
	}),
	nav_unread_next: msg({
		message: 'Jump to next unread channel',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes previous from next.',
	}),
	nav_mention_prev: msg({
		message: 'Jump to previous unread channel with mentions',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes previous from next.',
	}),
	nav_mention_next: msg({
		message: 'Jump to next unread channel with mentions',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes previous from next.',
	}),
	dnd_move_up: msg({
		message: 'Move item up',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes up from down.',
	}),
	dnd_move_down: msg({
		message: 'Move item down',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes up from down.',
	}),
	message_reply_prev: msg({
		message: 'Reply to message above',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes above from below.',
	}),
	message_reply_next: msg({
		message: 'Reply to message below',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes above from below.',
	}),
	message_edit_prev: msg({
		message: 'Edit your message above',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes above from below.',
	}),
	message_edit_next: msg({
		message: 'Edit your message below',
		comment: 'Label in the keybinds tab action dropdown. Distinguishes above from below.',
	}),
};

export function getCustomKeybindActionLabel(i18n: I18n, action: KeybindCommand, fallbackLabel?: string): string {
	const descriptor = CUSTOM_KEYBIND_ACTION_LABEL_DESCRIPTORS[action];
	if (descriptor) return i18n._(descriptor);
	return fallbackLabel ?? action;
}

export function getCustomKeybindActionLabelMap(
	i18n: I18n,
	defaults: ReadonlyArray<KeybindConfig>,
): Map<KeybindCommand, string> {
	const labelByAction = new Map<KeybindCommand, string>();
	for (const entry of defaults) {
		labelByAction.set(entry.action, getCustomKeybindActionLabel(i18n, entry.action, entry.label));
	}
	return labelByAction;
}

export function buildAssignableActionOptions(
	i18n: I18n,
	defaults: ReadonlyArray<KeybindConfig>,
	currentAction?: KeybindCommand | null,
): ReadonlyArray<ActionOption> {
	const labelByAction = getCustomKeybindActionLabelMap(i18n, defaults);
	const options: Array<ActionOption> = [{value: UNASSIGNED, label: i18n._(UNASSIGNED_DESCRIPTOR)}];
	const includedActions = new Set<KeybindCommand>();
	const appendAction = (action: KeybindCommand) => {
		if (includedActions.has(action)) return;
		const label = labelByAction.get(action) ?? getCustomKeybindActionLabel(i18n, action);
		if (!label) return;
		options.push({value: action, label});
		includedActions.add(action);
	};
	for (const action of FEATURED_CUSTOM_KEYBIND_ACTIONS) {
		appendAction(action);
	}
	for (const entry of defaults) {
		if (entry.informationalOnly) continue;
		appendAction(entry.action);
	}
	if (currentAction) {
		appendAction(currentAction);
	}
	return options;
}
