// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KeybindCommand, KeybindConfig, KeybindSection, KeyCombo} from '@app/features/input/state/InputKeybind';
import {formatKeyCombo, formatKeyComboParts} from '@app/features/input/utils/KeybindUtils';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type React from 'react';

const ANY_KEY_DESCRIPTOR = msg({message: 'Any key'});

export const UNASSIGNED = '__unassigned__' as const;

export type ActionValue = KeybindCommand | typeof UNASSIGNED;
export type ActionOption = ComboboxOption<ActionValue>;

export const ACTION_DROPDOWN_MENU_MIN_WIDTH = 300;
export const FEATURED_CUSTOM_KEYBIND_ACTIONS: ReadonlyArray<KeybindCommand> = [
	'voice_push_to_talk',
	'voice_push_to_talk_priority',
	'voice_push_to_mute',
	'voice_priority_vad',
	'voice_toggle_mute',
	'voice_toggle_deafen',
	'voice_toggle_vad',
	'voice_toggle_camera',
	'voice_switch_channel',
	'nav_history_back',
	'nav_history_forward',
];
export const DEFAULT_KEYBIND_SECTIONS: ReadonlyArray<KeybindSection> = [
	'defaults',
	'messages',
	'navigation',
	'drag_and_drop',
	'chat',
	'voice_and_video',
	'misc',
];
export const SECTION_DISPLAY_ORDER: Partial<Record<KeybindSection, ReadonlyArray<KeybindCommand>>> = {
	messages: [
		'message_reply',
		'message_edit',
		'message_delete',
		'message_react',
		'message_forward',
		'message_pin',
		'message_mark_unread',
		'message_copy_text',
		'message_speak',
		'message_focus_textarea',
	],
	navigation: [
		'nav_quick_switcher',
		'nav_guild_prev',
		'nav_guild_next',
		'nav_channel_prev',
		'nav_channel_next',
		'nav_toggle_last_guild_dms',
		'nav_history_back',
		'nav_history_forward',
		'nav_unread_prev',
		'nav_unread_next',
		'nav_mention_prev',
		'nav_mention_next',
		'nav_current_call',
		'nav_add_guild',
	],
	drag_and_drop: ['dnd_start', 'dnd_move_up', 'dnd_move_down', 'dnd_drop', 'dnd_cancel'],
	chat: [
		'chat_focus_textarea',
		'chat_upload',
		'chat_toggle_emoji',
		'chat_toggle_gif',
		'chat_toggle_sticker',
		'chat_toggle_saved_media',
		'chat_send_voice_message',
		'chat_toggle_pins',
		'chat_toggle_inbox',
		'chat_toggle_member_list',
		'chat_scroll_up',
		'chat_scroll_down',
		'chat_jump_oldest_unread',
		'chat_mark_channel_read',
		'chat_mark_guild_read',
		'chat_mark_inbox_read',
		'chat_mark_all_inbox_read',
		'chat_new_dm',
		'chat_copy_channel_link',
	],
	voice_and_video: [
		'voice_toggle_mute',
		'voice_toggle_deafen',
		'voice_toggle_soundboard',
		'voice_start_dm_call',
		'voice_answer_call',
		'voice_decline_call',
	],
	misc: ['misc_search', 'misc_open_context_menu', 'misc_help'],
};
export const SHORTCUT_MERGE_PAIRS: ReadonlyArray<readonly [KeybindCommand, KeybindCommand]> = [
	['nav_guild_prev', 'nav_guild_next'],
	['nav_channel_prev', 'nav_channel_next'],
	['nav_history_back', 'nav_history_forward'],
	['nav_unread_prev', 'nav_unread_next'],
	['nav_mention_prev', 'nav_mention_next'],
	['chat_scroll_up', 'chat_scroll_down'],
	['dnd_move_up', 'dnd_move_down'],
];

export type ShortcutRowModel = KeybindConfig | [KeybindConfig, KeybindConfig];

export const OVERRIDDEN_CHIP_STYLE: React.CSSProperties = {opacity: 0.5, textDecoration: 'line-through'};
export const PRINTABLE_NAV_KEYS_TO_SWALLOW = new Set([
	'Tab',
	'Escape',
	'F1',
	'F2',
	'F3',
	'F4',
	'F5',
	'F6',
	'F7',
	'F8',
	'F9',
	'F10',
	'F11',
	'F12',
	'PageUp',
	'PageDown',
	'Home',
	'End',
	'Insert',
	'ContextMenu',
]);

export function isShortcutMergePair(row: ShortcutRowModel): row is [KeybindConfig, KeybindConfig] {
	return Array.isArray(row);
}

export function sortBySectionDisplayOrder(
	section: KeybindSection,
	entries: ReadonlyArray<KeybindConfig>,
): Array<KeybindConfig> {
	const order = SECTION_DISPLAY_ORDER[section];
	if (!order) return [...entries];
	const indexFor = (action: KeybindCommand) => {
		const idx = order.indexOf(action);
		return idx === -1 ? Number.POSITIVE_INFINITY : idx;
	};
	return [...entries].sort((a, b) => indexFor(a.action) - indexFor(b.action));
}

export function chipsForDefaultEntry(i18n: I18n, entry: KeybindConfig): Array<string> {
	if (entry.defaultsShortcutDisplayKind === 'any_key') {
		return [i18n._(ANY_KEY_DESCRIPTOR)];
	}
	if (entry.defaultsShortcutDisplayKind === 'space_or_enter') {
		return [...formatKeyComboParts(i18n, {key: ' '}), ...formatKeyComboParts(i18n, {key: 'Enter'})];
	}
	return entry.combo ? formatKeyComboParts(i18n, entry.combo) : [];
}

export function partitionMergedShortcutRows(entries: ReadonlyArray<KeybindConfig>): Array<ShortcutRowModel> {
	const result: Array<ShortcutRowModel> = [];
	let i = 0;
	while (i < entries.length) {
		const pair = SHORTCUT_MERGE_PAIRS.find(([a, b]) => entries[i]?.action === a && entries[i + 1]?.action === b);
		if (pair && entries[i + 1]) {
			result.push([entries[i], entries[i + 1]]);
			i += 2;
			continue;
		}
		result.push(entries[i]);
		i += 1;
	}
	return result;
}

export function getRowActions(row: ShortcutRowModel): Array<KeybindConfig> {
	return isShortcutMergePair(row) ? [...row] : [row];
}

export function normalizeQuery(query: string): string {
	return query.trim().toLowerCase();
}

export function entryMatchesQuery(i18n: I18n, entry: KeybindConfig, normalized: string): boolean {
	if (!normalized) return true;
	if (entry.label.toLowerCase().includes(normalized)) return true;
	const chips = chipsForDefaultEntry(i18n, entry);
	for (const chip of chips) {
		if (chip.toLowerCase().includes(normalized)) return true;
	}
	return false;
}

export function comboMatchesQuery(i18n: I18n, combo: KeyCombo, normalized: string): boolean {
	if (!normalized) return true;
	const formatted = formatKeyCombo(i18n, combo);
	if (formatted?.toLowerCase().includes(normalized)) return true;
	if (combo.key?.toLowerCase().includes(normalized)) return true;
	return false;
}

export function combosLooseEqual(a: KeyCombo, b: KeyCombo): boolean {
	if (!a || !b) return false;
	if ((a.key ?? '') !== (b.key ?? '')) return false;
	if ((a.code ?? '') !== (b.code ?? '')) return false;
	if (!!a.ctrlOrMeta !== !!b.ctrlOrMeta) return false;
	if (!!a.ctrl !== !!b.ctrl) return false;
	if (!!a.alt !== !!b.alt) return false;
	if (!!a.shift !== !!b.shift) return false;
	if (!!a.meta !== !!b.meta) return false;
	if (!!a.modifierOnly !== !!b.modifierOnly) return false;
	if (!!a.bothSides !== !!b.bothSides) return false;
	if ((a.mouseButton ?? null) !== (b.mouseButton ?? null)) return false;
	if ((a.gamepadButton ?? null) !== (b.gamepadButton ?? null)) return false;
	return true;
}

export function isShortcutLikeKeyEvent(event: KeyboardEvent): boolean {
	if (event.ctrlKey || event.metaKey || event.altKey) return true;
	if (event.key.length > 1 && PRINTABLE_NAV_KEYS_TO_SWALLOW.has(event.key)) return true;
	return false;
}
