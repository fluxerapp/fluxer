// SPDX-License-Identifier: AGPL-3.0-or-later

import {COPY_TEXT_DESCRIPTOR, DELETE_MESSAGE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	ADD_REACTION_DESCRIPTOR,
	ANSWER_THE_INCOMING_CALL_DESCRIPTOR,
	BOOKMARK_MESSAGE_DESCRIPTOR,
	CANCEL_DESCRIPTOR,
	COPY_CHANNEL_LINK_DESCRIPTOR,
	COPY_MESSAGE_ID_DESCRIPTOR,
	COPY_MESSAGE_LINK_DESCRIPTOR,
	CREATE_OR_JOIN_A_COMMUNITY_DESCRIPTOR,
	DECLINE_THE_INCOMING_CALL_DESCRIPTOR,
	DISCONNECT_FROM_VOICE_DESCRIPTOR,
	DROP_ITEM_DESCRIPTOR,
	EDIT_MESSAGE_DESCRIPTOR,
	EXPAND_OR_COLLAPSE_COMPACT_CALL_VIEW_DESCRIPTOR,
	FOCUS_TEXT_AREA_DESCRIPTOR,
	FOCUS_THE_TEXT_AREA_DESCRIPTOR,
	FORWARD_MESSAGE_DESCRIPTOR,
	GO_TO_DIRECT_MESSAGES_DESCRIPTOR,
	GO_TO_EIGHTH_COMMUNITY_DESCRIPTOR,
	GO_TO_FIFTH_COMMUNITY_DESCRIPTOR,
	GO_TO_FIRST_COMMUNITY_DESCRIPTOR,
	GO_TO_FOURTH_COMMUNITY_DESCRIPTOR,
	GO_TO_SECOND_COMMUNITY_DESCRIPTOR,
	GO_TO_SEVENTH_COMMUNITY_DESCRIPTOR,
	GO_TO_SIXTH_COMMUNITY_DESCRIPTOR,
	GO_TO_THIRD_COMMUNITY_DESCRIPTOR,
	JUMP_BETWEEN_UNREAD_CHANNELS_DESCRIPTOR,
	JUMP_BETWEEN_UNREAD_CHANNELS_WITH_MENTIONS_DESCRIPTOR,
	JUMP_TO_THE_CURRENT_CALL_DESCRIPTOR,
	JUMP_TO_THE_OLDEST_UNREAD_MESSAGE_DESCRIPTOR,
	MARK_ALL_INBOX_CHANNELS_AS_READ_DESCRIPTOR,
	MARK_AS_UNREAD_DESCRIPTOR,
	MARK_CHANNEL_AS_READ_DESCRIPTOR,
	MARK_COMMUNITY_AS_READ_DESCRIPTOR,
	MARK_TOP_INBOX_CHANNEL_AS_READ_DESCRIPTOR,
	MOVE_BACK_THROUGH_VIEWED_CHANNEL_HISTORY_DESCRIPTOR,
	MOVE_DESCRIPTOR,
	MOVE_FORWARD_THROUGH_VIEWED_CHANNEL_HISTORY_DESCRIPTOR,
	OPEN_HELP_DESCRIPTOR,
	OPEN_THE_CONTEXT_MENU_DESCRIPTOR,
	OPEN_THEME_STUDIO_POPOUT_DESCRIPTOR,
	OPEN_YOUR_SETTINGS_DESCRIPTOR,
	PIN_MESSAGE_DESCRIPTOR,
	PUSH_TO_MUTE_DESCRIPTOR,
	PUSH_TO_TALK_DESCRIPTOR,
	PUSH_TO_TALK_PRIORITY_DESCRIPTOR,
	READ_MESSAGE_ALOUD_DESCRIPTOR,
	REPLY_TO_MESSAGE_DESCRIPTOR,
	RESET_ZOOM_DESCRIPTOR,
	SCROLL_CHAT_DOWN_DESCRIPTOR,
	SCROLL_CHAT_UP_DESCRIPTOR,
	SEARCH_MESSAGES_DESCRIPTOR,
	SEND_VOICE_MESSAGE_DESCRIPTOR,
	SHOW_KEYBOARD_SHORTCUTS_LIST_DESCRIPTOR,
	START_A_CALL_IN_A_DM_OR_GROUP_DESCRIPTOR,
	START_A_GROUP_DM_DESCRIPTOR,
	START_DRAG_AND_DROP_DESCRIPTOR,
	SWITCH_BETWEEN_CHANNELS_DESCRIPTOR,
	SWITCH_BETWEEN_COMMUNITIES_DESCRIPTOR,
	SWITCH_TO_NEXT_COMMUNITY_OR_DMS_DESCRIPTOR,
	SWITCH_TO_PREVIOUS_COMMUNITY_OR_DMS_DESCRIPTOR,
	SWITCH_VOICE_CHANNEL_DESCRIPTOR,
	TOGGLE_BETWEEN_LAST_COMMUNITY_AND_DMS_DESCRIPTOR,
	TOGGLE_CAMERA_DESCRIPTOR,
	TOGGLE_EMBED_SUPPRESSION_DESCRIPTOR,
	TOGGLE_MUTE_DESCRIPTOR,
	TOGGLE_PINNED_MESSAGES_DESCRIPTOR,
	TOGGLE_QUICK_SWITCHER_DESCRIPTOR,
	TOGGLE_SAVED_MEDIA_DESCRIPTOR,
	TOGGLE_THE_EMOJI_PICKER_DESCRIPTOR,
	TOGGLE_THE_GIF_PICKER_DESCRIPTOR,
	TOGGLE_THE_INBOX_DESCRIPTOR,
	TOGGLE_THE_MEMBER_LIST_OR_VOICE_CHAT_DESCRIPTOR,
	TOGGLE_THE_SOUNDBOARD_DESCRIPTOR,
	TOGGLE_THE_STICKER_PICKER_DESCRIPTOR,
	TOGGLE_VOICE_ACTIVITY_DESCRIPTOR,
	UPLOAD_A_FILE_DESCRIPTOR,
	VOICE_ACTIVITY_PRIORITY_DESCRIPTOR,
	ZOOM_IN_DESCRIPTOR,
	ZOOM_OUT_DESCRIPTOR,
} from '@app/features/input/state/input_keybind/shared';
import {awaitHydration, makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import UserSettings from '@app/features/user/state/UserSettings';
import {VOICE_TOGGLE_DEAFEN_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {create, type MessageInitShape} from '@bufbuild/protobuf';
import {
	type CustomKeybindSchema,
	type KeybindComboSchema,
	KeybindSettingsSchema,
	type CustomKeybind as SyncedCustomKeybind,
	type KeybindCombo as SyncedKeybindCombo,
} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import type {I18n} from '@lingui/core';
import {makeAutoObservable, runInAction} from 'mobx';
import {
	DEFAULT_KEYBOARD_SHORTCUTS_OVERLAY_COMBO,
	getKeyboardShortcutsOverlayComboForCurrentLayout,
	keyCombosEqual,
	SHIFTED_SLASH_FALLBACK_KEYBOARD_SHORTCUTS_OVERLAY_COMBO,
} from '../utils/KeyboardShortcutLayoutUtils';
import {getActiveCombosForResolvedAction, getDisplayKeybindForResolvedAction} from './KeybindResolution';

const KEYBIND_STORE_NAME = 'Keybind';
const KEYBIND_COMMAND_VALUES = [
	'message_edit',
	'message_delete',
	'message_pin',
	'message_react',
	'message_reply',
	'message_forward',
	'message_speak',
	'message_copy_text',
	'message_mark_unread',
	'message_focus_textarea',
	'nav_guild_prev',
	'nav_guild_next',
	'nav_channel_prev',
	'nav_channel_next',
	'nav_history_back',
	'nav_history_forward',
	'nav_unread_prev',
	'nav_unread_next',
	'nav_mention_prev',
	'nav_mention_next',
	'nav_current_call',
	'nav_toggle_last_guild_dms',
	'nav_guild_tab_prev',
	'nav_guild_tab_next',
	'nav_guild_slot_1',
	'nav_guild_slot_2',
	'nav_guild_slot_3',
	'nav_guild_slot_4',
	'nav_guild_slot_5',
	'nav_guild_slot_6',
	'nav_guild_slot_7',
	'nav_guild_slot_8',
	'nav_guild_slot_9',
	'nav_quick_switcher',
	'nav_add_guild',
	'dnd_start',
	'dnd_move_up',
	'dnd_move_down',
	'dnd_drop',
	'dnd_cancel',
	'chat_mark_guild_read',
	'chat_mark_channel_read',
	'chat_new_dm',
	'chat_toggle_pins',
	'chat_toggle_inbox',
	'chat_mark_inbox_read',
	'chat_mark_all_inbox_read',
	'chat_toggle_member_list',
	'chat_toggle_emoji',
	'chat_toggle_gif',
	'chat_toggle_sticker',
	'chat_scroll_up',
	'chat_scroll_down',
	'chat_jump_oldest_unread',
	'chat_focus_textarea',
	'chat_upload',
	'chat_copy_channel_link',
	'voice_toggle_mute',
	'voice_toggle_deafen',
	'voice_answer_call',
	'voice_decline_call',
	'voice_start_dm_call',
	'voice_toggle_soundboard',
	'voice_toggle_compact_call_view',
	'misc_help',
	'misc_search',
	'misc_open_context_menu',
	'message_bookmark',
	'message_toggle_embeds',
	'message_copy_link',
	'message_copy_id',
	'chat_toggle_saved_media',
	'chat_send_voice_message',
	'voice_push_to_talk',
	'voice_push_to_talk_priority',
	'voice_push_to_mute',
	'voice_priority_vad',
	'voice_toggle_vad',
	'voice_toggle_camera',
	'voice_switch_channel',
	'voice_disconnect',
	'system_toggle_settings',
	'system_toggle_shortcuts_overlay',
	'system_open_theme_studio_popout',
	'system_zoom_in',
	'system_zoom_out',
	'system_zoom_reset',
] as const;

export type KeybindCommand = (typeof KEYBIND_COMMAND_VALUES)[number];

const KEYBIND_COMMAND_SET = new Set<string>(KEYBIND_COMMAND_VALUES);
const isKeybindCommand = (value: unknown): value is KeybindCommand =>
	typeof value === 'string' && KEYBIND_COMMAND_SET.has(value);

export interface KeyCombo {
	key: string;
	code?: string;
	ctrlOrMeta?: boolean;
	ctrl?: boolean;
	alt?: boolean;
	shift?: boolean;
	meta?: boolean;
	global?: boolean;
	enabled?: boolean;
	mouseButton?: number;
	modifierOnly?: boolean;
	bothSides?: boolean;
	gamepadButton?: number;
}

export type KeybindSection =
	| 'defaults'
	| 'messages'
	| 'navigation'
	| 'drag_and_drop'
	| 'chat'
	| 'voice_and_video'
	| 'misc';
export type DefaultsShortcutDisplayKind = 'any_key' | 'space_or_enter';
export type EditableFocusShortcutBehavior = 'allow' | 'allow_when_empty';

export interface KeybindConfig {
	action: KeybindCommand;
	label: string;
	combo: KeyCombo;
	section: KeybindSection;
	assignable?: boolean;
	ignoreWhileTyping?: boolean;
	preventDefaultInEditable?: boolean;
	allowGlobal?: boolean;
	requiresKeyboardMode?: boolean;
	requiresMessageFocus?: boolean;
	informationalOnly?: boolean;
	hideFromDefaults?: boolean;
	defaultsShortcutDisplayKind?: DefaultsShortcutDisplayKind;
	editableFocusBehavior?: EditableFocusShortcutBehavior;
}

const TRANSMIT_MODES = ['voice_activity', 'voice_push_to_talk'] as const;

type TransmitMode = (typeof TRANSMIT_MODES)[number];

const DEFAULT_RELEASE_DELAY_MS = 20;
const MIN_RELEASE_DELAY_MS = 20;
const MAX_RELEASE_DELAY_MS = 2000;
const clampReleaseDelay = (delayMs: number): number =>
	Math.max(MIN_RELEASE_DELAY_MS, Math.min(MAX_RELEASE_DELAY_MS, Math.round(delayMs)));

type SyncedKeybindComboInit = MessageInitShape<typeof KeybindComboSchema>;
type SyncedCustomKeybindInit = MessageInitShape<typeof CustomKeybindSchema>;
type SyncedKeybindSettingsInit = MessageInitShape<typeof KeybindSettingsSchema>;
type KeybindLabelDescriptor =
	| string
	| {
			id?: string;
			message?: string;
	  };
type KeybindLabelI18n = {
	_(descriptor: KeybindLabelDescriptor): string;
};

const fallbackKeybindLabelI18n: KeybindLabelI18n = {
	_: (descriptor) => {
		if (typeof descriptor === 'string') {
			return descriptor;
		}
		return descriptor.message ?? descriptor.id ?? '';
	},
};

export interface CustomKeybindEntry {
	id: string;
	action: KeybindCommand | null;
	combo: KeyCombo;
	enabled: boolean;
}

const STORE_VERSION = 5 as const;
const generateId = (): string => {
	if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
		return crypto.randomUUID();
	}
	return `cb_${Math.random().toString(36).slice(2)}_${Date.now().toString(36)}`;
};
const toSyncedKeyCombo = (combo: KeyCombo): SyncedKeybindComboInit => ({
	key: combo.key ?? '',
	code: combo.code || undefined,
	ctrlOrMeta: !!combo.ctrlOrMeta,
	ctrl: !!combo.ctrl,
	alt: !!combo.alt,
	shift: !!combo.shift,
	meta: !!combo.meta,
	global: combo.global,
	enabled: combo.enabled,
	modifierOnly: !!combo.modifierOnly,
	bothSides: !!combo.bothSides,
	mouseButton: combo.mouseButton,
	gamepadButton: combo.gamepadButton,
});
const fromSyncedKeyCombo = (combo: SyncedKeybindCombo | undefined): KeyCombo => {
	if (!combo) return {key: '', enabled: true, global: true};
	return {
		key: combo.key ?? '',
		code: combo.code || undefined,
		ctrlOrMeta: combo.ctrlOrMeta || undefined,
		ctrl: combo.ctrl || undefined,
		alt: combo.alt || undefined,
		shift: combo.shift || undefined,
		meta: combo.meta || undefined,
		global: combo.global,
		enabled: combo.enabled,
		modifierOnly: combo.modifierOnly || undefined,
		bothSides: combo.bothSides || undefined,
		mouseButton: combo.mouseButton,
		gamepadButton: combo.gamepadButton,
	};
};
const toSyncedCustomKeybind = (entry: CustomKeybindEntry): SyncedCustomKeybindInit => ({
	id: entry.id,
	action: entry.action ?? undefined,
	combo: toSyncedKeyCombo(entry.combo),
	enabled: entry.enabled,
});
const fromSyncedCustomKeybind = (entry: SyncedCustomKeybind): CustomKeybindEntry => ({
	id: entry.id || generateId(),
	action: isKeybindCommand(entry.action) ? entry.action : null,
	combo: fromSyncedKeyCombo(entry.combo),
	enabled: entry.enabled,
});
const normalizeTransmitMode = (mode: string | undefined): TransmitMode => {
	if (mode && TRANSMIT_MODES.includes(mode as TransmitMode)) {
		return mode as TransmitMode;
	}
	return 'voice_activity';
};
const toSyncedKeybindSettings = (store: {
	customKeybinds: Array<CustomKeybindEntry>;
	transmitMode: TransmitMode;
	pushToTalkReleaseDelay: number;
}): SyncedKeybindSettingsInit => ({
	customKeybinds: store.customKeybinds.map(toSyncedCustomKeybind),
	transmitMode: store.transmitMode,
	pushToTalkReleaseDelayMs:
		store.pushToTalkReleaseDelay === DEFAULT_RELEASE_DELAY_MS ? undefined : store.pushToTalkReleaseDelay,
});

const getDefaultKeybinds = (
	i18n: KeybindLabelI18n,
	keyboardShortcutsOverlayCombo: KeyCombo = DEFAULT_KEYBOARD_SHORTCUTS_OVERLAY_COMBO,
): ReadonlyArray<KeybindConfig> =>
	[
		{
			action: 'system_toggle_shortcuts_overlay',
			label: i18n._(SHOW_KEYBOARD_SHORTCUTS_LIST_DESCRIPTOR),
			combo: {...keyboardShortcutsOverlayCombo},
			section: 'defaults',
		},
		{
			action: 'message_edit',
			label: i18n._(EDIT_MESSAGE_DESCRIPTOR),
			combo: {key: 'e'},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_delete',
			label: i18n._(DELETE_MESSAGE_DESCRIPTOR),
			combo: {key: 'Backspace'},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_pin',
			label: i18n._(PIN_MESSAGE_DESCRIPTOR),
			combo: {key: 'p'},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_react',
			label: i18n._(ADD_REACTION_DESCRIPTOR),
			combo: {key: '+'},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_reply',
			label: i18n._(REPLY_TO_MESSAGE_DESCRIPTOR),
			combo: {key: 'r'},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_forward',
			label: i18n._(FORWARD_MESSAGE_DESCRIPTOR),
			combo: {key: 'f'},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_speak',
			label: i18n._(READ_MESSAGE_ALOUD_DESCRIPTOR),
			combo: {key: 's'},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_copy_text',
			label: i18n._(COPY_TEXT_DESCRIPTOR),
			combo: {key: 'c', ctrlOrMeta: true},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_mark_unread',
			label: i18n._(MARK_AS_UNREAD_DESCRIPTOR),
			combo: {key: 'Enter', alt: true},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			section: 'messages',
		},
		{
			action: 'message_focus_textarea',
			label: i18n._(FOCUS_TEXT_AREA_DESCRIPTOR),
			combo: {key: 'Escape'},
			requiresKeyboardMode: true,
			section: 'messages',
		},
		{
			action: 'nav_guild_prev',
			label: i18n._(SWITCH_BETWEEN_COMMUNITIES_DESCRIPTOR),
			combo: {key: 'ArrowUp', ctrlOrMeta: true, alt: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_next',
			label: i18n._(SWITCH_BETWEEN_COMMUNITIES_DESCRIPTOR),
			combo: {key: 'ArrowDown', ctrlOrMeta: true, alt: true},
			section: 'navigation',
		},
		{
			action: 'nav_channel_prev',
			label: i18n._(SWITCH_BETWEEN_CHANNELS_DESCRIPTOR),
			combo: {key: 'ArrowUp', alt: true},
			section: 'navigation',
		},
		{
			action: 'nav_channel_next',
			label: i18n._(SWITCH_BETWEEN_CHANNELS_DESCRIPTOR),
			combo: {key: 'ArrowDown', alt: true},
			section: 'navigation',
		},
		{
			action: 'nav_history_back',
			label: i18n._(MOVE_BACK_THROUGH_VIEWED_CHANNEL_HISTORY_DESCRIPTOR),
			combo: {key: '[', ctrlOrMeta: true},
			ignoreWhileTyping: true,
			editableFocusBehavior: 'allow',
			section: 'navigation',
		},
		{
			action: 'nav_history_forward',
			label: i18n._(MOVE_FORWARD_THROUGH_VIEWED_CHANNEL_HISTORY_DESCRIPTOR),
			combo: {key: ']', ctrlOrMeta: true},
			ignoreWhileTyping: true,
			editableFocusBehavior: 'allow',
			section: 'navigation',
		},
		{
			action: 'nav_unread_prev',
			label: i18n._(JUMP_BETWEEN_UNREAD_CHANNELS_DESCRIPTOR),
			combo: {key: 'ArrowUp', alt: true, shift: true},
			section: 'navigation',
		},
		{
			action: 'nav_unread_next',
			label: i18n._(JUMP_BETWEEN_UNREAD_CHANNELS_DESCRIPTOR),
			combo: {key: 'ArrowDown', alt: true, shift: true},
			section: 'navigation',
		},
		{
			action: 'nav_mention_prev',
			label: i18n._(JUMP_BETWEEN_UNREAD_CHANNELS_WITH_MENTIONS_DESCRIPTOR),
			combo: {key: 'ArrowUp', alt: true, shift: true, ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_mention_next',
			label: i18n._(JUMP_BETWEEN_UNREAD_CHANNELS_WITH_MENTIONS_DESCRIPTOR),
			combo: {key: 'ArrowDown', alt: true, shift: true, ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_current_call',
			label: i18n._(JUMP_TO_THE_CURRENT_CALL_DESCRIPTOR),
			combo: {key: 'v', alt: true, shift: true, ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_toggle_last_guild_dms',
			label: i18n._(TOGGLE_BETWEEN_LAST_COMMUNITY_AND_DMS_DESCRIPTOR),
			combo: {key: 'ArrowRight', alt: true, ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_tab_prev',
			label: i18n._(SWITCH_TO_PREVIOUS_COMMUNITY_OR_DMS_DESCRIPTOR),
			combo: {key: 'Tab', ctrlOrMeta: true, shift: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_tab_next',
			label: i18n._(SWITCH_TO_NEXT_COMMUNITY_OR_DMS_DESCRIPTOR),
			combo: {key: 'Tab', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_1',
			label: i18n._(GO_TO_DIRECT_MESSAGES_DESCRIPTOR),
			combo: {key: '1', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_2',
			label: i18n._(GO_TO_FIRST_COMMUNITY_DESCRIPTOR),
			combo: {key: '2', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_3',
			label: i18n._(GO_TO_SECOND_COMMUNITY_DESCRIPTOR),
			combo: {key: '3', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_4',
			label: i18n._(GO_TO_THIRD_COMMUNITY_DESCRIPTOR),
			combo: {key: '4', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_5',
			label: i18n._(GO_TO_FOURTH_COMMUNITY_DESCRIPTOR),
			combo: {key: '5', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_6',
			label: i18n._(GO_TO_FIFTH_COMMUNITY_DESCRIPTOR),
			combo: {key: '6', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_7',
			label: i18n._(GO_TO_SIXTH_COMMUNITY_DESCRIPTOR),
			combo: {key: '7', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_8',
			label: i18n._(GO_TO_SEVENTH_COMMUNITY_DESCRIPTOR),
			combo: {key: '8', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_guild_slot_9',
			label: i18n._(GO_TO_EIGHTH_COMMUNITY_DESCRIPTOR),
			combo: {key: '9', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_quick_switcher',
			label: i18n._(TOGGLE_QUICK_SWITCHER_DESCRIPTOR),
			combo: {key: 'k', ctrlOrMeta: true},
			section: 'navigation',
		},
		{
			action: 'nav_add_guild',
			label: i18n._(CREATE_OR_JOIN_A_COMMUNITY_DESCRIPTOR),
			combo: {key: 'n', ctrlOrMeta: true, shift: true},
			section: 'navigation',
		},
		{
			action: 'dnd_start',
			label: i18n._(START_DRAG_AND_DROP_DESCRIPTOR),
			combo: {key: 'd', ctrlOrMeta: true},
			section: 'drag_and_drop',
			informationalOnly: true,
		},
		{
			action: 'dnd_move_up',
			label: i18n._(MOVE_DESCRIPTOR),
			combo: {key: 'ArrowUp'},
			section: 'drag_and_drop',
			informationalOnly: true,
		},
		{
			action: 'dnd_move_down',
			label: i18n._(MOVE_DESCRIPTOR),
			combo: {key: 'ArrowDown'},
			section: 'drag_and_drop',
			informationalOnly: true,
		},
		{
			action: 'dnd_drop',
			label: i18n._(DROP_ITEM_DESCRIPTOR),
			combo: {key: ' '},
			section: 'drag_and_drop',
			informationalOnly: true,
			defaultsShortcutDisplayKind: 'space_or_enter',
		},
		{
			action: 'dnd_cancel',
			label: i18n._(CANCEL_DESCRIPTOR),
			combo: {key: 'Escape'},
			section: 'drag_and_drop',
			informationalOnly: true,
		},
		{
			action: 'chat_mark_guild_read',
			label: i18n._(MARK_COMMUNITY_AS_READ_DESCRIPTOR),
			combo: {key: 'Escape', shift: true},
			editableFocusBehavior: 'allow',
			section: 'chat',
		},
		{
			action: 'chat_mark_channel_read',
			label: i18n._(MARK_CHANNEL_AS_READ_DESCRIPTOR),
			combo: {key: 'Escape'},
			section: 'chat',
		},
		{
			action: 'chat_new_dm',
			label: i18n._(START_A_GROUP_DM_DESCRIPTOR),
			combo: {key: 't', ctrlOrMeta: true, shift: true},
			section: 'chat',
		},
		{
			action: 'chat_toggle_pins',
			label: i18n._(TOGGLE_PINNED_MESSAGES_DESCRIPTOR),
			combo: {key: 'p', ctrlOrMeta: true},
			section: 'chat',
		},
		{
			action: 'chat_toggle_inbox',
			label: i18n._(TOGGLE_THE_INBOX_DESCRIPTOR),
			combo: {key: 'i', ctrlOrMeta: true},
			ignoreWhileTyping: true,
			editableFocusBehavior: 'allow_when_empty',
			section: 'chat',
		},
		{
			action: 'chat_mark_inbox_read',
			label: i18n._(MARK_TOP_INBOX_CHANNEL_AS_READ_DESCRIPTOR),
			combo: {key: 'e', ctrlOrMeta: true, shift: true},
			section: 'chat',
		},
		{
			action: 'chat_mark_all_inbox_read',
			label: i18n._(MARK_ALL_INBOX_CHANNELS_AS_READ_DESCRIPTOR),
			combo: {key: ''},
			assignable: true,
			section: 'chat',
		},
		{
			action: 'chat_toggle_member_list',
			label: i18n._(TOGGLE_THE_MEMBER_LIST_OR_VOICE_CHAT_DESCRIPTOR),
			combo: {key: 'u', ctrlOrMeta: true},
			editableFocusBehavior: 'allow',
			section: 'chat',
		},
		{
			action: 'chat_toggle_emoji',
			label: i18n._(TOGGLE_THE_EMOJI_PICKER_DESCRIPTOR),
			combo: {key: 'e', ctrlOrMeta: true},
			section: 'chat',
		},
		{
			action: 'chat_toggle_gif',
			label: i18n._(TOGGLE_THE_GIF_PICKER_DESCRIPTOR),
			combo: {key: 'g', ctrlOrMeta: true},
			section: 'chat',
		},
		{
			action: 'chat_toggle_sticker',
			label: i18n._(TOGGLE_THE_STICKER_PICKER_DESCRIPTOR),
			combo: {key: 's', ctrlOrMeta: true},
			section: 'chat',
		},
		{
			action: 'chat_scroll_up',
			label: i18n._(SCROLL_CHAT_UP_DESCRIPTOR),
			combo: {key: 'PageUp'},
			editableFocusBehavior: 'allow_when_empty',
			section: 'chat',
		},
		{
			action: 'chat_scroll_down',
			label: i18n._(SCROLL_CHAT_DOWN_DESCRIPTOR),
			combo: {key: 'PageDown'},
			editableFocusBehavior: 'allow_when_empty',
			section: 'chat',
		},
		{
			action: 'chat_jump_oldest_unread',
			label: i18n._(JUMP_TO_THE_OLDEST_UNREAD_MESSAGE_DESCRIPTOR),
			combo: {key: 'PageUp', shift: true},
			editableFocusBehavior: 'allow',
			section: 'chat',
		},
		{
			action: 'chat_focus_textarea',
			label: i18n._(FOCUS_THE_TEXT_AREA_DESCRIPTOR),
			combo: {key: 'Tab'},
			section: 'chat',
			defaultsShortcutDisplayKind: 'any_key',
		},
		{
			action: 'chat_upload',
			label: i18n._(UPLOAD_A_FILE_DESCRIPTOR),
			combo: {key: 'u', ctrlOrMeta: true, shift: true},
			editableFocusBehavior: 'allow',
			section: 'chat',
		},
		{
			action: 'chat_copy_channel_link',
			label: i18n._(COPY_CHANNEL_LINK_DESCRIPTOR),
			combo: {key: 'l', ctrlOrMeta: true, shift: true},
			section: 'chat',
		},
		{
			action: 'voice_toggle_mute',
			label: i18n._(TOGGLE_MUTE_DESCRIPTOR),
			combo: {key: 'm', ctrlOrMeta: true, shift: true, global: false, enabled: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
		},
		{
			action: 'voice_toggle_deafen',
			label: i18n._(VOICE_TOGGLE_DEAFEN_DESCRIPTOR),
			combo: {key: 'd', ctrlOrMeta: true, shift: true, global: false, enabled: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
		},
		{
			action: 'voice_answer_call',
			label: i18n._(ANSWER_THE_INCOMING_CALL_DESCRIPTOR),
			combo: {key: 'Enter', ctrlOrMeta: true},
			section: 'voice_and_video',
		},
		{
			action: 'voice_decline_call',
			label: i18n._(DECLINE_THE_INCOMING_CALL_DESCRIPTOR),
			combo: {key: 'Escape'},
			section: 'voice_and_video',
		},
		{
			action: 'voice_start_dm_call',
			label: i18n._(START_A_CALL_IN_A_DM_OR_GROUP_DESCRIPTOR),
			combo: {key: '`', code: 'Backquote', ctrl: true},
			section: 'voice_and_video',
		},
		{
			action: 'voice_toggle_soundboard',
			label: i18n._(TOGGLE_THE_SOUNDBOARD_DESCRIPTOR),
			combo: {key: 'b', ctrlOrMeta: true, shift: true},
			assignable: true,
			section: 'voice_and_video',
		},
		{
			action: 'voice_toggle_compact_call_view',
			label: i18n._(EXPAND_OR_COLLAPSE_COMPACT_CALL_VIEW_DESCRIPTOR),
			combo: {key: 'v', code: 'KeyV', ctrlOrMeta: true, shift: true},
			preventDefaultInEditable: true,
			assignable: true,
			section: 'voice_and_video',
		},
		{
			action: 'misc_help',
			label: i18n._(OPEN_HELP_DESCRIPTOR),
			combo: {key: 'h', ctrlOrMeta: true, shift: true},
			section: 'misc',
		},
		{
			action: 'misc_search',
			label: i18n._(SEARCH_MESSAGES_DESCRIPTOR),
			combo: {key: 'f', ctrlOrMeta: true},
			section: 'misc',
		},
		{
			action: 'misc_open_context_menu',
			label: i18n._(OPEN_THE_CONTEXT_MENU_DESCRIPTOR),
			combo: {key: 'F10', shift: true},
			section: 'misc',
			informationalOnly: true,
		},
		{
			action: 'system_toggle_settings',
			label: i18n._(OPEN_YOUR_SETTINGS_DESCRIPTOR),
			combo: {key: ',', ctrlOrMeta: true},
			section: 'misc',
			hideFromDefaults: true,
		},
		{
			action: 'system_open_theme_studio_popout',
			label: i18n._(OPEN_THEME_STUDIO_POPOUT_DESCRIPTOR),
			combo: {key: 't', ctrlOrMeta: true, alt: true, shift: true},
			ignoreWhileTyping: true,
			editableFocusBehavior: 'allow',
			section: 'misc',
			assignable: true,
		},
		{
			action: 'system_zoom_in',
			label: i18n._(ZOOM_IN_DESCRIPTOR),
			combo: {key: '=', ctrlOrMeta: true},
			section: 'misc',
			hideFromDefaults: true,
		},
		{
			action: 'system_zoom_out',
			label: i18n._(ZOOM_OUT_DESCRIPTOR),
			combo: {key: '-', ctrlOrMeta: true},
			section: 'misc',
			hideFromDefaults: true,
		},
		{
			action: 'system_zoom_reset',
			label: i18n._(RESET_ZOOM_DESCRIPTOR),
			combo: {key: '0', ctrlOrMeta: true},
			section: 'misc',
			hideFromDefaults: true,
		},
		{
			action: 'message_bookmark',
			label: i18n._(BOOKMARK_MESSAGE_DESCRIPTOR),
			combo: {key: ''},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			assignable: true,
			section: 'messages',
			hideFromDefaults: true,
		},
		{
			action: 'message_toggle_embeds',
			label: i18n._(TOGGLE_EMBED_SUPPRESSION_DESCRIPTOR),
			combo: {key: ''},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			assignable: true,
			section: 'messages',
			hideFromDefaults: true,
		},
		{
			action: 'message_copy_link',
			label: i18n._(COPY_MESSAGE_LINK_DESCRIPTOR),
			combo: {key: ''},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			assignable: true,
			section: 'messages',
			hideFromDefaults: true,
		},
		{
			action: 'message_copy_id',
			label: i18n._(COPY_MESSAGE_ID_DESCRIPTOR),
			combo: {key: ''},
			requiresKeyboardMode: true,
			requiresMessageFocus: true,
			assignable: true,
			section: 'messages',
			hideFromDefaults: true,
		},
		{
			action: 'chat_toggle_saved_media',
			label: i18n._(TOGGLE_SAVED_MEDIA_DESCRIPTOR),
			combo: {key: 'm', ctrlOrMeta: true},
			section: 'chat',
		},
		{
			action: 'chat_send_voice_message',
			label: i18n._(SEND_VOICE_MESSAGE_DESCRIPTOR),
			combo: {key: 'v', code: 'KeyV', ctrlOrMeta: true, alt: true},
			editableFocusBehavior: 'allow',
			assignable: true,
			section: 'chat',
		},
		{
			action: 'voice_push_to_talk',
			label: i18n._(PUSH_TO_TALK_DESCRIPTOR),
			combo: {key: '', enabled: true, global: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
			hideFromDefaults: true,
		},
		{
			action: 'voice_push_to_talk_priority',
			label: i18n._(PUSH_TO_TALK_PRIORITY_DESCRIPTOR),
			combo: {key: '', enabled: true, global: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
			hideFromDefaults: true,
		},
		{
			action: 'voice_push_to_mute',
			label: i18n._(PUSH_TO_MUTE_DESCRIPTOR),
			combo: {key: '', enabled: true, global: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
			hideFromDefaults: true,
		},
		{
			action: 'voice_priority_vad',
			label: i18n._(VOICE_ACTIVITY_PRIORITY_DESCRIPTOR),
			combo: {key: '', enabled: true, global: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
			hideFromDefaults: true,
		},
		{
			action: 'voice_toggle_vad',
			label: i18n._(TOGGLE_VOICE_ACTIVITY_DESCRIPTOR),
			combo: {key: '', enabled: true, global: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
			hideFromDefaults: true,
		},
		{
			action: 'voice_toggle_camera',
			label: i18n._(TOGGLE_CAMERA_DESCRIPTOR),
			combo: {key: '', enabled: true, global: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
			hideFromDefaults: true,
		},
		{
			action: 'voice_switch_channel',
			label: i18n._(SWITCH_VOICE_CHANNEL_DESCRIPTOR),
			combo: {key: '', enabled: true, global: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
			hideFromDefaults: true,
		},
		{
			action: 'voice_disconnect',
			label: i18n._(DISCONNECT_FROM_VOICE_DESCRIPTOR),
			combo: {key: '', enabled: true, global: true},
			allowGlobal: true,
			assignable: true,
			section: 'voice_and_video',
			hideFromDefaults: true,
		},
	] as const;

class Keybind {
	customKeybinds: Array<CustomKeybindEntry> = [];
	transmitMode: TransmitMode = 'voice_activity';
	pushToTalkHeld = false;
	pushToMuteHeld = false;
	prioritySpeakerHeld = false;
	pushToTalkReleaseDelay = DEFAULT_RELEASE_DELAY_MS;
	syncAcrossDevices = false;
	disableBuiltinKeybinds = false;
	mutedActions: Set<KeybindCommand> = new Set();
	private i18n: I18n | null = null;
	private initialized = false;
	private keyboardShortcutsOverlayCombo: KeyCombo = {...DEFAULT_KEYBOARD_SHORTCUTS_OVERLAY_COMBO};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(
			this,
			KEYBIND_STORE_NAME,
			['customKeybinds', 'transmitMode', 'pushToTalkReleaseDelay', 'syncAcrossDevices', 'disableBuiltinKeybinds'],
			{version: STORE_VERSION, syncAcrossTabs: true},
		);
		await makeSyncedField(this, {
			field: 'keybinds',
			schema: KeybindSettingsSchema,
			persist: [
				'customKeybinds',
				'transmitMode',
				'pushToTalkReleaseDelay',
				'syncAcrossDevices',
				'disableBuiltinKeybinds',
			],
			version: STORE_VERSION,
			enabled: () => this.syncAcrossDevices,
			toMessage: (store) => toSyncedKeybindSettings(store),
			applyMessage: (store, message) => {
				store.customKeybinds = message.customKeybinds.map(fromSyncedCustomKeybind);
				store.transmitMode = normalizeTransmitMode(message.transmitMode);
				store.pushToTalkReleaseDelay = clampReleaseDelay(message.pushToTalkReleaseDelayMs ?? DEFAULT_RELEASE_DELAY_MS);
			},
		});
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
		if (this.initialized) return;
		void awaitHydration(KEYBIND_STORE_NAME).then(() => {
			runInAction(() => {
				if (this.initialized) return;
				if (!Array.isArray(this.customKeybinds)) {
					this.customKeybinds = [];
				}
				this.initialized = true;
			});
		});
	}

	private getLabelI18n(): KeybindLabelI18n {
		return this.i18n ?? fallbackKeybindLabelI18n;
	}

	async refreshKeyboardShortcutLayout(): Promise<void> {
		try {
			const combo = await getKeyboardShortcutsOverlayComboForCurrentLayout();
			if (!combo) return;
			this.setKeyboardShortcutsOverlayCombo(combo);
		} catch {}
	}

	useKeyboardShortcutsOverlayFallback(): void {
		this.setKeyboardShortcutsOverlayCombo({...SHIFTED_SLASH_FALLBACK_KEYBOARD_SHORTCUTS_OVERLAY_COMBO});
	}

	private setKeyboardShortcutsOverlayCombo(combo: KeyCombo): void {
		if (keyCombosEqual(this.keyboardShortcutsOverlayCombo, combo)) return;
		runInAction(() => {
			this.keyboardShortcutsOverlayCombo = {...combo};
		});
	}

	getDefaults(): ReadonlyArray<KeybindConfig> {
		return getDefaultKeybinds(this.getLabelI18n(), this.keyboardShortcutsOverlayCombo);
	}

	getDefaultByAction(action: KeybindCommand): KeybindConfig | null {
		return this.getDefaults().find((c) => c.action === action) ?? null;
	}

	getActiveCombosForAction(action: KeybindCommand): Array<KeyCombo> {
		const fallback = this.getDefaultByAction(action);
		const customBindings = this.customKeybinds.filter((entry) => entry.action === action);
		return getActiveCombosForResolvedAction(fallback, customBindings);
	}

	hasActiveBindingFor(action: KeybindCommand): boolean {
		return this.getActiveCombosForAction(action).length > 0;
	}

	getDefaultsForRuntimeDispatch(): Array<KeybindConfig> {
		return this.getDefaults().filter((c) => !c.informationalOnly);
	}

	getCustomKeybinds(): ReadonlyArray<CustomKeybindEntry> {
		return this.customKeybinds;
	}

	addCustomKeybind(): CustomKeybindEntry {
		const entry: CustomKeybindEntry = {
			id: generateId(),
			action: null,
			combo: {key: '', enabled: true, global: true},
			enabled: true,
		};
		runInAction(() => {
			this.customKeybinds.push(entry);
		});
		return entry;
	}

	removeCustomKeybind(id: string): void {
		runInAction(() => {
			this.customKeybinds = this.customKeybinds.filter((c) => c.id !== id);
		});
	}

	updateCustomKeybind(id: string, patch: Partial<Omit<CustomKeybindEntry, 'id'>>): void {
		runInAction(() => {
			this.customKeybinds = this.customKeybinds.map((entry) => (entry.id === id ? {...entry, ...patch} : entry));
		});
	}

	updateCustomKeybindCombo(id: string, combo: KeyCombo): void {
		this.updateCustomKeybind(id, {combo});
	}

	addCustomKeybindForAction(action: KeybindCommand): CustomKeybindEntry {
		const entry: CustomKeybindEntry = {
			id: generateId(),
			action,
			combo: {key: '', enabled: true, global: true},
			enabled: true,
		};
		runInAction(() => {
			this.customKeybinds.push(entry);
		});
		return entry;
	}

	removeCustomKeybindsForAction(action: KeybindCommand): void {
		runInAction(() => {
			this.customKeybinds = this.customKeybinds.filter((entry) => entry.action !== action);
		});
	}

	setCustomKeybindAction(id: string, action: KeybindCommand | null): void {
		this.updateCustomKeybind(id, {action});
	}

	setCustomKeybindEnabled(id: string, enabled: boolean): void {
		this.updateCustomKeybind(id, {enabled});
	}

	resetCustomKeybinds(): void {
		runInAction(() => {
			this.customKeybinds = [];
		});
	}

	getSyncAcrossDevices(): boolean {
		return this.syncAcrossDevices;
	}

	getDisableBuiltinKeybinds(): boolean {
		return this.disableBuiltinKeybinds;
	}

	setDisableBuiltinKeybinds(value: boolean): void {
		runInAction(() => {
			this.disableBuiltinKeybinds = value;
		});
	}

	setSyncAcrossDevices(value: boolean): void {
		if (value) {
			void this.pushSyncedKeybinds();
		}
		runInAction(() => {
			this.syncAcrossDevices = value;
		});
	}

	private async pushSyncedKeybinds(): Promise<void> {
		if (!UserSettings.isHydrated()) return;
		await UserSettings.setSubPreference('keybinds', create(KeybindSettingsSchema, toSyncedKeybindSettings(this)));
	}

	getPrimaryCustomKeybind(action: KeybindCommand): CustomKeybindEntry | null {
		return this.customKeybinds.find((c) => c.action === action) ?? null;
	}

	setPrimaryCustomKeybindCombo(action: KeybindCommand, combo: KeyCombo): CustomKeybindEntry {
		const existing = this.getPrimaryCustomKeybind(action);
		if (existing) {
			this.updateCustomKeybindCombo(existing.id, combo);
			return {...existing, combo};
		}
		const created: CustomKeybindEntry = {
			id: generateId(),
			action,
			combo: {...combo, enabled: true, global: combo.global ?? true},
			enabled: true,
		};
		runInAction(() => {
			this.customKeybinds.push(created);
		});
		return created;
	}

	setTransmitMode(mode: TransmitMode): void {
		runInAction(() => {
			this.transmitMode = mode;
		});
	}

	toggleTransmitMode(): void {
		this.setTransmitMode(this.transmitMode === 'voice_push_to_talk' ? 'voice_activity' : 'voice_push_to_talk');
	}

	isPushToTalkEnabled(): boolean {
		return this.transmitMode === 'voice_push_to_talk';
	}

	setPushToTalkHeld(held: boolean): void {
		runInAction(() => {
			this.pushToTalkHeld = held;
		});
	}

	hasPushToTalkKeybind(): boolean {
		return this.hasActiveBindingFor('voice_push_to_talk') || this.hasActiveBindingFor('voice_push_to_talk_priority');
	}

	isPushToTalkEffective(): boolean {
		return this.isPushToTalkEnabled() && this.hasPushToTalkKeybind();
	}

	setPushToTalkReleaseDelay(delayMs: number): void {
		const clamped = clampReleaseDelay(delayMs);
		runInAction(() => {
			this.pushToTalkReleaseDelay = clamped;
		});
	}

	handlePushToTalkPress(): boolean {
		runInAction(() => {
			this.pushToTalkHeld = true;
		});
		return true;
	}

	handlePushToTalkRelease(): boolean {
		runInAction(() => {
			this.pushToTalkHeld = false;
		});
		return true;
	}

	resetPushToTalkState(): void {
		runInAction(() => {
			this.pushToTalkHeld = false;
		});
	}

	setPushToMuteHeld(held: boolean): void {
		runInAction(() => {
			this.pushToMuteHeld = held;
		});
	}

	hasPushToMuteKeybind(): boolean {
		return this.hasActiveBindingFor('voice_push_to_mute');
	}

	isPushToMuteEffective(): boolean {
		return !this.isPushToTalkEffective() && this.hasPushToMuteKeybind();
	}

	resetPushToMuteState(): void {
		runInAction(() => {
			this.pushToMuteHeld = false;
		});
	}

	setPrioritySpeakerHeld(held: boolean): void {
		runInAction(() => {
			this.prioritySpeakerHeld = held;
		});
	}

	resetPrioritySpeakerState(): void {
		this.setPrioritySpeakerHeld(false);
	}

	muteActions(actions: Iterable<KeybindCommand>): void {
		runInAction(() => {
			for (const action of actions) {
				this.mutedActions.add(action);
			}
		});
	}

	unmuteActions(actions: Iterable<KeybindCommand>): void {
		runInAction(() => {
			for (const action of actions) {
				this.mutedActions.delete(action);
			}
		});
	}

	isActionMuted(action: KeybindCommand): boolean {
		return this.mutedActions.has(action);
	}

	getByAction(action: KeybindCommand): KeybindConfig & {
		combo: KeyCombo;
	} {
		const base = this.getDefaultByAction(action);
		if (!base) throw new Error(`Unknown keybind action: ${action}`);
		const customBindings = this.customKeybinds.filter((c) => c.action === action);
		return getDisplayKeybindForResolvedAction(base, customBindings);
	}

	getAll(): Array<
		KeybindConfig & {
			combo: KeyCombo;
		}
	> {
		const defaults = this.getDefaults();
		return defaults.map((entry) => ({...entry, combo: entry.combo}));
	}
}

export default new Keybind();

export function getDefaultKeybind(action: KeybindCommand, i18n: I18n): KeyCombo | null {
	const entry = getDefaultKeybinds(i18n).find((k) => k.action === action);
	return entry ? {...entry.combo} : null;
}

export function getActionLabel(action: KeybindCommand, i18n: I18n): string {
	const entry = getDefaultKeybinds(i18n).find((k) => k.action === action);
	return entry?.label ?? action;
}
