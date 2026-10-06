// SPDX-License-Identifier: AGPL-3.0-or-later

import {MARK_AS_READ_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {INBOX_DESCRIPTOR} from '@app/features/user/components/settings_utils/search_index/SharedDescriptors';
import {
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DEAFEN_SHORTCUT_DESCRIPTOR,
	VOICE_SET_DEAFEN_SHORTCUT_DESCRIPTOR,
	VOICE_TOGGLE_DEAFEN_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';

const KEYBOARD_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Keyboard shortcuts',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const SHORTCUTS_DESCRIPTOR = msg({
	message: 'Shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HOTKEYS_DESCRIPTOR = msg({
	message: 'Hotkeys',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const KEYBOARD_DESCRIPTOR = msg({
	message: 'Keyboard',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const KEYS_DESCRIPTOR = msg({
	message: 'Keys',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HOTKEY_DESCRIPTOR = msg({
	message: 'Hotkey',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHORTCUT_DESCRIPTOR = msg({
	message: 'Shortcut',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BIND_DESCRIPTOR = msg({
	message: 'Bind',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BINDINGS_DESCRIPTOR = msg({
	message: 'Bindings',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const QUICK_SWITCHER_DESCRIPTOR = msg({
	message: 'Quick switcher',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MARK_READ_DESCRIPTOR = msg({
	message: 'Mark read',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MARK_CHANNEL_AS_READ_DESCRIPTOR = msg({
	message: 'Mark channel as read',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOOKMARK_DESCRIPTOR = msg({
	message: 'Bookmark',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BOOKMARK_MESSAGE_DESCRIPTOR = msg({
	message: 'Bookmark message',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PIN_DESCRIPTOR = msg({
	message: 'Pin',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PINS_POPOUT_DESCRIPTOR = msg({
	message: 'Pins popout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INBOX_POPOUT_DESCRIPTOR = msg({
	message: 'Inbox popout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ZOOM_IN_DESCRIPTOR = msg({
	message: 'Zoom in',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ZOOM_OUT_DESCRIPTOR = msg({
	message: 'Zoom out',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RELOAD_DESCRIPTOR = msg({
	message: 'Reload',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NAVIGATE_DESCRIPTOR = msg({
	message: 'Navigate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIGURE_KEYBOARD_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Configure keyboard shortcuts',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const CUSTOM_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Custom shortcuts',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const ADD_A_SHORTCUT_DESCRIPTOR = msg({
	message: 'Add a shortcut',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RESET_TO_DEFAULTS_DESCRIPTOR = msg({
	message: 'Reset to defaults',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISABLE_BUILT_IN_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Disable built-in shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYNC_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Sync shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHORTCUT_CONFLICTS_DESCRIPTOR = msg({
	message: 'Shortcut conflicts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ADD_CUSTOM_SHORTCUTS_DISABLE_BUILT_IN_SHORTCUTS_AND_DESCRIPTOR = msg({
	message: 'Add custom shortcuts, disable built-in shortcuts, and sync shortcut preferences',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const DEFAULT_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Default shortcuts',
	comment: 'Settings search entry label. Also used as a search synonym in the settings search bar.',
});
const BUILT_IN_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Built-in shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGES_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Message shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NAVIGATION_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Navigation shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHAT_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Chat shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VOICE_AND_VIDEO_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Voice and video shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REVIEW_THE_BUILT_IN_KEYBOARD_SHORTCUTS_BY_SECTION_DESCRIPTOR = msg({
	message: 'Review the built-in keyboard shortcuts by section',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const PUSH_TO_TALK_KEY_DESCRIPTOR = msg({
	message: 'Push to talk key',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const PUSH_TO_TALK_DESCRIPTOR = msg({
	message: 'Push to talk',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const KEY_DESCRIPTOR = msg({
	message: 'Key',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SET_PUSH_TO_TALK_KEY_DESCRIPTOR = msg({
	message: 'Set push to talk key',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const MUTE_SHORTCUT_DESCRIPTOR = msg({
	message: 'Mute shortcut',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'voice-control-action',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TOGGLE_MUTE_DESCRIPTOR = msg({
	message: 'Toggle mute',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SET_MUTE_SHORTCUT_DESCRIPTOR = msg({
	message: 'Set mute shortcut',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
export const keybindsIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'keybinds-shortcuts',
		tabType: 'keybinds',
		label: KEYBOARD_SHORTCUTS_DESCRIPTOR,
		keywords: [
			KEYBOARD_SHORTCUTS_DESCRIPTOR,
			SHORTCUTS_DESCRIPTOR,
			HOTKEYS_DESCRIPTOR,
			KEYBOARD_DESCRIPTOR,
			KEYS_DESCRIPTOR,
			HOTKEY_DESCRIPTOR,
			SHORTCUT_DESCRIPTOR,
			BIND_DESCRIPTOR,
			BINDINGS_DESCRIPTOR,
			QUICK_SWITCHER_DESCRIPTOR,
			MARK_READ_DESCRIPTOR,
			MARK_AS_READ_DESCRIPTOR,
			MARK_CHANNEL_AS_READ_DESCRIPTOR,
			BOOKMARK_DESCRIPTOR,
			BOOKMARK_MESSAGE_DESCRIPTOR,
			PIN_DESCRIPTOR,
			PINS_POPOUT_DESCRIPTOR,
			INBOX_DESCRIPTOR,
			INBOX_POPOUT_DESCRIPTOR,
			ZOOM_IN_DESCRIPTOR,
			ZOOM_OUT_DESCRIPTOR,
			RELOAD_DESCRIPTOR,
			NAVIGATE_DESCRIPTOR,
		],
		description: CONFIGURE_KEYBOARD_SHORTCUTS_DESCRIPTOR,
	},
	{
		id: 'keybinds-custom',
		tabType: 'keybinds',
		label: CUSTOM_SHORTCUTS_DESCRIPTOR,
		keywords: [
			CUSTOM_SHORTCUTS_DESCRIPTOR,
			ADD_A_SHORTCUT_DESCRIPTOR,
			RESET_TO_DEFAULTS_DESCRIPTOR,
			DISABLE_BUILT_IN_SHORTCUTS_DESCRIPTOR,
			SYNC_SHORTCUTS_DESCRIPTOR,
			SHORTCUT_CONFLICTS_DESCRIPTOR,
		],
		description: ADD_CUSTOM_SHORTCUTS_DISABLE_BUILT_IN_SHORTCUTS_AND_DESCRIPTOR,
	},
	{
		id: 'keybinds-defaults',
		tabType: 'keybinds',
		label: DEFAULT_SHORTCUTS_DESCRIPTOR,
		keywords: [
			DEFAULT_SHORTCUTS_DESCRIPTOR,
			BUILT_IN_SHORTCUTS_DESCRIPTOR,
			MESSAGES_SHORTCUTS_DESCRIPTOR,
			NAVIGATION_SHORTCUTS_DESCRIPTOR,
			CHAT_SHORTCUTS_DESCRIPTOR,
			VOICE_AND_VIDEO_SHORTCUTS_DESCRIPTOR,
		],
		description: REVIEW_THE_BUILT_IN_KEYBOARD_SHORTCUTS_BY_SECTION_DESCRIPTOR,
	},
	{
		id: 'keybinds-ptt',
		tabType: 'keybinds',
		label: PUSH_TO_TALK_KEY_DESCRIPTOR,
		keywords: [PUSH_TO_TALK_DESCRIPTOR, SHORTCUT_DESCRIPTOR, KEY_DESCRIPTOR],
		description: SET_PUSH_TO_TALK_KEY_DESCRIPTOR,
	},
	{
		id: 'keybinds-mute',
		tabType: 'keybinds',
		label: MUTE_SHORTCUT_DESCRIPTOR,
		keywords: [MUTE_DESCRIPTOR, SHORTCUT_DESCRIPTOR, TOGGLE_MUTE_DESCRIPTOR],
		description: SET_MUTE_SHORTCUT_DESCRIPTOR,
	},
	{
		id: 'keybinds-deafen',
		tabType: 'keybinds',
		label: VOICE_DEAFEN_SHORTCUT_DESCRIPTOR,
		keywords: [VOICE_DEAFEN_DESCRIPTOR, VOICE_DEAFEN_SHORTCUT_DESCRIPTOR, VOICE_TOGGLE_DEAFEN_DESCRIPTOR],
		description: VOICE_SET_DEAFEN_SHORTCUT_DESCRIPTOR,
	},
];
