// SPDX-License-Identifier: AGPL-3.0-or-later

import {VOICE_CHANNEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Platform} from '@app/features/platform/types/Platform';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	CHAT_INPUT_DESCRIPTOR,
	ENABLE_FAVORITES_DESCRIPTOR,
	KEEP_NEKO_STILL_DESCRIPTOR,
	KEYBOARD_HINTS_DESCRIPTOR,
	NEKO_DESCRIPTOR,
	SHOW_DOWNLOAD_BUTTON_DESCRIPTOR,
	SHOW_HELP_CENTER_BUTTON_DESCRIPTOR,
	SHOW_NEKO_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const CONFIRM_BEFORE_JOINING_VOICE_CHANNELS_DESCRIPTOR = msg({
	message: 'Confirm before joining voice channels',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const VOICE_CHANNEL_JOIN_BEHAVIOR_DESCRIPTOR = msg({
	message: 'Voice channel join behavior',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const VOICE_CHANNEL_CONFIRMATION_DESCRIPTOR = msg({
	message: 'Voice channel confirmation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIRM_VOICE_JOIN_DESCRIPTOR = msg({
	message: 'Confirm voice join',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const JOIN_VOICE_DESCRIPTOR = msg({
	message: 'Join voice',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMMUNITY_VOICE_DESCRIPTOR = msg({
	message: 'Community voice',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TWO_CLICKS_DESCRIPTOR = msg({
	message: 'Two clicks',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_WHETHER_JOINING_A_COMMUNITY_VOICE_CHANNEL_ASKS_DESCRIPTOR = msg({
	message: 'Confirmation or double-click for community voice joins',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const HIDE_KEYBOARD_HINTS_DESCRIPTOR = msg({
	message: 'Hide keyboard hints',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TOOLTIP_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Tooltip shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHORTCUT_BADGES_DESCRIPTOR = msg({
	message: 'Shortcut badges',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_WHETHER_TOOLTIPS_SHOW_KEYBOARD_SHORTCUT_HINTS_DESCRIPTOR = msg({
	message: 'Keyboard shortcut hints in tooltips',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});
const SHOW_OR_HIDE_NEKO_THAT_CHASES_THE_CURSOR_DESCRIPTOR = msg({
	message: 'Neko cat that chases your cursor',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});
const KEEP_NEKO_FROM_CHASING_THE_CURSOR_DESCRIPTOR = msg({
	message: 'Stop Neko from chasing your cursor while keeping it draggable and interactive.',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});
const STILL_NEKO_DESCRIPTOR = msg({
	message: 'Still Neko',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STATIC_NEKO_DESCRIPTOR = msg({
	message: 'Static Neko',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FAVORITE_CHANNELS_DESCRIPTOR = msg({
	message: 'Favorite channels',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STARRED_CHANNELS_DESCRIPTOR = msg({
	message: 'Starred channels',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_WHETHER_FAVORITES_ARE_VISIBLE_THROUGHOUT_THE_APP_DESCRIPTOR = msg({
	message: 'Show favorites throughout the app',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});
const HELP_BUTTON_DESCRIPTOR = msg({
	message: 'Help button',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DOWNLOAD_APP_DESCRIPTOR = msg({
	message: 'Download app',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HELP_CENTER_BUTTON_IN_THE_SIDEBAR_DESCRIPTOR = msg({
	message: 'Help center button in the sidebar',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});
const DOWNLOAD_BUTTON_IN_THE_SIDEBAR_DESCRIPTOR = msg({
	message: 'Download app button in the sidebar',
	comment: 'Settings search entry description. One-line summary of what the setting controls.',
});
export const appearanceIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'appearance-show-neko',
		tabType: 'appearance',
		sectionId: 'interface',
		label: SHOW_NEKO_DESCRIPTOR,
		keywords: [NEKO_DESCRIPTOR, CHAT_INPUT_DESCRIPTOR],
		description: SHOW_OR_HIDE_NEKO_THAT_CHASES_THE_CURSOR_DESCRIPTOR,
		audience: 'advanced',
		tags: ['appearance', 'chat'],
	},
	{
		id: 'appearance-keep-neko-still',
		tabType: 'appearance',
		sectionId: 'interface',
		label: KEEP_NEKO_STILL_DESCRIPTOR,
		keywords: [NEKO_DESCRIPTOR, STILL_NEKO_DESCRIPTOR, STATIC_NEKO_DESCRIPTOR],
		description: KEEP_NEKO_FROM_CHASING_THE_CURSOR_DESCRIPTOR,
		audience: 'advanced',
		tags: ['appearance', 'chat'],
	},
	{
		id: 'appearance-hide-keyboard-hints',
		tabType: 'appearance',
		sectionId: 'interface',
		label: KEYBOARD_HINTS_DESCRIPTOR,
		keywords: [HIDE_KEYBOARD_HINTS_DESCRIPTOR, TOOLTIP_SHORTCUTS_DESCRIPTOR, SHORTCUT_BADGES_DESCRIPTOR],
		description: CHOOSE_WHETHER_TOOLTIPS_SHOW_KEYBOARD_SHORTCUT_HINTS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['appearance'],
	},
	{
		id: 'appearance-enable-favorites',
		tabType: 'appearance',
		sectionId: 'channel-list',
		label: ENABLE_FAVORITES_DESCRIPTOR,
		keywords: [FAVORITE_CHANNELS_DESCRIPTOR, STARRED_CHANNELS_DESCRIPTOR],
		description: CHOOSE_WHETHER_FAVORITES_ARE_VISIBLE_THROUGHOUT_THE_APP_DESCRIPTOR,
		audience: 'advanced',
		tags: ['appearance'],
	},
	{
		id: 'appearance-show-help-center-button',
		tabType: 'appearance',
		sectionId: 'interface',
		label: SHOW_HELP_CENTER_BUTTON_DESCRIPTOR,
		keywords: [HELP_BUTTON_DESCRIPTOR],
		description: HELP_CENTER_BUTTON_IN_THE_SIDEBAR_DESCRIPTOR,
		audience: 'advanced',
		tags: ['appearance'],
	},
	{
		id: 'appearance-show-download-button',
		tabType: 'appearance',
		sectionId: 'interface',
		label: SHOW_DOWNLOAD_BUTTON_DESCRIPTOR,
		keywords: [DOWNLOAD_APP_DESCRIPTOR],
		description: DOWNLOAD_BUTTON_IN_THE_SIDEBAR_DESCRIPTOR,
		audience: 'advanced',
		tags: ['appearance'],
		isVisible: () => !Platform.isElectron && !Platform.isPWA,
	},
	{
		id: 'appearance-voice-channel-join-behavior',
		tabType: 'appearance',
		sectionId: 'interface',
		label: VOICE_CHANNEL_JOIN_BEHAVIOR_DESCRIPTOR,
		keywords: [
			CONFIRM_BEFORE_JOINING_VOICE_CHANNELS_DESCRIPTOR,
			VOICE_CHANNEL_CONFIRMATION_DESCRIPTOR,
			CONFIRM_VOICE_JOIN_DESCRIPTOR,
			JOIN_VOICE_DESCRIPTOR,
			VOICE_CHANNEL_DESCRIPTOR,
			COMMUNITY_VOICE_DESCRIPTOR,
			TWO_CLICKS_DESCRIPTOR,
		],
		description: CHOOSE_WHETHER_JOINING_A_COMMUNITY_VOICE_CHANNEL_ASKS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['appearance', 'voice'],
	},
];
