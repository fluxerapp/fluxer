// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	APP_ZOOM_LEVEL_DESCRIPTOR,
	ONLINE_DESCRIPTOR,
	VOICE_CHANNEL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {shouldShowHdrSettings} from '@app/features/user/components/modals/tabs/appearance_tab/AppearanceTabHdrTab';
import {shouldShowAppZoomLevel} from '@app/features/user/components/settings_utils/AppZoomLevelUtils';
import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	CHANNEL_LIST_DESCRIPTOR,
	CHAT_INPUT_DESCRIPTOR,
	COLOR_DESCRIPTOR,
	COLORS_DESCRIPTOR,
	DIRECT_MESSAGE_DESCRIPTOR,
	FONTS_DESCRIPTOR,
	FRIENDS_DESCRIPTOR,
	INDICATOR_DESCRIPTOR,
	KEYBOARD_2_DESCRIPTOR,
	MESSAGES_3_DESCRIPTOR,
	MUTED_CHANNELS_DESCRIPTOR,
	MUTED_DESCRIPTOR,
	NAVIGATION_DESCRIPTOR,
	NEKO_DESCRIPTOR,
	SHOW_NEKO_DESCRIPTOR,
	SIDEBAR_2_DESCRIPTOR,
	TYPING_DESCRIPTOR,
	UNREAD_DESCRIPTOR,
	VOICE_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {msg} from '@lingui/core/macro';

const CHOOSE_A_BASE_APPEARANCE_MANAGE_QUICK_CSS_ORGANIZE_DESCRIPTOR = msg({
	message: 'Choose a base appearance, manage quick CSS, organize saved theme files, and tune theme tokens.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const THEME_2_DESCRIPTOR = msg({
	message: 'Theme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DARK_MODE_DESCRIPTOR = msg({
	message: 'Dark mode',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LIGHT_MODE_DESCRIPTOR = msg({
	message: 'Light mode',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DARK_DESCRIPTOR = msg({
	message: 'Dark',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LIGHT_DESCRIPTOR = msg({
	message: 'Light',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COAL_DESCRIPTOR = msg({
	message: 'Coal',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COLOR_SCHEME_DESCRIPTOR = msg({
	message: 'Color scheme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYSTEM_THEME_DESCRIPTOR = msg({
	message: 'System theme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYNC_DESCRIPTOR = msg({
	message: 'Sync',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYNC_THEME_DESCRIPTOR = msg({
	message: 'Sync theme',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_SYNC_DESCRIPTOR = msg({
	message: 'Theme sync',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DEVICES_DESCRIPTOR = msg({
	message: 'Devices',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CSS_DESCRIPTOR = msg({
	message: 'CSS',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const QUICK_CSS_DESCRIPTOR = msg({
	message: 'Quick CSS',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_LIBRARY_DESCRIPTOR = msg({
	message: 'Theme library',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_FILES_DESCRIPTOR = msg({
	message: 'Theme files',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const IMPORT_THEMES_DESCRIPTOR = msg({
	message: 'Import themes',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const EXPORT_THEMES_DESCRIPTOR = msg({
	message: 'Export themes',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_ASSETS_DESCRIPTOR = msg({
	message: 'Theme assets',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOCAL_FILES_DESCRIPTOR = msg({
	message: 'Local files',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_TOKENS_DESCRIPTOR = msg({
	message: 'Theme tokens',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const OVERRIDES_DESCRIPTOR = msg({
	message: 'Overrides',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STYLING_DESCRIPTOR = msg({
	message: 'Styling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LOOK_AND_FEEL_DESCRIPTOR = msg({
	message: 'Look and feel',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIGH_DYNAMIC_RANGE_DESCRIPTOR = msg({
	message: 'High dynamic range',
	comment: 'Settings section label. Also used as a search synonym in the settings search bar.',
});
const CONTROL_HOW_HDR_IMAGES_ARE_DISPLAYED_ON_HDR_DESCRIPTOR = msg({
	message: 'Control how HDR images are displayed on HDR-capable monitors.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const HDR_DESCRIPTOR = msg({
	message: 'HDR',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BRIGHTNESS_DESCRIPTOR = msg({
	message: 'Brightness',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISPLAY_2_DESCRIPTOR = msg({
	message: 'Display',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TONE_DESCRIPTOR = msg({
	message: 'Tone',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHAT_FONT_SCALING_DESCRIPTOR = msg({
	message: 'Chat font scaling',
	comment: 'Settings section label. Also used as a search synonym in the settings search bar.',
});
const ADJUST_THE_FONT_SIZE_IN_THE_CHAT_AREA_DESCRIPTOR = msg({
	message: 'Adjust the font size in the chat area.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const FONT_DESCRIPTOR = msg({
	message: 'Font',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SIZE_DESCRIPTOR = msg({
	message: 'Size',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TEXT_SIZE_DESCRIPTOR = msg({
	message: 'Text size',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ZOOM_DESCRIPTOR = msg({
	message: 'Zoom',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCALE_DESCRIPTOR = msg({
	message: 'Scale',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHAT_FONT_DESCRIPTOR = msg({
	message: 'Chat font',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ADJUST_THE_APPLICATION_S_ZOOM_LEVEL_DESCRIPTOR = msg({
	message: "Adjust the application's zoom level.",
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const APP_ZOOM_DESCRIPTOR = msg({
	message: 'App zoom',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INTERFACE_SIZE_DESCRIPTOR = msg({
	message: 'Interface size',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISPLAY_SCALING_DESCRIPTOR = msg({
	message: 'Display scaling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STREAMER_MODE_DESCRIPTOR = msg({
	message: 'Streaming privacy',
	comment: 'Settings section label. Also used as a search synonym in the settings search bar.',
});
const HIDE_PERSONAL_INFO_WHILE_STREAMING_DESCRIPTOR = msg({
	message: 'Mask names, private details, invites, sounds, and alerts while sharing.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const STREAMING_DESCRIPTOR = msg({
	message: 'Screen sharing',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIDE_PERSONAL_INFO_DESCRIPTOR = msg({
	message: 'Private details',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INVITE_LINKS_DESCRIPTOR = msg({
	message: 'Invite masking',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISABLE_SOUNDS_DESCRIPTOR = msg({
	message: 'Mute sounds',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISABLE_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Pause alerts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_HOW_MESSAGES_ARE_DISPLAYED_IN_CHAT_CHANNELS_DESCRIPTOR = msg({
	message: 'Choose how messages are displayed in chat channels.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const COMPACT_DESCRIPTOR = msg({
	message: 'Compact',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COZY_DESCRIPTOR = msg({
	message: 'Cozy',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_STYLE_DESCRIPTOR = msg({
	message: 'Message style',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMFY_DESCRIPTOR = msg({
	message: 'Comfy',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DENSE_DESCRIPTOR = msg({
	message: 'Dense',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_LAYOUT_DESCRIPTOR = msg({
	message: 'Message layout',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISPLAY_MODE_DESCRIPTOR = msg({
	message: 'Display mode',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GROUPING_DESCRIPTOR = msg({
	message: 'Grouping',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPACING_DESCRIPTOR = msg({
	message: 'Spacing',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TIMESTAMPS_DESCRIPTOR = msg({
	message: 'Timestamps',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TIME_DESCRIPTOR = msg({
	message: 'Time',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DATE_DESCRIPTOR = msg({
	message: 'Date',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_TIME_DESCRIPTOR = msg({
	message: 'Message time',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AVATARS_DESCRIPTOR = msg({
	message: 'Avatars',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIDE_AVATARS_DESCRIPTOR = msg({
	message: 'Hide avatars',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMPACT_MODE_DESCRIPTOR = msg({
	message: 'Compact mode',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const USER_AVATARS_DESCRIPTOR = msg({
	message: 'User avatars',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PROFILE_PICTURES_DESCRIPTOR = msg({
	message: 'Profile pictures',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_GROUPS_DESCRIPTOR = msg({
	message: 'Message groups',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPACE_BETWEEN_MESSAGE_GROUPS_DESCRIPTOR = msg({
	message: 'Space between message groups',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const GAP_DESCRIPTOR = msg({
	message: 'Gap',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPACE_BETWEEN_DESCRIPTOR = msg({
	message: 'Space between',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_SPACING_DESCRIPTOR = msg({
	message: 'Message spacing',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOMIZE_INTERFACE_ELEMENTS_AND_BEHAVIORS_DESCRIPTOR = msg({
	message: 'Customize interface elements and behaviors.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const MEMBER_LIST_DESCRIPTOR = msg({
	message: 'Member list',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const USERS_DESCRIPTOR = msg({
	message: 'Users',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MEMBERS_DESCRIPTOR = msg({
	message: 'Members',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TYPING_INDICATOR_DESCRIPTOR = msg({
	message: 'Typing indicator',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TYPING_AVATARS_DESCRIPTOR = msg({
	message: 'Typing avatars',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WHO_IS_TYPING_DESCRIPTOR = msg({
	message: 'Who is typing',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HINTS_DESCRIPTOR = msg({
	message: 'Hints',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TOOLTIPS_DESCRIPTOR = msg({
	message: 'Tooltips',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHORTCUT_BADGES_DESCRIPTOR = msg({
	message: 'Shortcut badges',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const KEYBOARD_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Keyboard shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const JOIN_DESCRIPTOR = msg({
	message: 'Join',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DOUBLE_CLICK_DESCRIPTOR = msg({
	message: 'Double-click',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DOUBLE_CLICK_2_DESCRIPTOR = msg({
	message: 'Double-click',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SINGLE_CLICK_DESCRIPTOR = msg({
	message: 'Single click',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VOICE_JOIN_DESCRIPTOR = msg({
	message: 'Voice join',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIRM_VOICE_JOIN_DESCRIPTOR = msg({
	message: 'Confirm voice join',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VOICE_CHANNEL_CONFIRMATION_DESCRIPTOR = msg({
	message: 'Voice channel confirmation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TWO_CLICKS_DESCRIPTOR = msg({
	message: 'Two clicks',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_2_CLICKS_DESCRIPTOR = msg({
	message: '2 clicks',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTROL_UNREAD_INDICATOR_BEHAVIOR_FOR_MUTED_CHANNELS_IN_DESCRIPTOR = msg({
	message: 'Control unread indicator behavior for muted channels in channel lists.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const UNREAD_INDICATOR_DESCRIPTOR = msg({
	message: 'Unread indicator',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FADED_UNREAD_DESCRIPTOR = msg({
	message: 'Faded unread',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FAVORITES_2_DESCRIPTOR = msg({
	message: 'Favorites',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ACTIVE_NOW_DESCRIPTOR = msg({
	message: 'Active now',
	comment: 'Settings section label. Also used as a search synonym in the settings search bar.',
});
const CONTROL_HOW_ACTIVE_NOW_SURFACES_ACROSS_THE_APP_DESCRIPTOR = msg({
	message: 'Control how active now appears across the app.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const ACTIVITY_DESCRIPTOR = msg({
	message: 'Activity',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PRESENCE_DESCRIPTOR = msg({
	message: 'Presence',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VOICE_ACTIVITY_DESCRIPTOR = msg({
	message: 'Voice activity',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DM_SIDEBAR_DESCRIPTOR = msg({
	message: 'DM sidebar',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const JOIN_VOICE_DESCRIPTOR = msg({
	message: 'Join voice',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HOME_SCREEN_DESCRIPTOR = msg({
	message: 'Home screen',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THEME_DESCRIPTOR = msg({
	message: 'Theme',
	context: 'settings-section',
	comment: 'Appearance settings section for app themes and quick CSS.',
});
const MESSAGES_DESCRIPTOR = msg({
	message: 'Messages',
	context: 'appearance-settings-section',
	comment: 'Appearance settings section for message layout and display.',
});
const INTERFACE_DESCRIPTOR = msg({
	message: 'Interface',
	context: 'appearance-settings-section',
	comment: 'Appearance settings section for general app interface controls.',
});
export const appearanceSections = [
	{
		id: 'theme',
		tabType: 'appearance',
		label: THEME_DESCRIPTOR,
		description: CHOOSE_A_BASE_APPEARANCE_MANAGE_QUICK_CSS_ORGANIZE_DESCRIPTOR,
		keywords: [
			THEME_2_DESCRIPTOR,
			DARK_MODE_DESCRIPTOR,
			LIGHT_MODE_DESCRIPTOR,
			DARK_DESCRIPTOR,
			LIGHT_DESCRIPTOR,
			COAL_DESCRIPTOR,
			COLOR_SCHEME_DESCRIPTOR,
			SYSTEM_THEME_DESCRIPTOR,
			SYNC_DESCRIPTOR,
			SYNC_THEME_DESCRIPTOR,
			THEME_SYNC_DESCRIPTOR,
			DEVICES_DESCRIPTOR,
			CSS_DESCRIPTOR,
			QUICK_CSS_DESCRIPTOR,
			CUSTOM_DESCRIPTOR,
			THEME_LIBRARY_DESCRIPTOR,
			THEME_FILES_DESCRIPTOR,
			IMPORT_THEMES_DESCRIPTOR,
			EXPORT_THEMES_DESCRIPTOR,
			THEME_ASSETS_DESCRIPTOR,
			LOCAL_FILES_DESCRIPTOR,
			THEME_TOKENS_DESCRIPTOR,
			OVERRIDES_DESCRIPTOR,
			COLORS_DESCRIPTOR,
			FONTS_DESCRIPTOR,
			STYLING_DESCRIPTOR,
			LOOK_AND_FEEL_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'hdr',
		tabType: 'appearance',
		label: HIGH_DYNAMIC_RANGE_DESCRIPTOR,
		description: CONTROL_HOW_HDR_IMAGES_ARE_DISPLAYED_ON_HDR_DESCRIPTOR,
		keywords: [
			HDR_DESCRIPTOR,
			HIGH_DYNAMIC_RANGE_DESCRIPTOR,
			BRIGHTNESS_DESCRIPTOR,
			COLOR_DESCRIPTOR,
			DISPLAY_2_DESCRIPTOR,
			TONE_DESCRIPTOR,
		],
		isAdvanced: false,
		isVisible: shouldShowHdrSettings,
	},
	{
		id: 'app-zoom-level',
		tabType: 'appearance',
		label: APP_ZOOM_LEVEL_DESCRIPTOR,
		description: ADJUST_THE_APPLICATION_S_ZOOM_LEVEL_DESCRIPTOR,
		keywords: [
			ZOOM_DESCRIPTOR,
			SCALE_DESCRIPTOR,
			APP_ZOOM_DESCRIPTOR,
			INTERFACE_SIZE_DESCRIPTOR,
			DISPLAY_SCALING_DESCRIPTOR,
		],
		isAdvanced: false,
		isVisible: shouldShowAppZoomLevel,
	},
	{
		id: 'messages',
		tabType: 'appearance',
		label: MESSAGES_DESCRIPTOR,
		description: CHOOSE_HOW_MESSAGES_ARE_DISPLAYED_IN_CHAT_CHANNELS_DESCRIPTOR,
		keywords: [
			MESSAGES_3_DESCRIPTOR,
			DISPLAY_2_DESCRIPTOR,
			COMPACT_DESCRIPTOR,
			COZY_DESCRIPTOR,
			MESSAGE_STYLE_DESCRIPTOR,
			COMFY_DESCRIPTOR,
			DENSE_DESCRIPTOR,
			MESSAGE_LAYOUT_DESCRIPTOR,
			DISPLAY_MODE_DESCRIPTOR,
			GROUPING_DESCRIPTOR,
			SPACING_DESCRIPTOR,
			TIMESTAMPS_DESCRIPTOR,
			TIME_DESCRIPTOR,
			DATE_DESCRIPTOR,
			MESSAGE_TIME_DESCRIPTOR,
			AVATARS_DESCRIPTOR,
			HIDE_AVATARS_DESCRIPTOR,
			COMPACT_MODE_DESCRIPTOR,
			USER_AVATARS_DESCRIPTOR,
			PROFILE_PICTURES_DESCRIPTOR,
			MESSAGE_GROUPS_DESCRIPTOR,
			SPACE_BETWEEN_MESSAGE_GROUPS_DESCRIPTOR,
			GAP_DESCRIPTOR,
			SPACE_BETWEEN_DESCRIPTOR,
			MESSAGE_SPACING_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'chat-font-scaling',
		tabType: 'appearance',
		label: CHAT_FONT_SCALING_DESCRIPTOR,
		description: ADJUST_THE_FONT_SIZE_IN_THE_CHAT_AREA_DESCRIPTOR,
		keywords: [
			FONT_DESCRIPTOR,
			SIZE_DESCRIPTOR,
			TEXT_SIZE_DESCRIPTOR,
			ZOOM_DESCRIPTOR,
			SCALE_DESCRIPTOR,
			CHAT_FONT_DESCRIPTOR,
			CHAT_FONT_SCALING_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'interface',
		tabType: 'appearance',
		label: INTERFACE_DESCRIPTOR,
		description: CUSTOMIZE_INTERFACE_ELEMENTS_AND_BEHAVIORS_DESCRIPTOR,
		keywords: [
			SIDEBAR_2_DESCRIPTOR,
			CHANNEL_LIST_DESCRIPTOR,
			NAVIGATION_DESCRIPTOR,
			MEMBER_LIST_DESCRIPTOR,
			USERS_DESCRIPTOR,
			MEMBERS_DESCRIPTOR,
			TYPING_DESCRIPTOR,
			TYPING_INDICATOR_DESCRIPTOR,
			TYPING_AVATARS_DESCRIPTOR,
			INDICATOR_DESCRIPTOR,
			WHO_IS_TYPING_DESCRIPTOR,
			KEYBOARD_2_DESCRIPTOR,
			HINTS_DESCRIPTOR,
			TOOLTIPS_DESCRIPTOR,
			SHORTCUT_BADGES_DESCRIPTOR,
			KEYBOARD_SHORTCUTS_DESCRIPTOR,
			SHOW_NEKO_DESCRIPTOR,
			NEKO_DESCRIPTOR,
			CHAT_INPUT_DESCRIPTOR,
			VOICE_CHANNEL_DESCRIPTOR,
			JOIN_DESCRIPTOR,
			DOUBLE_CLICK_DESCRIPTOR,
			DOUBLE_CLICK_2_DESCRIPTOR,
			SINGLE_CLICK_DESCRIPTOR,
			VOICE_JOIN_DESCRIPTOR,
			CONFIRM_VOICE_JOIN_DESCRIPTOR,
			VOICE_CHANNEL_CONFIRMATION_DESCRIPTOR,
			TWO_CLICKS_DESCRIPTOR,
			MESSAGE_2_CLICKS_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'channel-list',
		tabType: 'appearance',
		label: CHANNEL_LIST_DESCRIPTOR,
		description: CONTROL_UNREAD_INDICATOR_BEHAVIOR_FOR_MUTED_CHANNELS_IN_DESCRIPTOR,
		keywords: [
			CHANNEL_LIST_DESCRIPTOR,
			MUTED_CHANNELS_DESCRIPTOR,
			MUTED_DESCRIPTOR,
			UNREAD_DESCRIPTOR,
			UNREAD_INDICATOR_DESCRIPTOR,
			FADED_UNREAD_DESCRIPTOR,
			SIDEBAR_2_DESCRIPTOR,
			FAVORITES_2_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'active-now',
		tabType: 'appearance',
		label: ACTIVE_NOW_DESCRIPTOR,
		description: CONTROL_HOW_ACTIVE_NOW_SURFACES_ACROSS_THE_APP_DESCRIPTOR,
		keywords: [
			ACTIVE_NOW_DESCRIPTOR,
			ACTIVITY_DESCRIPTOR,
			PRESENCE_DESCRIPTOR,
			ONLINE_DESCRIPTOR,
			VOICE_ACTIVITY_DESCRIPTOR,
			VOICE_DESCRIPTOR,
			DM_SIDEBAR_DESCRIPTOR,
			DIRECT_MESSAGE_DESCRIPTOR,
			FRIENDS_DESCRIPTOR,
			JOIN_VOICE_DESCRIPTOR,
			HOME_SCREEN_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'streamer-mode',
		tabType: 'appearance',
		label: STREAMER_MODE_DESCRIPTOR,
		description: HIDE_PERSONAL_INFO_WHILE_STREAMING_DESCRIPTOR,
		keywords: [
			STREAMER_MODE_DESCRIPTOR,
			STREAMING_DESCRIPTOR,
			'OBS',
			'XSplit',
			HIDE_PERSONAL_INFO_DESCRIPTOR,
			INVITE_LINKS_DESCRIPTOR,
			DISABLE_SOUNDS_DESCRIPTOR,
			DISABLE_NOTIFICATIONS_DESCRIPTOR,
		],
		isAdvanced: false,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
