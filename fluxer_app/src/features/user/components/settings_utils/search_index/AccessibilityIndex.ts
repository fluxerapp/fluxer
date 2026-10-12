// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {SearchableSettingDescriptor} from '@app/features/user/components/settings_utils/search_index/SearchIndexTypes';
import {
	ANIMATIONS_DESCRIPTOR,
	AUTOSCROLLING_DESCRIPTOR,
	CALL_CONFIRMATION_DESCRIPTOR,
	CONFIRM_CALLS_DESCRIPTOR,
	CONTEXT_MENU_DESCRIPTOR,
	CONTEXT_MENU_SHORTCUTS_DESCRIPTOR,
	COPY_TEXT_DESCRIPTOR,
	DELETED_TEXT_DESCRIPTOR,
	DIM_STRIKETHROUGH_TEXT_DESCRIPTOR,
	ENABLE_TEXT_SELECTION_DESCRIPTOR,
	ESCAPE_DESCRIPTOR,
	ESCAPE_KEY_DESCRIPTOR,
	FOCUS_RING_DESCRIPTOR,
	GRAY_TEXT_DESCRIPTOR,
	INBOX_PREVIEW_DESCRIPTOR,
	KEYBOARD_MODE_DESCRIPTOR,
	LINK_UNDERLINE_DESCRIPTOR,
	LIVE_REGION_DESCRIPTOR,
	MARKDOWN_DESCRIPTOR,
	MESSAGE_INPUT_DESCRIPTOR,
	MESSAGE_PREVIEW_DESCRIPTOR,
	MESSAGE_PREVIEWS_DESCRIPTOR,
	MIDDLE_CLICK_SCROLLING_DESCRIPTOR,
	MOTION_DESCRIPTOR,
	PREFERS_REDUCED_MOTION_DESCRIPTOR,
	READ_MESSAGES_DESCRIPTOR,
	RIGHT_CLICK_MENU_DESCRIPTOR,
	RING_DESCRIPTOR,
	SCROLL_ANIMATION_DESCRIPTOR,
	SCROLLING_DESCRIPTOR,
	SELECT_TEXT_DESCRIPTOR,
	SHOW_STRIKETHROUGH_MARKDOWN_TEXT_IN_A_SLIGHTLY_MUTED_COLOR_DESCRIPTOR,
	SMOOTH_SCROLLING_DESCRIPTOR,
	START_CALL_DESCRIPTOR,
	STRIKE_THROUGH_DESCRIPTOR,
	STRIKETHROUGH_DESCRIPTOR,
	SYNC_MOTION_DESCRIPTOR,
	SYNC_REDUCED_MOTION_DESCRIPTOR,
	SYSTEM_MOTION_DESCRIPTOR,
	SYSTEM_REDUCED_MOTION_DESCRIPTOR,
	TEXT_SELECTION_DESCRIPTOR,
	TEXTAREA_DESCRIPTOR,
	TEXTAREA_FOCUS_RING_DESCRIPTOR,
	TOOLTIPS_DESCRIPTOR,
	UNDERLINE_DESCRIPTOR,
	UNDERLINE_LINKS_DESCRIPTOR,
	VIDEO_CALL_DESCRIPTOR,
	VIDEO_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {
	ALWAYS_UNDERLINE_LINKS_DESCRIPTOR,
	ANNOUNCE_NEW_MESSAGES_DESCRIPTOR,
	CONFIRM_BEFORE_STARTING_CALLS_DESCRIPTOR,
	ESCAPE_KEY_EXITS_KEYBOARD_MODE_DESCRIPTOR,
	KEEP_ANIMATIONS_GIF_PLAYBACK_HOVER_EFFECTS_AND_TOOLTIPS_DESCRIPTOR,
	SHOW_CONTEXT_MENU_SHORTCUTS_DESCRIPTOR,
	SHOW_FOCUS_RING_ON_CHAT_TEXTAREA_DESCRIPTOR,
	STAY_FULLY_INTERACTIVE_WHEN_UNFOCUSED_DESCRIPTOR,
	SYNC_REDUCED_MOTION_SETTING_WITH_SYSTEM_DESCRIPTOR,
	USE_AUTOSCROLLING_DESCRIPTOR,
	VIDEO_SEEK_THUMBNAILS_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {VOICE_CALL_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';

const SILENT_CALL_DESCRIPTOR = msg({
	message: 'Silent call',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHIFT_DESCRIPTOR = msg({
	message: 'Shift',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_WHETHER_STARTING_A_CALL_ASKS_FOR_CONFIRMATION_DESCRIPTOR = msg({
	message: 'Ask before starting calls',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const HYPERLINK_DESCRIPTOR = msg({
	message: 'Hyperlink',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINK_STYLING_DESCRIPTOR = msg({
	message: 'Link styling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MAKE_LINKS_TO_WEBSITES_STAND_OUT_BY_ALWAYS_DESCRIPTOR = msg({
	message: 'Underline website links',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const SELECTABLE_TEXT_DESCRIPTOR = msg({
	message: 'Selectable text',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ENABLE_SELECTION_DESCRIPTOR = msg({
	message: 'Enable selection',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ALLOW_SELECTING_TEXT_CONTENT_IN_THE_APP_DESCRIPTOR = msg({
	message: 'Allow selecting text in the app',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const VIDEO_SCRUBBING_DESCRIPTOR = msg({
	message: 'Video scrubbing',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SEEK_PREVIEW_DESCRIPTOR = msg({
	message: 'Seek preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCRUB_PREVIEW_DESCRIPTOR = msg({
	message: 'Scrub preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const THUMBNAIL_PREVIEW_DESCRIPTOR = msg({
	message: 'Thumbnail preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CHOOSE_WHETHER_VIDEO_SCRUBBING_SHOWS_A_THUMBNAIL_DESCRIPTOR = msg({
	message: 'Thumbnail or live frame while scrubbing video',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const DM_MESSAGE_PREVIEWS_DESCRIPTOR = msg({
	message: 'DM message previews',
	comment: 'Settings search entry label. Names the settings search entry in the settings UI.',
});
const DIRECT_MESSAGE_PREVIEW_DESCRIPTOR = msg({
	message: 'Direct message preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DIRECT_MESSAGE_PREVIEWS_DESCRIPTOR = msg({
	message: 'Direct message previews',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DIRECT_MESSAGE_INBOX_PREVIEW_DESCRIPTOR = msg({
	message: 'Direct message inbox preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const CONTROL_WHEN_MESSAGE_PREVIEWS_ARE_SHOWN_IN_THE_DESCRIPTOR = msg({
	message: 'Message previews in the DM list',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const SCREEN_READER_ANNOUNCEMENTS_DESCRIPTOR = msg({
	message: 'Screen reader announcements',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const ACCESSIBLE_LIVE_REGION_DESCRIPTOR = msg({
	message: 'Accessible live region',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ASSISTIVE_TECHNOLOGY_DESCRIPTOR = msg({
	message: 'Assistive technology',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LET_SCREEN_READERS_ANNOUNCE_NEW_MESSAGES_AS_THEY_DESCRIPTOR = msg({
	message: 'Let screen readers announce new messages as they arrive in the open channel',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const EXIT_KEYBOARD_MODE_DESCRIPTOR = msg({
	message: 'Exit keyboard mode',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ALLOW_PRESSING_ESCAPE_TO_EXIT_KEYBOARD_NAVIGATION_MODE_DESCRIPTOR = msg({
	message: 'Allow pressing Escape to exit keyboard navigation mode',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const MENU_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Menu shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISPLAY_KEYBOARD_SHORTCUT_INDICATORS_NEXT_TO_CONTEXT_MENU_DESCRIPTOR = msg({
	message: 'Display keyboard shortcut indicators next to context menu items',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const COMPOSER_FOCUS_DESCRIPTOR = msg({
	message: 'Composer focus',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISPLAY_A_VISIBLE_FOCUS_INDICATOR_AROUND_THE_MESSAGE_DESCRIPTOR = msg({
	message: 'Display a visible focus indicator around the message input when focused',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const AUTOMATICALLY_USE_YOUR_SYSTEM_S_REDUCED_MOTION_PREFERENCE_DESCRIPTOR = msg({
	message: "Use this device's system reduced motion preference",
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const MIDDLE_MOUSE_SCROLLING_DESCRIPTOR = msg({
	message: 'Middle mouse scrolling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const CONTROL_NATIVE_SMOOTH_WHEEL_AND_KEYBOARD_SCROLLING_DESCRIPTOR = msg({
	message: 'Control native smooth wheel and keyboard scrolling in the desktop app',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});
const CONTROL_MIDDLE_CLICK_AUTOSCROLLING_DESCRIPTOR = msg({
	message: 'Control Chromium middle-click autoscrolling in the desktop app',
	comment: 'Settings search entry description. One-line summary of what the settings search entry controls.',
});

const UNFOCUSED_DESCRIPTOR = msg({
	message: 'Unfocused',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WINDOW_FOCUS_DESCRIPTOR = msg({
	message: 'Window focus',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATIONS_WHILE_UNFOCUSED_DESCRIPTOR = msg({
	message: 'Animations while unfocused',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATION_PLAYBACK_DESCRIPTOR = msg({
	message: 'Animation playback',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HOVER_EFFECTS_DESCRIPTOR = msg({
	message: 'Hover effects',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

const POWER_SAVING_DESCRIPTOR = msg({
	message: 'Power saving',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});

export const accessibilityIndex: Array<SearchableSettingDescriptor> = [
	{
		id: 'accessibility-confirm-calls',
		tabType: 'accessibility',
		sectionId: 'keyboard',
		label: CONFIRM_BEFORE_STARTING_CALLS_DESCRIPTOR,
		keywords: [
			CALL_CONFIRMATION_DESCRIPTOR,
			CONFIRM_CALLS_DESCRIPTOR,
			START_CALL_DESCRIPTOR,
			VOICE_CALL_DESCRIPTOR,
			VIDEO_CALL_DESCRIPTOR,
			RING_DESCRIPTOR,
			SILENT_CALL_DESCRIPTOR,
			SHIFT_DESCRIPTOR,
		],
		description: CHOOSE_WHETHER_STARTING_A_CALL_ASKS_FOR_CONFIRMATION_DESCRIPTOR,
	},
	{
		id: 'accessibility-underline-links',
		tabType: 'accessibility',
		sectionId: 'visual',
		label: ALWAYS_UNDERLINE_LINKS_DESCRIPTOR,
		keywords: [
			UNDERLINE_DESCRIPTOR,
			UNDERLINE_LINKS_DESCRIPTOR,
			LINK_UNDERLINE_DESCRIPTOR,
			HYPERLINK_DESCRIPTOR,
			LINK_STYLING_DESCRIPTOR,
		],
		description: MAKE_LINKS_TO_WEBSITES_STAND_OUT_BY_ALWAYS_DESCRIPTOR,
	},
	{
		id: 'accessibility-dim-strikethrough-text',
		tabType: 'accessibility',
		sectionId: 'visual',
		label: DIM_STRIKETHROUGH_TEXT_DESCRIPTOR,
		keywords: [
			STRIKETHROUGH_DESCRIPTOR,
			STRIKE_THROUGH_DESCRIPTOR,
			DELETED_TEXT_DESCRIPTOR,
			GRAY_TEXT_DESCRIPTOR,
			MARKDOWN_DESCRIPTOR,
		],
		description: SHOW_STRIKETHROUGH_MARKDOWN_TEXT_IN_A_SLIGHTLY_MUTED_COLOR_DESCRIPTOR,
	},
	{
		id: 'accessibility-text-selection',
		tabType: 'accessibility',
		sectionId: 'visual',
		label: ENABLE_TEXT_SELECTION_DESCRIPTOR,
		keywords: [
			TEXT_SELECTION_DESCRIPTOR,
			SELECT_TEXT_DESCRIPTOR,
			COPY_TEXT_DESCRIPTOR,
			SELECTABLE_TEXT_DESCRIPTOR,
			ENABLE_SELECTION_DESCRIPTOR,
		],
		description: ALLOW_SELECTING_TEXT_CONTENT_IN_THE_APP_DESCRIPTOR,
		audience: 'advanced',
		tags: ['accessibility'],
	},
	{
		id: 'accessibility-video-seek-thumbnails',
		tabType: 'accessibility',
		sectionId: 'visual',
		label: VIDEO_SEEK_THUMBNAILS_DESCRIPTOR,
		keywords: [
			VIDEO_DESCRIPTOR,
			VIDEO_SCRUBBING_DESCRIPTOR,
			SEEK_PREVIEW_DESCRIPTOR,
			SCRUB_PREVIEW_DESCRIPTOR,
			THUMBNAIL_PREVIEW_DESCRIPTOR,
		],
		description: CHOOSE_WHETHER_VIDEO_SCRUBBING_SHOWS_A_THUMBNAIL_DESCRIPTOR,
		audience: 'advanced',
		tags: ['accessibility', 'media'],
		addedAt: '2026-06-03T00:00:00.000Z',
	},
	{
		id: 'accessibility-dm-message-previews',
		tabType: 'accessibility',
		sectionId: 'visual',
		label: DM_MESSAGE_PREVIEWS_DESCRIPTOR,
		keywords: [
			DIRECT_MESSAGE_PREVIEW_DESCRIPTOR,
			DIRECT_MESSAGE_PREVIEWS_DESCRIPTOR,
			DIRECT_MESSAGE_INBOX_PREVIEW_DESCRIPTOR,
			MESSAGE_PREVIEW_DESCRIPTOR,
			MESSAGE_PREVIEWS_DESCRIPTOR,
			INBOX_PREVIEW_DESCRIPTOR,
		],
		description: CONTROL_WHEN_MESSAGE_PREVIEWS_ARE_SHOWN_IN_THE_DESCRIPTOR,
	},
	{
		id: 'accessibility-announce-new-messages',
		tabType: 'accessibility',
		sectionId: 'screen-reader',
		label: ANNOUNCE_NEW_MESSAGES_DESCRIPTOR,
		keywords: [
			ANNOUNCE_NEW_MESSAGES_DESCRIPTOR,
			SCREEN_READER_ANNOUNCEMENTS_DESCRIPTOR,
			READ_MESSAGES_DESCRIPTOR,
			LIVE_REGION_DESCRIPTOR,
			ACCESSIBLE_LIVE_REGION_DESCRIPTOR,
			ASSISTIVE_TECHNOLOGY_DESCRIPTOR,
		],
		description: LET_SCREEN_READERS_ANNOUNCE_NEW_MESSAGES_AS_THEY_DESCRIPTOR,
	},
	{
		id: 'accessibility-escape-keyboard-mode',
		tabType: 'accessibility',
		sectionId: 'keyboard',
		label: ESCAPE_KEY_EXITS_KEYBOARD_MODE_DESCRIPTOR,
		keywords: [ESCAPE_DESCRIPTOR, ESCAPE_KEY_DESCRIPTOR, KEYBOARD_MODE_DESCRIPTOR, EXIT_KEYBOARD_MODE_DESCRIPTOR],
		description: ALLOW_PRESSING_ESCAPE_TO_EXIT_KEYBOARD_NAVIGATION_MODE_DESCRIPTOR,
	},
	{
		id: 'accessibility-context-menu-shortcuts',
		tabType: 'accessibility',
		sectionId: 'keyboard',
		label: SHOW_CONTEXT_MENU_SHORTCUTS_DESCRIPTOR,
		keywords: [
			CONTEXT_MENU_DESCRIPTOR,
			CONTEXT_MENU_SHORTCUTS_DESCRIPTOR,
			RIGHT_CLICK_MENU_DESCRIPTOR,
			MENU_SHORTCUTS_DESCRIPTOR,
		],
		description: DISPLAY_KEYBOARD_SHORTCUT_INDICATORS_NEXT_TO_CONTEXT_MENU_DESCRIPTOR,
	},
	{
		id: 'accessibility-textarea-focus-ring',
		tabType: 'accessibility',
		sectionId: 'keyboard',
		label: SHOW_FOCUS_RING_ON_CHAT_TEXTAREA_DESCRIPTOR,
		keywords: [
			FOCUS_RING_DESCRIPTOR,
			TEXTAREA_DESCRIPTOR,
			TEXTAREA_FOCUS_RING_DESCRIPTOR,
			MESSAGE_INPUT_DESCRIPTOR,
			COMPOSER_FOCUS_DESCRIPTOR,
		],
		description: DISPLAY_A_VISIBLE_FOCUS_INDICATOR_AROUND_THE_MESSAGE_DESCRIPTOR,
	},
	{
		id: 'accessibility-stay-interactive-unfocused',
		tabType: 'accessibility',
		sectionId: 'motion',
		label: STAY_FULLY_INTERACTIVE_WHEN_UNFOCUSED_DESCRIPTOR,
		keywords: [
			UNFOCUSED_DESCRIPTOR,
			WINDOW_FOCUS_DESCRIPTOR,
			ANIMATIONS_WHILE_UNFOCUSED_DESCRIPTOR,
			ANIMATION_PLAYBACK_DESCRIPTOR,
			HOVER_EFFECTS_DESCRIPTOR,
			TOOLTIPS_DESCRIPTOR,
			POWER_SAVING_DESCRIPTOR,
		],
		description: KEEP_ANIMATIONS_GIF_PLAYBACK_HOVER_EFFECTS_AND_TOOLTIPS_DESCRIPTOR,
		audience: 'advanced',
		tags: ['accessibility'],
	},
	{
		id: 'accessibility-sync-reduced-motion',
		tabType: 'accessibility',
		sectionId: 'motion',
		label: SYNC_REDUCED_MOTION_SETTING_WITH_SYSTEM_DESCRIPTOR,
		keywords: [
			SYNC_MOTION_DESCRIPTOR,
			SYNC_REDUCED_MOTION_DESCRIPTOR,
			SYSTEM_REDUCED_MOTION_DESCRIPTOR,
			SYSTEM_MOTION_DESCRIPTOR,
			PREFERS_REDUCED_MOTION_DESCRIPTOR,
		],
		description: AUTOMATICALLY_USE_YOUR_SYSTEM_S_REDUCED_MOTION_PREFERENCE_DESCRIPTOR,
	},
	{
		id: 'accessibility-smooth-scrolling',
		tabType: 'accessibility',
		sectionId: 'motion',
		label: SMOOTH_SCROLLING_DESCRIPTOR,
		keywords: [SCROLLING_DESCRIPTOR, SCROLL_ANIMATION_DESCRIPTOR, MOTION_DESCRIPTOR, ANIMATIONS_DESCRIPTOR],
		description: CONTROL_NATIVE_SMOOTH_WHEEL_AND_KEYBOARD_SCROLLING_DESCRIPTOR,
		audience: 'advanced',
		tags: ['accessibility', 'desktop'],
		isVisible: isDesktop,
	},
	{
		id: 'accessibility-middle-click-autoscroll',
		tabType: 'accessibility',
		sectionId: 'motion',
		label: USE_AUTOSCROLLING_DESCRIPTOR,
		keywords: [
			AUTOSCROLLING_DESCRIPTOR,
			MIDDLE_CLICK_SCROLLING_DESCRIPTOR,
			MIDDLE_MOUSE_SCROLLING_DESCRIPTOR,
			SCROLLING_DESCRIPTOR,
		],
		description: CONTROL_MIDDLE_CLICK_AUTOSCROLLING_DESCRIPTOR,
		audience: 'advanced',
		tags: ['accessibility', 'desktop'],
		isVisible: () => isDesktop() && getElectronAPI()?.platform === 'linux',
	},
];
