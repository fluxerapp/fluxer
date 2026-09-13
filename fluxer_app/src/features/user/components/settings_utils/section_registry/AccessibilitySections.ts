// SPDX-License-Identifier: AGPL-3.0-or-later

import {STICKERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	ACCESSIBILITY_DESCRIPTOR,
	CALL_DESCRIPTOR,
	COLOR_DESCRIPTOR,
	COLORS_DESCRIPTOR,
	CONTEXT_MENU_DESCRIPTOR,
	DELETED_TEXT_DESCRIPTOR,
	DIM_STRIKETHROUGH_TEXT_DESCRIPTOR,
	EMOJI_DESCRIPTOR,
	GIF_DESCRIPTOR,
	GRAY_TEXT_DESCRIPTOR,
	KEYBOARD_2_DESCRIPTOR,
	MARKDOWN_DESCRIPTOR,
	NARRATION_DESCRIPTOR,
	NAVIGATION_DESCRIPTOR,
	READ_ALOUD_DESCRIPTOR,
	READ_MESSAGES_DESCRIPTOR,
	SPEECH_DESCRIPTOR,
	STRIKE_THROUGH_DESCRIPTOR,
	STRIKETHROUGH_DESCRIPTOR,
	TEXT_TO_SPEECH_3_DESCRIPTOR,
	TTS_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {VOICE_CALL_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';

const CONTRAST_DESCRIPTOR = msg({
	message: 'Contrast',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const HIGH_CONTRAST_DESCRIPTOR = msg({
	message: 'High contrast',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VISIBILITY_DESCRIPTOR = msg({
	message: 'Visibility',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SATURATION_DESCRIPTOR = msg({
	message: 'Saturation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIBRANCY_DESCRIPTOR = msg({
	message: 'Vibrancy',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCREEN_READER_2_DESCRIPTOR = msg({
	message: 'Screen reader',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const A11Y_DESCRIPTOR = msg({
	message: 'A11y',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ARIA_DESCRIPTOR = msg({
	message: 'ARIA',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const UNDERLINE_DESCRIPTOR = msg({
	message: 'Underline',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const UNDERLINE_LINKS_DESCRIPTOR = msg({
	message: 'Underline links',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LINK_UNDERLINE_DESCRIPTOR = msg({
	message: 'Link underline',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TEXT_SELECTION_DESCRIPTOR = msg({
	message: 'Text selection',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SELECT_TEXT_DESCRIPTOR = msg({
	message: 'Select text',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COPY_TEXT_DESCRIPTOR = msg({
	message: 'Copy text',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ENABLE_TEXT_SELECTION_DESCRIPTOR = msg({
	message: 'Enable text selection',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DM_PREVIEW_DESCRIPTOR = msg({
	message: 'DM preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DM_PREVIEWS_DESCRIPTOR = msg({
	message: 'DM previews',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_PREVIEW_DESCRIPTOR = msg({
	message: 'Message preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_PREVIEWS_DESCRIPTOR = msg({
	message: 'Message previews',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DM_MESSAGE_PREVIEW_DESCRIPTOR = msg({
	message: 'DM message preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INBOX_PREVIEW_DESCRIPTOR = msg({
	message: 'Inbox preview',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTROL_WHICH_CHAT_UPDATES_ARE_ANNOUNCED_BY_ASSISTIVE_DESCRIPTOR = msg({
	message: 'Control which chat updates are announced by assistive technology.',
	comment: 'Settings section description. One-line summary of what the settings section controls.',
});
const ANNOUNCE_DESCRIPTOR = msg({
	message: 'Announce',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANNOUNCEMENTS_DESCRIPTOR = msg({
	message: 'Announcements',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const LIVE_REGION_DESCRIPTOR = msg({
	message: 'Live region',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTO_READ_DESCRIPTOR = msg({
	message: 'Auto read',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOREAD_DESCRIPTOR = msg({
	message: 'Autoread',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOMATIC_MESSAGE_READING_DESCRIPTOR = msg({
	message: 'Automatic message reading',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_ANNOUNCEMENTS_DESCRIPTOR = msg({
	message: 'Message announcements',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NEW_MESSAGES_DESCRIPTOR = msg({
	message: 'New messages',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPEECH_RATE_DESCRIPTOR = msg({
	message: 'Speech rate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PLAYBACK_SPEED_DESCRIPTOR = msg({
	message: 'Playback speed',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TTS_SPEED_DESCRIPTOR = msg({
	message: 'TTS speed',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const READING_SPEED_DESCRIPTOR = msg({
	message: 'Reading speed',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPEED_DESCRIPTOR = msg({
	message: 'Speed',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RATE_DESCRIPTOR = msg({
	message: 'Rate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SHORTCUTS_DESCRIPTOR = msg({
	message: 'Shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FOCUS_DESCRIPTOR = msg({
	message: 'Focus',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const FOCUS_RING_DESCRIPTOR = msg({
	message: 'Focus ring',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TAB_DESCRIPTOR = msg({
	message: 'Tab',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CALLS_DESCRIPTOR = msg({
	message: 'Calls',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const START_CALL_DESCRIPTOR = msg({
	message: 'Start call',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VIDEO_CALL_DESCRIPTOR = msg({
	message: 'Video call',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONFIRM_CALLS_DESCRIPTOR = msg({
	message: 'Confirm calls',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CALL_CONFIRMATION_DESCRIPTOR = msg({
	message: 'Call confirmation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ESCAPE_DESCRIPTOR = msg({
	message: 'Escape',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ESCAPE_KEY_DESCRIPTOR = msg({
	message: 'Escape key',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ESC_DESCRIPTOR = msg({
	message: 'Esc',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const KEYBOARD_MODE_DESCRIPTOR = msg({
	message: 'Keyboard mode',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CONTEXT_MENU_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Context menu shortcuts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RIGHT_CLICK_MENU_DESCRIPTOR = msg({
	message: 'Right-click menu',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TEXTAREA_DESCRIPTOR = msg({
	message: 'Text area',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_INPUT_DESCRIPTOR = msg({
	message: 'Message input',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const COMPOSER_DESCRIPTOR = msg({
	message: 'Composer',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TEXTAREA_FOCUS_RING_DESCRIPTOR = msg({
	message: 'Text area focus ring',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATION_2_DESCRIPTOR = msg({
	message: 'Animation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATED_STICKERS_DESCRIPTOR = msg({
	message: 'Animated stickers',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATED_EMOJI_DESCRIPTOR = msg({
	message: 'Animated emoji',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOPLAY_DESCRIPTOR = msg({
	message: 'Autoplay',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTO_PLAY_DESCRIPTOR = msg({
	message: 'Autoplay',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MOTION_2_DESCRIPTOR = msg({
	message: 'Motion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REDUCED_MOTION_DESCRIPTOR = msg({
	message: 'Reduced motion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ANIMATIONS_DESCRIPTOR = msg({
	message: 'Animations',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYNC_MOTION_DESCRIPTOR = msg({
	message: 'Sync motion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYNC_REDUCED_MOTION_DESCRIPTOR = msg({
	message: 'Sync reduced motion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYSTEM_MOTION_DESCRIPTOR = msg({
	message: 'System motion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SYSTEM_REDUCED_MOTION_DESCRIPTOR = msg({
	message: 'System reduced motion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PREFERS_REDUCED_MOTION_DESCRIPTOR = msg({
	message: 'Prefers reduced motion',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SMOOTH_SCROLLING_DESCRIPTOR = msg({
	message: 'Smooth scrolling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOSCROLLING_DESCRIPTOR = msg({
	message: 'Autoscrolling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MIDDLE_CLICK_SCROLLING_DESCRIPTOR = msg({
	message: 'Middle-click scrolling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCROLL_ANIMATION_DESCRIPTOR = msg({
	message: 'Scroll animation',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCROLLING_DESCRIPTOR = msg({
	message: 'Scrolling',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const VISUAL_DESCRIPTOR = msg({
	message: 'Visual',
	context: 'accessibility-settings-section',
	comment: 'Accessibility settings section for visual preferences.',
});
const SCREEN_READER_DESCRIPTOR = msg({
	message: 'Screen reader',
	context: 'accessibility-settings-section',
	comment: 'Accessibility settings section for screen-reader behavior.',
});
const TEXT_TO_SPEECH_DESCRIPTOR = msg({
	message: 'Text-to-speech',
	context: 'accessibility-settings-section',
	comment: 'Accessibility settings section for reading messages aloud.',
});
const KEYBOARD_DESCRIPTOR = msg({
	message: 'Keyboard',
	context: 'accessibility-settings-section',
	comment: 'Accessibility settings section for keyboard navigation preferences.',
});
const ANIMATION_DESCRIPTOR = msg({
	message: 'Animation',
	context: 'accessibility-settings-section',
	comment: 'Accessibility settings section for animation preferences.',
});
const MOTION_DESCRIPTOR = msg({
	message: 'Motion',
	context: 'accessibility-settings-section',
	comment: 'Accessibility settings section for reduced-motion preferences.',
});
export const accessibilitySections = [
	{
		id: 'visual',
		tabType: 'accessibility',
		label: VISUAL_DESCRIPTOR,
		keywords: [
			CONTRAST_DESCRIPTOR,
			HIGH_CONTRAST_DESCRIPTOR,
			VISIBILITY_DESCRIPTOR,
			ACCESSIBILITY_DESCRIPTOR,
			SATURATION_DESCRIPTOR,
			COLOR_DESCRIPTOR,
			COLORS_DESCRIPTOR,
			VIBRANCY_DESCRIPTOR,
			SCREEN_READER_2_DESCRIPTOR,
			A11Y_DESCRIPTOR,
			ARIA_DESCRIPTOR,
			UNDERLINE_DESCRIPTOR,
			UNDERLINE_LINKS_DESCRIPTOR,
			LINK_UNDERLINE_DESCRIPTOR,
			DIM_STRIKETHROUGH_TEXT_DESCRIPTOR,
			STRIKETHROUGH_DESCRIPTOR,
			STRIKE_THROUGH_DESCRIPTOR,
			DELETED_TEXT_DESCRIPTOR,
			GRAY_TEXT_DESCRIPTOR,
			MARKDOWN_DESCRIPTOR,
			TEXT_SELECTION_DESCRIPTOR,
			SELECT_TEXT_DESCRIPTOR,
			COPY_TEXT_DESCRIPTOR,
			ENABLE_TEXT_SELECTION_DESCRIPTOR,
			DM_PREVIEW_DESCRIPTOR,
			DM_PREVIEWS_DESCRIPTOR,
			MESSAGE_PREVIEW_DESCRIPTOR,
			MESSAGE_PREVIEWS_DESCRIPTOR,
			DM_MESSAGE_PREVIEW_DESCRIPTOR,
			INBOX_PREVIEW_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'screen-reader',
		tabType: 'accessibility',
		label: SCREEN_READER_DESCRIPTOR,
		description: CONTROL_WHICH_CHAT_UPDATES_ARE_ANNOUNCED_BY_ASSISTIVE_DESCRIPTOR,
		keywords: [
			SCREEN_READER_2_DESCRIPTOR,
			A11Y_DESCRIPTOR,
			ACCESSIBILITY_DESCRIPTOR,
			ANNOUNCE_DESCRIPTOR,
			ANNOUNCEMENTS_DESCRIPTOR,
			LIVE_REGION_DESCRIPTOR,
			'ARIA live',
			READ_MESSAGES_DESCRIPTOR,
			READ_ALOUD_DESCRIPTOR,
			AUTO_READ_DESCRIPTOR,
			AUTOREAD_DESCRIPTOR,
			AUTOMATIC_MESSAGE_READING_DESCRIPTOR,
			MESSAGE_ANNOUNCEMENTS_DESCRIPTOR,
			NEW_MESSAGES_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'tts',
		tabType: 'accessibility',
		label: TEXT_TO_SPEECH_DESCRIPTOR,
		keywords: [
			TTS_DESCRIPTOR,
			TEXT_TO_SPEECH_3_DESCRIPTOR,
			SPEECH_DESCRIPTOR,
			NARRATION_DESCRIPTOR,
			READ_ALOUD_DESCRIPTOR,
			SPEECH_RATE_DESCRIPTOR,
			PLAYBACK_SPEED_DESCRIPTOR,
			TTS_SPEED_DESCRIPTOR,
			READING_SPEED_DESCRIPTOR,
			SPEED_DESCRIPTOR,
			RATE_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'keyboard',
		tabType: 'accessibility',
		label: KEYBOARD_DESCRIPTOR,
		keywords: [
			KEYBOARD_2_DESCRIPTOR,
			NAVIGATION_DESCRIPTOR,
			SHORTCUTS_DESCRIPTOR,
			FOCUS_DESCRIPTOR,
			FOCUS_RING_DESCRIPTOR,
			TAB_DESCRIPTOR,
			CALL_DESCRIPTOR,
			CALLS_DESCRIPTOR,
			START_CALL_DESCRIPTOR,
			VOICE_CALL_DESCRIPTOR,
			VIDEO_CALL_DESCRIPTOR,
			CONFIRM_CALLS_DESCRIPTOR,
			CALL_CONFIRMATION_DESCRIPTOR,
			ESCAPE_DESCRIPTOR,
			ESCAPE_KEY_DESCRIPTOR,
			ESC_DESCRIPTOR,
			KEYBOARD_MODE_DESCRIPTOR,
			CONTEXT_MENU_DESCRIPTOR,
			CONTEXT_MENU_SHORTCUTS_DESCRIPTOR,
			RIGHT_CLICK_MENU_DESCRIPTOR,
			TEXTAREA_DESCRIPTOR,
			MESSAGE_INPUT_DESCRIPTOR,
			COMPOSER_DESCRIPTOR,
			TEXTAREA_FOCUS_RING_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'animation',
		tabType: 'accessibility',
		label: ANIMATION_DESCRIPTOR,
		keywords: [
			STICKERS_DESCRIPTOR,
			ANIMATION_2_DESCRIPTOR,
			ANIMATED_STICKERS_DESCRIPTOR,
			GIF_DESCRIPTOR,
			EMOJI_DESCRIPTOR,
			ANIMATED_EMOJI_DESCRIPTOR,
			AUTOPLAY_DESCRIPTOR,
			AUTO_PLAY_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'motion',
		tabType: 'accessibility',
		label: MOTION_DESCRIPTOR,
		keywords: [
			MOTION_2_DESCRIPTOR,
			ANIMATION_2_DESCRIPTOR,
			REDUCED_MOTION_DESCRIPTOR,
			ACCESSIBILITY_DESCRIPTOR,
			ANIMATIONS_DESCRIPTOR,
			SYNC_MOTION_DESCRIPTOR,
			SYNC_REDUCED_MOTION_DESCRIPTOR,
			SYSTEM_MOTION_DESCRIPTOR,
			SYSTEM_REDUCED_MOTION_DESCRIPTOR,
			PREFERS_REDUCED_MOTION_DESCRIPTOR,
			SMOOTH_SCROLLING_DESCRIPTOR,
			AUTOSCROLLING_DESCRIPTOR,
			MIDDLE_CLICK_SCROLLING_DESCRIPTOR,
			SCROLL_ANIMATION_DESCRIPTOR,
			SCROLLING_DESCRIPTOR,
		],
		isAdvanced: false,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
