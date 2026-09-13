// SPDX-License-Identifier: AGPL-3.0-or-later

import {GENERAL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {SectionDefinition} from '@app/features/user/components/settings_utils/section_registry/SectionRegistryTypes';
import {
	ACCESSIBILITY_DESCRIPTOR,
	AUDIO_2_DESCRIPTOR,
	CALL_DESCRIPTOR,
	DIRECT_MESSAGE_DESCRIPTOR,
	DM_DESCRIPTOR,
	MENTIONS_DESCRIPTOR,
	NARRATION_DESCRIPTOR,
	PING_DESCRIPTOR,
	PLUTONIUM_DESCRIPTOR,
	POPUP_DESCRIPTOR,
	READ_ALOUD_DESCRIPTOR,
	READ_MESSAGES_DESCRIPTOR,
	SPEECH_DESCRIPTOR,
	TEXT_TO_SPEECH_3_DESCRIPTOR,
	TTS_DESCRIPTOR,
	UNREAD_DESCRIPTOR,
	VOLUME_DESCRIPTOR,
} from '@app/features/user/components/settings_utils/section_registry/SharedDescriptors';
import {
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DEAFEN_SOUND_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {msg} from '@lingui/core/macro';

const NOTIFICATIONS_2_DESCRIPTOR = msg({
	message: 'Notifications',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DESKTOP_DESCRIPTOR = msg({
	message: 'Desktop',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ALERTS_DESCRIPTOR = msg({
	message: 'Alerts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TOAST_DESCRIPTOR = msg({
	message: 'Toast',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_DESCRIPTOR = msg({
	message: '@',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NOTIFY_DESCRIPTOR = msg({
	message: 'Notify',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PRIVATE_MESSAGE_DESCRIPTOR = msg({
	message: 'Private message',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MENTION_PREFERENCE_DESCRIPTOR = msg({
	message: 'Mention preference',
	comment: 'Settings section label. Names the settings section in the settings UI.',
});
const DECIDE_WHETHER_REPLIES_MENTION_YOU_BY_DEFAULT_DESCRIPTOR = msg({
	message: 'Decide whether replies @mention you by default',
	comment:
		'Settings section description. One-line summary of what the mention preference settings section controls. The @ is literal.',
});
const MENTION_DESCRIPTOR = msg({
	message: 'Mention',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REPLY_DESCRIPTOR = msg({
	message: 'Reply',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REPLIES_DESCRIPTOR = msg({
	message: 'Replies',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const REPLY_MENTION_DESCRIPTOR = msg({
	message: 'Reply mention',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MENTION_2_DESCRIPTOR = msg({
	message: '@mention',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PING_ON_REPLY_DESCRIPTOR = msg({
	message: 'Ping on reply',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NOTIFICATION_DESCRIPTOR = msg({
	message: 'Notification',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NOTIFY_ON_REPLY_DESCRIPTOR = msg({
	message: 'Notify on reply',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const UNREAD_BADGE_DESCRIPTOR = msg({
	message: 'Unread badge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const UNREAD_MESSAGE_BADGE_DESCRIPTOR = msg({
	message: 'Unread message badge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const APP_ICON_BADGE_DESCRIPTOR = msg({
	message: 'App icon badge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_BADGE_DESCRIPTOR = msg({
	message: 'Message badge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DOCK_BADGE_DESCRIPTOR = msg({
	message: 'Dock badge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TASKBAR_BADGE_DESCRIPTOR = msg({
	message: 'Taskbar badge',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const BROWSER_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Browser notifications',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ENABLE_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Enable notifications',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PUSH_SUBSCRIPTION_DESCRIPTOR = msg({
	message: 'Push subscription',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SOUNDS_2_DESCRIPTOR = msg({
	message: 'Sounds',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NOTIFICATION_SOUND_DESCRIPTOR = msg({
	message: 'Notification sound',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const ALERT_SOUND_DESCRIPTOR = msg({
	message: 'Alert sound',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'playback-control-action',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MESSAGE_SOUND_DESCRIPTOR = msg({
	message: 'Message sound',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SOUND_EFFECT_DESCRIPTOR = msg({
	message: 'Sound effect',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NEW_MESSAGE_DESCRIPTOR = msg({
	message: 'New message',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RING_DESCRIPTOR = msg({
	message: 'Ring',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const RINGTONE_DESCRIPTOR = msg({
	message: 'Ringtone',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const INCOMING_CALL_DESCRIPTOR = msg({
	message: 'Incoming call',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const DISABLE_SOUNDS_DESCRIPTOR = msg({
	message: 'Disable sounds',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MUTE_ALL_DESCRIPTOR = msg({
	message: 'Mute all',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SILENCE_DESCRIPTOR = msg({
	message: 'Silence',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const QUIET_DESCRIPTOR = msg({
	message: 'Quiet',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MUTE_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Mute notifications',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_SOUNDS_DESCRIPTOR = msg({
	message: 'Custom sounds',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const UPLOAD_SOUND_DESCRIPTOR = msg({
	message: 'Upload sound',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_NOTIFICATION_DESCRIPTOR = msg({
	message: 'Custom notification',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const CUSTOM_RINGTONE_DESCRIPTOR = msg({
	message: 'Custom ringtone',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MP3_DESCRIPTOR = msg({
	message: 'MP3',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const WAV_DESCRIPTOR = msg({
	message: 'WAV',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const MASTER_VOLUME_DESCRIPTOR = msg({
	message: 'Master volume',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SOUND_VOLUME_DESCRIPTOR = msg({
	message: 'Sound volume',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const OUTPUT_VOLUME_DESCRIPTOR = msg({
	message: 'Output volume',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PER_SOUND_VOLUME_DESCRIPTOR = msg({
	message: 'Per-sound volume',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const STREAM_DESCRIPTOR = msg({
	message: 'Stream',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SCREEN_SHARE_SOUND_DESCRIPTOR = msg({
	message: 'Screen share sound',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TTS_2_DESCRIPTOR = msg({
	message: '/tts',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const TTS_COMMAND_DESCRIPTOR = msg({
	message: 'TTS command',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPEECH_COMMAND_DESCRIPTOR = msg({
	message: 'Speech command',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const PLAYBACK_DESCRIPTOR = msg({
	message: 'Playback',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTO_NARRATION_DESCRIPTOR = msg({
	message: 'Auto narration',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const AUTOMATIC_DESCRIPTOR = msg({
	message: 'Automatic',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SPEAK_MESSAGES_DESCRIPTOR = msg({
	message: 'Speak messages',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const NARRATE_DESCRIPTOR = msg({
	message: 'Narrate',
	comment: 'Settings search synonym. Used to match this term when the user types it in the settings search bar.',
});
const SOUNDS_DESCRIPTOR = msg({
	message: 'Sounds',
	context: 'notifications-settings-section',
	comment: 'Settings section for notification sounds.',
});
const TEXT_TO_SPEECH_2_DESCRIPTOR = msg({
	message: 'Text-to-speech notifications',
	context: 'notifications-settings-section',
	comment: 'Settings section for text-to-speech notification behavior.',
});
export const notificationsSections = [
	{
		id: 'notifications',
		tabType: 'notifications',
		label: GENERAL_DESCRIPTOR,
		keywords: [
			NOTIFICATIONS_2_DESCRIPTOR,
			DESKTOP_DESCRIPTOR,
			ALERTS_DESCRIPTOR,
			POPUP_DESCRIPTOR,
			TOAST_DESCRIPTOR,
			MENTIONS_DESCRIPTOR,
			PING_DESCRIPTOR,
			MESSAGE_DESCRIPTOR,
			NOTIFY_DESCRIPTOR,
			DM_DESCRIPTOR,
			DIRECT_MESSAGE_DESCRIPTOR,
			PRIVATE_MESSAGE_DESCRIPTOR,
			UNREAD_DESCRIPTOR,
			UNREAD_BADGE_DESCRIPTOR,
			UNREAD_MESSAGE_BADGE_DESCRIPTOR,
			APP_ICON_BADGE_DESCRIPTOR,
			MESSAGE_BADGE_DESCRIPTOR,
			DOCK_BADGE_DESCRIPTOR,
			TASKBAR_BADGE_DESCRIPTOR,
			BROWSER_NOTIFICATIONS_DESCRIPTOR,
			ENABLE_NOTIFICATIONS_DESCRIPTOR,
			PUSH_SUBSCRIPTION_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'mention-preference',
		tabType: 'notifications',
		label: MENTION_PREFERENCE_DESCRIPTOR,
		description: DECIDE_WHETHER_REPLIES_MENTION_YOU_BY_DEFAULT_DESCRIPTOR,
		keywords: [
			MENTION_DESCRIPTOR,
			MENTIONS_DESCRIPTOR,
			REPLY_DESCRIPTOR,
			REPLIES_DESCRIPTOR,
			REPLY_MENTION_DESCRIPTOR,
			MENTION_2_DESCRIPTOR,
			PING_DESCRIPTOR,
			PING_ON_REPLY_DESCRIPTOR,
			NOTIFICATION_DESCRIPTOR,
			NOTIFY_DESCRIPTOR,
			NOTIFY_ON_REPLY_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'sounds',
		tabType: 'notifications',
		label: SOUNDS_DESCRIPTOR,
		keywords: [
			SOUNDS_2_DESCRIPTOR,
			AUDIO_2_DESCRIPTOR,
			NOTIFICATION_SOUND_DESCRIPTOR,
			ALERT_SOUND_DESCRIPTOR,
			MUTE_DESCRIPTOR,
			MESSAGE_SOUND_DESCRIPTOR,
			SOUND_EFFECT_DESCRIPTOR,
			NEW_MESSAGE_DESCRIPTOR,
			CALL_DESCRIPTOR,
			RING_DESCRIPTOR,
			RINGTONE_DESCRIPTOR,
			INCOMING_CALL_DESCRIPTOR,
			DISABLE_SOUNDS_DESCRIPTOR,
			MUTE_ALL_DESCRIPTOR,
			SILENCE_DESCRIPTOR,
			QUIET_DESCRIPTOR,
			MUTE_NOTIFICATIONS_DESCRIPTOR,
			CUSTOM_SOUNDS_DESCRIPTOR,
			UPLOAD_SOUND_DESCRIPTOR,
			CUSTOM_NOTIFICATION_DESCRIPTOR,
			CUSTOM_RINGTONE_DESCRIPTOR,
			MP3_DESCRIPTOR,
			WAV_DESCRIPTOR,
			PLUTONIUM_DESCRIPTOR,
			VOLUME_DESCRIPTOR,
			MASTER_VOLUME_DESCRIPTOR,
			SOUND_VOLUME_DESCRIPTOR,
			OUTPUT_VOLUME_DESCRIPTOR,
			PER_SOUND_VOLUME_DESCRIPTOR,
			VOICE_DEAFEN_DESCRIPTOR,
			VOICE_DEAFEN_SOUND_DESCRIPTOR,
			STREAM_DESCRIPTOR,
			SCREEN_SHARE_SOUND_DESCRIPTOR,
		],
		isAdvanced: false,
	},
	{
		id: 'text-to-speech',
		tabType: 'notifications',
		label: TEXT_TO_SPEECH_2_DESCRIPTOR,
		keywords: [
			TTS_DESCRIPTOR,
			TEXT_TO_SPEECH_3_DESCRIPTOR,
			SPEECH_DESCRIPTOR,
			NARRATION_DESCRIPTOR,
			READ_ALOUD_DESCRIPTOR,
			ACCESSIBILITY_DESCRIPTOR,
			TTS_2_DESCRIPTOR,
			TTS_COMMAND_DESCRIPTOR,
			SPEECH_COMMAND_DESCRIPTOR,
			PLAYBACK_DESCRIPTOR,
			AUTO_NARRATION_DESCRIPTOR,
			AUTOMATIC_DESCRIPTOR,
			SPEAK_MESSAGES_DESCRIPTOR,
			READ_MESSAGES_DESCRIPTOR,
			NARRATE_DESCRIPTOR,
		],
		isAdvanced: false,
	},
] as const satisfies ReadonlyArray<SectionDefinition>;
