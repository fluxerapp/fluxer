// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getVoiceAudioDeviceMetadata,
	type VoiceAudioDefaultDevicePlatform,
} from '@app/features/voice/utils/VoiceDeviceManager';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const CAMERA_DESCRIPTOR = msg({
	message: 'Camera',
	comment: 'Fallback video device label when the browser or operating system does not provide a camera name.',
});
const WINDOWS_DEFAULT_AUDIO_DEVICE_DESCRIPTOR = msg({
	message: 'Windows default',
	comment: 'Audio device label prefix for the operating system default audio device on Windows.',
});
const MACOS_DEFAULT_AUDIO_DEVICE_DESCRIPTOR = msg({
	message: 'macOS default',
	comment: 'Audio device label prefix for the operating system default audio device on macOS.',
});
const LINUX_DEFAULT_AUDIO_DEVICE_DESCRIPTOR = msg({
	message: 'Linux default',
	comment: 'Audio device label prefix for the operating system default audio device on Linux.',
});
const BROWSER_DEFAULT_AUDIO_DEVICE_DESCRIPTOR = msg({
	message: 'Browser default',
	comment: 'Audio device label prefix for the browser-managed default audio device.',
});
const DEFAULT_AUDIO_DEVICE_WITH_ENDPOINT_DESCRIPTOR = msg({
	message: '{defaultDeviceLabel} ({endpointLabel})',
	comment:
		'Audio device label for a default route with the concrete microphone or speaker endpoint in parentheses. defaultDeviceLabel is already localized, for example "Windows Default".',
});
export const VOICE_CALL_DESCRIPTOR = msg({
	message: 'Voice call',
	comment: 'Short label for a voice call (lobby header, DM action, settings section).',
});
export const VOICE_MUTED_BY_MODERATORS_DESCRIPTOR = msg({
	message: 'Muted by moderators',
	comment:
		'Voice chat status label. A moderator muted the user in this community, so they cannot speak but can still hear others.',
});
export const VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR = msg({
	message: "You can't speak in this channel",
	comment: 'Voice control tooltip shown when channel permissions prevent the current user from speaking.',
});
export const VOICE_PARTICIPANT_NO_SPEAK_PERMISSION_DESCRIPTOR = msg({
	message: "This participant can't speak in this channel",
	comment:
		'Voice participant status tooltip shown when channel permissions or voice suppression prevent another participant from speaking.',
});
export const VOICE_DEAFENED_BY_MODERATORS_DESCRIPTOR = msg({
	message: 'Deafened by moderators',
	comment: 'Voice chat status label. A moderator deafened the user in this community, so they cannot hear voice chat.',
});
export const VOICE_SELF_DEAFENED_BY_MODERATORS_DESCRIPTOR = msg({
	message: 'You were deafened by moderators',
	comment: 'Voice chat status tooltip for the current user. A moderator deafened them so they cannot hear voice chat.',
});
export const VOICE_PARTICIPANT_DEAFENED_BY_MODERATORS_DESCRIPTOR = msg({
	message: 'This participant was deafened by moderators',
	comment:
		'Voice participant status tooltip for another user. A moderator deafened that participant so they cannot hear voice chat.',
});
export const VOICE_DEAFENED_DESCRIPTOR = msg({
	message: 'Deafened',
	comment: 'Voice chat status label. The user cannot hear voice chat. Distinct from muted.',
});
export const VOICE_SELF_DEAFENED_STATUS_DESCRIPTOR = msg({
	message: 'You are deafened',
	comment: 'Voice participant status tooltip for the current user. They cannot hear voice chat. Distinct from muted.',
});
export const VOICE_PARTICIPANT_DEAFENED_STATUS_DESCRIPTOR = msg({
	message: 'This participant is deafened',
	comment: 'Voice participant status tooltip for another user. They cannot hear voice chat. Distinct from muted.',
});
export const VOICE_DEAFEN_DESCRIPTOR = msg({
	message: 'Deafen',
	comment: 'Voice action label. Turns off what the user hears in voice chat. Distinct from microphone mute.',
});
export const VOICE_UNDEAFEN_DESCRIPTOR = msg({
	message: 'Undeafen',
	comment: 'Voice action label. Restores what the user hears in voice chat. Distinct from microphone unmute.',
});
export const VOICE_DISCONNECT_DESCRIPTOR = msg({
	message: 'Disconnect',
	comment: 'Voice action label. Leaves the current voice call or disconnects a user from voice chat.',
});
export const INCOMING_CALL_ACCEPT_ACTION_DESCRIPTOR = msg({
	message: 'Accept',
	context: 'incoming-call-action',
	comment:
		'Button label for accepting an incoming direct call. Means answering a ringing call, not approving a request.',
});
export const INCOMING_CALL_REJECT_ACTION_DESCRIPTOR = msg({
	message: 'Reject',
	comment: 'Button label for rejecting an incoming direct call.',
});
export const INCOMING_CALL_IGNORE_ACTION_DESCRIPTOR = msg({
	message: 'Ignore',
	comment: 'Button label for silencing an incoming direct call without joining.',
});
export const VOICE_DISCONNECT_DEVICE_DESCRIPTOR = msg({
	message: 'Disconnect device',
	comment: 'Voice menu action that disconnects one active device or session from voice chat.',
});
export const VOICE_DISCONNECT_ALL_DEVICES_DESCRIPTOR = msg({
	message: 'Disconnect all devices',
	comment: 'Voice menu action that disconnects every active device or session from voice chat.',
});
export const VOICE_MUTE_ALL_DEVICES_DESCRIPTOR = msg({
	message: 'Mute all devices',
	comment: "Voice menu action that mutes every device or session's microphone for this user.",
});
export const VOICE_UNMUTE_ALL_DEVICES_DESCRIPTOR = msg({
	message: 'Unmute all devices',
	comment: 'Voice menu action that unmutes every device or session for this user.',
});
export const VOICE_COMMUNITY_MUTE_DESCRIPTOR = msg({
	message: 'Community mute',
	comment: "Moderation voice menu label. Mutes a member's microphone in this community voice chat.",
});
export const VOICE_DEAFEN_SELF_DESCRIPTOR = msg({
	message: 'Deafen yourself',
	comment: 'Voice control label. Turns off what the current user hears in voice chat. Distinct from microphone mute.',
});
export const VOICE_UNDEAFEN_SELF_DESCRIPTOR = msg({
	message: 'Undeafen yourself',
	comment: 'Voice control label. Restores what the current user hears in voice chat. Distinct from microphone unmute.',
});
export const VOICE_DEAFEN_DEVICE_DESCRIPTOR = msg({
	message: 'Deafen device',
	comment:
		'Voice menu action. Deafens one active device or session so it stops hearing voice audio. Distinct from muting its microphone.',
});
export const VOICE_DEAFEN_ALL_DEVICES_DESCRIPTOR = msg({
	message: 'Deafen all devices',
	comment: 'Voice menu action that deafens every device or session so this user cannot hear voice chat.',
});
export const VOICE_UNDEAFEN_ALL_DEVICES_DESCRIPTOR = msg({
	message: 'Undeafen all devices',
	comment: 'Voice menu action that restores voice audio on every device or session for this user.',
});
export const VOICE_COMMUNITY_DEAFEN_DESCRIPTOR = msg({
	message: 'Community deafen',
	comment:
		'Moderation voice menu label. Makes a member unable to hear others in this community voice chat. Distinct from community mute.',
});
export const VOICE_TOGGLE_DEAFEN_DESCRIPTOR = msg({
	message: 'Toggle deafen',
	comment:
		'Keyboard shortcut label. Toggles voice deafen, which controls whether the user hears voice chat. Distinct from microphone mute.',
});
export const VOICE_DEAFEN_SHORTCUT_DESCRIPTOR = msg({
	message: 'Deafen shortcut',
	comment:
		'Settings label for the keyboard shortcut that toggles voice deafen. Deafen controls hearing voice chat, not microphone mute.',
});
export const VOICE_SET_DEAFEN_SHORTCUT_DESCRIPTOR = msg({
	message: 'Set deafen shortcut',
	comment:
		'Settings description for assigning the keyboard shortcut that toggles voice deafen. Deafen controls hearing voice chat, not microphone mute.',
});
export const VOICE_DEAFEN_SOUND_DESCRIPTOR = msg({
	message: 'Voice deafen',
	comment:
		'Notification sound label for the event where the user becomes deafened in voice chat. Distinct from voice mute.',
});
export const VOICE_UNDEAFEN_SOUND_DESCRIPTOR = msg({
	message: 'Voice undeafen',
	comment:
		'Notification sound label for the event where the user is no longer deafened in voice chat. Distinct from voice unmute.',
});
export const VOICE_IN_CHAT_DESCRIPTOR = msg({
	message: 'In voice chat',
	comment: 'Short status label shown when the user is currently connected to voice chat.',
});
export const VOICE_INPUT_DEVICE_DESCRIPTOR = msg({
	message: 'Input device',
	comment: 'Voice settings label for selecting a microphone input device.',
});
export const VOICE_OUTPUT_DEVICE_DESCRIPTOR = msg({
	message: 'Output device',
	comment: 'Voice settings label for selecting a speaker or output device.',
});
export const VOICE_INPUT_VOLUME_DESCRIPTOR = msg({
	message: 'Input volume',
	comment: 'Voice settings slider label for microphone volume.',
});
export const VOICE_OUTPUT_VOLUME_DESCRIPTOR = msg({
	message: 'Output volume',
	comment: 'Voice settings slider label for speaker or output volume.',
});
export const VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR = msg({
	message: 'Focused voice',
	comment: 'Voice input processing profile optimized for spoken voice.',
});
export const VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR = msg({
	message: 'Direct input',
	comment: 'Voice input processing profile with raw microphone audio.',
});
export const VOICE_NOISE_SUPPRESSION_DESCRIPTOR = msg({
	message: 'Noise suppression',
	comment: 'Voice setting label for microphone background-noise filtering.',
});
export const VOICE_ECHO_CANCELLATION_DESCRIPTOR = msg({
	message: 'Echo cancellation',
	comment: 'Voice setting label for preventing speaker audio from feeding into the microphone.',
});
export const VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR = msg({
	message: 'Automatic gain control',
	comment: 'Voice setting label for automatically evening out microphone volume.',
});
export const VOICE_TURN_ON_CAMERA_DESCRIPTOR = msg({
	message: 'Turn on camera',
	comment: 'Voice control button or tooltip label for enabling the camera.',
});
export const VOICE_SHARE_SCREEN_DESCRIPTOR = msg({
	message: 'Share your screen',
	comment: 'Voice control button or tooltip label for starting screen sharing.',
});
export const VOICE_INPUT_SETTINGS_DESCRIPTOR = msg({
	message: 'Input settings',
	comment: 'Voice control dropdown label or menu item for microphone input settings.',
});
export const VOICE_OUTPUT_SETTINGS_DESCRIPTOR = msg({
	message: 'Output settings',
	comment: 'Voice control dropdown label or menu item for speaker/output settings.',
});
export const VOICE_CAMERA_SETTINGS_DESCRIPTOR = msg({
	message: 'Camera settings',
	comment: 'Voice control dropdown label or menu item for camera settings.',
});
export const VOICE_SCREEN_SHARE_SETTINGS_DESCRIPTOR = msg({
	message: 'Screen share settings',
	comment: 'Voice control dropdown label or menu item for screen sharing and stream quality settings.',
});
export const VOICE_STOP_WATCHING_DESCRIPTOR = msg({
	message: 'Stop watching',
	comment: 'Voice control action for stopping the currently focused or selected stream.',
});
export const VOICE_USER_VOLUME_DESCRIPTOR = msg({
	message: 'User volume',
	comment: 'Voice menu slider label for the saved volume of a specific user.',
});
const VOICE_VIDEO_SETTINGS_MENU_DESCRIPTOR = msg({
	message: 'Voice & video settings',
	comment: 'Voice menu item that opens the user settings tab for microphone, speaker, camera, and call stats.',
});
const OPEN_VOICE_VIDEO_SETTINGS_DESCRIPTOR = msg({
	message: 'Open voice & video settings',
	comment: 'Accessible button label that opens the user settings tab for microphone, speaker, camera, and call stats.',
});

export function getVoiceVideoSettingsLabel(i18n: I18n): string {
	return i18n._(VOICE_VIDEO_SETTINGS_MENU_DESCRIPTOR);
}

export function getOpenVoiceVideoSettingsLabel(i18n: I18n): string {
	return i18n._(OPEN_VOICE_VIDEO_SETTINGS_DESCRIPTOR);
}

export function getVoiceNoSpeakPermissionLabel(i18n: I18n, isCurrentUser: boolean): string {
	return i18n._(
		isCurrentUser ? VOICE_NO_SPEAK_PERMISSION_DESCRIPTOR : VOICE_PARTICIPANT_NO_SPEAK_PERMISSION_DESCRIPTOR,
	);
}

export function getVoiceDeafenedByModeratorsStatusLabel(i18n: I18n, isCurrentUser: boolean): string {
	return i18n._(
		isCurrentUser ? VOICE_SELF_DEAFENED_BY_MODERATORS_DESCRIPTOR : VOICE_PARTICIPANT_DEAFENED_BY_MODERATORS_DESCRIPTOR,
	);
}

export function getVoiceDeafenedStatusLabel(i18n: I18n, isCurrentUser: boolean): string {
	return i18n._(isCurrentUser ? VOICE_SELF_DEAFENED_STATUS_DESCRIPTOR : VOICE_PARTICIPANT_DEAFENED_STATUS_DESCRIPTOR);
}

export function formatFallbackCameraLabel(i18n: I18n): string {
	return i18n._(CAMERA_DESCRIPTOR);
}

function getDefaultAudioDeviceLabel(i18n: I18n, platform: VoiceAudioDefaultDevicePlatform): string {
	switch (platform) {
		case 'windows':
			return i18n._(WINDOWS_DEFAULT_AUDIO_DEVICE_DESCRIPTOR);
		case 'macos':
			return i18n._(MACOS_DEFAULT_AUDIO_DEVICE_DESCRIPTOR);
		case 'linux':
			return i18n._(LINUX_DEFAULT_AUDIO_DEVICE_DESCRIPTOR);
		case 'browser':
			return i18n._(BROWSER_DEFAULT_AUDIO_DEVICE_DESCRIPTOR);
	}
}

export function formatVoiceAudioDeviceLabel(i18n: I18n, device: MediaDeviceInfo, fallbackLabel: string): string {
	const metadata = getVoiceAudioDeviceMetadata(device);
	if (metadata?.role === 'default' && metadata.defaultPlatform) {
		const defaultDeviceLabel = getDefaultAudioDeviceLabel(i18n, metadata.defaultPlatform);
		return metadata.endpointLabel
			? i18n._(DEFAULT_AUDIO_DEVICE_WITH_ENDPOINT_DESCRIPTOR, {
					defaultDeviceLabel,
					endpointLabel: metadata.endpointLabel,
				})
			: defaultDeviceLabel;
	}
	return device.label || fallbackLabel;
}

export const VOICE_CHANNEL_E2EE_ENCRYPTED_DESCRIPTOR = msg({
	message: 'Microphone, camera, and screen share content are end-to-end encrypted.',
	comment:
		'Pre-join indicator shown beneath the Join button on a voice channel where every connected participant supports E2EE.',
});
export const VOICE_CALL_E2EE_ENCRYPTED_DESCRIPTOR = msg({
	message: 'This call is end-to-end encrypted.',
	comment:
		'Pre-join indicator shown beneath the Join button on a DM or group DM call where every connected participant supports E2EE.',
});
export const VOICE_CHANNEL_E2EE_BROKEN_DESCRIPTOR = msg({
	message: 'End-to-end encryption is unavailable because an unsupported participant is in this voice channel.',
	comment:
		'Pre-join indicator shown beneath the Join button on a voice channel where at least one connected participant (typically an outdated bot) does not support E2EE.',
});
export const VOICE_CALL_E2EE_BROKEN_DESCRIPTOR = msg({
	message: 'End-to-end encryption is unavailable because an unsupported participant is in this call.',
	comment:
		'Pre-join indicator shown beneath the Join button on a DM or group DM call where at least one connected participant (typically an outdated bot) does not support E2EE.',
});
