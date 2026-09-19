// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScreenShareDeliveryNotice} from '@app/features/voice/engine/ScreenShareUnderperformance';
import type {ScreenShareDeliveryPlan} from '@app/features/voice/state/ScreenShareDelivery';
import {CODEC_DISPLAY_LABEL} from '@app/features/voice/utils/ScreenShareCodecPolicy';
import {
	resolveEffectiveScreenShareDimensions,
	type ScreenShareTarget,
} from '@app/features/voice/utils/ScreenShareOptions';
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

const SCREEN_SHARE_NOTICE_DEVICE_FRAMERATE_DESCRIPTOR = msg({
	message: 'Your device could not keep up, so your stream now sends {frameRate} FPS to keep {resolution} sharp.',
	comment:
		'Screen share notice shown to the person sharing after their stream dropped frame rate to protect sharpness. frameRate is a whole number and resolution is a technical token such as 1080p or 2714x762. FPS is a technical token.',
});
const SCREEN_SHARE_NOTICE_DEVICE_RESOLUTION_DESCRIPTOR = msg({
	message: 'Your device could not keep up, so your stream now sends {resolution} to keep {frameRate} FPS smooth.',
	comment:
		'Screen share notice shown to the person sharing after their stream dropped resolution to protect smoothness. resolution is a technical token such as 720p and frameRate is a whole number. FPS is a technical token.',
});
const SCREEN_SHARE_NOTICE_DEVICE_CROSSOVER_DESCRIPTOR = msg({
	message:
		'Your device could not keep up, so your stream now sends {resolution} at {frameRate} FPS instead of {targetResolution} at {targetFrameRate} FPS.',
	comment:
		'Screen share notice shown to the person sharing after their stream dropped both resolution and frame rate. resolution and frameRate are what the stream sends now, targetResolution and targetFrameRate are what the person asked for. Resolutions are technical tokens such as 480p and rates are whole numbers. FPS is a technical token.',
});
const SCREEN_SHARE_NOTICE_CODEC_SWITCHED_DESCRIPTOR = msg({
	message:
		'Your stream switched to {codec} because {previousCodec} could not keep up on this device. Viewers may see a short pause.',
	comment:
		'Screen share notice shown to the person sharing after the app replaced the video codec. codec and previousCodec are codec names such as VP9 or H.264.',
});
const SCREEN_SHARE_NOTICE_DEVICE_SHORT_DESCRIPTOR = msg({
	message:
		'Your device cannot keep up. Your stream is set to {frameRate} FPS at {resolution} and viewers are getting about {deliveredFrameRate} FPS.',
	comment:
		'Screen share notice shown to the person sharing when their device is the limit and nothing lower is available. frameRate is the whole number of frames a second the stream is set to send and deliveredFrameRate is the smaller whole number that reaches viewers. Keep the two rates in separate clauses so neither can be read as the other. resolution is a technical token such as 720p. FPS is a technical token.',
});
const SCREEN_SHARE_NOTICE_CONNECTION_SHORT_DESCRIPTOR = msg({
	message:
		'Your connection cannot keep up. Your stream is set to {frameRate} FPS at {resolution} and viewers are getting about {deliveredFrameRate} FPS.',
	comment:
		'Screen share notice shown to the person sharing when their upload is the limit. frameRate is the whole number of frames a second the stream is set to send and deliveredFrameRate is the smaller whole number that reaches viewers. Keep the two rates in separate clauses so neither can be read as the other. resolution is a technical token such as 720p. FPS is a technical token.',
});
const SCREEN_SHARE_NOTICE_BROWSER_RESOLUTION_DESCRIPTOR = msg({
	message: 'The browser is sending {deliveredResolution} instead of {resolution}.',
	comment:
		'Screen share notice shown to the person sharing when the browser ignores the requested size. deliveredResolution is the size the browser really sends and resolution is the size the app asked it for. Both are technical tokens such as 720p or 2714x762.',
});
const SCREEN_SHARE_NOTICE_CAPTURE_RESOLUTION_DESCRIPTOR = msg({
	message: 'Your capture device is sending {deliveredResolution} instead of {resolution}.',
	comment:
		'Screen share notice shown to the person sharing a capture device, such as a capture card, that ignores the requested size. deliveredResolution is the size the capture device really sends and resolution is the size the app asked it for. Both are technical tokens such as 720p.',
});
const SCREEN_SHARE_NOTICE_STARTED_LOWER_DESCRIPTOR = msg({
	message:
		'This device could not keep up with {targetResolution} at {targetFrameRate} FPS last time, so your stream started at {resolution} at {frameRate} FPS.',
	comment:
		'Screen share notice shown to the person sharing when a remembered result made the stream start below the asked-for settings. targetResolution and targetFrameRate are what the person asked for, resolution and frameRate are the lower settings the stream started at. Resolutions are technical tokens such as 1080p and rates are whole numbers. FPS is a technical token.',
});
const SCREEN_SHARE_NOTICE_PINNED_CODEC_SHORT_DESCRIPTOR = msg({
	message:
		'{codec} cannot keep up on this device. Set the codec to Automatic in Advanced settings to allow a faster one.',
	comment:
		'Screen share notice shown to the person sharing when they pinned a codec that underperforms. codec is a codec name such as AV1. Automatic and Advanced settings are labels in the same app.',
});
const SCREEN_SHARE_NOTICE_ENCODER_RECOVERED_DESCRIPTOR = msg({
	message: 'Your video encoder stopped, so your stream switched to {codec}.',
	comment:
		'Screen share notice shown to the person sharing after the app recovered a stalled encoder on another codec. codec is a codec name such as VP8.',
});
const SCREEN_SHARE_NOTICE_CODEC_VIEWER_DESCRIPTOR = msg({
	message: 'A viewer cannot play {pinnedCodec}, so your stream uses {codec}.',
	comment:
		'Screen share notice shown to the person sharing when a watcher cannot decode the pinned codec. pinnedCodec and codec are codec names such as AV1 or VP8.',
});
const SCREEN_SHARE_NOTICE_CODEC_DEVICE_DESCRIPTOR = msg({
	message: '{pinnedCodec} is not available on this device, so your stream uses {codec}.',
	comment:
		'Screen share notice shown to the person sharing when this device cannot encode the pinned codec. pinnedCodec and codec are codec names such as H.265 (HEVC) or VP9.',
});
const SCREEN_SHARE_NOTICE_APPLY_FAILED_DESCRIPTOR = msg({
	message: 'Some stream settings could not be applied to your live stream.',
	comment: 'Screen share notice shown to the person sharing when the app failed to push new settings to a live share.',
});

export const SCREEN_SHARE_STATUS_SENDING_DESCRIPTOR = msg({
	message: 'Set to send {resolution} at {frameRate} FPS with {codec}',
	comment:
		'Screen share status row in the stream menu and in video settings, describing the settings the live stream is running at rather than what reaches viewers. resolution is a technical token such as 1080p, frameRate is the whole number of frames a second the stream is set to send and codec is a codec name such as VP9. FPS is a technical token.',
});
export const SCREEN_SHARE_STATUS_ASKED_FOR_DESCRIPTOR = msg({
	message: 'You asked for {resolution} at {frameRate} FPS',
	comment:
		'Screen share status row in the stream menu, describing the settings the person chose. resolution is a technical token such as 1440p and frameRate is a whole number. FPS is a technical token.',
});
export const SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR = msg({
	message: 'Source',
	comment:
		'Screen share resolution token in the stream menu status rows, used when the person asked for the original source resolution and the app does not know the source size. Matches the Source option in the stream settings menu.',
});
export const SCREEN_SHARE_STATUS_SOURCE_RATE_DESCRIPTOR = msg({
	message: 'Your source is producing about {sourceFrameRate} FPS',
	comment:
		'Screen share status row in the stream menu, shown when the captured window or screen itself produces fewer frames than asked for. sourceFrameRate is a whole number. FPS is a technical token.',
});
export const SCREEN_SHARE_TRY_FULL_QUALITY_AGAIN_DESCRIPTOR = msg({
	message: 'Try full quality again',
	comment:
		'Screen share action in the stream menu. Forgets what this device measured and restarts the stream at the asked-for settings.',
});
export const SCREEN_SHARE_SETTINGS_ADJUSTED_DESCRIPTOR = msg({
	message: 'Stream settings were adjusted',
	comment:
		'Accessible label for the information badge on the sharing tile, shown when the app changed stream settings without a problem to report.',
});

const RESOLUTION_LABEL_HEIGHTS = [480, 720, 1080, 1440, 2160];
const RESOLUTION_LABEL_ASPECT = 16 / 9;
const RESOLUTION_LABEL_ASPECT_TOLERANCE = 0.01;

export function formatScreenShareResolutionLabel(width: number, height: number): string {
	const matchesAspect = Math.abs(width / height / RESOLUTION_LABEL_ASPECT - 1) <= RESOLUTION_LABEL_ASPECT_TOLERANCE;
	if (matchesAspect && RESOLUTION_LABEL_HEIGHTS.includes(height)) {
		return `${height}p`;
	}
	return `${width}×${height}`;
}

type ScreenShareDeliveryNoticeFormatters = {
	[Kind in ScreenShareDeliveryNotice['kind']]: (
		i18n: I18n,
		notice: Extract<ScreenShareDeliveryNotice, {kind: Kind}>,
	) => string;
};

const SCREEN_SHARE_DELIVERY_NOTICE_FORMATTERS = {
	'device-framerate': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_DEVICE_FRAMERATE_DESCRIPTOR, {
			frameRate: notice.frameRate,
			resolution: formatScreenShareResolutionLabel(notice.width, notice.height),
		}),
	'device-resolution': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_DEVICE_RESOLUTION_DESCRIPTOR, {
			frameRate: notice.frameRate,
			resolution: formatScreenShareResolutionLabel(notice.width, notice.height),
		}),
	'device-crossover': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_DEVICE_CROSSOVER_DESCRIPTOR, {
			frameRate: notice.frameRate,
			resolution: formatScreenShareResolutionLabel(notice.width, notice.height),
			targetFrameRate: notice.targetFrameRate,
			targetResolution: formatScreenShareResolutionLabel(notice.targetWidth, notice.targetHeight),
		}),
	'codec-switched': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_CODEC_SWITCHED_DESCRIPTOR, {
			codec: CODEC_DISPLAY_LABEL[notice.codec],
			previousCodec: CODEC_DISPLAY_LABEL[notice.previousCodec],
		}),
	'device-short': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_DEVICE_SHORT_DESCRIPTOR, {
			deliveredFrameRate: Math.round(notice.deliveredFrameRate),
			frameRate: notice.frameRate,
			resolution: formatScreenShareResolutionLabel(notice.width, notice.height),
		}),
	'connection-short': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_CONNECTION_SHORT_DESCRIPTOR, {
			deliveredFrameRate: Math.round(notice.deliveredFrameRate),
			frameRate: notice.frameRate,
			resolution: formatScreenShareResolutionLabel(notice.width, notice.height),
		}),
	'browser-resolution': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_BROWSER_RESOLUTION_DESCRIPTOR, {
			deliveredResolution: formatScreenShareResolutionLabel(notice.deliveredWidth, notice.deliveredHeight),
			resolution: formatScreenShareResolutionLabel(notice.width, notice.height),
		}),
	'capture-resolution': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_CAPTURE_RESOLUTION_DESCRIPTOR, {
			deliveredResolution: formatScreenShareResolutionLabel(notice.deliveredWidth, notice.deliveredHeight),
			resolution: formatScreenShareResolutionLabel(notice.width, notice.height),
		}),
	'started-lower': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_STARTED_LOWER_DESCRIPTOR, {
			frameRate: notice.frameRate,
			resolution: formatScreenShareResolutionLabel(notice.width, notice.height),
			targetFrameRate: notice.targetFrameRate,
			targetResolution: formatScreenShareResolutionLabel(notice.targetWidth, notice.targetHeight),
		}),
	'pinned-codec-short': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_PINNED_CODEC_SHORT_DESCRIPTOR, {codec: CODEC_DISPLAY_LABEL[notice.codec]}),
	'encoder-recovered': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_ENCODER_RECOVERED_DESCRIPTOR, {codec: CODEC_DISPLAY_LABEL[notice.codec]}),
	'codec-viewer': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_CODEC_VIEWER_DESCRIPTOR, {
			codec: CODEC_DISPLAY_LABEL[notice.codec],
			pinnedCodec: CODEC_DISPLAY_LABEL[notice.pinnedCodec],
		}),
	'codec-device': (i18n, notice) =>
		i18n._(SCREEN_SHARE_NOTICE_CODEC_DEVICE_DESCRIPTOR, {
			codec: CODEC_DISPLAY_LABEL[notice.codec],
			pinnedCodec: CODEC_DISPLAY_LABEL[notice.pinnedCodec],
		}),
	'apply-failed': (i18n) => i18n._(SCREEN_SHARE_NOTICE_APPLY_FAILED_DESCRIPTOR),
} satisfies ScreenShareDeliveryNoticeFormatters;

export function formatScreenShareDeliveryNotice(i18n: I18n, notice: ScreenShareDeliveryNotice): string {
	const format = SCREEN_SHARE_DELIVERY_NOTICE_FORMATTERS[notice.kind] as (
		i18n: I18n,
		notice: ScreenShareDeliveryNotice,
	) => string;
	return format(i18n, notice);
}

const TRY_FULL_QUALITY_BLOCKING_NOTICE_KINDS = new Set<ScreenShareDeliveryNotice['kind']>([
	'browser-resolution',
	'capture-resolution',
	'codec-device',
	'codec-switched',
	'codec-viewer',
	'connection-short',
	'encoder-recovered',
	'pinned-codec-short',
]);

export interface ScreenShareDeliveryStatus {
	sending: string | null;
	askedFor: string | null;
	notice: string | null;
	sourceRate: string | null;
	hasRows: boolean;
	canTryFullQuality: boolean;
}

const SCREEN_SHARE_SOURCE_BOX = resolveEffectiveScreenShareDimensions('source', null);

function formatScreenShareTargetLabel(i18n: I18n, target: ScreenShareTarget): string {
	if (
		target.resolution === 'source' &&
		target.width === SCREEN_SHARE_SOURCE_BOX.width &&
		target.height === SCREEN_SHARE_SOURCE_BOX.height
	) {
		return i18n._(SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR);
	}
	return formatScreenShareResolutionLabel(target.width, target.height);
}

export function formatScreenShareDeliveryStatus(
	i18n: I18n,
	plan: ScreenShareDeliveryPlan | null,
	notice: ScreenShareDeliveryNotice | null,
): ScreenShareDeliveryStatus {
	const startedLower = notice?.kind === 'started-lower';
	const steppedDown = plan !== null && plan.levelIndex > 0;
	const sending =
		plan === null
			? null
			: i18n._(SCREEN_SHARE_STATUS_SENDING_DESCRIPTOR, {
					resolution: formatScreenShareResolutionLabel(plan.level.width, plan.level.height),
					frameRate: plan.level.frameRate,
					codec: CODEC_DISPLAY_LABEL[plan.codec],
				});
	const askedFor =
		plan !== null &&
		(plan.level.width !== plan.target.width ||
			plan.level.height !== plan.target.height ||
			plan.level.frameRate !== plan.target.frameRate)
			? i18n._(SCREEN_SHARE_STATUS_ASKED_FOR_DESCRIPTOR, {
					resolution: formatScreenShareTargetLabel(i18n, plan.target),
					frameRate: plan.target.frameRate,
				})
			: null;
	const noticeText = notice === null ? null : formatScreenShareDeliveryNotice(i18n, notice);
	const sourceRate =
		plan === null || plan.sourceFrameRate === null
			? null
			: i18n._(SCREEN_SHARE_STATUS_SOURCE_RATE_DESCRIPTOR, {sourceFrameRate: Math.round(plan.sourceFrameRate)});
	return {
		sending,
		askedFor,
		notice: noticeText,
		sourceRate,
		hasRows: sending !== null || askedFor !== null || noticeText !== null || sourceRate !== null,
		canTryFullQuality:
			(steppedDown || startedLower) && (notice === null || !TRY_FULL_QUALITY_BLOCKING_NOTICE_KINDS.has(notice.kind)),
	};
}
