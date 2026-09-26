// SPDX-License-Identifier: AGPL-3.0-or-later

import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {
	VOICE_DEAFEN_SOUND_DESCRIPTOR,
	VOICE_UNDEAFEN_SOUND_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const CURRENT_CHANNEL_MESSAGE_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Current channel message notifications',
	comment:
		'Notification sound setting label. Controls the sound for incoming messages in the currently focused channel.',
});
const VOICE_MUTE_DESCRIPTOR = msg({
	message: 'Voice mute',
	comment: 'Notification sound setting label. Sound played when the user mutes their microphone in a voice channel.',
});
const VOICE_UNMUTE_DESCRIPTOR = msg({
	message: 'Voice unmute',
	comment: 'Notification sound setting label. Sound played when the user unmutes their microphone in a voice channel.',
});
const USER_JOINS_CHANNEL_DESCRIPTOR = msg({
	message: 'User joins channel',
	comment: 'Notification sound setting label. Sound played when another user joins the voice channel.',
});
const USER_LEAVES_CHANNEL_DESCRIPTOR = msg({
	message: 'User leaves channel',
	comment: 'Notification sound setting label. Sound played when another user leaves the voice channel.',
});
const USER_MOVED_CHANNEL_DESCRIPTOR = msg({
	message: 'User moves channel',
	comment: 'Notification sound setting label. Sound played when a user is moved between voice channels.',
});
const VIEWER_JOINS_STREAM_DESCRIPTOR = msg({
	message: 'Viewer joins stream',
	comment: 'Notification sound setting label. Sound played when a viewer joins your screen share or stream.',
});
const VIEWER_LEAVES_STREAM_DESCRIPTOR = msg({
	message: 'Viewer leaves stream',
	comment: 'Notification sound setting label. Sound played when a viewer leaves your screen share or stream.',
});
const VOICE_DISCONNECTED_DESCRIPTOR = msg({
	message: 'Voice disconnected',
	comment: 'Notification sound setting label. Sound played when the voice connection drops.',
});
const INCOMING_CALL_DESCRIPTOR = msg({
	message: 'Incoming call',
	comment: 'Notification sound setting label. Ringtone for an incoming DM or group DM call.',
});
const CAMERA_ON_DESCRIPTOR = msg({
	message: 'Camera on',
	comment: 'Notification sound setting label. Sound played when the user turns their camera on in a call.',
});
const CAMERA_OFF_DESCRIPTOR = msg({
	message: 'Camera off',
	comment: 'Notification sound setting label. Sound played when the user turns their camera off in a call.',
});
const SCREEN_SHARE_START_DESCRIPTOR = msg({
	message: 'Screen share start',
	comment: 'Notification sound setting label. Sound played when screen sharing starts.',
});
const SCREEN_SHARE_STOP_DESCRIPTOR = msg({
	message: 'Screen share stop',
	comment: 'Notification sound setting label. Sound played when screen sharing stops.',
});
const COMMUNITY_MESSAGE_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Community message notifications',
	comment: 'Notification sound setting label. Controls sounds for incoming messages in communities/servers.',
});
const DIRECT_MESSAGE_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Direct message notifications',
	comment: 'Notification sound setting label. Controls sounds for incoming one-to-one and group DM messages.',
});

export function getSoundLabels(i18n: I18n): Record<SoundType, string> {
	return {
		[SoundType.Message]: i18n._(COMMUNITY_MESSAGE_NOTIFICATIONS_DESCRIPTOR),
		[SoundType.DirectMessage]: i18n._(DIRECT_MESSAGE_NOTIFICATIONS_DESCRIPTOR),
		[SoundType.SameChannelMessage]: i18n._(CURRENT_CHANNEL_MESSAGE_NOTIFICATIONS_DESCRIPTOR),
		[SoundType.Mute]: i18n._(VOICE_MUTE_DESCRIPTOR),
		[SoundType.Unmute]: i18n._(VOICE_UNMUTE_DESCRIPTOR),
		[SoundType.Deaf]: i18n._(VOICE_DEAFEN_SOUND_DESCRIPTOR),
		[SoundType.Undeaf]: i18n._(VOICE_UNDEAFEN_SOUND_DESCRIPTOR),
		[SoundType.UserJoin]: i18n._(USER_JOINS_CHANNEL_DESCRIPTOR),
		[SoundType.UserLeave]: i18n._(USER_LEAVES_CHANNEL_DESCRIPTOR),
		[SoundType.UserMove]: i18n._(USER_MOVED_CHANNEL_DESCRIPTOR),
		[SoundType.ViewerJoin]: i18n._(VIEWER_JOINS_STREAM_DESCRIPTOR),
		[SoundType.ViewerLeave]: i18n._(VIEWER_LEAVES_STREAM_DESCRIPTOR),
		[SoundType.VoiceDisconnect]: i18n._(VOICE_DISCONNECTED_DESCRIPTOR),
		[SoundType.IncomingRing]: i18n._(INCOMING_CALL_DESCRIPTOR),
		[SoundType.CameraOn]: i18n._(CAMERA_ON_DESCRIPTOR),
		[SoundType.CameraOff]: i18n._(CAMERA_OFF_DESCRIPTOR),
		[SoundType.ScreenShareStart]: i18n._(SCREEN_SHARE_START_DESCRIPTOR),
		[SoundType.ScreenShareStop]: i18n._(SCREEN_SHARE_STOP_DESCRIPTOR),
	};
}
