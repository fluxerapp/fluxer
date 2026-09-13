// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const YOU_CAN_T_JOIN_WHILE_YOU_RE_ON_DESCRIPTOR = msg({
	message: "You can't join while you're timed out.",
	comment: 'Toast / error shown when a user tries to join voice while under moderation timeout. Tone stays plain.',
});
export const CLAIM_YOUR_ACCOUNT_TO_JOIN_VOICE_CHANNELS_YOU_DESCRIPTOR = msg({
	message: "Claim your account to join voice channels you don't own.",
	comment: 'Toast / error shown when an unclaimed (guest) account tries to join a voice channel they do not own.',
});
export const CLAIM_YOUR_ACCOUNT_TO_START_OR_JOIN_1_DESCRIPTOR = msg({
	message: 'Claim your account to start or join 1:1 calls.',
	comment: 'Toast / error shown when an unclaimed (guest) account tries to start or join a 1:1 voice call.',
});
export const CLAIM_YOUR_ACCOUNT_TO_JOIN_THIS_VOICE_CHANNEL_DESCRIPTOR = msg({
	message: 'Claim your account to join this voice channel.',
	comment: 'Toast / error shown when an unclaimed (guest) account tries to join this voice channel.',
});
export const RECONNECT_SUCCEEDED_PICK_A_SCREEN_AGAIN_IF_YOU_DESCRIPTOR = msg({
	message: 'Reconnect succeeded. Pick a screen again if you want to resume your previous stream.',
	comment: 'Toast shown after a successful voice reconnect when an in-progress screen share could not be auto-resumed.',
});
export const VOICE_CHANNEL_NO_LONGER_AVAILABLE_DESCRIPTOR = msg({
	message: 'This voice channel is no longer available.',
	comment:
		'Toast / error shown when the user tries to join or restore a voice channel that was deleted or is no longer accessible.',
});
export const VOICE_CONNECTION_LIMIT_REACHED_DESCRIPTOR = msg({
	message: "You've reached this voice channel's connection limit.",
	comment:
		'Toast / error shown when the user tries to join voice with too many active connections in the same channel.',
});
export const VOICE_CONNECTION_FAILED_DESCRIPTOR = msg({
	message: "Couldn't connect to voice. Please try again.",
	comment: 'Toast shown when joining a voice channel fails because the connection timed out or the transport dropped.',
});
export const VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Camera limit reached — up to {voiceChannelCameraUserLimit} people can share video in this channel.',
	comment:
		'Error shown when enabling the camera is rejected because the channel already has the maximum number of users sharing video. {voiceChannelCameraUserLimit} is the configured cap.',
});
export const AFK_CHECK_INTERVAL_MS = 10000;
export const DEFERRED_DISCONNECT_TIMEOUT_MS = 5000;
