// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const YOU_CAN_T_JOIN_WHILE_YOU_RE_ON_DESCRIPTOR = msg({
	message: "You can't join while you're timed out.",
	comment: 'Toast / error shown when a user tries to join voice while under moderation timeout. Tone stays plain.',
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
	message: "You're at this voice channel's connection limit.",
	comment:
		'Toast / error shown when the user tries to join voice with too many active connections in the same channel.',
});
export const VOICE_E2EE_REQUIRED_DESCRIPTOR = msg({
	message: "This voice channel uses end-to-end encryption, and this browser can't join encrypted calls.",
	comment:
		'Error shown when someone tries to join an end-to-end encrypted voice channel from a browser that cannot encrypt calls.',
});
export const VOICE_E2EE_NEEDS_HTTPS_DESCRIPTOR = msg({
	message:
		"This voice channel uses end-to-end encryption. Browsers only allow encrypted calls on sites that use HTTPS, and this instance doesn't use HTTPS.",
	comment:
		'Error shown when someone tries to join an end-to-end encrypted voice channel on a self-hosted instance opened over plain HTTP. HTTPS is a technical term, keep it as is.',
});
export const VOICE_CONNECTION_FAILED_DESCRIPTOR = msg({
	message: "Couldn't connect to voice. Please try again.",
	comment: 'Toast shown when joining a voice channel fails because the connection timed out or the transport dropped.',
});
export const VOICE_CAMERA_USER_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'This channel is at its camera limit. Up to {voiceChannelCameraUserLimit} people can share video in it.',
	comment:
		'Error shown when enabling the camera is rejected because the channel already has the maximum number of users sharing video. {voiceChannelCameraUserLimit} is the configured cap.',
});
export const AFK_CHECK_INTERVAL_MS = 10000;
export const DEFERRED_DISCONNECT_TIMEOUT_MS = 5000;
