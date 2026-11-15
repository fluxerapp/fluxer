// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const GAMING_DESCRIPTOR = msg({
	message: 'Gaming',
	comment: 'Streaming preset label in the stream settings menu. Higher frame rate, optimized for video games.',
});

export const SCREENSHARE_DESCRIPTOR = msg({
	message: 'Screen share',
	comment: 'Streaming preset label in the stream settings menu. Optimized for sharp text in screen shares.',
});

export const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Streaming preset label in the stream settings menu. Lets user pick resolution and frame rate manually.',
});

export const SCREEN_SHARE_STATUS_SOURCE_RESOLUTION_DESCRIPTOR = msg({
	message: 'Source',
	comment:
		'Screen share resolution token in the stream info pill on the sharing tile, used when the person asked for the original source resolution and the app does not know the source size. Matches the Source option in the stream settings menu.',
});

export const SYSTEM_DEFAULT_DESCRIPTOR = msg({
	message: 'System default',
	comment: 'Audio device option label meaning the OS default audio input device.',
});

export const UNNAMED_INPUT_DESCRIPTOR = msg({
	message: 'Unnamed input',
	comment: 'Fallback label for an audio input device whose name is not reported by the OS.',
});

export const VIEW_PROFILE_FOR_DESCRIPTOR = msg({
	message: 'View profile for {displayName}',
	comment:
		"Aria label on an avatar tile in the voice participants list. {displayName} is the participant's display name.",
});

export const WATCHING_STREAM_DESCRIPTOR = msg({
	message: 'Watching stream',
	comment: 'Status label on the screen-share hover card when the local user is already watching the stream.',
});

export const ADD_STREAM_DESCRIPTOR = msg({
	message: 'Add stream',
	comment: 'Action button on the screen-share hover card. Starts a new local screen share.',
});

export const DISCONNECTED_DESCRIPTOR = msg({
	message: 'Disconnected',
	comment: 'Overlay status text in the compact / floating voice call tile when not connected.',
});

export const EXIT_FULLSCREEN_DESCRIPTOR = msg({
	message: 'Exit fullscreen',
	comment: 'Tooltip / aria label on the fullscreen toggle in the compact voice call tile (currently in fullscreen).',
});

export const ENTER_FULLSCREEN_DESCRIPTOR = msg({
	message: 'Enter fullscreen',
	comment: 'Tooltip / aria label on the fullscreen toggle in the compact voice call tile (not in fullscreen).',
});

export const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute',
	context: 'voice-control-action',
	comment: 'Mic toggle button label in the mobile direct-call lobby bottom sheet (currently muted).',
});

export const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	context: 'voice-control-action',
	comment: 'Mic toggle button label in the mobile direct-call lobby bottom sheet (currently unmuted).',
});

export const YOU_DON_T_HAVE_PERMISSION_TO_TURN_ON_DESCRIPTOR = msg({
	message: "You can't turn on your camera in this channel",
	comment:
		'Tooltip / error shown in the camera preview modal when the user lacks Video permission in the current channel. Tone stays plain.',
});

export const NO_SCREEN_SHARE_PERMISSION_DESCRIPTOR = msg({
	message: "You can't share your screen in this channel",
	comment:
		'Tooltip in the voice control bar when the user lacks the Screen Share permission in this channel. Tone stays plain.',
});

export const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback label for a voice participant tile when no user or participant name is available.',
});

export const CUSTOM_DESCRIPTOR_2 = msg({
	message: 'Custom',
	comment: 'Voice input processing profile where the user configures processing manually.',
	context: 'voice-processing-profile',
});

export const MICROPHONE_DESCRIPTOR = msg({
	message: 'Microphone',
	comment: 'Screen-share audio summary on a video device share whose audio comes from an unnamed microphone.',
});

export const SPEAKER_2_DESCRIPTOR = msg({
	message: 'Speaker',
	comment: 'Fallback speaker label in the voice settings menu when the OS does not report a device name.',
});

export const DEFAULT_3_DESCRIPTOR = msg({
	message: 'Default',
	context: 'device-option',
	comment:
		'Default device option label in the voice settings menu (alternate call site for the same concept as DEFAULT_DESCRIPTOR).',
});

export const VOICE_SETTINGS_DESCRIPTOR = msg({
	message: 'Voice settings',
	comment: 'Title of the mobile voice settings bottom sheet.',
});

export const VIDEO_SETTINGS_DESCRIPTOR = msg({
	message: 'Video settings',
	comment: 'Title of the mobile video settings bottom sheet.',
});

export const MIRROR_CAMERA_DESCRIPTOR = msg({
	message: 'Mirror camera',
	comment: 'Switch label in the camera preview modal for flipping the local camera preview horizontally.',
});

export const CONNECTING_DESCRIPTOR = msg({
	message: 'Connecting…',
	comment:
		'Status label in the mobile direct-call lobby bottom sheet while joining the call. Trailing ellipsis indicates in-progress.',
});

export const OPEN_CALL_VIEW_DESCRIPTOR = msg({
	message: 'Open call view',
	comment:
		'Primary CTA button in the mobile direct-call lobby bottom sheet when already in the call. Opens the full call UI.',
});

export const LEAVE_CALL_DESCRIPTOR = msg({
	message: 'Leave call',
	comment: 'Destructive button in the mobile direct-call lobby bottom sheet. Disconnects from the call.',
});

export const CAMERA_OFF_DESCRIPTOR = msg({
	message: 'Camera off',
	comment: 'Camera toggle button label in the mobile direct-call lobby bottom sheet (currently on).',
});

export const PING_DESCRIPTOR = msg({
	message: 'Ping',
	comment: 'Voice stats field label (mobile voice lobby). Shows current latency in ms.',
});

export const ENDPOINT_DESCRIPTOR = msg({
	message: 'Endpoint',
	comment: 'Voice stats field label (mobile voice lobby). Shows the voice server endpoint hostname.',
});

export const CONNECTION_ID_DESCRIPTOR = msg({
	message: 'Connection ID',
	comment: 'Voice stats field label (mobile lobby). Shows the voice connection identifier. Developer / debug surface.',
});

export const PACKET_LOSS_DESCRIPTOR = msg({
	message: 'Packet loss',
	comment: 'Voice stats field label (mobile voice lobby). Shows packet loss percentage.',
});

export const JITTER_DESCRIPTOR = msg({
	message: 'Jitter',
	comment: 'Voice stats field label (mobile voice lobby). Shows network jitter in ms.',
});

export const UNMUTE_DESCRIPTOR_2 = msg({
	message: 'Unmute',
	context: 'playback-control-action',
	comment: 'Mute toggle button label in the media player volume control (currently muted).',
});

export const MUTE_DESCRIPTOR_2 = msg({
	message: 'Mute',
	context: 'playback-control-action',
	comment: 'Mute toggle button label in the media player volume control (currently unmuted).',
});

export const VOLUME_CONTROL_DESCRIPTOR = msg({
	message: 'Volume control',
	comment: 'Aria label for the volume control container in the media player.',
});

export const WINDOWS_APP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR = msg({
	message: "{productName} cannot capture only one app's audio here without risking unrelated app audio or call audio.",
	comment:
		'Inline notice body on Windows. Explains that app audio is disabled because productName cannot guarantee isolated audio capture on this OS build.',
});

export const WINDOWS_DESKTOP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR = msg({
	message: "{productName} cannot capture desktop audio here while excluding {productName}'s call audio.",
	comment:
		'Inline notice body on Windows. Explains that desktop audio is disabled because productName cannot exclude its own WebRTC/call playback on this OS build.',
});

export const POP_BACK_IN_DESCRIPTOR = msg({
	message: 'Pop back in',
	comment: 'Button label in a popped-out voice window titlebar that restores the content into the main app window.',
});
