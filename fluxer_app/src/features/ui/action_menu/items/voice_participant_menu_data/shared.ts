// SPDX-License-Identifier: AGPL-3.0-or-later

export {BLOCKED_USER_DM_WARNING_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';

import {msg} from '@lingui/core/macro';

export const COPY_DEVICE_ID_DESCRIPTOR = msg({
	message: 'Copy device ID',
	comment: 'Developer-mode voice menu action that copies the unique identifier of a voice device or connection.',
});
export const CONNECTION_VOLUME_DESCRIPTOR = msg({
	message: 'Connection volume',
	comment:
		'Voice menu slider label that controls volume for a single device or connection of the selected participant.',
});
export const STREAM_VOLUME_DESCRIPTOR = msg({
	message: 'Stream volume',
	comment: 'Voice menu slider label that controls volume of an incoming screen share with audio.',
});
export const STOP_STREAMING_DESCRIPTOR = msg({
	message: 'Stop streaming',
	comment: 'Voice screen share menu action that stops the current user stream.',
});
export const CHANGE_STREAM_DESCRIPTOR = msg({
	message: 'Change stream',
	comment: 'Voice screen share menu action that changes the shared source.',
});
export const PAUSE_OWN_STREAM_PREVIEW_DESCRIPTOR = msg({
	message: 'Pause preview when Fluxer isn’t focused',
	comment: 'Voice screen share menu preference that pauses the local stream preview while the app is unfocused.',
});
export const SCREEN_SHARE_PRIVACY_DESCRIPTOR = msg({
	message: 'Screen share privacy',
	comment: 'Voice screen share menu action that opens the screen share preview privacy controls.',
});
export const MUTE_DESCRIPTOR = msg({
	message: 'Mute',
	comment:
		'Voice menu toggle label. Mutes the current user microphone or, for a stream, mutes that stream audio locally.',
});
export const UNFOCUS_DESCRIPTOR = msg({
	message: 'Unfocus',
	comment: 'Voice call layout action that removes focus from the currently pinned participant.',
});
export const POP_OUT_CAMERA_DESCRIPTOR = msg({
	message: 'Pop out video',
	comment: 'Voice menu action on desktop that opens the active participant camera feed in a separate window.',
});
export const POP_OUT_USER_DESCRIPTOR = msg({
	message: 'Pop out user',
	comment: 'Voice menu action on desktop that opens the participant placeholder tile in a separate window.',
});
export const PREVIEW_CAMERA_DESCRIPTOR = msg({
	message: 'Preview camera',
	comment: 'Voice menu action that opens a preview of the current user camera.',
});
export const POP_OUT_STREAM_DESCRIPTOR = msg({
	message: 'Pop out stream',
	comment: 'Voice menu action on desktop that opens the participant screen share in a separate window.',
});
export const FOCUS_THIS_DEVICE_DESCRIPTOR = msg({
	message: 'Focus this device',
	comment: 'Voice call layout action that pins one of several active devices of the same participant.',
});
export const FOCUS_THIS_PERSON_DESCRIPTOR = msg({
	message: 'Focus this person',
	comment: 'Voice call layout action that pins the selected participant in the focused view.',
});
export const MENTION_DESCRIPTOR = msg({
	message: 'Mention',
	comment: 'Voice participant menu action that inserts a mention of the selected user into the message composer.',
});
export const MESSAGE_DESCRIPTOR = msg({
	message: 'Message',
	comment: 'Voice participant menu action that opens a DM conversation with the selected user.',
});
export const MUTE_DEVICE_DESCRIPTOR = msg({
	message: 'Mute device',
	comment: 'Voice menu toggle that mutes a single device when the current user is connected on multiple devices.',
});
export const TURN_OFF_DEVICE_CAMERA_DESCRIPTOR = msg({
	message: 'Turn off device camera',
	comment: 'Voice menu action that turns off the camera on a single device of the current user.',
});
export const TURN_OFF_DEVICE_STREAM_DESCRIPTOR = msg({
	message: 'Turn off device stream',
	comment: 'Voice menu action that ends the screen share on a single device of the current user.',
});
export const TURN_OFF_STREAM_DESCRIPTOR = msg({
	message: 'Turn off stream',
	comment: 'Voice menu action that ends the current user screen share.',
});
export const SHOW_MY_OWN_CAMERA_DESCRIPTOR = msg({
	message: 'Show my own camera',
	comment: 'Voice display preference checkbox that controls whether the current user sees their own camera tile.',
});
export const SHOW_MY_SCREEN_SHARE_DESCRIPTOR = msg({
	message: 'Show my screen share',
	comment: 'Voice display preference checkbox that controls whether the current user sees their own screen share tile.',
});
export const SHOW_NON_VIDEO_PARTICIPANTS_DESCRIPTOR = msg({
	message: 'Show non-video participants',
	comment: 'Voice display preference checkbox that controls whether participants without video are shown in the grid.',
});
export const PRIORITIZE_SPEAKERS_DESCRIPTOR = msg({
	message: 'Prioritize speakers',
	comment:
		'Voice display preference checkbox that moves active speakers toward the front of the call grid when enabled.',
});
export const DISABLE_VIDEO_LOCALLY_DESCRIPTOR = msg({
	message: 'Disable video locally',
	comment: 'Voice menu checkbox that hides a participant video on the current device only.',
});
export const TURN_OFF_ALL_DEVICE_CAMERAS_DESCRIPTOR = msg({
	message: 'Turn off all device cameras',
	comment: 'Bulk voice action that turns off the camera on every active device of the current user.',
});
export const BAN_MEMBER_DESCRIPTOR = msg({
	message: 'Ban member',
	comment: 'Moderation action that bans the selected member from the community.',
});
export const COLLAPSE_DEVICES_DESCRIPTOR = msg({
	message: 'Collapse devices',
	comment: 'Voice menu action that collapses expanded device tiles back into a single tile.',
});
