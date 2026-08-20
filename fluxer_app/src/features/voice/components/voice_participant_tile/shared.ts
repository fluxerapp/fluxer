// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {msg} from '@lingui/core/macro';
import type {TrackReferenceOrPlaceholder} from '@livekit/components-react';
import type React from 'react';

export const logger = new Logger('VoiceParticipantTile');
export const TILE_AVATAR_BASE = 192;
export const TILE_AVATAR_STYLE = {
	width: 'var(--tile-avatar-size)',
	height: 'var(--tile-avatar-size)',
} satisfies React.CSSProperties;
export const UNMUTE_STREAM_AUDIO_DESCRIPTOR = msg({
	message: 'Unmute stream audio',
	comment: 'Tooltip / button label on a voice participant tile that unmutes the remote screen-share or camera audio.',
});
export const MUTE_STREAM_AUDIO_DESCRIPTOR = msg({
	message: 'Mute stream audio',
	comment: 'Tooltip / button label on a voice participant tile that mutes the remote screen-share or camera audio.',
});
export const STREAM_ENDED_DESCRIPTOR = msg({
	message: 'Stream ended',
	comment: 'Status overlay on a voice participant tile when the remote screen share has ended.',
});
export const STREAM_HIDDEN_DESCRIPTOR = msg({
	message: 'Stream hidden',
	comment: 'Overlay text on a participant tile when the local user hid the remote screen share.',
});
export const STREAM_BUFFERING_DESCRIPTOR = msg({
	message: 'Stream buffering',
	comment: 'Accessible status label for the black buffering frame shown while a screen share codec is renegotiated.',
});
export const WATCHING_FAILED_DESCRIPTOR = msg({
	message: 'Watching failed :(',
	comment: 'Title on a remote screen-share tile when loading the watched stream failed.',
});
export const WATCHING_FAILED_ERROR_CODE_DESCRIPTOR = msg({
	message: 'error code {code}',
	comment:
		'Subtitle on a remote screen-share tile after loading the watched stream failed. {code} is a numeric diagnostic code.',
});
export const CAMERA_HIDDEN_DESCRIPTOR = msg({
	message: 'Camera hidden',
	comment: 'Overlay text on a participant tile when the local user hid the remote camera feed.',
});
export const CAMERA_BUFFERING_DESCRIPTOR = msg({
	message: 'Camera loading',
	comment: 'Accessible status label while a voice participant camera feed is starting before video is renderable.',
});
export const SHOW_CAMERA_DESCRIPTOR = msg({
	message: 'Show camera',
	comment: 'Action label that reveals a previously hidden remote camera feed.',
});
export const PREVIEW_PAUSED_TO_SAVE_RESOURCES_DESCRIPTOR = msg({
	message: 'Preview paused to save resources',
	comment:
		'Overlay on the local screen-share preview when playback is paused to reduce CPU / battery use. Local share is still being sent to others.',
});
export const YOUR_STREAM_IS_STILL_LIVE_DESCRIPTOR = msg({
	message: 'Your stream is still live',
	comment:
		'Subtitle under the preview-paused overlay reassuring the user that the screen share is still live for others.',
});
export const WATCH_DESCRIPTOR = msg({
	message: 'Watch',
	comment: 'Compact button label on a participant tile. Joins / opens the remote screen share.',
});
export const WATCHING_DESCRIPTOR = msg({
	message: '{length} watching',
	comment:
		'Spectator count badge on a screen-share tile. {length} is the integer number of viewers. Consider pluralization on review.',
});
export const MUTED_DESCRIPTOR = msg({
	message: 'Muted',
	comment: "Status badge on a voice participant tile. The participant's microphone is muted.",
});
export const MOBILE_DEVICE_DESCRIPTOR = msg({
	message: 'Mobile device',
	comment: 'Aria label on a participant tile icon that indicates the participant is on mobile.',
});
export const DESKTOP_DEVICE_DESCRIPTOR = msg({
	message: 'Desktop device',
	comment: 'Aria label on a participant tile icon that indicates the participant is on desktop.',
});
export const CONNECTION_DESCRIPTOR = msg({
	message: 'Connection: {connectionId}',
	comment: 'Developer / debug overlay text on a participant tile. {connectionId} is the voice connection identifier.',
});
export const PARTICIPANT_OPTIONS_FOR_DESCRIPTOR = msg({
	message: 'Participant options for {participantDisplayName}',
	comment:
		"Aria label for the participant context-menu trigger button. {participantDisplayName} is the participant's display name.",
});

export type VoiceParticipantTilePresentation = 'grid' | 'focus-main' | 'focus-secondary';

export interface VoiceParticipantTileProps {
	trackRef?: TrackReferenceOrPlaceholder;
	guildId?: string;
	channelId?: string;
	onClick?: (participantIdentity: string) => void;
	isPinned?: boolean;
	showFocusIndicator?: boolean;
	allowAutoSubscribe?: boolean;
	renderFocusedPlaceholder?: boolean;
	presentation?: VoiceParticipantTilePresentation;
	showParticipantMetadata?: boolean;
}

export interface VoiceParticipantTileInnerProps {
	trackRef: TrackReferenceOrPlaceholder;
	elementProps: React.HTMLAttributes<HTMLElement>;
	guildId?: string;
	channelId?: string;
	onClick?: (participantIdentity: string) => void;
	isPinned?: boolean;
	showFocusIndicator?: boolean;
	allowAutoSubscribe: boolean;
	renderFocusedPlaceholder: boolean;
	presentation: VoiceParticipantTilePresentation;
	showParticipantMetadata: boolean;
}

export function isCameraSource(source: unknown) {
	return source === VoiceTrackSource.Camera;
}

export function getSourceDataAttr(source: unknown) {
	switch (source) {
		case VoiceTrackSource.ScreenShare:
			return 'screen_share';
		case VoiceTrackSource.Camera:
			return 'camera';
		default:
			return 'other';
	}
}
