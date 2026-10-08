// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import type {User} from '@app/features/user/models/User';
import type {VoiceParticipantAvatarEntry} from '@app/features/voice/components/VoiceParticipantAvatarList';
import type {Call} from '@app/features/voice/state/CallState';
import {
	COMPACT_VOICE_CALL_HEIGHT_MAX,
	getCompactVoiceCallExpansionKey,
} from '@app/features/voice/state/CompactVoiceCallHeight';
import {msg} from '@lingui/core/macro';

export const logger = new Logger('DMChannelView');
const CALL_AVATAR_MIN_SIZE = 40;
export const CALL_AVATAR_DEFAULT_SIZE = 64;
const CALL_AVATAR_MAX_SIZE = 88;
const CALL_AVATAR_MIN_GAP = 8;
const CALL_AVATAR_MAX_GAP = 16;
export const CALL_AVATAR_SPRING = {stiffness: 520, damping: 34, mass: 0.6} as const;
export const COMPACT_CALL_RESIZE_DRAG_THRESHOLD_SQ = 9;
const COMPACT_CALL_RESIZE_VIEWPORT_MARGIN = 32;
export const COMPACT_CALL_RESIZE_STEP = 16;

export interface CallParticipant {
	user: User;
	isRinging: boolean;
}

export interface CallParticipantsRowProps {
	call: Call;
	channel: Channel;
	participantAvatarEntries: ReadonlyArray<VoiceParticipantAvatarEntry>;
	className?: string;
}

export interface CompactCallResizeListeners {
	move: (event: PointerEvent) => void;
	up: (event: PointerEvent) => void;
}

export interface CompactCallResizeState {
	pointerId: number;
	startY: number;
	startHeight: number;
	dragging: boolean;
	lastHeight?: number;
}

export type CallControlRenderMode = 'mobile' | 'voiceControlBar';

export interface CallParticipantLayoutMetrics {
	avatarSize: number;
	gap: number;
}

export function getCompactCallHeightKey(channelId: string, callMessageId: string | null): string {
	return getCompactVoiceCallExpansionKey(channelId, callMessageId);
}

export function getCompactCallHeightMax(compactHeightMin: number): number {
	const viewportLimitedMax = Math.round(appZoomLayoutPx(window.innerHeight) - COMPACT_CALL_RESIZE_VIEWPORT_MARGIN);
	return Math.max(compactHeightMin, Math.min(viewportLimitedMax, COMPACT_VOICE_CALL_HEIGHT_MAX));
}

export function getCallParticipantLayoutMetrics(
	count: number,
	width: number,
	height: number,
): CallParticipantLayoutMetrics {
	if (count <= 0 || width <= 0 || height <= 0) {
		return {avatarSize: CALL_AVATAR_DEFAULT_SIZE, gap: 12};
	}
	const availableWidth = Math.max(CALL_AVATAR_MIN_SIZE, width);
	const availableHeight = Math.max(CALL_AVATAR_MIN_SIZE, height);
	const baseGap = Math.round(Math.min(availableWidth, availableHeight) / 18);
	const gap = Math.max(CALL_AVATAR_MIN_GAP, Math.min(CALL_AVATAR_MAX_GAP, baseGap));
	let bestSize = CALL_AVATAR_MIN_SIZE;
	for (let columns = 1; columns <= count; columns += 1) {
		const rows = Math.ceil(count / columns);
		const sizeByWidth = (availableWidth - gap * Math.max(0, columns - 1)) / columns;
		const sizeByHeight = (availableHeight - gap * Math.max(0, rows - 1)) / rows;
		const candidateSize = Math.floor(Math.min(CALL_AVATAR_MAX_SIZE, sizeByWidth, sizeByHeight));
		if (candidateSize > bestSize) {
			bestSize = candidateSize;
		}
	}
	return {
		avatarSize: Math.max(CALL_AVATAR_MIN_SIZE, Math.min(CALL_AVATAR_MAX_SIZE, bestSize)),
		gap,
	};
}

export const CALL_PARTICIPANTS_DESCRIPTOR = msg({
	message: 'Call participants',
	comment:
		'Accessible label for the floating call participants strip shown above the DM message list during an active call.',
});
export const CALL_AVAILABLE_DESCRIPTOR = msg({
	message: 'Call available',
	comment: 'Status text in the DM channel header pill when an ongoing call exists that the user can join.',
});
export const CONNECTING_DESCRIPTOR = msg({
	message: 'Connecting…',
	comment:
		'Status text in the DM channel header pill while joining a call. Trailing ellipsis (typographically) is intentional.',
});
export const IN_CALL_DESCRIPTOR = msg({
	message: 'In call',
	comment: 'Status text in the DM channel header pill while the user is actively in the call on this device.',
});
export const IN_CALL_ON_ANOTHER_DEVICE_DESCRIPTOR = msg({
	message: 'In call on another device',
	comment: 'Status text in the DM channel header pill while the user is in the same call from another device.',
});
export const VIEW_INCOMING_CALL_DESCRIPTOR = msg({
	message: 'View incoming call',
	comment: 'Action label in the DM channel header pill that opens the incoming call ringer view.',
});
export const VIEW_CALL_DESCRIPTOR = msg({
	message: 'View call',
	comment: 'Action label in the DM channel header pill that opens an existing call without joining.',
});
export const JOIN_CALL_DESCRIPTOR = msg({
	message: 'Join call',
	comment: 'Action label in the DM channel header pill that joins an ongoing call.',
});
export const CONNECTING_2_DESCRIPTOR = msg({
	message: 'Connecting...',
	comment:
		'Action label state in the DM channel header pill while the join request is in flight. Trailing ASCII ellipsis is intentional.',
});
export const JOIN_ON_THIS_DEVICE_DESCRIPTOR = msg({
	message: 'Join on this device',
	comment:
		'Action label in the DM channel header pill that switches an in-progress call from another device to this device.',
});
export const RESIZE_CALL_VIEW_DESCRIPTOR = msg({
	message: 'Resize call view',
	comment: 'Short label in the channel and chat dm channel view. Keep it concise.',
});
