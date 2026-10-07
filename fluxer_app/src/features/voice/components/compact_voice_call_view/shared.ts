// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import {appZoomLayoutPx} from '@app/features/ui/utils/AppZoomUtils';
import {
	COMPACT_HEIGHT_MIN as COMPACT_HEIGHT_MIN_BOUND,
	getCompactHeightMax as getCompactHeightMaxFromBounds,
} from '@app/features/voice/components/compact_voice_call_view/CompactVoiceCallHeightBounds';
import {getCompactVoiceCallExpansionKey} from '@app/features/voice/state/CompactVoiceCallHeight';
import {msg} from '@lingui/core/macro';
import type React from 'react';

export const DISCONNECTED_DESCRIPTOR = msg({
	message: 'Disconnected',
	comment: 'Overlay status text in the compact / floating voice call tile when not connected.',
});
export const VOICE_CALL_DESCRIPTOR = msg({
	message: 'Voice call. {statusText}.',
	comment:
		'Aria label for the compact / floating voice call tile. {statusText} is one of the connection status strings.',
});
export const EXIT_FULLSCREEN_DESCRIPTOR = msg({
	message: 'Exit fullscreen',
	comment: 'Tooltip / aria label on the fullscreen toggle in the compact voice call tile (currently in fullscreen).',
});
export const ENTER_FULLSCREEN_DESCRIPTOR = msg({
	message: 'Enter fullscreen',
	comment: 'Tooltip / aria label on the fullscreen toggle in the compact voice call tile (not in fullscreen).',
});
export const RESIZE_CALL_VIEW_DESCRIPTOR = msg({
	message: 'Resize call view',
	comment: 'Aria label on the resize handle of the floating voice call tile.',
});

export interface CompactVoiceCallHeightToggle {
	isExpanded: boolean;
	onToggle: () => void;
	unreadCount?: number;
}

type CompactVoiceCallMediaMode = 'live' | 'placeholder';

export interface CompactVoiceCallViewProps {
	channel: Channel;
	className?: string;
	hideHeader?: boolean;
	hideControlBar?: boolean;
	controlBar?: React.ReactNode;
	avatarFallback?: React.ReactNode;
	showAvatarFallback?: boolean;
	audioOnly?: boolean;
	onFullscreenRequest?: () => void;
	fullscreenRequestNonce?: number;
	fillHeight?: boolean;
	reserveHeaderChrome?: boolean;
	heightToggle?: CompactVoiceCallHeightToggle;
	avatarFallbackFullBleed?: boolean;
	mediaMode?: CompactVoiceCallMediaMode;
}

export interface ResizeListeners {
	move: (event: PointerEvent) => void;
	up: (event: PointerEvent) => void;
}

export interface ResizeState {
	pointerId: number;
	startY: number;
	startHeight: number;
	dragging: boolean;
	lastHeight?: number;
}

export interface CompactCallMetrics {
	width: number;
	height: number;
	contentHeight: number;
	controlBarHeight: number;
}

export interface CompactVoiceCallContainerStyle extends React.CSSProperties {
	'--compact-call-width'?: string;
	'--compact-call-height'?: string;
	'--compact-call-content-height'?: string;
	'--compact-call-control-height'?: string;
	'--compact-call-audio-avatar-padding-top'?: string;
	'--compact-call-audio-avatar-padding-bottom'?: string;
	'--compact-call-edge-gradient-extension'?: string;
	'--compact-call-participant-count'?: string;
}

export const COMPACT_HEIGHT_DRAG_THRESHOLD_SQ = 9;
export const COMPACT_HEIGHT_MIN = COMPACT_HEIGHT_MIN_BOUND;
export const COMPACT_HEIGHT_STEP = 16;
export const VOICE_HUD_IDLE_TIMEOUT_MS = 2500;
const COMPACT_METRICS_CHANGE_EPSILON = 0.5;

export function toLayoutPx(value: number): number {
	if (!Number.isFinite(value)) {
		return 0;
	}
	return Math.max(0, appZoomLayoutPx(value));
}

export function hasCompactCallMetricsChanged(
	previousMetrics: CompactCallMetrics,
	nextMetrics: CompactCallMetrics,
): boolean {
	return (
		Math.abs(previousMetrics.width - nextMetrics.width) >= COMPACT_METRICS_CHANGE_EPSILON ||
		Math.abs(previousMetrics.height - nextMetrics.height) >= COMPACT_METRICS_CHANGE_EPSILON ||
		Math.abs(previousMetrics.contentHeight - nextMetrics.contentHeight) >= COMPACT_METRICS_CHANGE_EPSILON ||
		Math.abs(previousMetrics.controlBarHeight - nextMetrics.controlBarHeight) >= COMPACT_METRICS_CHANGE_EPSILON
	);
}

export function getCompactHeightKey(channelId: string, callMessageId: string | null) {
	return getCompactVoiceCallExpansionKey(channelId, callMessageId);
}

export function getCompactHeightMax(compactHeightMin: number): number {
	return getCompactHeightMaxFromBounds(compactHeightMin);
}
