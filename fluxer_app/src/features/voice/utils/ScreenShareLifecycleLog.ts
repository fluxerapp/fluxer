// SPDX-License-Identifier: AGPL-3.0-or-later

import ActiveScreenShareSource, {
	type PublishedScreenShareSource,
} from '@app/features/voice/state/ActiveScreenShareSource';
import type {VideoCodec} from 'livekit-client';

const MAX_RETAINED_SCREEN_SHARES = 8;

export type ScreenShareStopTrigger =
	| 'user'
	| 'media-track-ended'
	| 'server-unpublish'
	| 'gateway-echo'
	| 'codec-republish-failed';

export type ScreenShareEndedModal = 'source-stopped' | 'encoder-failed' | 'codec-policy-failed';

export type ScreenShareEncoderVerification =
	| 'recover-stalled'
	| 'ignore-repeated-stall'
	| 'accept-negotiated'
	| 'correct-negotiated';

export interface ScreenShareLifecycleEntry {
	startedAt: number;
	sourceKind: PublishedScreenShareSource | null;
	requestedCodec: VideoCodec | null;
	negotiatedCodecs: ReadonlyArray<VideoCodec> | null;
	encoderVerification: ScreenShareEncoderVerification | null;
	startError: string | null;
	stoppedAt: number | null;
	stopTrigger: ScreenShareStopTrigger | null;
	modalShown: ScreenShareEndedModal | null;
}

let entries: Array<ScreenShareLifecycleEntry> = [];

function getOpenEntry(): ScreenShareLifecycleEntry | null {
	const entry = entries.at(-1);
	if (!entry || entry.stoppedAt != null || entry.startError != null) return null;
	return entry;
}

export function recordScreenShareStarted(): void {
	entries.push({
		startedAt: Date.now(),
		sourceKind: null,
		requestedCodec: null,
		negotiatedCodecs: null,
		encoderVerification: null,
		startError: null,
		stoppedAt: null,
		stopTrigger: null,
		modalShown: null,
	});
	if (entries.length > MAX_RETAINED_SCREEN_SHARES) {
		entries = entries.slice(-MAX_RETAINED_SCREEN_SHARES);
	}
}

export function recordScreenShareStartError(error: unknown): void {
	const entry = getOpenEntry();
	if (!entry) return;
	entry.sourceKind = ActiveScreenShareSource.getPublishedSource();
	entry.startError = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
}

export function recordScreenShareRequestedCodec(codec: VideoCodec): void {
	const entry = getOpenEntry();
	if (!entry) return;
	entry.requestedCodec = codec;
}

export function recordScreenShareEncoderVerification(
	verification: ScreenShareEncoderVerification,
	negotiatedCodecs: ReadonlyArray<VideoCodec> | null,
): void {
	const entry = getOpenEntry();
	if (!entry) return;
	entry.encoderVerification = verification;
	entry.negotiatedCodecs = negotiatedCodecs;
}

export function recordScreenShareEndedModal(modal: ScreenShareEndedModal): void {
	const entry = getOpenEntry();
	if (!entry) return;
	entry.modalShown = modal;
}

export function recordScreenShareStopped(trigger: ScreenShareStopTrigger): void {
	const entry = getOpenEntry();
	if (!entry) return;
	entry.sourceKind = ActiveScreenShareSource.getPublishedSource();
	entry.stoppedAt = Date.now();
	entry.stopTrigger = trigger;
}

export function getRecentScreenShares(): Array<ScreenShareLifecycleEntry> {
	const open = getOpenEntry();
	return entries.map((entry) => ({
		...entry,
		sourceKind: entry === open ? ActiveScreenShareSource.getPublishedSource() : entry.sourceKind,
	}));
}

export function resetRecentScreenSharesForTests(): void {
	entries = [];
}
