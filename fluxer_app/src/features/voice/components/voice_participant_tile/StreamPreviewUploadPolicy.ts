// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	STREAM_PREVIEW_INITIAL_UPLOAD_INTERVAL_MS,
	STREAM_PREVIEW_INITIAL_UPLOAD_MAX_ATTEMPTS,
	STREAM_PREVIEW_UPLOAD_INTERVAL_MS,
	STREAM_PREVIEW_UPLOAD_JITTER_MS,
} from '@fluxer/constants/src/StreamConstants';

const STREAM_PREVIEW_UPLOAD_URL_REFRESH_SKEW_MS = 60_000;
const STREAM_PREVIEW_DEMAND_IDLE_POLL_MS = 5_000;

export interface StreamPreviewUploadUrlResponseLike {
	expires_at: string;
	expires_in: number;
}

export interface StreamPreviewUploadUrlCacheEntryLike {
	streamKey: string;
	expiresAtMs: number;
}

export interface StreamPreviewUploadDecisionInput {
	now: number;
	hasUploadedOnce: boolean;
	initialAttempts: number;
	hasSpectatorDemand: boolean;
	previewsDisabled: boolean;
}

export interface StreamPreviewUploadDecision {
	action: 'upload' | 'wait';
	nextDelayMs: number;
}

export function getUploadUrlExpiresAtMs(response: StreamPreviewUploadUrlResponseLike, now: number): number {
	const expiresAtMs = Date.parse(response.expires_at);
	if (Number.isFinite(expiresAtMs)) return expiresAtMs;
	return now + response.expires_in * 1000;
}

export function isUploadUrlFresh(
	entry: StreamPreviewUploadUrlCacheEntryLike | null,
	streamKey: string,
	now: number,
	skewMs: number = STREAM_PREVIEW_UPLOAD_URL_REFRESH_SKEW_MS,
): entry is StreamPreviewUploadUrlCacheEntryLike {
	return entry !== null && entry.streamKey === streamKey && entry.expiresAtMs - now > skewMs;
}

function isWithinBootstrap(input: StreamPreviewUploadDecisionInput): boolean {
	return !input.hasUploadedOnce && input.initialAttempts < STREAM_PREVIEW_INITIAL_UPLOAD_MAX_ATTEMPTS;
}

export class StreamPreviewUploadScheduler {
	constructor(private readonly random: () => number = Math.random) {}

	private steadyDelayMs(): number {
		const jitter = Math.round((this.random() * 2 - 1) * STREAM_PREVIEW_UPLOAD_JITTER_MS);
		return STREAM_PREVIEW_UPLOAD_INTERVAL_MS + jitter;
	}

	decide(input: StreamPreviewUploadDecisionInput): StreamPreviewUploadDecision {
		if (input.previewsDisabled) {
			return {action: 'wait', nextDelayMs: STREAM_PREVIEW_DEMAND_IDLE_POLL_MS};
		}
		if (isWithinBootstrap(input)) {
			return {action: 'upload', nextDelayMs: STREAM_PREVIEW_INITIAL_UPLOAD_INTERVAL_MS};
		}
		if (!input.hasSpectatorDemand) {
			return {action: 'wait', nextDelayMs: STREAM_PREVIEW_DEMAND_IDLE_POLL_MS};
		}
		return {action: 'upload', nextDelayMs: this.steadyDelayMs()};
	}
}
