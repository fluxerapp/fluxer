// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';

export const WATCH_ATTEMPT_TIMEOUT_MS = 15000;
export const PUBLISHER_REPUBLISH_GRACE_MS = 4000;
export const PUBLICATION_MISSING_TIMEOUT_MS = 15000;

type VoiceMediaGraphDeadlineKind = 'watchAttempt' | 'deferredStop' | 'publicationMissing';

export interface VoiceMediaGraphDeadline {
	kind: VoiceMediaGraphDeadlineKind;
	streamKey: string | null;
	subscriptionKey: string | null;
	generation: number;
	attemptKey: string | null;
	dueAt: number;
}

export interface VoiceMediaGraphTimeoutFailureDescriptor {
	code: number;
	reason: string;
}

export const VOICE_MEDIA_GRAPH_PUBLICATION_MISSING_TIMEOUT_FAILURE: VoiceMediaGraphTimeoutFailureDescriptor = {
	code: -2301,
	reason: 'publication-missing-timeout',
};

export const VOICE_MEDIA_GRAPH_SUBSCRIPTION_ATTACH_TIMEOUT_FAILURE: VoiceMediaGraphTimeoutFailureDescriptor = {
	code: -2302,
	reason: 'subscription-attach-timeout',
};

export const VOICE_MEDIA_GRAPH_FIRST_FRAME_TIMEOUT_FAILURE: VoiceMediaGraphTimeoutFailureDescriptor = {
	code: -2303,
	reason: 'first-frame-timeout',
};

export const VOICE_MEDIA_GRAPH_REPUBLISH_TIMEOUT_FAILURE: VoiceMediaGraphTimeoutFailureDescriptor = {
	code: -2304,
	reason: 'republish-timeout',
};

export function voiceMediaGraphWatchAttemptDeadlineKey(streamKey: string): string {
	assert.ok(streamKey.length > 0, 'streamKey is required');
	return `watchAttempt:${streamKey}`;
}

export function voiceMediaGraphDeferredStopDeadlineKey(streamKey: string): string {
	assert.ok(streamKey.length > 0, 'streamKey is required');
	return `deferredStop:${streamKey}`;
}

export function voiceMediaGraphPublicationMissingDeadlineKey(subscriptionKey: string): string {
	assert.ok(subscriptionKey.length > 0, 'subscriptionKey is required');
	return `publicationMissing:${subscriptionKey}`;
}
