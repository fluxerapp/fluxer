// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	selectVoiceMediaGraphAttempt,
	selectVoiceMediaGraphFailure,
	selectVoiceMediaGraphSubscriptionEntry,
	type VoiceMediaGraphFailure,
	type VoiceMediaGraphSnapshot,
	type VoiceMediaGraphSubscriptionEntry,
} from './VoiceMediaGraph';
import {VOICE_MEDIA_GRAPH_FIRST_FRAME_TIMEOUT_FAILURE} from './VoiceMediaGraphDeadlines';
import type {VoiceTrackSource} from './VoiceTrackSource';

export const VOICE_MEDIA_GRAPH_FIRST_FRAME_RECOVERY_VISIBLE_MS = 30_000;

export type VoiceMediaGraphStreamTileState =
	| 'idle'
	| 'watchDesired'
	| 'publicationMissing'
	| 'attaching'
	| 'subscribedAwaitingFrame'
	| 'rendering'
	| 'recovering'
	| 'failed';

export interface VoiceMediaGraphStreamTileTarget {
	streamKey: string | null;
	participantIdentity: string | null;
	source: VoiceTrackSource;
}

export interface VoiceMediaGraphStreamTileRecovery {
	hasRecoveryBudget: boolean;
	nowMs: number;
}

function selectTileFailure(
	snapshot: VoiceMediaGraphSnapshot,
	target: VoiceMediaGraphStreamTileTarget,
): VoiceMediaGraphFailure | null {
	if (!target.streamKey && !target.participantIdentity) return null;
	return selectVoiceMediaGraphFailure(snapshot, {
		streamKey: target.streamKey,
		participantIdentity: target.participantIdentity,
		source: target.source,
	});
}

function selectFailureTileState(
	failure: VoiceMediaGraphFailure,
	recovery: VoiceMediaGraphStreamTileRecovery | undefined,
): VoiceMediaGraphStreamTileState {
	if (!recovery) return 'failed';
	if (failure.code !== VOICE_MEDIA_GRAPH_FIRST_FRAME_TIMEOUT_FAILURE.code) return 'failed';
	if (!recovery.hasRecoveryBudget) return 'failed';
	if (recovery.nowMs - failure.reportedAt >= VOICE_MEDIA_GRAPH_FIRST_FRAME_RECOVERY_VISIBLE_MS) return 'failed';
	return 'recovering';
}

function tileIsRendering(
	snapshot: VoiceMediaGraphSnapshot,
	target: VoiceMediaGraphStreamTileTarget,
	entry: VoiceMediaGraphSubscriptionEntry | null,
): boolean {
	if (entry !== null && entry.firstFrame.renderedAt !== null) return true;
	if (!target.streamKey) return false;
	const attempt = selectVoiceMediaGraphAttempt(snapshot, target.streamKey);
	return attempt?.hasRenderedVideoFrame ?? false;
}

export function selectVoiceMediaGraphStreamTileState(
	snapshot: VoiceMediaGraphSnapshot,
	target: VoiceMediaGraphStreamTileTarget,
	recovery?: VoiceMediaGraphStreamTileRecovery,
): VoiceMediaGraphStreamTileState {
	const entry = target.participantIdentity
		? selectVoiceMediaGraphSubscriptionEntry(snapshot, target.participantIdentity, target.source)
		: null;
	const failure = selectTileFailure(snapshot, target);
	if (failure !== null) return selectFailureTileState(failure, recovery);
	if (tileIsRendering(snapshot, target, entry)) return 'rendering';
	if (entry?.actual.lastError) return 'failed';
	if (entry?.actual.subscribed === true) return 'subscribedAwaitingFrame';
	if (entry?.publication.available) return 'attaching';
	if (entry) return 'publicationMissing';
	if (target.streamKey && snapshot.watchIntent.viewerStreamKeys.includes(target.streamKey)) return 'watchDesired';
	return 'idle';
}
