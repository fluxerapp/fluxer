// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {VOICE_MEDIA_GRAPH_ENTRY_LIMIT, type VoiceMediaGraphSnapshot} from '@app/features/voice/engine/VoiceMediaGraph';

function collectSubscriptionViolations(snapshot: VoiceMediaGraphSnapshot, violations: Array<string>): void {
	let visited = 0;
	for (const [key, entry] of snapshot.subscriptionsByKey) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'invariant subscription scan exceeded graph limit');
		if (entry.actual.subscribed !== true) continue;
		if (entry.subscribed) continue;
		violations.push(`subscription ${key} is actually subscribed without a desired entry`);
	}
}

function collectFailureViolations(snapshot: VoiceMediaGraphSnapshot, violations: Array<string>): void {
	let visited = 0;
	for (const [key, failure] of snapshot.failuresByKey) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'invariant failure scan exceeded graph limit');
		if (failure.generation === undefined) continue;
		if (!failure.streamKey) continue;
		const currentGeneration = snapshot.watchGenerationByStreamKey.get(failure.streamKey) ?? 0;
		if (failure.generation <= currentGeneration) continue;
		violations.push(`failure ${key} carries generation ${failure.generation} newer than current ${currentGeneration}`);
	}
}

function collectDeadlineViolations(snapshot: VoiceMediaGraphSnapshot, violations: Array<string>): void {
	let visited = 0;
	for (const [key, deadline] of snapshot.deadlinesByKey) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'invariant deadline scan exceeded graph limit');
		if (deadline.attemptKey === null) continue;
		if (deadline.streamKey === null) {
			violations.push(`deadline ${key} references attempt ${deadline.attemptKey} without a stream key`);
			continue;
		}
		const attempt = snapshot.attemptsByStreamKey.get(deadline.streamKey);
		if (attempt?.attemptKey === deadline.attemptKey) continue;
		violations.push(`deadline ${key} references missing attempt ${deadline.attemptKey}`);
	}
}

function collectStatsViolations(snapshot: VoiceMediaGraphSnapshot, violations: Array<string>): void {
	let visited = 0;
	for (const [key, entry] of snapshot.statsByTrackKey) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'invariant stats scan exceeded graph limit');
		if (entry.connectionId === snapshot.statsConnectionId) continue;
		violations.push(`stats entry ${key} belongs to unknown connection ${entry.connectionId}`);
	}
}

function collectDeferredStopViolations(snapshot: VoiceMediaGraphSnapshot, violations: Array<string>): void {
	const viewerKeys = new Set(snapshot.watchIntent.viewerStreamKeys);
	let visited = 0;
	for (const key of snapshot.watchIntent.deferredStopKeys) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_ENTRY_LIMIT, 'invariant deferred stop scan exceeded graph limit');
		if (viewerKeys.has(key)) continue;
		violations.push(`deferred stop key ${key} is not a viewer stream key`);
	}
}

export function checkVoiceMediaGraphInvariants(snapshot: VoiceMediaGraphSnapshot): Array<string> {
	const violations: Array<string> = [];
	collectSubscriptionViolations(snapshot, violations);
	collectFailureViolations(snapshot, violations);
	collectDeadlineViolations(snapshot, violations);
	collectStatsViolations(snapshot, violations);
	collectDeferredStopViolations(snapshot, violations);
	return violations;
}
