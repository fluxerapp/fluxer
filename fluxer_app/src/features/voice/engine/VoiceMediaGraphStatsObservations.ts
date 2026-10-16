// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';

export type VoiceMediaGraphStatsPlatform = 'native' | 'web';
type VoiceMediaGraphStatsDirection = 'send' | 'recv';
export type VoiceMediaGraphStatsKind = 'audio' | 'video';

export interface VoiceMediaGraphStatsTrackObservation {
	trackSid: string | null;
	trackIdentifier: string | null;
	mediaSourceId: string | null;
	mid: string | null;
	rid: string | null;
	ssrc: number | null;
	participantIdentity: string | null;
	participantSid: string | null;
	source: string | null;
	direction: VoiceMediaGraphStatsDirection;
	kind: VoiceMediaGraphStatsKind;
	fps: number | null;
	width: number | null;
	height: number | null;
	sourceFps: number | null;
	sourceWidth: number | null;
	sourceHeight: number | null;
}

export interface VoiceMediaGraphStatsEntry {
	connectionId: string;
	platform: VoiceMediaGraphStatsPlatform;
	observedAt: number;
	observation: VoiceMediaGraphStatsTrackObservation;
}

export interface VoiceMediaGraphStatsTrackTarget {
	trackSid?: string | null;
	trackIdentifier?: string | null;
	mediaSourceId?: string | null;
	mid?: string | null;
	rid?: string | null;
	ssrc?: number | null;
	participantIdentity?: string | null;
	participantSid?: string | null;
	source?: string | null;
	direction?: VoiceMediaGraphStatsDirection;
	kind?: VoiceMediaGraphStatsKind;
}

const STATS_OBSERVATION_IDENTIFIER_LIMIT = 8;

function observationPrimaryIdentifier(observation: VoiceMediaGraphStatsTrackObservation): string | null {
	if (observation.trackSid) return `sid:${observation.trackSid}`;
	if (observation.trackIdentifier) return `id:${observation.trackIdentifier}`;
	if (observation.mediaSourceId) return `media:${observation.mediaSourceId}`;
	if (observation.participantIdentity && observation.source) {
		return `participant:${observation.participantIdentity}:${observation.source}`;
	}
	if (observation.mid) return `mid:${observation.mid}`;
	if (observation.rid) return `rid:${observation.rid}`;
	if (observation.ssrc != null) return `ssrc:${observation.ssrc}`;
	return null;
}

export function voiceMediaGraphStatsTrackKey(observation: VoiceMediaGraphStatsTrackObservation): string | null {
	const identifier = observationPrimaryIdentifier(observation);
	if (!identifier) return null;
	return `${observation.direction}:${observation.kind}:${identifier}`;
}

function observationIdentifiers(observation: VoiceMediaGraphStatsTrackObservation): Array<string> {
	const identifiers: Array<string> = [];
	if (observation.trackSid) identifiers.push(observation.trackSid);
	if (observation.trackIdentifier) identifiers.push(observation.trackIdentifier);
	if (observation.mediaSourceId) identifiers.push(observation.mediaSourceId);
	if (observation.mid) identifiers.push(observation.mid);
	if (observation.rid) identifiers.push(observation.rid);
	if (observation.ssrc != null) identifiers.push(String(observation.ssrc));
	assert.ok(identifiers.length <= STATS_OBSERVATION_IDENTIFIER_LIMIT, 'stats identifier list exceeded fixed limit');
	return identifiers;
}

function targetIdentifierMatches(
	observation: VoiceMediaGraphStatsTrackObservation,
	target: VoiceMediaGraphStatsTrackTarget,
): boolean {
	const wanted: Array<string> = [];
	if (target.trackSid) wanted.push(target.trackSid);
	if (target.trackIdentifier) wanted.push(target.trackIdentifier);
	if (target.mediaSourceId) wanted.push(target.mediaSourceId);
	if (target.mid) wanted.push(target.mid);
	if (target.rid) wanted.push(target.rid);
	if (target.ssrc != null) wanted.push(String(target.ssrc));
	if (wanted.length === 0) return true;
	const identifiers = observationIdentifiers(observation);
	for (const candidate of wanted) {
		if (identifiers.includes(candidate)) return true;
	}
	return false;
}

export function voiceMediaGraphStatsObservationMatchesTarget(
	observation: VoiceMediaGraphStatsTrackObservation,
	target: VoiceMediaGraphStatsTrackTarget,
): boolean {
	if (target.direction && observation.direction !== target.direction) return false;
	if (target.kind && observation.kind !== target.kind) return false;
	if (target.participantIdentity) {
		if (observation.participantIdentity && observation.participantIdentity !== target.participantIdentity) return false;
	}
	if (target.participantSid) {
		if (observation.participantSid && observation.participantSid !== target.participantSid) return false;
	}
	if (target.source) {
		if (observation.source && observation.source !== target.source) return false;
	}
	return targetIdentifierMatches(observation, target);
}
