// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2PerTrackStats} from '@fluxer/voice_engine_v2';
import {
	type VoiceMediaGraphStatsEntry,
	type VoiceMediaGraphStatsKind,
	type VoiceMediaGraphStatsPlatform,
	type VoiceMediaGraphStatsTrackObservation,
	type VoiceMediaGraphStatsTrackTarget,
	voiceMediaGraphStatsObservationMatchesTarget,
	voiceMediaGraphStatsTrackKey,
} from './VoiceMediaGraphStatsObservations';
import type {VoiceTrackSource} from './VoiceTrackSource';

const VOICE_MEDIA_GRAPH_STATS_TRACK_LIMIT = 1024;

export interface VoiceMediaGraphTrackInfo {
	width: number;
	height: number;
	fps: number;
}

export interface VoiceMediaGraphPartialTrackInfo {
	width?: number;
	height?: number;
	fps?: number;
}

export interface VoiceMediaGraphNativeStatsTarget {
	nativeSource?: VoiceTrackSource | null;
	nativeTrackSid?: string | null;
	participantIdentity?: string | null;
}

export interface VoiceMediaGraphPerTrackStatsTarget {
	trackSid?: string | null;
	mediaTrackId?: string | null;
}

function isPositiveDimension(value: number | undefined): value is number {
	return typeof value === 'number' && Number.isFinite(value) && value > 0;
}

function isPositiveFrameRate(value: number | undefined): value is number {
	return typeof value === 'number' && Number.isFinite(value) && value > 0;
}

function normalizeId(value: string | null | undefined): string | null {
	const trimmed = value?.trim();
	return trimmed ? trimmed : null;
}

function perTrackStatIdentifiers(track: VoiceEngineV2PerTrackStats): Array<string> {
	const identifiers: Array<string> = [];
	if (track.trackIdentifier) identifiers.push(track.trackIdentifier);
	if (track.mediaSourceId) identifiers.push(track.mediaSourceId);
	if (track.mid) identifiers.push(track.mid);
	if (track.rid) identifiers.push(track.rid);
	if (track.ssrc !== undefined) identifiers.push(String(track.ssrc));
	assert.ok(identifiers.length <= 5, 'per-track stats identifier list exceeded fixed limit');
	return identifiers;
}

function perTrackStatMatches(track: VoiceEngineV2PerTrackStats, target: VoiceMediaGraphPerTrackStatsTarget): boolean {
	const mediaTrackId = normalizeId(target.mediaTrackId);
	const trackSid = normalizeId(target.trackSid);
	if (!mediaTrackId && !trackSid) return false;
	const identifiers = perTrackStatIdentifiers(track);
	if (mediaTrackId && identifiers.includes(mediaTrackId)) return true;
	if (trackSid && identifiers.includes(trackSid)) return true;
	return false;
}

function perTrackStatToInfo(track: VoiceEngineV2PerTrackStats): VoiceMediaGraphPartialTrackInfo | null {
	if (track.kind !== 'video') return null;
	const width = track.frameWidth ?? track.sourceFrameWidth;
	const height = track.frameHeight ?? track.sourceFrameHeight;
	const fps = track.effectiveFramesPerSecond ?? track.framesPerSecond ?? track.sourceFramesPerSecond;
	const info: VoiceMediaGraphPartialTrackInfo = {};
	if (isPositiveDimension(width) && isPositiveDimension(height)) {
		info.width = width;
		info.height = height;
	}
	if (isPositiveFrameRate(fps)) info.fps = fps;
	return info.width !== undefined || info.fps !== undefined ? info : null;
}

export function resolveVoiceMediaGraphPerTrackInfo(
	tracks: ReadonlyArray<VoiceEngineV2PerTrackStats>,
	target: VoiceMediaGraphPerTrackStatsTarget,
): VoiceMediaGraphPartialTrackInfo | null {
	assert.ok(tracks.length <= VOICE_MEDIA_GRAPH_STATS_TRACK_LIMIT, 'per-track stats list exceeded graph limit');
	for (const track of tracks) {
		if (!perTrackStatMatches(track, target)) continue;
		const info = perTrackStatToInfo(track);
		if (info) return info;
	}
	return null;
}

function trackInfoFromPartial(partial: VoiceMediaGraphPartialTrackInfo | null): VoiceMediaGraphTrackInfo | null {
	if (!partial) return null;
	if (!isPositiveDimension(partial.width)) return null;
	if (!isPositiveDimension(partial.height)) return null;
	return {width: partial.width, height: partial.height, fps: Math.round(partial.fps ?? 0)};
}

export function mergeVoiceMediaGraphTrackInfo(
	primary: VoiceMediaGraphTrackInfo | null,
	fallback: VoiceMediaGraphPartialTrackInfo | null,
): VoiceMediaGraphTrackInfo | null {
	const merged: VoiceMediaGraphPartialTrackInfo = {
		width: primary?.width ?? fallback?.width,
		height: primary?.height ?? fallback?.height,
		fps: isPositiveFrameRate(primary?.fps) ? primary?.fps : fallback?.fps,
	};
	return trackInfoFromPartial(merged);
}

export interface VoiceMediaGraphStatsView {
	statsConnectionId: string | null;
	statsByTrackKey: ReadonlyMap<string, VoiceMediaGraphStatsEntry>;
}

function positiveOrNull(value: number | null | undefined): number | null {
	return typeof value === 'number' && Number.isFinite(value) && value > 0 ? value : null;
}

function perTrackStatsKindOrNull(kind: VoiceEngineV2PerTrackStats['kind']): VoiceMediaGraphStatsKind | null {
	if (kind === 'audio') return 'audio';
	if (kind === 'video') return 'video';
	return null;
}

function perTrackStatsToObservation(track: VoiceEngineV2PerTrackStats): VoiceMediaGraphStatsTrackObservation | null {
	const kind = perTrackStatsKindOrNull(track.kind);
	if (!kind) return null;
	return {
		trackSid: null,
		trackIdentifier: normalizeId(track.trackIdentifier),
		mediaSourceId: normalizeId(track.mediaSourceId),
		mid: normalizeId(track.mid),
		rid: normalizeId(track.rid),
		ssrc: typeof track.ssrc === 'number' && Number.isFinite(track.ssrc) ? track.ssrc : null,
		participantIdentity: null,
		participantSid: null,
		source: null,
		direction: track.direction,
		kind,
		fps: positiveOrNull(track.effectiveFramesPerSecond ?? track.framesPerSecond),
		width: positiveOrNull(track.frameWidth),
		height: positiveOrNull(track.frameHeight),
		sourceFps: positiveOrNull(track.sourceFramesPerSecond),
		sourceWidth: positiveOrNull(track.sourceFrameWidth),
		sourceHeight: positiveOrNull(track.sourceFrameHeight),
	};
}

export function voiceMediaGraphStatsObservationsFromPerTrackStats(
	tracks: ReadonlyArray<VoiceEngineV2PerTrackStats>,
): Array<VoiceMediaGraphStatsTrackObservation> {
	assert.ok(tracks.length <= VOICE_MEDIA_GRAPH_STATS_TRACK_LIMIT, 'per-track stats list exceeded graph limit');
	const observations: Array<VoiceMediaGraphStatsTrackObservation> = [];
	for (const track of tracks) {
		const observation = perTrackStatsToObservation(track);
		if (observation) observations.push(observation);
	}
	return observations;
}

export function buildVoiceMediaGraphStatsView(
	observations: ReadonlyArray<VoiceMediaGraphStatsTrackObservation>,
	platform: VoiceMediaGraphStatsPlatform,
	observedAt: number,
	connectionId: string,
): VoiceMediaGraphStatsView {
	assert.ok(observations.length <= VOICE_MEDIA_GRAPH_STATS_TRACK_LIMIT, 'stats view observations exceeded graph limit');
	const statsByTrackKey = new Map<string, VoiceMediaGraphStatsEntry>();
	for (const observation of observations) {
		const trackKey = voiceMediaGraphStatsTrackKey(observation);
		if (!trackKey) continue;
		statsByTrackKey.set(trackKey, {connectionId, platform, observedAt, observation});
	}
	return {statsConnectionId: connectionId, statsByTrackKey};
}

function observationToPartialTrackInfo(
	observation: VoiceMediaGraphStatsTrackObservation,
): VoiceMediaGraphPartialTrackInfo | null {
	const width = observation.width ?? observation.sourceWidth ?? undefined;
	const height = observation.height ?? observation.sourceHeight ?? undefined;
	const fps = observation.fps ?? observation.sourceFps ?? undefined;
	const info: VoiceMediaGraphPartialTrackInfo = {};
	if (isPositiveDimension(width) && isPositiveDimension(height)) {
		info.width = width;
		info.height = height;
	}
	if (isPositiveFrameRate(fps)) info.fps = fps;
	return info.width !== undefined || info.fps !== undefined ? info : null;
}

function streamTrackTargetTiers(target: VoiceMediaGraphStatsTrackTarget): Array<VoiceMediaGraphStatsTrackTarget> {
	const base: VoiceMediaGraphStatsTrackTarget = {
		direction: target.direction,
		kind: target.kind,
		source: target.source,
		participantIdentity: target.participantIdentity,
		participantSid: target.participantSid,
	};
	const tiers: Array<VoiceMediaGraphStatsTrackTarget> = [];
	if (normalizeId(target.trackSid)) tiers.push({...base, trackSid: target.trackSid});
	if (normalizeId(target.trackIdentifier)) tiers.push({...base, trackIdentifier: target.trackIdentifier});
	if (normalizeId(target.mediaSourceId)) tiers.push({...base, mediaSourceId: target.mediaSourceId});
	if (normalizeId(target.mid)) tiers.push({...base, mid: target.mid});
	if (normalizeId(target.rid)) tiers.push({...base, rid: target.rid});
	if (target.ssrc != null) tiers.push({...base, ssrc: target.ssrc});
	return tiers;
}

function findStreamTrackInfoInView(
	view: VoiceMediaGraphStatsView,
	target: VoiceMediaGraphStatsTrackTarget,
): VoiceMediaGraphPartialTrackInfo | null {
	let visited = 0;
	for (const entry of view.statsByTrackKey.values()) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_STATS_TRACK_LIMIT, 'stats view exceeded graph limit');
		if (view.statsConnectionId !== null && entry.connectionId !== view.statsConnectionId) continue;
		if (!voiceMediaGraphStatsObservationMatchesTarget(entry.observation, target)) continue;
		const info = observationToPartialTrackInfo(entry.observation);
		if (info) return info;
	}
	return null;
}

function findParticipantStreamTrackInfoInView(
	view: VoiceMediaGraphStatsView,
	target: VoiceMediaGraphStatsTrackTarget,
): VoiceMediaGraphPartialTrackInfo | null {
	const participantIdentity = normalizeId(target.participantIdentity);
	if (!participantIdentity) return null;
	let visited = 0;
	for (const entry of view.statsByTrackKey.values()) {
		visited += 1;
		assert.ok(visited <= VOICE_MEDIA_GRAPH_STATS_TRACK_LIMIT, 'stats view exceeded graph limit');
		if (view.statsConnectionId !== null && entry.connectionId !== view.statsConnectionId) continue;
		const observation = entry.observation;
		if (observation.participantIdentity !== participantIdentity) continue;
		if (target.direction && observation.direction !== target.direction) continue;
		if (target.kind && observation.kind !== target.kind) continue;
		if (target.source && observation.source !== target.source) continue;
		const info = observationToPartialTrackInfo(observation);
		if (info) return info;
	}
	return null;
}

export function selectVoiceMediaGraphStreamTrackInfo(
	view: VoiceMediaGraphStatsView,
	target: VoiceMediaGraphStatsTrackTarget,
): VoiceMediaGraphPartialTrackInfo | null {
	for (const tier of streamTrackTargetTiers(target)) {
		const info = findStreamTrackInfoInView(view, tier);
		if (info) return info;
	}
	return findParticipantStreamTrackInfoInView(view, target);
}
