// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	VoiceEngineV2InboundStats,
	VoiceEngineV2OutboundStats,
	VoiceEngineV2SendStats,
	VoiceEngineV2Stats,
	VoiceEngineV2TrackKind,
} from '../protocol';

export const VoiceEngineV2StatsTrackSource = Object.freeze({
	Microphone: 'microphone',
	Camera: 'camera',
	ScreenShare: 'screen_share',
	ScreenShareAudio: 'screen_share_audio',
	Unknown: 'unknown',
});

export type VoiceEngineV2StatsTrackSource =
	(typeof VoiceEngineV2StatsTrackSource)[keyof typeof VoiceEngineV2StatsTrackSource];

export interface VoiceEngineV2StatsNetworkSummary {
	audioSendBitrateKbps: number;
	audioRecvBitrateKbps: number;
	videoSendBitrateKbps: number;
	videoRecvBitrateKbps: number;
	audioPacketLossPercent: number;
	videoPacketLossPercent: number;
	jitterMs: number;
	rttMs: number | null;
}

export interface VoiceEngineV2StatsTrackSummary {
	direction: 'send' | 'recv';
	kind: 'audio' | 'video' | 'unknown';
	trackIdentifier: string;
	codec?: string;
	bitrateKbps: number;
	packetsLost?: number;
	jitterMs?: number;
	framesPerSecond?: number;
	sourceFramesPerSecond?: number;
	configuredFramesPerSecond?: number;
	targetFramesPerSecond?: number;
	effectiveFramesPerSecond?: number;
	frameWidth?: number;
	frameHeight?: number;
	sourceFrameWidth?: number;
	sourceFrameHeight?: number;
	targetBitrateKbps?: number;
	framesProduced?: number;
	framesAccepted?: number;
	framesDropped?: number;
	framesCoalesced?: number;
	framesCaptured?: number;
	captureFailures?: number;
	maxQueueAgeMs?: number;
	maxPushLatencyMs?: number;
	adaptiveSendTier?: string;
	adaptiveSendReason?: string;
}

export interface VoiceEngineV2StatsSummary {
	network: VoiceEngineV2StatsNetworkSummary;
	localVideo: VoiceEngineV2StatsTrackSummary | null;
	localAudio: VoiceEngineV2StatsTrackSummary | null;
	localScreenShare: VoiceEngineV2StatsTrackSummary | null;
	localScreenShareAudio: VoiceEngineV2StatsTrackSummary | null;
	remoteVideo: VoiceEngineV2StatsTrackSummary | null;
	remoteAudio: VoiceEngineV2StatsTrackSummary | null;
	remoteScreenShare: VoiceEngineV2StatsTrackSummary | null;
	remoteScreenShareAudio: VoiceEngineV2StatsTrackSummary | null;
}

export interface VoiceEngineV2StatsTrackRoleCandidate {
	direction: 'send' | 'recv';
	kind: 'audio' | 'video' | 'unknown';
	rid?: string;
	trackIdentifier?: string;
	bitrateKbps: number;
}

export interface VoiceEngineV2StatsTrackPublicationIds {
	localCameraTrackId: string | null;
	localMicrophoneTrackId: string | null;
	localScreenShareTrackId: string | null;
	localScreenShareAudioTrackId: string | null;
	remoteMicrophoneTrackIds: Array<string>;
	remoteScreenShareTrackIds: Array<string>;
	remoteScreenShareAudioTrackIds: Array<string>;
}

export interface VoiceEngineV2StatsTrackClassificationInput {
	tracks: ReadonlyArray<VoiceEngineV2StatsTrackRoleCandidate>;
	publications: VoiceEngineV2StatsTrackPublicationIds;
}

export interface VoiceEngineV2StatsTrackRoleSelection {
	localVideoTrackIndex: number | null;
	localAudioTrackIndex: number | null;
	localScreenShareTrackIndex: number | null;
	localScreenShareAudioTrackIndex: number | null;
	remoteVideoTrackIndex: number | null;
	remoteAudioTrackIndex: number | null;
	remoteScreenShareTrackIndex: number | null;
	remoteScreenShareAudioTrackIndex: number | null;
}

interface IndexedVoiceEngineV2StatsTrack {
	index: number;
	track: VoiceEngineV2StatsTrackRoleCandidate;
}

function asTrackKind(value: unknown): VoiceEngineV2TrackKind {
	return value === 'video' ? 'video' : 'audio';
}

function asNumber(value: unknown): number {
	return typeof value === 'number' && Number.isFinite(value) ? value : 0;
}

function asNonNegativeNumber(value: unknown): number {
	return Math.max(0, asNumber(value));
}

function asOptionalNumber(value: unknown): number | undefined {
	return typeof value === 'number' && Number.isFinite(value) ? value : undefined;
}

function asOptionalCount(value: unknown): number | undefined {
	const number = asOptionalNumber(value);
	return number == null ? undefined : Math.max(0, Math.trunc(number));
}

function asString(value: unknown): string | undefined {
	return typeof value === 'string' && value.length > 0 ? value : undefined;
}

function coerceVoiceEngineV2SendStats(value: unknown): VoiceEngineV2SendStats | null | undefined {
	if (value == null || typeof value !== 'object') return value === null ? null : undefined;
	const payload = value as Record<string, unknown>;
	return {
		outgoingVideoQueueDepth: asNonNegativeNumber(payload.outgoingVideoQueueDepth),
		outgoingVideoQueueCapacity: asNonNegativeNumber(payload.outgoingVideoQueueCapacity),
		outgoingVideoMaxQueueDepth: asNonNegativeNumber(payload.outgoingVideoMaxQueueDepth),
		outgoingVideoFramesProduced: asNonNegativeNumber(payload.outgoingVideoFramesProduced),
		outgoingVideoFramesAccepted: asNonNegativeNumber(payload.outgoingVideoFramesAccepted),
		outgoingVideoFramesDropped: asNonNegativeNumber(payload.outgoingVideoFramesDropped),
		outgoingVideoFramesCoalesced: asNonNegativeNumber(payload.outgoingVideoFramesCoalesced),
		outgoingVideoFramesCaptured: asNonNegativeNumber(payload.outgoingVideoFramesCaptured),
		outgoingVideoCaptureFailures: asNonNegativeNumber(payload.outgoingVideoCaptureFailures),
		outgoingVideoEffectiveFps: asNonNegativeNumber(payload.outgoingVideoEffectiveFps),
		outgoingVideoTargetFps: asNonNegativeNumber(payload.outgoingVideoTargetFps),
		outgoingVideoPacingTargetFps: asNonNegativeNumber(payload.outgoingVideoPacingTargetFps),
		outgoingVideoMaxQueueAgeMs: asNonNegativeNumber(payload.outgoingVideoMaxQueueAgeMs),
		outgoingVideoMaxPushLatencyMs: asNonNegativeNumber(payload.outgoingVideoMaxPushLatencyMs),
		outgoingVideoPacingMode: asString(payload.outgoingVideoPacingMode) ?? 'unknown',
		outgoingVideoBusActive: payload.outgoingVideoBusActive === true,
		outgoingAudioBufferTargetMs: asNonNegativeNumber(payload.outgoingAudioBufferTargetMs),
		outgoingAudioBufferMaxMs: asNonNegativeNumber(payload.outgoingAudioBufferMaxMs),
		outgoingAudioUnderruns: asNonNegativeNumber(payload.outgoingAudioUnderruns),
		outgoingAudioRebuffers: asNonNegativeNumber(payload.outgoingAudioRebuffers),
		outgoingAudioMaxFrameGapMs: asNonNegativeNumber(payload.outgoingAudioMaxFrameGapMs),
		adaptiveSendTier: asString(payload.adaptiveSendTier) ?? 'unknown',
		adaptiveSendReason: asString(payload.adaptiveSendReason) ?? 'unknown',
	};
}

export function coerceVoiceEngineV2Stats(payload: Record<string, unknown>): VoiceEngineV2Stats {
	const rttRaw = payload.rttMs;
	const rttMs = typeof rttRaw === 'number' && Number.isFinite(rttRaw) ? rttRaw : null;
	const droppedVideoFrameCallbacks = asOptionalCount(payload.droppedVideoFrameCallbacks);
	const droppedNativeVideoFrames = asOptionalCount(payload.droppedNativeVideoFrames);
	const outbound = Array.isArray(payload.outbound)
		? payload.outbound.flatMap((entry) => {
				if (entry == null || typeof entry !== 'object') return [];
				const e = entry as Record<string, unknown>;
				const zeroCopy = e.zeroCopy === true ? true : undefined;
				return [
					{
						trackSid: asString(e.trackSid) ?? '',
						source: asString(e.source) ?? '',
						kind: asTrackKind(e.kind),
						codec: asString(e.codec),
						bitrateKbps: asNumber(e.bitrateKbps),
						packetsLost: asNumber(e.packetsLost),
						packetsSent: asOptionalCount(e.packetsSent),
						fps: asOptionalNumber(e.fps),
						audioLevel: asOptionalNumber(e.audioLevel),
						width: asOptionalCount(e.width),
						height: asOptionalCount(e.height),
						sourceWidth: asOptionalCount(e.sourceWidth),
						sourceHeight: asOptionalCount(e.sourceHeight),
						targetBitrateKbps: asOptionalNumber(e.targetBitrateKbps),
						configuredFps: asOptionalNumber(e.configuredFps),
						targetFps: asOptionalNumber(e.targetFps),
						effectiveFps: asOptionalNumber(e.effectiveFps),
						framesProduced: asOptionalCount(e.framesProduced),
						framesAccepted: asOptionalCount(e.framesAccepted),
						framesDropped: asOptionalCount(e.framesDropped),
						framesCoalesced: asOptionalCount(e.framesCoalesced),
						framesCaptured: asOptionalCount(e.framesCaptured),
						captureFailures: asOptionalCount(e.captureFailures),
						maxQueueAgeMs: asOptionalNumber(e.maxQueueAgeMs),
						maxPushLatencyMs: asOptionalNumber(e.maxPushLatencyMs),
						adaptiveSendTier: asString(e.adaptiveSendTier),
						adaptiveSendReason: asString(e.adaptiveSendReason),
						...(zeroCopy === true ? {zeroCopy} : {}),
					},
				];
			})
		: [];
	const inbound = Array.isArray(payload.inbound)
		? payload.inbound.flatMap((entry) => {
				if (entry == null || typeof entry !== 'object') return [];
				const e = entry as Record<string, unknown>;
				const participantIdentity = asString(e.participantIdentity);
				return [
					{
						...(participantIdentity ? {participantIdentity} : {}),
						participantSid: asString(e.participantSid) ?? '',
						trackSid: asString(e.trackSid) ?? '',
						source: asString(e.source),
						kind: asTrackKind(e.kind),
						codec: asString(e.codec),
						bitrateKbps: asNumber(e.bitrateKbps),
						packetsLost: asNumber(e.packetsLost),
						packetsReceived: asOptionalCount(e.packetsReceived),
						jitterMs: asOptionalNumber(e.jitterMs),
						audioLevel: asOptionalNumber(e.audioLevel),
						width: asOptionalCount(e.width),
						height: asOptionalCount(e.height),
						fps: asOptionalNumber(e.fps),
						sourceWidth: asOptionalCount(e.sourceWidth),
						sourceHeight: asOptionalCount(e.sourceHeight),
					},
				];
			})
		: [];
	const send = coerceVoiceEngineV2SendStats(payload.send);
	return {rttMs, outbound, inbound, droppedVideoFrameCallbacks, droppedNativeVideoFrames, send};
}

export function asVoiceEngineV2StatsTrackSource(source: unknown): VoiceEngineV2StatsTrackSource {
	switch (source) {
		case VoiceEngineV2StatsTrackSource.Microphone:
			return VoiceEngineV2StatsTrackSource.Microphone;
		case VoiceEngineV2StatsTrackSource.Camera:
			return VoiceEngineV2StatsTrackSource.Camera;
		case VoiceEngineV2StatsTrackSource.ScreenShare:
		case 'screen':
		case 'screenshare':
			return VoiceEngineV2StatsTrackSource.ScreenShare;
		case VoiceEngineV2StatsTrackSource.ScreenShareAudio:
		case 'screenAudio':
		case 'screenshareAudio':
		case 'screenshare_audio':
		case 'screen_audio':
		case 'system_audio':
			return VoiceEngineV2StatsTrackSource.ScreenShareAudio;
		default:
			return VoiceEngineV2StatsTrackSource.Unknown;
	}
}

function isScreenShareAudioSource(source: unknown): boolean {
	return asVoiceEngineV2StatsTrackSource(source) === VoiceEngineV2StatsTrackSource.ScreenShareAudio;
}

function isScreenShareVideoSource(source: unknown): boolean {
	return asVoiceEngineV2StatsTrackSource(source) === VoiceEngineV2StatsTrackSource.ScreenShare;
}

function sanitizeKbps(kbps: number): number {
	if (!Number.isFinite(kbps) || kbps < 0) return 0;
	return Math.round(kbps * 10) / 10;
}

function roundedNonNegative(value: number): number {
	if (!Number.isFinite(value) || value <= 0) return 0;
	return Math.round(value);
}

function sumOutboundBitrate(tracks: ReadonlyArray<VoiceEngineV2OutboundStats>): number {
	return roundedNonNegative(tracks.reduce((total, track) => total + sanitizeKbps(track.bitrateKbps), 0));
}

function sumInboundBitrate(tracks: ReadonlyArray<VoiceEngineV2InboundStats>): number {
	return roundedNonNegative(tracks.reduce((total, track) => total + sanitizeKbps(track.bitrateKbps), 0));
}

function lossPercent(lost: number, expected: number): number {
	const safeLost = Math.max(0, lost);
	if (expected <= 0) return 0;
	return Math.min(100, (safeLost / expected) * 100);
}

function outboundLossPercent(track: VoiceEngineV2OutboundStats): number {
	const sent = Math.max(0, track.packetsSent ?? 0);
	return lossPercent(track.packetsLost, sent);
}

function inboundLossPercent(track: VoiceEngineV2InboundStats): number {
	const received = Math.max(0, track.packetsReceived ?? 0);
	return lossPercent(track.packetsLost, received + Math.max(0, track.packetsLost));
}

function worstPacketLossPercent(
	outbound: ReadonlyArray<VoiceEngineV2OutboundStats>,
	inbound: ReadonlyArray<VoiceEngineV2InboundStats>,
): number {
	let worst = 0;
	for (const track of outbound) worst = Math.max(worst, outboundLossPercent(track));
	for (const track of inbound) worst = Math.max(worst, inboundLossPercent(track));
	return Math.round(worst * 10) / 10;
}

function maxJitterMs(inboundAudio: ReadonlyArray<VoiceEngineV2InboundStats>): number {
	return roundedNonNegative(
		inboundAudio.reduce(
			(max, track) =>
				typeof track.jitterMs === 'number' && Number.isFinite(track.jitterMs) && track.jitterMs >= 0
					? Math.max(max, track.jitterMs)
					: max,
			0,
		),
	);
}

function maxOptionalNumber(...values: ReadonlyArray<number | undefined>): number | undefined {
	let max: number | undefined;
	for (const value of values) {
		if (typeof value !== 'number' || !Number.isFinite(value)) continue;
		max = max == null ? value : Math.max(max, value);
	}
	return max;
}

function firstString(...values: ReadonlyArray<string | undefined>): string | undefined {
	return values.find((value) => typeof value === 'string' && value.length > 0);
}

function outboundSourceKey(source: string): string {
	const normalized = asVoiceEngineV2StatsTrackSource(source);
	return normalized === VoiceEngineV2StatsTrackSource.Unknown ? source : normalized;
}

function outboundTrackGroupKey(track: VoiceEngineV2OutboundStats): string {
	return [track.kind, outboundSourceKey(track.source), track.trackSid || 'unknown-track'].join(':');
}

function mergeOutboundTrackGroup(tracks: ReadonlyArray<VoiceEngineV2OutboundStats>): VoiceEngineV2OutboundStats {
	const first = tracks[0];
	if (!first) {
		return {trackSid: '', source: '', kind: 'audio', bitrateKbps: 0, packetsLost: 0};
	}
	const merged: VoiceEngineV2OutboundStats = {
		...first,
		bitrateKbps: sanitizeKbps(first.bitrateKbps),
		packetsLost: Math.max(0, first.packetsLost),
		...(first.packetsSent === undefined ? {} : {packetsSent: Math.max(0, first.packetsSent)}),
	};
	for (const track of tracks.slice(1)) {
		merged.codec = firstString(merged.codec, track.codec);
		merged.bitrateKbps = sanitizeKbps(merged.bitrateKbps + sanitizeKbps(track.bitrateKbps));
		merged.packetsLost = Math.max(0, merged.packetsLost) + Math.max(0, track.packetsLost);
		if (track.packetsSent !== undefined) {
			merged.packetsSent = Math.max(0, merged.packetsSent ?? 0) + Math.max(0, track.packetsSent);
		}
		merged.fps = maxOptionalNumber(merged.fps, track.fps);
		merged.audioLevel = maxOptionalNumber(merged.audioLevel, track.audioLevel);
		merged.width = maxOptionalNumber(merged.width, track.width);
		merged.height = maxOptionalNumber(merged.height, track.height);
		merged.sourceWidth = maxOptionalNumber(merged.sourceWidth, track.sourceWidth);
		merged.sourceHeight = maxOptionalNumber(merged.sourceHeight, track.sourceHeight);
		merged.targetBitrateKbps = maxOptionalNumber(merged.targetBitrateKbps, track.targetBitrateKbps);
		merged.configuredFps = maxOptionalNumber(merged.configuredFps, track.configuredFps);
		merged.targetFps = maxOptionalNumber(merged.targetFps, track.targetFps);
		merged.effectiveFps = maxOptionalNumber(merged.effectiveFps, track.effectiveFps);
		merged.framesProduced = maxOptionalNumber(merged.framesProduced, track.framesProduced);
		merged.framesAccepted = maxOptionalNumber(merged.framesAccepted, track.framesAccepted);
		merged.framesDropped = maxOptionalNumber(merged.framesDropped, track.framesDropped);
		merged.framesCoalesced = maxOptionalNumber(merged.framesCoalesced, track.framesCoalesced);
		merged.framesCaptured = maxOptionalNumber(merged.framesCaptured, track.framesCaptured);
		merged.captureFailures = maxOptionalNumber(merged.captureFailures, track.captureFailures);
		merged.maxQueueAgeMs = maxOptionalNumber(merged.maxQueueAgeMs, track.maxQueueAgeMs);
		merged.maxPushLatencyMs = maxOptionalNumber(merged.maxPushLatencyMs, track.maxPushLatencyMs);
		merged.adaptiveSendTier = firstString(merged.adaptiveSendTier, track.adaptiveSendTier);
		merged.adaptiveSendReason = firstString(merged.adaptiveSendReason, track.adaptiveSendReason);
	}
	return merged;
}

export function coalesceVoiceEngineV2OutboundStats(
	tracks: ReadonlyArray<VoiceEngineV2OutboundStats>,
): Array<VoiceEngineV2OutboundStats> {
	const grouped = new Map<string, Array<VoiceEngineV2OutboundStats>>();
	for (const track of tracks) {
		const key = outboundTrackGroupKey(track);
		const group = grouped.get(key);
		if (group) {
			group.push(track);
		} else {
			grouped.set(key, [track]);
		}
	}
	return Array.from(grouped.values(), mergeOutboundTrackGroup);
}

function pickActiveOutbound(tracks: ReadonlyArray<VoiceEngineV2OutboundStats>): VoiceEngineV2StatsTrackSummary | null {
	const track = tracks.find((candidate) => candidate.bitrateKbps > 0) ?? tracks[0] ?? null;
	if (!track) return null;
	return {
		direction: 'send',
		kind: track.kind,
		trackIdentifier: track.trackSid,
		codec: track.codec,
		bitrateKbps: track.bitrateKbps,
		packetsLost: track.packetsLost,
		framesPerSecond: track.effectiveFps ?? track.fps,
		sourceFramesPerSecond: track.effectiveFps,
		configuredFramesPerSecond: track.configuredFps,
		targetFramesPerSecond: track.targetFps,
		effectiveFramesPerSecond: track.effectiveFps,
		frameWidth: track.width,
		frameHeight: track.height,
		sourceFrameWidth: track.sourceWidth,
		sourceFrameHeight: track.sourceHeight,
		targetBitrateKbps: track.targetBitrateKbps,
		framesProduced: track.framesProduced,
		framesAccepted: track.framesAccepted,
		framesDropped: track.framesDropped,
		framesCoalesced: track.framesCoalesced,
		framesCaptured: track.framesCaptured,
		captureFailures: track.captureFailures,
		maxQueueAgeMs: track.maxQueueAgeMs,
		maxPushLatencyMs: track.maxPushLatencyMs,
		adaptiveSendTier: track.adaptiveSendTier,
		adaptiveSendReason: track.adaptiveSendReason,
	};
}

function pickActiveInbound(tracks: ReadonlyArray<VoiceEngineV2InboundStats>): VoiceEngineV2StatsTrackSummary | null {
	const track = tracks.find((candidate) => candidate.bitrateKbps > 0) ?? tracks[0] ?? null;
	if (!track) return null;
	return {
		direction: 'recv',
		kind: track.kind,
		trackIdentifier: track.trackSid,
		codec: track.codec,
		bitrateKbps: track.bitrateKbps,
		packetsLost: track.packetsLost,
		jitterMs: track.jitterMs,
		framesPerSecond: track.fps,
		frameWidth: track.width,
		frameHeight: track.height,
		sourceFrameWidth: track.sourceWidth,
		sourceFrameHeight: track.sourceHeight,
	};
}

export function summarizeVoiceEngineV2Stats(stats: VoiceEngineV2Stats): VoiceEngineV2StatsSummary {
	const outbound = coalesceVoiceEngineV2OutboundStats(stats.outbound);
	const outAudioAll = outbound.filter((track) => track.kind === 'audio');
	const outAudio = outAudioAll.filter((track) => !isScreenShareAudioSource(track.source));
	const outScreenShareAudio = outAudioAll.filter((track) => isScreenShareAudioSource(track.source));
	const outVideoAll = outbound.filter((track) => track.kind === 'video');
	const outScreenShare = outVideoAll.filter(
		(track) => asVoiceEngineV2StatsTrackSource(track.source) === VoiceEngineV2StatsTrackSource.ScreenShare,
	);
	const outCamera = outVideoAll.filter(
		(track) => asVoiceEngineV2StatsTrackSource(track.source) !== VoiceEngineV2StatsTrackSource.ScreenShare,
	);
	const inAudio = stats.inbound.filter((track) => track.kind === 'audio');
	const inVideo = stats.inbound.filter((track) => track.kind === 'video');
	const inScreenShare = inVideo.filter((track) => isScreenShareVideoSource(track.source));
	const inCamera = inVideo.filter((track) => !isScreenShareVideoSource(track.source));

	return {
		network: {
			audioSendBitrateKbps: sumOutboundBitrate(outAudioAll),
			audioRecvBitrateKbps: sumInboundBitrate(inAudio),
			videoSendBitrateKbps: sumOutboundBitrate(outVideoAll),
			videoRecvBitrateKbps: sumInboundBitrate(inVideo),
			audioPacketLossPercent: worstPacketLossPercent(outAudio, inAudio),
			videoPacketLossPercent: worstPacketLossPercent(outVideoAll, inVideo),
			jitterMs: maxJitterMs(inAudio),
			rttMs: stats.rttMs,
		},
		localAudio: pickActiveOutbound(outAudio),
		localVideo: pickActiveOutbound(outCamera),
		localScreenShare: pickActiveOutbound(outScreenShare),
		localScreenShareAudio: pickActiveOutbound(outScreenShareAudio),
		remoteAudio: pickActiveInbound(inAudio),
		remoteVideo: pickActiveInbound(inCamera),
		remoteScreenShare: pickActiveInbound(inScreenShare),
		remoteScreenShareAudio: null,
	};
}

function indexedTracks(
	tracks: ReadonlyArray<VoiceEngineV2StatsTrackRoleCandidate>,
	direction: 'send' | 'recv',
	kind: 'audio' | 'video',
): Array<IndexedVoiceEngineV2StatsTrack> {
	return tracks
		.map((track, index) => ({index, track}))
		.filter((candidate) => candidate.track.direction === direction && candidate.track.kind === kind);
}

function normalizedId(id: string | null | undefined): string | null {
	const trimmed = id?.trim();
	return trimmed ? trimmed : null;
}

function canonicalId(id: string | null | undefined): string | null {
	const normalized = normalizedId(id);
	if (normalized === null) return null;
	const unwrapped =
		normalized.startsWith('{') && normalized.endsWith('}') ? normalized.slice(1, -1).trim() : normalized;
	return unwrapped ? unwrapped.toLowerCase() : null;
}

function idList(ids: ReadonlyArray<string>): Array<string> {
	return ids.map((id) => canonicalId(id)).filter((id): id is string => id !== null);
}

function matchesId(candidate: IndexedVoiceEngineV2StatsTrack, id: string | null): boolean {
	const trackId = canonicalId(candidate.track.trackIdentifier);
	const publicationId = canonicalId(id);
	return trackId !== null && publicationId !== null && trackId === publicationId;
}

function idInList(candidate: IndexedVoiceEngineV2StatsTrack, ids: ReadonlyArray<string>): boolean {
	const trackId = canonicalId(candidate.track.trackIdentifier);
	return trackId !== null && ids.includes(trackId);
}

function hasId(candidate: IndexedVoiceEngineV2StatsTrack): boolean {
	return canonicalId(candidate.track.trackIdentifier) !== null;
}

function hasRid(candidate: IndexedVoiceEngineV2StatsTrack): boolean {
	return normalizedId(candidate.track.rid) !== null;
}

function couldBeLocalScreenShare(
	candidate: IndexedVoiceEngineV2StatsTrack,
	localScreenShareTrackId: string | null,
): boolean {
	if (localScreenShareTrackId === null) return false;
	return matchesId(candidate, localScreenShareTrackId) || !hasId(candidate);
}

function unidentifiedLocalScreenShareIndex(
	sentVideoTracks: ReadonlyArray<IndexedVoiceEngineV2StatsTrack>,
	localCameraTrackId: string | null,
	localScreenShareTrackId: string | null,
): number | null {
	if (localScreenShareTrackId === null || localCameraTrackId !== null || sentVideoTracks.length !== 1) return null;
	const candidate = sentVideoTracks[0];
	return candidate && !hasId(candidate) ? candidate.index : null;
}

function filterById(
	candidates: ReadonlyArray<IndexedVoiceEngineV2StatsTrack>,
	id: string | null,
): Array<IndexedVoiceEngineV2StatsTrack> {
	return candidates.filter((candidate) => matchesId(candidate, id));
}

function pickActiveTrackIndex(candidates: ReadonlyArray<IndexedVoiceEngineV2StatsTrack>): number | null {
	return (
		candidates.find((candidate) => Number.isFinite(candidate.track.bitrateKbps) && candidate.track.bitrateKbps > 0)
			?.index ??
		candidates[0]?.index ??
		null
	);
}

export function classifyVoiceEngineV2TrackStats(
	input: VoiceEngineV2StatsTrackClassificationInput,
): VoiceEngineV2StatsTrackRoleSelection {
	const sentVideoTracks = indexedTracks(input.tracks, 'send', 'video');
	const sentAudioTracks = indexedTracks(input.tracks, 'send', 'audio');
	const receivedVideoTracks = indexedTracks(input.tracks, 'recv', 'video');
	const receivedAudioTracks = indexedTracks(input.tracks, 'recv', 'audio');
	const localCameraTrackId = normalizedId(input.publications.localCameraTrackId);
	const localMicrophoneTrackId = normalizedId(input.publications.localMicrophoneTrackId);
	const localScreenShareTrackId = normalizedId(input.publications.localScreenShareTrackId);
	const localScreenShareAudioTrackId = normalizedId(input.publications.localScreenShareAudioTrackId);
	const remoteMicrophoneTrackIds = idList(input.publications.remoteMicrophoneTrackIds);
	const remoteScreenShareTrackIds = idList(input.publications.remoteScreenShareTrackIds);
	const remoteScreenShareAudioTrackIds = idList(input.publications.remoteScreenShareAudioTrackIds);

	const remoteScreenShareTracks = receivedVideoTracks.filter((track) => idInList(track, remoteScreenShareTrackIds));
	const remoteScreenShareTrackIndex = pickActiveTrackIndex(remoteScreenShareTracks);
	const fallbackRemoteVideoTracks = receivedVideoTracks.filter((track) => !idInList(track, remoteScreenShareTrackIds));
	const localScreenShareFallbackIndex = unidentifiedLocalScreenShareIndex(
		sentVideoTracks,
		localCameraTrackId,
		localScreenShareTrackId,
	);

	return {
		localVideoTrackIndex:
			pickActiveTrackIndex(filterById(sentVideoTracks, localCameraTrackId)) ??
			pickActiveTrackIndex(
				sentVideoTracks.filter((track) => !hasRid(track) && !couldBeLocalScreenShare(track, localScreenShareTrackId)),
			),
		localAudioTrackIndex:
			pickActiveTrackIndex(filterById(sentAudioTracks, localMicrophoneTrackId)) ??
			pickActiveTrackIndex(sentAudioTracks.filter((track) => !matchesId(track, localScreenShareAudioTrackId))),
		localScreenShareTrackIndex:
			pickActiveTrackIndex(filterById(sentVideoTracks, localScreenShareTrackId)) ??
			pickActiveTrackIndex(sentVideoTracks.filter((track) => hasRid(track) && !matchesId(track, localCameraTrackId))) ??
			localScreenShareFallbackIndex,
		localScreenShareAudioTrackIndex: pickActiveTrackIndex(filterById(sentAudioTracks, localScreenShareAudioTrackId)),
		remoteVideoTrackIndex:
			pickActiveTrackIndex(fallbackRemoteVideoTracks) ??
			(remoteScreenShareTrackIndex === null ? pickActiveTrackIndex(receivedVideoTracks) : null),
		remoteAudioTrackIndex:
			pickActiveTrackIndex(receivedAudioTracks.filter((track) => idInList(track, remoteMicrophoneTrackIds))) ??
			pickActiveTrackIndex(receivedAudioTracks.filter((track) => !idInList(track, remoteScreenShareAudioTrackIds))),
		remoteScreenShareTrackIndex,
		remoteScreenShareAudioTrackIndex: pickActiveTrackIndex(
			receivedAudioTracks.filter((track) => idInList(track, remoteScreenShareAudioTrackIds)),
		),
	};
}

export type VoiceStatsNetworkSummary = VoiceEngineV2StatsNetworkSummary;
export type VoiceStatsTrackSummary = VoiceEngineV2StatsTrackSummary;
export type VoiceStatsSummary = VoiceEngineV2StatsSummary;
export type VoiceStatsTrackRoleCandidate = VoiceEngineV2StatsTrackRoleCandidate;
export type VoiceStatsTrackPublicationIds = VoiceEngineV2StatsTrackPublicationIds;
export type VoiceStatsTrackClassificationInput = VoiceEngineV2StatsTrackClassificationInput;
export type VoiceStatsTrackRoleSelection = VoiceEngineV2StatsTrackRoleSelection;
