// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Store} from '@app/features/voice/engine/Store';
import type {LatencyDataPoint} from '@app/features/voice/engine/VoiceLatencyTracker';
import {voiceMediaGraphStatsObservationsFromPerTrackStats} from '@app/features/voice/engine/VoiceMediaGraphStats';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {
	createInitialVoiceStats,
	createVoiceStatsSnapshot,
	selectVoiceStatsCollectionDecision,
	transitionVoiceStatsSnapshot,
	type VoiceStatsCollectDecision,
	type VoiceStatsEvent,
	type VoiceStatsRtpCounter,
	type VoiceStatsSnapshot,
} from '@app/features/voice/engine/VoiceStatsStateMachine';
import {asVoiceConnectionQuality, VoiceConnectionQuality} from '@app/features/voice/engine/VoiceTrackSource';
import {
	classifyVideoDecoderAcceleration,
	classifyVideoEncoderAcceleration,
	type VideoAccelerationStatus,
} from '@app/features/voice/utils/VideoAccelerationClassification';
import {parseVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import type {
	VoiceEngineV2InboundStats,
	VoiceEngineV2OutboundStats,
	VoiceEngineV2Stats,
	VoiceEngineV2TrackKind,
} from '@fluxer/voice_engine_v2';
import type {Room} from 'livekit-client';
import {assertFiniteNumber, assertNonNullObject} from './VoiceEngineV2AppAdapterAssertions';

const logger = new Logger('VoiceEngineV2AppStatsHostAdapter');

export type {LatencyDataPoint} from '@app/features/voice/engine/VoiceLatencyTracker';

export interface VoiceStats {
	audioSendBitrate: number;
	audioRecvBitrate: number;
	videoSendBitrate: number;
	videoRecvBitrate: number;
	audioPacketLoss: number;
	videoPacketLoss: number;
	rtt: number;
	jitter: number;
	participantCount: number;
	duration: number;
}

export interface PerTrackStats {
	direction: 'send' | 'recv';
	kind: 'audio' | 'video' | 'unknown';
	ssrc?: number;
	rid?: string;
	active?: boolean;
	mid?: string;
	trackIdentifier?: string;
	mediaSourceId?: string;
	codec?: string;
	codecSdpFmtpLine?: string;
	payloadType?: number;
	bitrateKbps: number;
	bitrateWindowMs?: number;
	packetsLost?: number;
	packetsLossPercent?: number;
	packetsReceived?: number;
	bytesReceived?: number;
	framesReceived?: number;
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
	framesProduced?: number;
	framesAccepted?: number;
	framesCoalesced?: number;
	framesCaptured?: number;
	captureFailures?: number;
	maxQueueAgeMs?: number;
	maxPushLatencyMs?: number;
	adaptiveSendTier?: string;
	adaptiveSendReason?: string;
	sourceFrames?: number;
	framesEncoded?: number;
	framesDecoded?: number;
	framesDropped?: number;
	framesSent?: number;
	freezeCount?: number;
	totalFreezesDurationMs?: number;
	totalEncodeTimeMs?: number;
	encoderImplementation?: string;
	powerEfficientEncoder?: boolean;
	encoderAcceleration?: VideoAccelerationStatus;
	scalabilityMode?: string;
	qualityLimitationReason?: string;
	qualityLimitationResolutionChanges?: number;
	targetBitrateKbps?: number;
	totalPacketSendDelayMs?: number;
	nackCount?: number;
	pliCount?: number;
	firCount?: number;
	retransmittedPacketsSent?: number;
	retransmittedBytesSent?: number;
	keyFramesEncoded?: number;
	keyFramesDecoded?: number;
	decoderImplementation?: string;
	powerEfficientDecoder?: boolean;
	decoderAcceleration?: VideoAccelerationStatus;
	totalDecodeTimeMs?: number;
	jitterBufferDelayMs?: number;
	jitterBufferEmittedCount?: number;
	concealedSamples?: number;
	silentConcealedSamples?: number;
	totalSamplesReceived?: number;
}

export interface TransportInfo {
	candidatePairState?: string;
	localCandidateType?: string;
	localProtocol?: string;
	localRelayProtocol?: string;
	localCandidateUrlHost?: string;
	peerConnectionMode?: string;
	localNetworkType?: string;
	remoteCandidateType?: string;
	remoteProtocol?: string;
	currentRoundTripTimeMs?: number;
	availableOutgoingBitrate?: number;
	availableIncomingBitrate?: number;
	dtlsState?: string;
	iceState?: string;
	selectedCandidatePairChanges?: number;
	dtlsCipher?: string;
	srtpCipher?: string;
	tlsVersion?: string;
	networkType?: string;
}

export interface VoiceStatsSample {
	timestamp: number;
	rtt: number;
	jitter: number;
	audioPacketLoss: number;
	videoPacketLoss: number;
	audioSendBitrate: number;
	audioRecvBitrate: number;
	videoSendBitrate: number;
	videoRecvBitrate: number;
}

type RoomWithEngine = Room & {
	engine?: {
		client?: {
			rtt?: number;
		};
		pcManager?: {
			publisher?: StatsSource;
			subscriber?: StatsSource;
			mode?: string;
		};
	};
};

interface StatsSource {
	getStats(): Promise<StatsReportMap>;
	getTransceivers?(): ReadonlyArray<StatsTransceiver>;
}

interface StatsTransceiver {
	mid: string | null;
	sender?: {track?: {id: string} | null} | null;
}

interface StatsReportMap {
	values(): IterableIterator<unknown>;
}

interface RTCStatsEntry {
	type: string;
	id: string;
	kind?: string;
	mediaType?: string;
	bytesSent?: number;
	bytesReceived?: number;
	packetsSent?: number;
	packetsLost?: number;
	packetsReceived?: number;
	jitter?: number;
	state?: string;
	currentRoundTripTime?: number;
	availableOutgoingBitrate?: number;
	availableIncomingBitrate?: number;
	codecId?: string;
	mimeType?: string;
	sdpFmtpLine?: string;
	payloadType?: number;
	ssrc?: number;
	rid?: string;
	active?: boolean;
	mid?: string;
	trackId?: string;
	trackIdentifier?: string;
	mediaSourceId?: string;
	framesPerSecond?: number;
	frameWidth?: number;
	frameHeight?: number;
	width?: number;
	height?: number;
	frames?: number;
	framesReceived?: number;
	framesEncoded?: number;
	framesDecoded?: number;
	framesDropped?: number;
	framesSent?: number;
	freezeCount?: number;
	totalFreezesDuration?: number;
	totalEncodeTime?: number;
	encoderImplementation?: string;
	powerEfficientEncoder?: boolean;
	scalabilityMode?: string;
	qualityLimitationReason?: string;
	qualityLimitationResolutionChanges?: number;
	targetBitrate?: number;
	totalPacketSendDelay?: number;
	localCandidateId?: string;
	remoteCandidateId?: string;
	candidateType?: string;
	protocol?: string;
	relayProtocol?: string;
	url?: string;
	networkType?: string;
	nominated?: boolean;
	selected?: boolean;
	dtlsState?: string;
	iceState?: string;
	selectedCandidatePairChanges?: number;
	selectedCandidatePairId?: string;
	nackCount?: number;
	pliCount?: number;
	firCount?: number;
	retransmittedPacketsSent?: number;
	retransmittedBytesSent?: number;
	keyFramesEncoded?: number;
	keyFramesDecoded?: number;
	decoderImplementation?: string;
	powerEfficientDecoder?: boolean;
	totalDecodeTime?: number;
	jitterBufferDelay?: number;
	jitterBufferEmittedCount?: number;
	concealedSamples?: number;
	silentConcealedSamples?: number;
	totalSamplesReceived?: number;
	dtlsCipher?: string;
	srtpCipher?: string;
	tlsVersion?: string;
	address?: string;
	ip?: string;
	port?: number;
}

const LATENCY_UPDATE_INTERVAL_MS = 2000;
const STATS_CLOCK_INTERVAL_MS = 1000;
const STATS_UPDATE_INTERVAL_MS = 2000;

interface BitrateSample {
	bitrateKbps: number;
	windowMs?: number;
}

function normalizeTrackKind(kind: string | undefined): PerTrackStats['kind'] {
	if (kind === 'audio' || kind === 'video') return kind;
	return 'unknown';
}

function getReportKind(report: RTCStatsEntry, reportsById: Map<string, RTCStatsEntry>): PerTrackStats['kind'] {
	const directKind = normalizeTrackKind(report.kind ?? report.mediaType);
	if (directKind !== 'unknown') return directKind;
	const codec = report.codecId ? reportsById.get(report.codecId) : undefined;
	if (codec?.mimeType?.startsWith('audio/')) return 'audio';
	if (codec?.mimeType?.startsWith('video/')) return 'video';
	const track = report.trackId ? reportsById.get(report.trackId) : undefined;
	const mediaSource = report.mediaSourceId ? reportsById.get(report.mediaSourceId) : undefined;
	return normalizeTrackKind(track?.kind ?? track?.mediaType ?? mediaSource?.kind ?? mediaSource?.mediaType);
}

function getBitrateSample(
	reportId: string,
	currentBytes: number | undefined,
	now: number,
	rtpCounters: Map<string, VoiceStatsRtpCounter>,
): BitrateSample {
	if (typeof currentBytes !== 'number' || !Number.isFinite(currentBytes)) {
		return {bitrateKbps: 0};
	}
	const previous = rtpCounters.get(reportId);
	let bitrateKbps = 0;
	let windowMs: number | undefined;
	if (previous?.bytes !== undefined) {
		const dt = (now - previous.timestamp) / 1000;
		const db = currentBytes - previous.bytes;
		if (dt > 0 && db >= 0) {
			bitrateKbps = (db * 8) / 1000 / dt;
			windowMs = Math.round(now - previous.timestamp);
		}
	}
	rtpCounters.set(reportId, {
		...previous,
		bytes: currentBytes,
		timestamp: now,
	});
	return {bitrateKbps, windowMs};
}

function hasUsableLossDeltas(lostDelta: number, receivedDelta: number): boolean {
	if (lostDelta < 0) return false;
	if (receivedDelta < 0) return false;
	return lostDelta + receivedDelta > 0;
}

function getPacketLossPercent(
	reportId: string,
	report: RTCStatsEntry,
	now: number,
	rtpCounters: Map<string, VoiceStatsRtpCounter>,
): number | undefined {
	if (typeof report.packetsLost !== 'number') return undefined;
	if (typeof report.packetsReceived !== 'number') return undefined;
	const previous = rtpCounters.get(reportId);
	let lostPackets = report.packetsLost;
	let receivedPackets = report.packetsReceived;
	if (previous?.packetsLost !== undefined && previous.packetsReceived !== undefined) {
		const lostDelta = report.packetsLost - previous.packetsLost;
		const receivedDelta = report.packetsReceived - previous.packetsReceived;
		if (hasUsableLossDeltas(lostDelta, receivedDelta)) {
			lostPackets = lostDelta;
			receivedPackets = receivedDelta;
		}
	}
	rtpCounters.set(reportId, {
		...previous,
		packetsLost: report.packetsLost,
		packetsReceived: report.packetsReceived,
		timestamp: now,
	});
	const totalPackets = receivedPackets + lostPackets;
	return totalPackets > 0 ? (lostPackets / totalPackets) * 100 : undefined;
}

function isActiveCandidatePair(report: RTCStatsEntry): boolean {
	if (report.type !== 'candidate-pair') return false;
	if (report.state !== 'succeeded') return false;
	if (report.selected) return true;
	return report.nominated !== false;
}

function shouldAdoptActivePair(report: RTCStatsEntry, activePair: RTCStatsEntry | null): boolean {
	if (!activePair) return true;
	if (typeof report.availableOutgoingBitrate !== 'number') return false;
	return !activePair.availableOutgoingBitrate;
}

function hasUsableJitterBuffer(report: RTCStatsEntry): boolean {
	if (typeof report.jitterBufferDelay !== 'number') return false;
	if (typeof report.jitterBufferEmittedCount !== 'number') return false;
	return report.jitterBufferEmittedCount > 0;
}

function buildOutboundTrackExtras(report: RTCStatsEntry): Partial<PerTrackStats> {
	return {
		active: report.active,
		retransmittedPacketsSent: report.retransmittedPacketsSent,
		retransmittedBytesSent: report.retransmittedBytesSent,
		keyFramesEncoded: report.keyFramesEncoded,
	};
}

function buildInboundTrackExtras(
	report: RTCStatsEntry,
	decoderAcceleration: VideoAccelerationStatus | undefined,
): Partial<PerTrackStats> {
	return {
		packetsReceived: report.packetsReceived,
		bytesReceived: report.bytesReceived,
		framesReceived: report.framesReceived,
		keyFramesDecoded: report.keyFramesDecoded,
		decoderImplementation: report.decoderImplementation,
		powerEfficientDecoder: report.powerEfficientDecoder,
		decoderAcceleration,
		totalDecodeTimeMs:
			typeof report.totalDecodeTime === 'number' ? Math.round(report.totalDecodeTime * 1000) : undefined,
		jitterBufferDelayMs: hasUsableJitterBuffer(report)
			? Math.round((report.jitterBufferDelay! / report.jitterBufferEmittedCount!) * 1000)
			: undefined,
		jitterBufferEmittedCount: report.jitterBufferEmittedCount,
		concealedSamples: report.concealedSamples,
		silentConcealedSamples: report.silentConcealedSamples,
		totalSamplesReceived: report.totalSamplesReceived,
	};
}

function buildPerTrackStat(args: {
	report: RTCStatsEntry;
	sourceId: string;
	isOutbound: boolean;
	isInbound: boolean;
	now: number;
	rtpCounters: Map<string, VoiceStatsRtpCounter>;
	reportsById: Map<string, RTCStatsEntry>;
	midToSenderTrackId: Map<string, string>;
}): PerTrackStats {
	const {report, sourceId, isOutbound, isInbound, now, rtpCounters, reportsById, midToSenderTrackId} = args;
	const id = `${sourceId}:${report.type}:${report.id}`;
	const currentBytes = isOutbound ? report.bytesSent : report.bytesReceived;
	const bitrate = getBitrateSample(id, currentBytes, now, rtpCounters);
	const codec = report.codecId ? reportsById.get(report.codecId) : undefined;
	const mediaSource = report.mediaSourceId ? reportsById.get(report.mediaSourceId) : undefined;
	const packetsLossPercent = isInbound ? getPacketLossPercent(id, report, now, rtpCounters) : undefined;
	const kind = getReportKind(report, reportsById);
	const encoderAcceleration =
		isOutbound && kind === 'video'
			? classifyVideoEncoderAcceleration(report.encoderImplementation, report.powerEfficientEncoder)
			: undefined;
	const decoderAcceleration =
		isInbound && kind === 'video'
			? classifyVideoDecoderAcceleration(report.decoderImplementation, report.powerEfficientDecoder)
			: undefined;
	return {
		direction: isOutbound ? 'send' : 'recv',
		kind,
		ssrc: report.ssrc,
		rid: report.rid,
		mid: report.mid,
		trackIdentifier:
			report.trackIdentifier ??
			mediaSource?.trackIdentifier ??
			(isOutbound && report.mid ? midToSenderTrackId.get(report.mid) : undefined),
		mediaSourceId: report.mediaSourceId,
		codec: codec?.mimeType,
		codecSdpFmtpLine: codec?.sdpFmtpLine,
		payloadType: codec?.payloadType,
		bitrateKbps: Math.round(bitrate.bitrateKbps),
		bitrateWindowMs: bitrate.windowMs,
		packetsLost: report.packetsLost,
		packetsLossPercent: packetsLossPercent !== undefined ? Math.round(packetsLossPercent * 10) / 10 : undefined,
		jitterMs: typeof report.jitter === 'number' ? Math.round(report.jitter * 1000 * 10) / 10 : undefined,
		framesPerSecond: report.framesPerSecond,
		sourceFramesPerSecond: mediaSource?.framesPerSecond,
		frameWidth: report.frameWidth,
		frameHeight: report.frameHeight,
		sourceFrameWidth: mediaSource?.frameWidth ?? mediaSource?.width,
		sourceFrameHeight: mediaSource?.frameHeight ?? mediaSource?.height,
		sourceFrames: mediaSource?.frames,
		framesEncoded: report.framesEncoded,
		framesDecoded: report.framesDecoded,
		framesDropped: report.framesDropped,
		framesSent: report.framesSent,
		freezeCount: report.freezeCount,
		totalFreezesDurationMs:
			typeof report.totalFreezesDuration === 'number' ? Math.round(report.totalFreezesDuration * 1000) : undefined,
		totalEncodeTimeMs:
			typeof report.totalEncodeTime === 'number' ? Math.round(report.totalEncodeTime * 1000) : undefined,
		encoderImplementation: report.encoderImplementation,
		powerEfficientEncoder: report.powerEfficientEncoder,
		encoderAcceleration,
		scalabilityMode: report.scalabilityMode,
		qualityLimitationReason: report.qualityLimitationReason,
		qualityLimitationResolutionChanges: report.qualityLimitationResolutionChanges,
		targetBitrateKbps: typeof report.targetBitrate === 'number' ? Math.round(report.targetBitrate / 1000) : undefined,
		totalPacketSendDelayMs:
			typeof report.totalPacketSendDelay === 'number' ? Math.round(report.totalPacketSendDelay * 1000) : undefined,
		nackCount: report.nackCount,
		pliCount: report.pliCount,
		firCount: report.firCount,
		...(isOutbound ? buildOutboundTrackExtras(report) : buildInboundTrackExtras(report, decoderAcceleration)),
	};
}

function getCandidateUrlHost(url: string | undefined): string | undefined {
	if (!url) return undefined;
	const schemeIndex = url.indexOf(':');
	const authority = (schemeIndex === -1 ? url : url.slice(schemeIndex + 1)).split('?')[0];
	const portIndex = authority.lastIndexOf(':');
	const host = portIndex > 0 ? authority.slice(0, portIndex) : authority;
	return host || undefined;
}

function buildTransportInfo(
	activePair: RTCStatsEntry | null,
	transportReport: RTCStatsEntry | null,
	reportsById: Map<string, RTCStatsEntry>,
	peerConnectionMode: string | undefined,
): TransportInfo | null {
	if (!activePair && !transportReport) return null;
	const local = activePair?.localCandidateId ? reportsById.get(activePair.localCandidateId) : undefined;
	const remote = activePair?.remoteCandidateId ? reportsById.get(activePair.remoteCandidateId) : undefined;
	return {
		candidatePairState: activePair?.state,
		localCandidateType: local?.candidateType,
		localProtocol: local?.protocol,
		localRelayProtocol: local?.relayProtocol,
		localCandidateUrlHost: getCandidateUrlHost(local?.url),
		peerConnectionMode,
		localNetworkType: local?.networkType,
		remoteCandidateType: remote?.candidateType,
		remoteProtocol: remote?.protocol,
		currentRoundTripTimeMs:
			typeof activePair?.currentRoundTripTime === 'number'
				? Math.round(activePair.currentRoundTripTime * 1000)
				: undefined,
		availableOutgoingBitrate: activePair?.availableOutgoingBitrate,
		availableIncomingBitrate: activePair?.availableIncomingBitrate,
		dtlsState: transportReport?.dtlsState,
		iceState: transportReport?.iceState,
		selectedCandidatePairChanges: transportReport?.selectedCandidatePairChanges,
		dtlsCipher: transportReport?.dtlsCipher,
		srtpCipher: transportReport?.srtpCipher,
		tlsVersion: transportReport?.tlsVersion,
		networkType: local?.networkType,
	};
}

async function collectFromStatsSource(
	source: StatsSource,
	sourceId: string,
	now: number,
	rtpCounters: Map<string, VoiceStatsRtpCounter>,
	activeCounterIds: Set<string>,
	peerConnectionMode?: string,
): Promise<{
	tracks: Array<PerTrackStats>;
	rtt: number;
	transport: TransportInfo | null;
}> {
	const reports = await source.getStats();
	const midToSenderTrackId = new Map<string, string>();
	for (const transceiver of source.getTransceivers?.() ?? []) {
		const senderTrackId = transceiver.sender?.track?.id;
		if (transceiver.mid && senderTrackId) midToSenderTrackId.set(transceiver.mid, senderTrackId);
	}
	const reportsById = new Map<string, RTCStatsEntry>();
	for (const report of reports.values()) {
		const statsEntry = report as RTCStatsEntry;
		reportsById.set(statsEntry.id, statsEntry);
	}
	const tracks: Array<PerTrackStats> = [];
	let rtt = 0;
	let activePair: RTCStatsEntry | null = null;
	let transportReport: RTCStatsEntry | null = null;
	for (const raw of reports.values()) {
		const report = raw as RTCStatsEntry;
		if (isActiveCandidatePair(report)) {
			const crt = report.currentRoundTripTime;
			if (crt) rtt = Math.max(rtt, crt * 1000);
			if (shouldAdoptActivePair(report, activePair)) activePair = report;
			continue;
		}
		if (report.type === 'transport') {
			transportReport = report;
			continue;
		}
		const isOutbound = report.type === 'outbound-rtp';
		const isInbound = report.type === 'inbound-rtp';
		if (!isOutbound && !isInbound) continue;
		const id = `${sourceId}:${report.type}:${report.id}`;
		activeCounterIds.add(id);
		tracks.push(
			buildPerTrackStat({report, sourceId, isOutbound, isInbound, now, rtpCounters, reportsById, midToSenderTrackId}),
		);
	}
	if (!activePair && transportReport?.selectedCandidatePairId) {
		const selectedPair = reportsById.get(transportReport.selectedCandidatePairId);
		if (selectedPair?.type === 'candidate-pair') activePair = selectedPair;
	}
	const transport = buildTransportInfo(activePair, transportReport, reportsById, peerConnectionMode);
	return {tracks, rtt, transport};
}

function toVoiceEngineV2TrackKind(kind: PerTrackStats['kind']): VoiceEngineV2TrackKind | null {
	if (kind === 'audio' || kind === 'video') return kind;
	return null;
}

function getVoiceEngineV2StatsTrackSid(track: PerTrackStats): string {
	return (
		track.trackIdentifier ??
		track.mediaSourceId ??
		track.mid ??
		track.rid ??
		(track.ssrc !== undefined ? String(track.ssrc) : `${track.direction}:${track.kind}`)
	);
}

function toVoiceEngineV2OutboundStats(track: PerTrackStats): VoiceEngineV2OutboundStats | null {
	const kind = toVoiceEngineV2TrackKind(track.kind);
	if (!kind) return null;
	return {
		trackSid: getVoiceEngineV2StatsTrackSid(track),
		source: 'unknown',
		kind,
		bitrateKbps: track.bitrateKbps,
		packetsLost: track.packetsLost ?? 0,
		...(track.codec !== undefined ? {codec: track.codec} : {}),
		...(track.frameWidth !== undefined ? {width: track.frameWidth} : {}),
		...(track.frameHeight !== undefined ? {height: track.frameHeight} : {}),
		...(track.framesPerSecond !== undefined ? {fps: track.framesPerSecond} : {}),
		...(track.sourceFrameWidth !== undefined ? {sourceWidth: track.sourceFrameWidth} : {}),
		...(track.sourceFrameHeight !== undefined ? {sourceHeight: track.sourceFrameHeight} : {}),
		...(track.targetBitrateKbps !== undefined ? {targetBitrateKbps: track.targetBitrateKbps} : {}),
		...(track.configuredFramesPerSecond !== undefined ? {configuredFps: track.configuredFramesPerSecond} : {}),
		...(track.targetFramesPerSecond !== undefined ? {targetFps: track.targetFramesPerSecond} : {}),
		...(track.effectiveFramesPerSecond !== undefined ? {effectiveFps: track.effectiveFramesPerSecond} : {}),
		...(track.framesProduced !== undefined ? {framesProduced: track.framesProduced} : {}),
		...(track.framesAccepted !== undefined ? {framesAccepted: track.framesAccepted} : {}),
		...(track.framesDropped !== undefined ? {framesDropped: track.framesDropped} : {}),
		...(track.framesCoalesced !== undefined ? {framesCoalesced: track.framesCoalesced} : {}),
		...(track.framesCaptured !== undefined ? {framesCaptured: track.framesCaptured} : {}),
		...(track.captureFailures !== undefined ? {captureFailures: track.captureFailures} : {}),
		...(track.maxQueueAgeMs !== undefined ? {maxQueueAgeMs: track.maxQueueAgeMs} : {}),
		...(track.maxPushLatencyMs !== undefined ? {maxPushLatencyMs: track.maxPushLatencyMs} : {}),
		...(track.adaptiveSendTier !== undefined ? {adaptiveSendTier: track.adaptiveSendTier} : {}),
		...(track.adaptiveSendReason !== undefined ? {adaptiveSendReason: track.adaptiveSendReason} : {}),
	};
}

function toVoiceEngineV2InboundStats(track: PerTrackStats): VoiceEngineV2InboundStats | null {
	const kind = toVoiceEngineV2TrackKind(track.kind);
	if (!kind) return null;
	return {
		trackSid: getVoiceEngineV2StatsTrackSid(track),
		kind,
		bitrateKbps: track.bitrateKbps,
		packetsLost: track.packetsLost ?? 0,
		...(track.codec !== undefined ? {codec: track.codec} : {}),
		...(track.jitterMs !== undefined ? {jitterMs: track.jitterMs} : {}),
		...(track.frameWidth !== undefined ? {width: track.frameWidth} : {}),
		...(track.frameHeight !== undefined ? {height: track.frameHeight} : {}),
		...(track.framesPerSecond !== undefined ? {fps: track.framesPerSecond} : {}),
		...(track.sourceFrameWidth !== undefined ? {sourceWidth: track.sourceFrameWidth} : {}),
		...(track.sourceFrameHeight !== undefined ? {sourceHeight: track.sourceFrameHeight} : {}),
	};
}

function toVoiceEngineV2Stats(rttMs: number | null, tracks: ReadonlyArray<PerTrackStats>): VoiceEngineV2Stats {
	return {
		rttMs,
		outbound: tracks.flatMap((track) =>
			track.direction === 'send' ? (toVoiceEngineV2OutboundStats(track) ?? []) : [],
		),
		inbound: tracks.flatMap((track) => (track.direction === 'recv' ? (toVoiceEngineV2InboundStats(track) ?? []) : [])),
	};
}

export interface VoiceEngineV2AppStatsHostAdapterScheduler {
	setInterval(handler: () => void, intervalMs: number): unknown;
	clearInterval(handle: unknown): void;
}

export interface VoiceEngineV2AppStatsHostAdapterOptions {
	now?: () => number;
	scheduler?: VoiceEngineV2AppStatsHostAdapterScheduler;
}

const DEFAULT_STATS_SCHEDULER: VoiceEngineV2AppStatsHostAdapterScheduler = {
	setInterval(handler, intervalMs) {
		return setInterval(handler, intervalMs);
	},
	clearInterval(handle) {
		clearInterval(handle as NodeJS.Timeout);
	},
};

export class VoiceEngineV2AppStatsHostAdapter extends Store {
	private readonly now: () => number;
	private readonly scheduler: VoiceEngineV2AppStatsHostAdapterScheduler;
	private room: Room | null = null;
	private latencyIntervalId: unknown = null;
	private statsIntervalId: unknown = null;
	private statsClockIntervalId: unknown = null;
	private statsSnapshot: VoiceStatsSnapshot = createVoiceStatsSnapshot();
	currentLatency: number | null = null;
	averageLatency: number | null = null;
	latencyHistory: Array<LatencyDataPoint> = [];
	voiceStats: VoiceStats = createInitialVoiceStats();
	perTrackStats: Array<PerTrackStats> = [];
	statsTimeSeries: Array<VoiceStatsSample> = [];
	publisherTransport: TransportInfo | null = null;
	subscriberTransport: TransportInfo | null = null;
	connectionStartTime: number | null = null;
	reconnectionCount: number = 0;
	private graphStatsConnectionId: string | null = null;

	constructor(options: VoiceEngineV2AppStatsHostAdapterOptions = {}) {
		super();
		this.now = options.now ?? (() => Date.now());
		this.scheduler = options.scheduler ?? DEFAULT_STATS_SCHEDULER;
		assert.equal(typeof this.now, 'function', 'now must be function');
		assertNonNullObject(this.scheduler, 'scheduler');
	}

	private transitionStats(event: VoiceStatsEvent): void {
		this.update(() => {
			this.applyStatsEvent(event);
		});
	}

	private applyStatsEvent(event: VoiceStatsEvent): void {
		this.statsSnapshot = transitionVoiceStatsSnapshot(this.statsSnapshot, event);
		this.syncFromStatsSnapshot();
	}

	private syncFromStatsSnapshot(): void {
		const context = this.statsSnapshot.context;
		this.currentLatency = context.currentLatency;
		this.averageLatency = context.averageLatency;
		this.latencyHistory = context.latencyHistory;
		this.voiceStats = context.voiceStats;
		this.perTrackStats = context.perTrackStats;
		this.statsTimeSeries = context.statsTimeSeries;
		this.publisherTransport = context.publisherTransport;
		this.subscriberTransport = context.subscriberTransport;
		this.connectionStartTime = context.connectionStartTime;
		this.reconnectionCount = context.reconnectionCount;
	}

	get estimatedLatency(): number | null {
		if (!this.room) return null;
		const localParticipant = this.room.localParticipant;
		if (!localParticipant) return null;
		const quality = asVoiceConnectionQuality(localParticipant.connectionQuality);
		switch (quality) {
			case VoiceConnectionQuality.Excellent:
				return 30;
			case VoiceConnectionQuality.Good:
				return 60;
			case VoiceConnectionQuality.Poor:
				return 120;
			default:
				return null;
		}
	}

	get displayLatency(): number | null {
		const measured = this.currentLatency;
		return measured !== null ? measured : this.estimatedLatency;
	}

	get duration(): number {
		if (!this.connectionStartTime) return 0;
		return Math.floor((this.now() - this.connectionStartTime) / 1000);
	}

	setRoom(room: Room | null): void {
		assert.ok(room === null || typeof room === 'object', 'room must be null or object');
		this.update(() => {
			this.room = room;
			this.applyStatsEvent({type: 'room.set', roomIdentity: room, now: this.now()});
		});
		assert.equal(this.room, room, 'room must be persisted after set');
	}

	async collectStats(): Promise<VoiceEngineV2Stats> {
		const room = this.room as RoomWithEngine | null;
		const engine = room?.engine;
		if (!engine?.pcManager) {
			return {
				rttMs: this.currentLatency ?? this.displayLatency,
				outbound: [],
				inbound: [],
			};
		}
		const now = this.now();
		assertFiniteNumber(now, 'now');
		const tracks: Array<PerTrackStats> = [];
		const activeCounterIds = new Set<string>();
		const rtpCounters = new Map<string, VoiceStatsRtpCounter>(this.statsSnapshot.context.rtpCounters);
		let rtt = 0;
		if (engine.pcManager.publisher) {
			const result = await collectFromStatsSource(
				engine.pcManager.publisher,
				'publisher',
				now,
				rtpCounters,
				activeCounterIds,
			);
			tracks.push(...result.tracks);
			if (result.rtt > rtt) rtt = result.rtt;
		}
		if (engine.pcManager.subscriber) {
			const result = await collectFromStatsSource(
				engine.pcManager.subscriber,
				'subscriber',
				now,
				rtpCounters,
				activeCounterIds,
			);
			tracks.push(...result.tracks);
			if (result.rtt > rtt) rtt = result.rtt;
		}
		if (rtt === 0 && engine.client?.rtt) rtt = engine.client.rtt;
		const result = toVoiceEngineV2Stats(rtt > 0 ? Math.round(rtt) : null, tracks);
		assertNonNullObject(result, 'result');
		assert.ok(Array.isArray(result.outbound), 'result.outbound must be array');
		return result;
	}

	incrementReconnectionCount(): void {
		this.transitionStats({type: 'reconnection.increment'});
	}

	private clearLatencyInterval(): void {
		if (this.latencyIntervalId === null) return;
		this.scheduler.clearInterval(this.latencyIntervalId);
		this.latencyIntervalId = null;
	}

	private clearStatsInterval(): void {
		if (this.statsIntervalId === null) return;
		this.scheduler.clearInterval(this.statsIntervalId);
		this.statsIntervalId = null;
	}

	private onLatencyTick(): void {
		const timestamp = this.now();
		const engineWrap = this.room as RoomWithEngine;
		if (!engineWrap?.engine?.client?.rtt) {
			this.transitionStats({type: 'latency.gap', timestamp});
			return;
		}
		const rtt = Math.round(engineWrap.engine.client.rtt);
		this.transitionStats({type: 'latency.sample', timestamp, latency: rtt});
	}

	startLatencyTracking(): void {
		this.transitionStats({type: 'latency.start'});
		if (this.latencyIntervalId !== null) return;
		logger.debug('Starting latency tracking');
		this.latencyIntervalId = this.scheduler.setInterval(() => this.onLatencyTick(), LATENCY_UPDATE_INTERVAL_MS);
		assert.ok(this.latencyIntervalId !== null, 'latencyIntervalId must be set');
	}

	stopLatencyTracking(): void {
		this.clearLatencyInterval();
		this.transitionStats({type: 'latency.stop'});
		assert.equal(this.latencyIntervalId, null, 'latencyIntervalId must be null after stop');
	}

	private startStatsClock(): void {
		if (this.statsClockIntervalId !== null) return;
		this.transitionStats({type: 'stats.tick', timestamp: this.now()});
		this.statsClockIntervalId = this.scheduler.setInterval(() => {
			this.transitionStats({type: 'stats.tick', timestamp: this.now()});
		}, STATS_CLOCK_INTERVAL_MS);
	}

	private stopStatsClock(): void {
		if (this.statsClockIntervalId === null) return;
		this.scheduler.clearInterval(this.statsClockIntervalId);
		this.statsClockIntervalId = null;
	}

	private async runStatsCollectionTick(): Promise<void> {
		const roomSnapshot = this.room;
		const decision = selectVoiceStatsCollectionDecision(this.statsSnapshot);
		if (decision.type !== 'collect') return;
		if (decision.roomIdentity !== roomSnapshot) return;
		const statsGeneration = decision.generation;
		this.transitionStats({
			type: 'stats.collectionStarted',
			generation: statsGeneration,
			roomIdentity: decision.roomIdentity,
		});
		if (!this.statsSnapshot.context.statsCollectionInFlight) return;
		try {
			await this.collectAndDispatchStats(roomSnapshot, decision, statsGeneration);
		} catch (error) {
			logger.debug('Error collecting stats', error);
		} finally {
			this.transitionStats({type: 'stats.collectionFinished', generation: statsGeneration});
		}
	}

	private async collectAndDispatchStats(
		roomSnapshot: Room | null,
		decision: VoiceStatsCollectDecision,
		statsGeneration: number,
	): Promise<void> {
		const room = roomSnapshot as RoomWithEngine | null;
		const engine = room?.engine;
		if (!engine?.pcManager) return;
		const publisher = engine.pcManager.publisher;
		const subscriber = engine.pcManager.subscriber;
		const peerConnectionMode = engine.pcManager.mode;
		if (!publisher && !subscriber) return;
		const now = this.now();
		const tracks: Array<PerTrackStats> = [];
		const activeCounterIds = new Set<string>();
		const rtpCounters = new Map<string, VoiceStatsRtpCounter>(decision.rtpCounters);
		let rtt = 0;
		let publisherTransport: TransportInfo | null = null;
		let subscriberTransport: TransportInfo | null = null;
		if (publisher) {
			const result = await collectFromStatsSource(
				publisher,
				'publisher',
				now,
				rtpCounters,
				activeCounterIds,
				peerConnectionMode,
			);
			tracks.push(...result.tracks);
			if (result.rtt > rtt) rtt = result.rtt;
			publisherTransport = result.transport;
		}
		if (subscriber) {
			const result = await collectFromStatsSource(
				subscriber,
				'subscriber',
				now,
				rtpCounters,
				activeCounterIds,
				peerConnectionMode,
			);
			tracks.push(...result.tracks);
			if (result.rtt > rtt) rtt = result.rtt;
			subscriberTransport = result.transport;
		}
		if (rtt === 0 && engine.client?.rtt) rtt = engine.client.rtt;
		this.transitionStats({
			type: 'stats.collectionSucceeded',
			generation: statsGeneration,
			roomIdentity: decision.roomIdentity,
			timestamp: now,
			participantCount: roomSnapshot?.numParticipants ?? 0,
			rtt,
			tracks,
			publisherTransport,
			subscriberTransport,
			rtpCounters,
			activeCounterIds,
		});
		this.dispatchGraphStatsObservations(roomSnapshot, tracks);
	}

	private resolveGraphStatsConnectionId(roomSnapshot: Room | null): string | null {
		const identity = roomSnapshot?.localParticipant?.identity;
		if (!identity) return null;
		const {connectionId} = parseVoiceParticipantIdentity(identity);
		return connectionId || null;
	}

	private dispatchGraphStatsObservations(roomSnapshot: Room | null, tracks: Array<PerTrackStats>): void {
		const connectionId = this.resolveGraphStatsConnectionId(roomSnapshot);
		if (!connectionId) return;
		if (this.graphStatsConnectionId !== connectionId) {
			voiceMediaGraphStore.transition({type: 'stats.connectionChanged', connectionId});
			this.graphStatsConnectionId = connectionId;
		}
		voiceMediaGraphStore.transition({
			type: 'stats.observed',
			at: voiceMediaGraphStore.nowMs(),
			connectionId,
			platform: 'web',
			tracks: voiceMediaGraphStatsObservationsFromPerTrackStats(tracks),
		});
	}

	private clearGraphStatsObservations(): void {
		if (this.graphStatsConnectionId === null) return;
		voiceMediaGraphStore.transition({type: 'stats.connectionChanged', connectionId: null});
		this.graphStatsConnectionId = null;
	}

	startStatsTracking(): void {
		this.transitionStats({type: 'stats.start'});
		this.startStatsClock();
		if (this.statsIntervalId !== null) return;
		logger.debug('Starting stats tracking');
		this.statsIntervalId = this.scheduler.setInterval(() => {
			void this.runStatsCollectionTick();
		}, STATS_UPDATE_INTERVAL_MS);
		assert.ok(this.statsIntervalId !== null, 'statsIntervalId must be set');
	}

	stopStatsTracking(): void {
		this.clearStatsInterval();
		this.stopStatsClock();
		this.transitionStats({type: 'stats.stop'});
		assert.equal(this.statsIntervalId, null, 'statsIntervalId must be null after stop');
		assert.equal(this.statsClockIntervalId, null, 'statsClockIntervalId must be null after stop');
	}

	cleanup(): void {
		logger.debug('Cleaning up');
		this.clearLatencyInterval();
		this.clearStatsInterval();
		this.stopStatsClock();
		this.clearGraphStatsObservations();
		this.update(() => {
			this.room = null;
			this.applyStatsEvent({type: 'stats.cleanup'});
		});
		assert.equal(this.latencyIntervalId, null, 'latencyIntervalId must be null after cleanup');
		assert.equal(this.statsIntervalId, null, 'statsIntervalId must be null after cleanup');
	}

	reset(): void {
		this.clearLatencyInterval();
		this.clearStatsInterval();
		this.stopStatsClock();
		this.clearGraphStatsObservations();
		this.update(() => {
			this.room = null;
			this.applyStatsEvent({type: 'stats.reset'});
		});
		assert.equal(this.latencyIntervalId, null, 'latencyIntervalId must be null after reset');
	}
}
