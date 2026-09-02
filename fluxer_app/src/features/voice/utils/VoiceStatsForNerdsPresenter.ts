// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AppMetricsSnapshot, DesktopInfo, GpuInfo} from '@app/features/platform/types/Electron';
import {
	isScreenShareAudioPublicationLike,
	type VoiceTrackPublicationSourceLike,
	VoiceTrackSource,
} from '@app/features/voice/engine/VoiceTrackSource';
import type {ScreenShareAudioPumpDiagnostics} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareAudioPump';
import {
	classifyVoiceEngineV2TrackStats,
	selectVoiceEngineV2StatsPresentationProjection,
	type VoiceEngineV2PerTrackStats,
	type VoiceEngineV2Stats,
	type VoiceEngineV2StatsPresentationProjection,
	type VoiceEngineV2StatsSample,
	type VoiceEngineV2StatsTrackClassificationInput,
	type VoiceEngineV2StatsTrackRoleSelection,
	type VoiceEngineV2TransportInfo,
	type VoiceEngineV2VoiceStats,
} from '@fluxer/voice_engine_v2';
import type {Track} from 'livekit-client';

export interface StatsForNerdsData {
	session: {
		connectionId: string;
		connectionQuality: string;
		latencyMs: number | null;
		avgLatencyMs: number | null;
		durationSeconds: number;
		participants: number;
	};
	network: {
		audioSendBitrateKbps: number;
		audioRecvBitrateKbps: number;
		videoSendBitrateKbps: number;
		videoRecvBitrateKbps: number;
		audioPacketLossPercent: number;
		videoPacketLossPercent: number;
		jitterMs: number;
		rttMs: number | null;
		droppedVideoFrameCallbacks?: number;
		publisherTransport: VoiceEngineV2TransportInfo | null;
		subscriberTransport: VoiceEngineV2TransportInfo | null;
	};
	localVideo: VoiceEngineV2PerTrackStats | null;
	localAudio: VoiceEngineV2PerTrackStats | null;
	localScreenShare: VoiceEngineV2PerTrackStats | null;
	localScreenShareAudio: VoiceEngineV2PerTrackStats | null;
	remoteVideo: VoiceEngineV2PerTrackStats | null;
	remoteScreenShare: VoiceEngineV2PerTrackStats | null;
	remoteAudio: VoiceEngineV2PerTrackStats | null;
	remoteScreenShareAudio: VoiceEngineV2PerTrackStats | null;
	connection: {
		voiceServerEndpoint: string;
		reconnectionCount: number;
	};
	audio: {
		echoCancellation: boolean;
		noiseSuppression: boolean;
		autoGainControl: boolean;
		deepFilterNoiseSuppression: boolean;
		deepFilterNoiseSuppressionLevel: number;
		processingMode: string;
	};
	screenShareSettings: {
		resolution: string;
		frameRate: number;
		streamingMode: string;
		preferredCodec: string;
		selectedCodec: string | null;
		codecPreferenceOrder: Array<string>;
		contentHint: string;
		encoderMode: string;
		softwareQuality: string;
		scalabilityMode: string;
		backupCodecMode: string;
		maxBitrateMbps: number;
		audioSourceMode: string;
		audioIncludeSources: Array<Record<string, string>>;
		audioExcludeSources: Array<Record<string, string>>;
		shareDesktopAudio: boolean;
		shareAppAudio: boolean;
		muteStreamAudio: boolean;
		openH264Enabled: boolean;
	};
	screenShareAudioCapture: {
		pump: ScreenShareAudioPumpDiagnostics;
		nativeCapture: Record<string, unknown>;
	};
	appInfo: {
		appVersion: string;
		electronVersion: string | null;
		chromiumVersion: string | null;
		hardwareAccelerationEnabled: boolean | null;
		chromiumRuntime: DesktopInfo['chromiumRuntime'] | null;
	};
	gpu: GpuInfo | null;
	appMetrics: AppMetricsSnapshot | null;
	system: {
		platform: string;
		userAgent: string;
		hardwareConcurrency: number;
		deviceMemoryGB: number | null;
		jsHeapUsedMB: number | null;
		jsHeapTotalMB: number | null;
		jsHeapLimitMB: number | null;
	};
	heapHistory: Array<number>;
	cpuHistory: Array<number>;
	sparklines: {
		latency: Array<number>;
		bitrate: Array<number>;
		packetLoss: Array<number>;
	};
}

export interface VoiceStatsForNerdsPresentation {
	session: StatsForNerdsData['session'];
	network: StatsForNerdsData['network'];
	localVideo: VoiceEngineV2PerTrackStats | null;
	localAudio: VoiceEngineV2PerTrackStats | null;
	localScreenShare: VoiceEngineV2PerTrackStats | null;
	localScreenShareAudio: VoiceEngineV2PerTrackStats | null;
	remoteVideo: VoiceEngineV2PerTrackStats | null;
	remoteScreenShare: VoiceEngineV2PerTrackStats | null;
	remoteAudio: VoiceEngineV2PerTrackStats | null;
	remoteScreenShareAudio: VoiceEngineV2PerTrackStats | null;
	sparklines: StatsForNerdsData['sparklines'];
}

interface PublicationTrackLike {
	mediaStreamTrack?: MediaStreamTrack;
}

interface TrackPublicationLike extends VoiceTrackPublicationSourceLike {
	audioTrack?: PublicationTrackLike | null;
	videoTrack?: PublicationTrackLike | null;
	track?: PublicationTrackLike | null;
}

export interface ParticipantPublicationLookup {
	getTrackPublication(source: Track.Source): TrackPublicationLike | undefined;
	audioTrackPublications?: {
		values(): Iterable<TrackPublicationLike>;
	};
}

export interface VoiceStatsForNerdsPresentationInput {
	connectionId: string | null;
	connectionQuality: string;
	currentLatency: number | null;
	averageLatency: number | null;
	stats: VoiceEngineV2VoiceStats;
	perTrackStats: ReadonlyArray<VoiceEngineV2PerTrackStats>;
	statsTimeSeries: ReadonlyArray<VoiceEngineV2StatsSample>;
	nativeStats: VoiceEngineV2Stats | null;
	publisherTransport: VoiceEngineV2TransportInfo | null;
	subscriberTransport: VoiceEngineV2TransportInfo | null;
	localParticipant: ParticipantPublicationLookup | null | undefined;
	remoteParticipants: Iterable<ParticipantPublicationLookup> | null | undefined;
}

interface VoiceTrackStatsClassificationRequestInput {
	perTrackStats: ReadonlyArray<VoiceEngineV2PerTrackStats>;
	localParticipant: ParticipantPublicationLookup | null | undefined;
	remoteParticipants: Iterable<ParticipantPublicationLookup> | null | undefined;
}

const CAMERA_SOURCE = VoiceTrackSource.Camera as Track.Source;
const MICROPHONE_SOURCE = VoiceTrackSource.Microphone as Track.Source;
const SCREEN_SHARE_SOURCE = VoiceTrackSource.ScreenShare as Track.Source;
const SCREEN_SHARE_AUDIO_SOURCE = VoiceTrackSource.ScreenShareAudio as Track.Source;

function buildVoiceStatsSparklines(
	statsTimeSeries: ReadonlyArray<VoiceEngineV2StatsSample>,
): StatsForNerdsData['sparklines'] {
	const sampleCount = Math.min(statsTimeSeries.length, 60);
	const startIndex = statsTimeSeries.length - sampleCount;
	const latency = new Array<number>(sampleCount);
	const bitrate = new Array<number>(sampleCount);
	const packetLoss = new Array<number>(sampleCount);
	for (let i = 0; i < sampleCount; i += 1) {
		const sample = statsTimeSeries[startIndex + i];
		latency[i] = sample.rtt;
		bitrate[i] = sample.audioSendBitrate + sample.audioRecvBitrate + sample.videoSendBitrate + sample.videoRecvBitrate;
		packetLoss[i] = Math.max(sample.audioPacketLoss, sample.videoPacketLoss);
	}
	return {latency, bitrate, packetLoss};
}

function getPublicationTrackIdentifier(publication: TrackPublicationLike | null | undefined): string | null {
	const track =
		publication?.audioTrack?.mediaStreamTrack ??
		publication?.videoTrack?.mediaStreamTrack ??
		publication?.track?.mediaStreamTrack;
	return track?.id ?? null;
}

function collectRemotePublicationTrackIdentifiers(
	remoteParticipants: Iterable<ParticipantPublicationLookup> | null | undefined,
	source: Track.Source,
): Array<string> {
	const identifiers = new Set<string>();
	if (!remoteParticipants) return [];
	for (const participant of remoteParticipants) {
		const identifier = getPublicationTrackIdentifier(participant.getTrackPublication(source));
		if (identifier) {
			identifiers.add(identifier);
		}
	}
	return Array.from(identifiers);
}

function collectParticipantScreenShareAudioTrackIdentifiers(
	participant: ParticipantPublicationLookup | null | undefined,
): Array<string> {
	const identifiers = new Set<string>();
	const directIdentifier = getPublicationTrackIdentifier(participant?.getTrackPublication(SCREEN_SHARE_AUDIO_SOURCE));
	if (directIdentifier) {
		identifiers.add(directIdentifier);
	}
	const audioPublications = participant?.audioTrackPublications?.values();
	if (!audioPublications) {
		return Array.from(identifiers);
	}
	for (const publication of audioPublications) {
		if (!isScreenShareAudioPublicationLike(publication)) continue;
		const identifier = getPublicationTrackIdentifier(publication);
		if (identifier) {
			identifiers.add(identifier);
		}
	}
	return Array.from(identifiers);
}

function collectRemoteScreenShareAudioTrackIdentifiers(
	remoteParticipants: Iterable<ParticipantPublicationLookup> | null | undefined,
): Array<string> {
	const identifiers = new Set<string>();
	if (!remoteParticipants) return [];
	for (const participant of remoteParticipants) {
		for (const identifier of collectParticipantScreenShareAudioTrackIdentifiers(participant)) {
			identifiers.add(identifier);
		}
	}
	return Array.from(identifiers);
}

function buildTrackClassificationRequest({
	perTrackStats,
	localParticipant,
	remoteParticipants,
}: VoiceTrackStatsClassificationRequestInput): VoiceEngineV2StatsTrackClassificationInput {
	const localScreenShareAudioTrackIds = collectParticipantScreenShareAudioTrackIdentifiers(localParticipant);
	return {
		tracks: perTrackStats.map((track) => ({
			direction: track.direction,
			kind: track.kind,
			rid: track.rid,
			trackIdentifier: track.trackIdentifier,
			bitrateKbps: track.bitrateKbps,
		})),
		publications: {
			localCameraTrackId:
				localParticipant?.getTrackPublication(CAMERA_SOURCE)?.videoTrack?.mediaStreamTrack?.id ?? null,
			localMicrophoneTrackId: getPublicationTrackIdentifier(localParticipant?.getTrackPublication(MICROPHONE_SOURCE)),
			localScreenShareTrackId:
				localParticipant?.getTrackPublication(SCREEN_SHARE_SOURCE)?.videoTrack?.mediaStreamTrack?.id ?? null,
			localScreenShareAudioTrackId: localScreenShareAudioTrackIds[0] ?? null,
			remoteMicrophoneTrackIds: collectRemotePublicationTrackIdentifiers(remoteParticipants, MICROPHONE_SOURCE),
			remoteScreenShareTrackIds: collectRemotePublicationTrackIdentifiers(remoteParticipants, SCREEN_SHARE_SOURCE),
			remoteScreenShareAudioTrackIds: collectRemoteScreenShareAudioTrackIdentifiers(remoteParticipants),
		},
	};
}

function getClassifiedTrack(
	perTrackStats: ReadonlyArray<VoiceEngineV2PerTrackStats>,
	classification: VoiceEngineV2StatsTrackRoleSelection | null,
	index: number | null | undefined,
): VoiceEngineV2PerTrackStats | null {
	if (!classification || index == null || index < 0 || index >= perTrackStats.length) return null;
	return perTrackStats[index] ?? null;
}

function selectNativeStatsProjection(
	nativeStats: VoiceEngineV2Stats | null,
): VoiceEngineV2StatsPresentationProjection | null {
	return selectVoiceEngineV2StatsPresentationProjection(nativeStats);
}

function selectNetworkRttMs(
	nativeProjection: VoiceEngineV2StatsPresentationProjection | null,
	currentLatency: number | null,
	stats: VoiceEngineV2VoiceStats,
): number | null {
	if (!nativeProjection) return stats.rtt;
	return nativeProjection.network.rttMs ?? currentLatency;
}

export function buildVoiceStatsForNerdsPresentation(
	input: VoiceStatsForNerdsPresentationInput,
): VoiceStatsForNerdsPresentation {
	const {
		connectionId,
		connectionQuality,
		currentLatency,
		averageLatency,
		stats,
		perTrackStats,
		statsTimeSeries,
		nativeStats,
		publisherTransport,
		subscriberTransport,
		localParticipant,
		remoteParticipants,
	} = input;
	const classificationRequest = buildTrackClassificationRequest({perTrackStats, localParticipant, remoteParticipants});
	const classification = classifyVoiceEngineV2TrackStats(classificationRequest);
	const localVideo = getClassifiedTrack(perTrackStats, classification, classification?.localVideoTrackIndex);
	const localAudio = getClassifiedTrack(perTrackStats, classification, classification?.localAudioTrackIndex);
	const localScreenShare = getClassifiedTrack(
		perTrackStats,
		classification,
		classification?.localScreenShareTrackIndex,
	);
	const localScreenShareAudio = getClassifiedTrack(
		perTrackStats,
		classification,
		classification?.localScreenShareAudioTrackIndex,
	);
	const remoteVideo = getClassifiedTrack(perTrackStats, classification, classification?.remoteVideoTrackIndex);
	const remoteAudio = getClassifiedTrack(perTrackStats, classification, classification?.remoteAudioTrackIndex);
	const remoteScreenShare = getClassifiedTrack(
		perTrackStats,
		classification,
		classification?.remoteScreenShareTrackIndex,
	);
	const remoteScreenShareAudio = getClassifiedTrack(
		perTrackStats,
		classification,
		classification?.remoteScreenShareAudioTrackIndex,
	);
	const nativeProjection = selectNativeStatsProjection(nativeStats);
	return {
		session: {
			connectionId: connectionId ?? 'n/a',
			connectionQuality,
			latencyMs: currentLatency,
			avgLatencyMs: averageLatency,
			durationSeconds: stats.duration,
			participants: stats.participantCount,
		},
		network: {
			audioSendBitrateKbps: nativeProjection?.network.audioSendBitrateKbps ?? stats.audioSendBitrate,
			audioRecvBitrateKbps: nativeProjection?.network.audioRecvBitrateKbps ?? stats.audioRecvBitrate,
			videoSendBitrateKbps: nativeProjection?.network.videoSendBitrateKbps ?? stats.videoSendBitrate,
			videoRecvBitrateKbps: nativeProjection?.network.videoRecvBitrateKbps ?? stats.videoRecvBitrate,
			audioPacketLossPercent: nativeProjection?.network.audioPacketLossPercent ?? stats.audioPacketLoss,
			videoPacketLossPercent: nativeProjection?.network.videoPacketLossPercent ?? stats.videoPacketLoss,
			jitterMs: nativeProjection?.network.jitterMs ?? stats.jitter,
			rttMs: selectNetworkRttMs(nativeProjection, currentLatency, stats),
			droppedVideoFrameCallbacks: nativeProjection?.network.droppedVideoFrameCallbacks,
			publisherTransport,
			subscriberTransport,
		},
		localVideo: nativeProjection ? nativeProjection.localVideo : localVideo,
		localAudio: nativeProjection ? nativeProjection.localAudio : localAudio,
		localScreenShare: nativeProjection ? nativeProjection.localScreenShare : localScreenShare,
		localScreenShareAudio: nativeProjection ? nativeProjection.localScreenShareAudio : localScreenShareAudio,
		remoteVideo: nativeProjection ? nativeProjection.remoteVideo : remoteVideo,
		remoteScreenShare: nativeProjection ? nativeProjection.remoteScreenShare : remoteScreenShare,
		remoteAudio: nativeProjection ? nativeProjection.remoteAudio : remoteAudio,
		remoteScreenShareAudio: nativeProjection ? nativeProjection.remoteScreenShareAudio : remoteScreenShareAudio,
		sparklines: buildVoiceStatsSparklines(statsTimeSeries),
	};
}
