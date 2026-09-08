// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildVoiceStatsForNerdsPresentation,
	collectScreenShareAudioPublicationDiagnostics,
	type ParticipantPublicationLookup,
} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';
import type {
	VoiceEngineV2PerTrackStats,
	VoiceEngineV2Stats,
	VoiceEngineV2StatsSample,
	VoiceEngineV2VoiceStats,
} from '@fluxer/voice_engine_v2';
import {describe, expect, it} from 'vitest';

function participant(publications: Record<string, string>): ParticipantPublicationLookup {
	return {
		getTrackPublication(source) {
			const trackId = publications[String(source)];
			if (!trackId) return undefined;
			const mediaStreamTrack = {id: trackId} as MediaStreamTrack;
			if (String(source) === 'camera' || String(source) === 'screen_share') {
				return {videoTrack: {mediaStreamTrack}};
			}
			if (String(source) === 'microphone' || String(source) === 'screen_share_audio') {
				return {audioTrack: {mediaStreamTrack}};
			}
			return {track: {mediaStreamTrack}};
		},
	};
}

function participantWithAudioPublications(
	publications: Record<string, string>,
	audioPublications: Array<{
		source: string;
		trackName?: string;
		trackId: string;
	}>,
): ParticipantPublicationLookup {
	const baseParticipant = participant(publications);
	return {
		getTrackPublication: baseParticipant.getTrackPublication,
		audioTrackPublications: new Map(
			audioPublications.map((publication) => [
				publication.trackId,
				{
					source: publication.source,
					trackName: publication.trackName,
					audioTrack: {
						mediaStreamTrack: {id: publication.trackId} as MediaStreamTrack,
					},
				},
			]),
		),
	};
}

const voiceStats: VoiceEngineV2VoiceStats = {
	audioSendBitrate: 48,
	audioRecvBitrate: 64,
	videoSendBitrate: 1200,
	videoRecvBitrate: 900,
	audioPacketLoss: 1.5,
	videoPacketLoss: 2.5,
	rtt: 37,
	jitter: 8,
	participantCount: 3,
	duration: 12,
};

const timeSeries: Array<VoiceEngineV2StatsSample> = [
	{
		timestamp: 1000,
		rtt: 31,
		jitter: 6,
		audioPacketLoss: 1,
		videoPacketLoss: 2,
		audioSendBitrate: 40,
		audioRecvBitrate: 50,
		videoSendBitrate: 600,
		videoRecvBitrate: 700,
	},
];

function nativeStats(overrides: Partial<VoiceEngineV2Stats>): VoiceEngineV2Stats {
	return {rttMs: null, outbound: [], inbound: [], ...overrides};
}

function chromiumCameraSimulcastTracks(): Array<VoiceEngineV2PerTrackStats> {
	return [
		{
			direction: 'send',
			kind: 'video',
			ssrc: 1001,
			rid: 'q',
			active: false,
			mid: '2',
			trackIdentifier: 'cam-track',
			mediaSourceId: 'SV1',
			bitrateKbps: 0,
			bitrateWindowMs: 2001,
			framesEncoded: 1,
			targetBitrateKbps: 0,
		},
		{
			direction: 'send',
			kind: 'video',
			ssrc: 1002,
			rid: 'h',
			active: false,
			mid: '2',
			trackIdentifier: 'cam-track',
			mediaSourceId: 'SV1',
			bitrateKbps: 0,
			bitrateWindowMs: 2001,
			framesEncoded: 1,
			targetBitrateKbps: 0,
		},
		{
			direction: 'send',
			kind: 'video',
			ssrc: 1003,
			rid: 'f',
			active: true,
			mid: '2',
			trackIdentifier: 'cam-track',
			mediaSourceId: 'SV1',
			bitrateKbps: 0,
			bitrateWindowMs: 2001,
			framesEncoded: 1,
			frameWidth: 1280,
			frameHeight: 720,
			targetBitrateKbps: 1133,
		},
		{
			direction: 'send',
			kind: 'video',
			ssrc: 2001,
			mid: '3',
			trackIdentifier: 'screen-track',
			mediaSourceId: 'SV2',
			bitrateKbps: 3500,
		},
	];
}

describe('buildVoiceStatsForNerdsPresentation', () => {
	it('classifies browser per-track stats once for overlay and copy consumers', () => {
		const localParticipant = participant({
			camera: 'local-camera',
			microphone: 'local-mic',
			screen_share: 'local-screen',
			screen_share_audio: 'local-screen-audio',
		});
		const remoteParticipant = participant({
			microphone: 'remote-mic',
			screen_share: 'remote-screen',
			screen_share_audio: 'remote-screen-audio',
		});
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = [
			{direction: 'send', kind: 'video', trackIdentifier: 'local-camera', bitrateKbps: 500},
			{direction: 'send', kind: 'audio', trackIdentifier: 'local-mic', bitrateKbps: 48},
			{direction: 'send', kind: 'video', trackIdentifier: 'local-screen', bitrateKbps: 1200},
			{direction: 'send', kind: 'audio', trackIdentifier: 'local-screen-audio', bitrateKbps: 96},
			{direction: 'recv', kind: 'audio', trackIdentifier: 'remote-mic', bitrateKbps: 64},
			{direction: 'recv', kind: 'video', trackIdentifier: 'remote-camera', bitrateKbps: 900},
			{direction: 'recv', kind: 'video', trackIdentifier: 'remote-screen', bitrateKbps: 1800},
			{direction: 'recv', kind: 'audio', trackIdentifier: 'remote-screen-audio', bitrateKbps: 128},
		];

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant,
			remoteParticipants: [remoteParticipant],
		});

		expect(presentation.session).toMatchObject({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			latencyMs: 37,
			avgLatencyMs: 41,
			durationSeconds: 12,
			participants: 3,
		});
		expect(presentation.localVideo?.trackIdentifier).toBe('local-camera');
		expect(presentation.localAudio?.trackIdentifier).toBe('local-mic');
		expect(presentation.localScreenShare?.trackIdentifier).toBe('local-screen');
		expect(presentation.localScreenShareAudio?.trackIdentifier).toBe('local-screen-audio');
		expect(presentation.remoteVideo?.trackIdentifier).toBe('remote-camera');
		expect(presentation.remoteAudio?.trackIdentifier).toBe('remote-mic');
		expect(presentation.remoteScreenShare?.trackIdentifier).toBe('remote-screen');
		expect(presentation.remoteScreenShareAudio?.trackIdentifier).toBe('remote-screen-audio');
		expect(presentation.network.audioSendBitrateKbps).toBe(48);
		expect(presentation.sparklines).toEqual({
			latency: [31],
			bitrate: [1390],
			packetLoss: [2],
		});
	});

	it('classifies native named screen-share audio publications when source lookup misses', () => {
		const remoteParticipant = participantWithAudioPublications(
			{
				screen_share: 'remote-screen',
			},
			[{source: 'microphone', trackName: 'screen-audio', trackId: 'remote-screen-audio'}],
		);
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = [
			{direction: 'recv', kind: 'audio', trackIdentifier: 'remote-screen-audio', bitrateKbps: 4},
			{direction: 'recv', kind: 'video', trackIdentifier: 'remote-screen', bitrateKbps: 1800},
		];

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant: null,
			remoteParticipants: [remoteParticipant],
		});

		expect(presentation.remoteScreenShareAudio?.trackIdentifier).toBe('remote-screen-audio');
		expect(presentation.remoteAudio).toBeNull();
	});

	it('classifies a firefox screen share whose outbound stats omit the track identifier', () => {
		const localParticipant = participant({
			microphone: 'b0d2f1a7-6c3e-4f8a-9b21-5d7c4e0a1f36',
			screen_share: '2f4c8de1-9a07-4b53-8c6d-1e5b7a02d94f',
		});
		const remoteParticipant = participant({microphone: '8a106956-d202-4916-8e19-0a1672c64b73'});
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = [
			{
				direction: 'recv',
				kind: 'audio',
				mid: '2',
				trackIdentifier: '{8a106956-d202-4916-8e19-0a1672c64b73}',
				bitrateKbps: 46,
			},
			{direction: 'send', kind: 'audio', mid: '1', bitrateKbps: 30},
			{direction: 'send', kind: 'video', mid: '3', bitrateKbps: 47, framesPerSecond: 58, frameWidth: 1920},
		];

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant,
			remoteParticipants: [remoteParticipant],
		});

		expect(presentation.localScreenShare?.mid).toBe('3');
		expect(presentation.localVideo).toBeNull();
	});

	it('classifies a firefox inbound screen share reported with a braced track identifier', () => {
		const remoteParticipant = participant({screen_share: '34171e0d-174d-4d56-bb71-76b4fba330c3'});
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = [
			{
				direction: 'recv',
				kind: 'video',
				mid: '3',
				trackIdentifier: '{34171e0d-174d-4d56-bb71-76b4fba330c3}',
				bitrateKbps: 3585,
			},
		];

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant: null,
			remoteParticipants: [remoteParticipant],
		});

		expect(presentation.remoteScreenShare?.mid).toBe('3');
		expect(presentation.remoteVideo).toBeNull();
	});

	it('exposes every camera simulcast layer next to the projected localVideo row', () => {
		const localParticipant = participant({camera: 'cam-track', screen_share: 'screen-track'});
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = chromiumCameraSimulcastTracks();

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant,
			remoteParticipants: null,
		});

		expect(presentation.localVideo?.rid).toBe('q');
		expect(presentation.localVideoLayers).toHaveLength(3);
		expect(presentation.localVideoLayers.map((layer) => layer.rid)).toEqual(['q', 'h', 'f']);
		expect(presentation.localVideoLayers.map((layer) => layer.active)).toEqual([false, false, true]);
		expect(presentation.localVideoLayers.map((layer) => layer.bitrateWindowMs)).toEqual([2001, 2001, 2001]);
	});

	it('keeps the screen-share publication out of the camera layer view', () => {
		const localParticipant = participant({camera: 'cam-track', screen_share: 'screen-track'});
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = chromiumCameraSimulcastTracks();

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant,
			remoteParticipants: null,
		});

		expect(presentation.localScreenShare?.trackIdentifier).toBe('screen-track');
		expect(presentation.localVideoLayers).not.toContain(perTrackStats[3]);
	});

	it('reports a single camera layer when firefox omits rid and media source ids', () => {
		const localParticipant = participant({camera: 'cam-track'});
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = [
			{direction: 'send', kind: 'audio', mid: '1', bitrateKbps: 30},
			{direction: 'send', kind: 'video', mid: '3', bitrateKbps: 47, framesPerSecond: 58, frameWidth: 1920},
		];

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant,
			remoteParticipants: null,
		});

		expect(presentation.localVideo?.mid).toBe('3');
		expect(presentation.localVideoLayers).toEqual([perTrackStats[1]]);
	});

	it('reports no camera layers rather than a false single layer when firefox simulcast is unidentifiable', () => {
		const localParticipant = participant({camera: 'cam-track'});
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = [
			{direction: 'send', kind: 'audio', mid: '1', bitrateKbps: 30},
			{direction: 'send', kind: 'video', mid: '3', rid: 'q', bitrateKbps: 0},
			{direction: 'send', kind: 'video', mid: '3', rid: 'h', bitrateKbps: 0},
			{direction: 'send', kind: 'video', mid: '3', rid: 'f', bitrateKbps: 1133},
		];

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant,
			remoteParticipants: null,
		});

		expect(presentation.localVideo).toBeNull();
		expect(presentation.localVideoLayers).toEqual([]);
	});

	it('keeps the chromium simulcast projection byte-identical', () => {
		const localParticipant = participant({camera: 'cam-track', screen_share: 'screen-track'});
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = chromiumCameraSimulcastTracks();

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: voiceStats,
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: null,
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant,
			remoteParticipants: null,
		});

		expect(presentation.localVideo).toBe(perTrackStats[0]);
		expect(presentation.localScreenShare).toBe(perTrackStats[3]);
		expect(presentation.localAudio).toBeNull();
		expect(presentation.remoteVideo).toBeNull();
		expect(presentation.network.videoSendBitrateKbps).toBe(1200);
	});

	it('uses the canonical v2 native stats projection when native stats are available', () => {
		const perTrackStats: Array<VoiceEngineV2PerTrackStats> = [
			{direction: 'send', kind: 'video', trackIdentifier: 'browser-camera', bitrateKbps: 500},
			{direction: 'send', kind: 'audio', trackIdentifier: 'browser-mic', bitrateKbps: 48},
		];

		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: {
				...voiceStats,
				audioSendBitrate: 1,
				videoSendBitrate: 2,
				rtt: 999,
			},
			perTrackStats,
			statsTimeSeries: timeSeries,
			nativeStats: nativeStats({
				rttMs: 25,
				droppedNativeVideoFrames: 7,
				outbound: [
					{
						trackSid: 'native-camera',
						source: 'camera',
						kind: 'video',
						bitrateKbps: 700,
						packetsLost: 0,
						fps: 30,
					},
					{
						trackSid: 'native-screen-audio',
						source: 'screen_share_audio',
						kind: 'audio',
						bitrateKbps: 96,
						packetsLost: 1,
						packetsSent: 100,
					},
				],
				inbound: [
					{
						participantSid: 'remote-a',
						trackSid: 'native-remote-audio',
						kind: 'audio',
						bitrateKbps: 64,
						packetsLost: 3,
						packetsReceived: 97,
						jitterMs: 9,
					},
				],
			}),
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant: null,
			remoteParticipants: null,
		});

		expect(presentation.network).toMatchObject({
			audioSendBitrateKbps: 96,
			audioRecvBitrateKbps: 64,
			videoSendBitrateKbps: 700,
			audioPacketLossPercent: 3,
			jitterMs: 9,
			rttMs: 25,
			droppedVideoFrameCallbacks: 7,
		});
		expect(presentation.localVideo?.trackIdentifier).toBe('native-camera');
		expect(presentation.localAudio).toBeNull();
		expect(presentation.localScreenShareAudio?.trackIdentifier).toBe('native-screen-audio');
		expect(presentation.remoteAudio?.trackIdentifier).toBe('native-remote-audio');
	});

	it('uses the tracked latency for native RTT when a sparse native stats sample omits RTT', () => {
		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: 37,
			averageLatency: 41,
			stats: {
				...voiceStats,
				rtt: 999,
			},
			perTrackStats: [],
			statsTimeSeries: timeSeries,
			nativeStats: nativeStats({
				rttMs: null,
				outbound: [
					{
						trackSid: 'native-mic',
						source: 'microphone',
						kind: 'audio',
						bitrateKbps: 48,
						packetsLost: 0,
					},
				],
				inbound: [],
			}),
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant: null,
			remoteParticipants: null,
		});

		expect(presentation.network.rttMs).toBe(37);
	});

	it('keeps native RTT unknown before the first tracked latency sample', () => {
		const presentation = buildVoiceStatsForNerdsPresentation({
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			currentLatency: null,
			averageLatency: null,
			stats: {
				...voiceStats,
				rtt: 999,
			},
			perTrackStats: [],
			statsTimeSeries: timeSeries,
			nativeStats: nativeStats({rttMs: null}),
			publisherTransport: null,
			subscriberTransport: null,
			localParticipant: null,
			remoteParticipants: null,
		});

		expect(presentation.network.rttMs).toBeNull();
	});
});

describe('collectScreenShareAudioPublicationDiagnostics', () => {
	it('reports the mute and upstream state of every local screen-share audio publication', () => {
		const displayPublication = {
			trackSid: 'TR_display',
			source: 'screen_share_audio',
			isMuted: false,
			audioTrack: {
				isUpstreamPaused: true,
				mediaStreamTrack: {
					id: 'display-audio',
					readyState: 'live',
					muted: true,
					enabled: true,
				} as MediaStreamTrack,
			},
		};
		const devicePublication = {
			trackSid: 'TR_device',
			source: 'screen_share_audio',
			isMuted: false,
			audioTrack: {
				isUpstreamPaused: false,
				mediaStreamTrack: {
					id: 'device-audio',
					readyState: 'live',
					muted: false,
					enabled: true,
				} as MediaStreamTrack,
			},
		};
		const micPublication = {
			trackSid: 'TR_mic',
			source: 'microphone',
			isMuted: false,
			audioTrack: {
				isUpstreamPaused: false,
				mediaStreamTrack: {
					id: 'mic-audio',
					readyState: 'live',
					muted: false,
					enabled: true,
				} as MediaStreamTrack,
			},
		};
		const localParticipant: ParticipantPublicationLookup = {
			getTrackPublication: (source) => (String(source) === 'screen_share_audio' ? displayPublication : undefined),
			audioTrackPublications: new Map([
				['TR_display', displayPublication],
				['TR_device', devicePublication],
				['TR_mic', micPublication],
			]),
		};

		const diagnostics = collectScreenShareAudioPublicationDiagnostics(localParticipant);

		expect(diagnostics).toEqual([
			{
				trackSid: 'TR_display',
				source: 'screen_share_audio',
				isMuted: false,
				isUpstreamPaused: true,
				mediaStreamTrackId: 'display-audio',
				mediaStreamTrackReadyState: 'live',
				mediaStreamTrackMuted: true,
				mediaStreamTrackEnabled: true,
			},
			{
				trackSid: 'TR_device',
				source: 'screen_share_audio',
				isMuted: false,
				isUpstreamPaused: false,
				mediaStreamTrackId: 'device-audio',
				mediaStreamTrackReadyState: 'live',
				mediaStreamTrackMuted: false,
				mediaStreamTrackEnabled: true,
			},
		]);
	});

	it('reports nothing when there is no local participant', () => {
		expect(collectScreenShareAudioPublicationDiagnostics(null)).toEqual([]);
	});
});
