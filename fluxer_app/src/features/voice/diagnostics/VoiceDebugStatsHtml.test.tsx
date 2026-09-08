// SPDX-License-Identifier: AGPL-3.0-or-later

import {renderVoiceDebugStatsHtml} from '@app/features/voice/diagnostics/VoiceDebugStatsHtml';
import type {StatsForNerdsData} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';
import {describe, expect, it} from 'vitest';

function createStatsData(): StatsForNerdsData {
	return {
		session: {
			connectionId: 'connection-a',
			connectionQuality: 'excellent',
			latencyMs: 24,
			avgLatencyMs: 31,
			durationSeconds: 90,
			participants: 3,
		},
		network: {
			audioSendBitrateKbps: 48,
			audioRecvBitrateKbps: 64,
			videoSendBitrateKbps: 1200,
			videoRecvBitrateKbps: 900,
			audioPacketLossPercent: 0.5,
			videoPacketLossPercent: 1.25,
			jitterMs: 6,
			rttMs: 24,
			publisherTransport: null,
			subscriberTransport: null,
		},
		localVideo: null,
		localVideoLayers: [],
		localAudio: null,
		localScreenShare: null,
		localScreenShareAudio: null,
		remoteVideo: null,
		remoteScreenShare: null,
		remoteAudio: null,
		remoteScreenShareAudio: null,
		connection: {
			voiceServerEndpoint: 'voice.example',
			reconnectionCount: 0,
		},
		audio: {
			echoCancellation: true,
			noiseSuppression: true,
			autoGainControl: true,
			deepFilterNoiseSuppression: false,
			deepFilterNoiseSuppressionLevel: 0,
			processingMode: 'default',
		},
		screenShareSettings: {
			resolution: '1080p',
			frameRate: 30,
			streamingMode: 'quality',
			preferredCodec: 'h264',
			selectedCodec: 'h264',
			codecPreferenceOrder: ['h264', 'vp8'],
			contentHint: 'detail',
			encoderMode: 'auto',
			softwareQuality: 'balanced',
			scalabilityMode: 'none',
			backupCodecMode: 'auto',
			maxBitrateMbps: 4.5,
			audioSourceMode: 'none',
			audioIncludeSources: [],
			audioExcludeSources: [],
			shareDesktopAudio: false,
			shareAppAudio: false,
			muteStreamAudio: true,
			openH264Enabled: true,
		},
		screenShareAudioCapture: {
			nativeCapture: {},
			publications: [],
		},
		appInfo: {
			appVersion: 'dev',
			electronVersion: '41.2.2',
			chromiumVersion: '140.0.0.0',
			hardwareAccelerationEnabled: true,
			chromiumRuntime: null,
		},
		gpu: null,
		appMetrics: null,
		system: {
			platform: 'MacIntel',
			userAgent: 'Chrome',
			hardwareConcurrency: 10,
			deviceMemoryGB: 8,
			jsHeapUsedMB: 42,
			jsHeapTotalMB: 80,
			jsHeapLimitMB: 4096,
		},
		heapHistory: [40, 42, 45],
		cpuHistory: [3, 8, 4],
		sparklines: {
			latency: [31, 24, 44],
			bitrate: [1000, 1400, 1300],
			packetLoss: [0, 1.25, 0.5],
		},
	};
}

function createNativeCaptureRecord(): Record<string, unknown> {
	return {
		armedCapture: null,
		activeBridge: {captureId: 'native-audio:live'},
		supersededBridge: null,
		lastStartedCapture: null,
		lastArmFailure: null,
		bridgeStats: {
			active: true,
			bridgeMode: 'generator',
			captureId: 'native-audio:live',
			startedAt: 1772000000000,
			lastFrameAt: 1772000011240,
			lastFrameTimestampUs: 11240000,
			framesReceived: 1124,
			framesDropped: 0,
			lateFrameCount: 0,
			rebufferCount: 0,
			maxFrameArrivalGapMs: 32,
			maxFrameTimestampGapMs: 10,
			maxPendingFrames: 3,
			maxBufferedDurationMs: 30,
			lastFramePeak: 0.42,
			lastFrameRms: 0.11,
			maxFramePeak: 0.71,
			maxFrameRms: 0.19,
			nonSilentFrameCount: 1118,
			prebufferTargetMs: 60,
			frameDurationMs: 10,
			endReason: null,
			endDetail: null,
			endedAt: null,
		},
		endedBridgeCaptures: [],
		lifecycleFaults: [],
	};
}

function screenShareAudioCaptureSection(html: string): string {
	const match = /<section[^>]*><h3[^>]*>screenShareAudioCapture \(native\)<\/h3>[\s\S]*?<\/section>/.exec(html);
	expect(match).not.toBeNull();
	return match?.[0] ?? '';
}

describe('renderVoiceDebugStatsHtml', () => {
	it('renders pure SVG sparklines for key popout metrics', () => {
		const html = renderVoiceDebugStatsHtml(createStatsData(), '2026-06-10T18:00:00.000Z');

		expect(html).toContain('keyMetricSparklines');
		expect(html).toContain('<svg');
		expect(html).toContain('<polyline');
		expect(html).toContain('latencyMs sparkline');
		expect(html).toContain('totalBitrateKbps sparkline');
		expect(html).toContain('packetLossPercent sparkline');
		expect(html).toContain('heapUsedMB sparkline');
		expect(html).toContain('mainProcessCpuPercent sparkline');
	});

	it('does not render a screen-share audio pump section while the native capture is live', () => {
		const data = createStatsData();
		data.screenShareAudioCapture = {nativeCapture: createNativeCaptureRecord(), publications: []};

		const html = renderVoiceDebugStatsHtml(data, '2026-06-10T18:00:00.000Z');

		expect(html).toContain('screenShareAudioCapture (native)');
		expect(html).toContain('framesReceived');
		expect(html).not.toContain('screenShareAudioCapture (pump)');
		expect(html).not.toContain('publishStrategy');
		expect(html).not.toContain('droppedPushFrames');
		expect(html).not.toContain('usesNativeSink');
	});

	it('renders the screen-share audio capture section identically for Chromium and Firefox dumps', () => {
		const chromium = createStatsData();
		chromium.screenShareAudioCapture = {nativeCapture: createNativeCaptureRecord(), publications: []};
		chromium.system.platform = 'Win32';
		chromium.system.userAgent =
			'(Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
		const firefox = createStatsData();
		firefox.screenShareAudioCapture = {nativeCapture: createNativeCaptureRecord(), publications: []};
		firefox.system.platform = 'Win32';
		firefox.system.userAgent = '(Windows NT 10.0; Win64; x64; rv:155.0) Gecko/20100101 Firefox/155.0';
		firefox.appInfo.electronVersion = null;
		firefox.appInfo.chromiumVersion = null;
		firefox.appInfo.hardwareAccelerationEnabled = null;

		const chromiumHtml = renderVoiceDebugStatsHtml(chromium, '2026-06-10T18:00:00.000Z');
		const firefoxHtml = renderVoiceDebugStatsHtml(firefox, '2026-06-10T18:00:00.000Z');

		expect(screenShareAudioCaptureSection(chromiumHtml)).toBe(screenShareAudioCaptureSection(firefoxHtml));
		expect(chromiumHtml).not.toContain('screenShareAudioCapture (pump)');
		expect(firefoxHtml).not.toContain('screenShareAudioCapture (pump)');
	});

	it('renders every camera simulcast layer instead of only the projected localVideo row', () => {
		const data = createStatsData();
		data.localVideoLayers = [
			{direction: 'send', kind: 'video', ssrc: 1001, rid: 'q', active: false, bitrateKbps: 0, bitrateWindowMs: 2001},
			{direction: 'send', kind: 'video', ssrc: 1002, rid: 'h', active: false, bitrateKbps: 0, bitrateWindowMs: 2001},
			{direction: 'send', kind: 'video', ssrc: 1003, rid: 'f', active: true, bitrateKbps: 1133, bitrateWindowMs: 2001},
		];
		data.localVideo = data.localVideoLayers[0];

		const html = renderVoiceDebugStatsHtml(data, '2026-06-10T18:00:00.000Z');

		expect(html).toContain('track:localVideoLayers[0]');
		expect(html).toContain('track:localVideoLayers[1]');
		expect(html).toContain('track:localVideoLayers[2]');
		expect(html).toContain('bitrateWindowMs');
	});
});
