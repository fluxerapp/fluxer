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
			pump: {
				active: false,
				captureId: null,
				sampleRate: null,
				channels: null,
				usesNativeSink: false,
				publishStrategy: 'none',
				publishedFormatKey: null,
				eagerPublish: null,
				eagerPublishError: null,
				droppedPushFrames: 0,
				pendingPushFrames: 0,
			},
			nativeCapture: {},
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
});
