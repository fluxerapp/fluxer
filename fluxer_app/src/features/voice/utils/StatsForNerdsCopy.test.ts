// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StatsForNerdsData} from '@app/features/voice/utils/VoiceStatsForNerdsPresenter';
import {describe, expect, it, vi} from 'vitest';

const failureRecord = {
	code: -2303,
	reason: 'first-frame-timeout',
	reportedAt: 1000,
	streamKey: 'guild-a:channel-a:connection-a',
	trackSid: 'TR_screen_share',
	participantIdentity: 'user_1_connection-a',
	inbound: {
		packetsReceived: 4210,
		bytesReceived: 3_500_000,
		framesReceived: 0,
		framesDecoded: 0,
		keyFramesDecoded: 0,
		framesDropped: 0,
		pliCount: 7,
		firCount: 0,
		nackCount: 2,
		freezeCount: 0,
		decoderImplementation: 'ExternalDecoder (D3D11VideoDecoder)',
		powerEfficientDecoder: true,
		mimeType: 'video/H264',
		sdpFmtpLine: 'profile-level-id=42e01f',
		frameWidth: 1920,
		frameHeight: 1080,
	},
	tile: {
		hasVideoElement: true,
		readyState: 0,
		videoWidth: 0,
		videoHeight: 0,
		clientWidth: 960,
		clientHeight: 540,
		visibilityState: 'visible',
	},
};

const localCodecs = [{name: 'h264', type: 'video', payload_type: 108, priority: 900, encode: true, decode: true}];

const subscriptionDebugReport = {capturedAt: '2026-09-11T00:00:00.000Z', failures: []};

vi.mock('@app/features/app/config/Config', () => ({default: {PUBLIC_BUILD_VERSION: 'test'}}));

vi.mock('@app/features/devtools/utils/DesktopTroubleshootingUtils', () => ({
	getCachedDesktopTroubleshootingSettings: () => null,
	getDesktopTroubleshootingSettings: async () => ({}),
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	getElectronAPI: () => null,
	getNativePlatform: async () => 'web',
	isDesktop: () => false,
	supportsDesktopScreenShareAudioCapture: () => false,
}));

vi.mock('@app/features/voice/diagnostics/VoiceSubscriptionDebugReport', () => ({
	collectVoiceSubscriptionDebugReport: () => subscriptionDebugReport,
}));

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({
	default: {
		room: null,
		connected: false,
		connecting: false,
		guildId: null,
		channelId: null,
		connectionId: null,
		voiceServerEndpoint: null,
		voiceStats: null,
		perTrackStats: [],
		statsTimeSeries: [],
		publisherTransport: null,
		subscriberTransport: null,
	},
}));

vi.mock('@app/features/voice/engine/ScreenShareCodecNegotiation', () => ({
	default: {
		getSelectedCodec: () => 'h264',
		getLocalCodecAdvertisements: () => localCodecs,
		getRemoteDecodeCodecsByIdentity: () => ({'user_1_connection-a': ['h264', 'vp8']}),
	},
	getScreenShareCodecPreferenceOrder: () => ['h264', 'vp8'],
}));

vi.mock('@app/features/voice/engine/voice_screen_share_manager/NativeEngineAudioTrackPump', () => ({
	getNativeEngineAudioTrackPumpStats: () => null,
}));

vi.mock('@app/features/voice/engine/voice_screen_share_manager/shared', () => ({
	getPublishedScreenShareMaxBitrateBps: () => null,
}));

vi.mock('@app/features/voice/state/ScreenShareWatchFailures', () => ({
	ScreenShareWatchFailures: {getFailureHistory: () => [failureRecord]},
}));

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: new Proxy({}, {get: () => () => null}),
}));

vi.mock('@app/features/voice/utils/CodecCapabilityDetector', () => ({
	getCameraPublishCodecPolicy: () => ({primary: 'h264'}),
	getCodecCapabilityReport: () => ({}),
	getLiveKitSupportedCodecs: () => ['h264', 'vp8'],
	selectOptimalScreenShareCodec: () => 'h264',
}));

vi.mock('@app/features/voice/utils/GpuEncoderCapabilities', () => ({loadGpuEncoderReport: async () => null}));

vi.mock('@app/features/voice/utils/NativeAudioCaptureBridge', () => ({
	getNativeAudioBridgeStats: () => null,
	getNativeAudioCaptureDiagnosticState: () => null,
}));

vi.mock('@app/features/voice/utils/ScreenShareEnvironment', () => ({getDisplayShareEnvironment: async () => ({})}));

vi.mock('@app/features/voice/utils/ScreenShareOptions', () => ({
	getScreenShareBitrateBps: () => 2_500_000,
	resolveStreamingModeSettings: () => ({resolution: 1080, frameRate: 60}),
}));

vi.mock('@app/features/voice/utils/VideoDecoderCapabilities', () => ({
	getScreenShareDecodeFailures: () => new Set(['vp9']),
}));

vi.mock('@app/features/voice/utils/VideoQualityEntitlement', () => ({hasHigherVideoQuality: () => false}));

vi.mock('@app/features/voice/utils/VoiceStatsForNerdsPresenter', () => ({
	buildVoiceStatsForNerdsPresentation: () => ({}),
	collectScreenShareAudioPublicationDiagnostics: () => [],
}));

const {buildStatsForNerdsCopyPayload} = await import('@app/features/voice/utils/StatsForNerdsCopy');

describe('buildStatsForNerdsCopyPayload watch failure diagnostics', () => {
	it('copies the screen share failure ring with its codes, track sids and inbound counters', async () => {
		const payload = await buildStatsForNerdsCopyPayload({} as StatsForNerdsData);

		expect(payload.screenShareWatchFailures).toEqual([failureRecord]);
	});

	it('copies the screen share negotiation block with the selected codec and decode sets', async () => {
		const payload = await buildStatsForNerdsCopyPayload({} as StatsForNerdsData);

		expect(payload.screenShareNegotiation).toEqual({
			selectedCodec: 'h264',
			localCodecs,
			remoteDecodeCodecsByIdentity: {'user_1_connection-a': ['h264', 'vp8']},
			decodeFailures: ['vp9'],
		});
	});

	it('copies the voice subscription debug report that was only reachable from the debug console', async () => {
		const payload = await buildStatsForNerdsCopyPayload({} as StatsForNerdsData);

		expect(payload.voiceSubscriptionDebug).toEqual(subscriptionDebugReport);
	});
});
