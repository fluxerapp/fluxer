// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FluxerCodecAdvertisement} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import type {HardwareEncodeReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {beforeEach, describe, expect, it, vi} from 'vitest';

let av1OptIn = false;
let hevcOptIn = false;
let preferredScreenShareCodec = 'auto';
let gpuReport: HardwareEncodeReport | null = null;

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getScreenShareAv1OptIn: () => av1OptIn,
		getScreenShareHevcOptIn: () => hevcOptIn,
		getPreferredScreenShareCodec: () => preferredScreenShareCodec,
		getScreenShareEncoderMode: () => 'auto',
	},
}));

vi.mock('@app/features/devtools/utils/DesktopTroubleshootingUtils', () => ({
	getCachedDesktopTroubleshootingSettings: () => null,
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	guessPlatform: () => 'windows',
	isChromiumBrowser: () => true,
	isDesktop: () => true,
	isFirefoxBrowser: () => false,
}));

vi.mock('@app/features/voice/utils/GpuEncoderCapabilities', () => ({
	getGpuEncoderReportSync: () => gpuReport,
	loadGpuEncoderReport: async () => gpuReport,
}));

vi.mock('@app/features/voice/utils/NativeHardwareEncoderCapabilities', () => ({
	getNativeHardwareEncoderCapabilitiesSync: () => null,
	hasNativeHardwareEncoder: () => false,
	resetNativeHardwareEncoderCapabilities: () => undefined,
	loadNativeHardwareEncoderCapabilities: async () => null,
}));

vi.mock('@app/features/voice/utils/OpenH264Status', () => ({
	getOpenH264StatusSync: () => null,
	resetOpenH264Status: () => undefined,
	loadOpenH264Status: async () => null,
}));

vi.mock('@app/features/voice/utils/VideoDecoderCapabilities', () => ({
	getVideoDecoderExclusionsSync: () => [],
	loadVideoDecoderExclusions: async () => [],
}));

const VIDEO_CAPABILITIES = {
	codecs: [
		{mimeType: 'video/VP8'},
		{mimeType: 'video/VP9'},
		{mimeType: 'video/H264'},
		{mimeType: 'video/H265'},
		{mimeType: 'video/AV1'},
	],
};

Object.defineProperty(globalThis, 'RTCRtpSender', {
	configurable: true,
	writable: true,
	value: {getCapabilities: () => VIDEO_CAPABILITIES},
});

Object.defineProperty(globalThis, 'RTCRtpReceiver', {
	configurable: true,
	writable: true,
	value: {getCapabilities: () => VIDEO_CAPABILITIES},
});

const {buildLocalCodecAdvertisements, computeNegotiatedVideoCodec, getScreenShareCodecPreferenceOrder} = await import(
	'./ScreenShareCodecNegotiation'
);
const {resetCachedCodecCapabilities} = await import('@app/features/voice/utils/CodecCapabilityDetector');

function videoAdvertisement(name: 'AV1' | 'VP9' | 'H265', encode: boolean, decode: boolean): FluxerCodecAdvertisement {
	const payloadType = name === 'AV1' ? 101 : name === 'H265' ? 105 : 109;
	return {name, type: 'video', payload_type: payloadType, priority: 1, encode, decode};
}

describe('screen-share codec negotiation with the AV1 opt-in off', () => {
	beforeEach(() => {
		av1OptIn = false;
		hevcOptIn = false;
		preferredScreenShareCodec = 'auto';
		gpuReport = {av1: 'hardware', h265: 'software', h264: 'hardware', vp9: 'software', vp8: 'software'};
		resetCachedCodecCapabilities();
	});

	it('keeps AV1 out of the negotiated preference order the hardware tail would refill', () => {
		expect(getScreenShareCodecPreferenceOrder()).not.toContain('av1');
		av1OptIn = true;
		resetCachedCodecCapabilities();
		expect(getScreenShareCodecPreferenceOrder()).toContain('av1');
	});

	it('drops an explicitly requested AV1 preference from the order', () => {
		expect(getScreenShareCodecPreferenceOrder('av1')).not.toContain('av1');
		av1OptIn = true;
		resetCachedCodecCapabilities();
		expect(getScreenShareCodecPreferenceOrder('av1')[0]).toBe('av1');
	});

	it('stops advertising AV1 encode while still advertising AV1 decode', () => {
		const av1 = buildLocalCodecAdvertisements().find((codec) => codec.name === 'AV1');
		expect(av1).toMatchObject({encode: false, decode: true});
		av1OptIn = true;
		resetCachedCodecCapabilities();
		expect(buildLocalCodecAdvertisements().find((codec) => codec.name === 'AV1')).toMatchObject({
			encode: true,
			decode: true,
		});
	});

	it('negotiates away from AV1 even when both ends can encode and decode it', () => {
		const local = [videoAdvertisement('AV1', true, true), videoAdvertisement('VP9', true, true)];
		const remote = [[videoAdvertisement('AV1', true, true), videoAdvertisement('VP9', true, true)]];
		expect(computeNegotiatedVideoCodec(local, remote, 0, getScreenShareCodecPreferenceOrder()).codec).toBe('vp9');
		av1OptIn = true;
		resetCachedCodecCapabilities();
		expect(computeNegotiatedVideoCodec(local, remote, 0, getScreenShareCodecPreferenceOrder()).codec).toBe('av1');
	});

	it('avoids exotic codecs while any participant codec set is still unknown', () => {
		av1OptIn = true;
		resetCachedCodecCapabilities();
		const local = [videoAdvertisement('AV1', true, true), videoAdvertisement('VP9', true, true)];
		const remote = [[videoAdvertisement('AV1', true, true), videoAdvertisement('VP9', true, true)]];
		expect(computeNegotiatedVideoCodec(local, remote, 0, getScreenShareCodecPreferenceOrder()).codec).toBe('av1');
		expect(computeNegotiatedVideoCodec(local, remote, 1, getScreenShareCodecPreferenceOrder()).codec).toBe('vp9');
	});
});

describe('screen-share codec negotiation with the HEVC opt-in off', () => {
	beforeEach(() => {
		av1OptIn = false;
		hevcOptIn = false;
		preferredScreenShareCodec = 'auto';
		gpuReport = {av1: 'hardware', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'};
		resetCachedCodecCapabilities();
	});

	it('keeps HEVC out of the negotiated preference order the hardware tail would refill', () => {
		expect(getScreenShareCodecPreferenceOrder()).not.toContain('h265');
		hevcOptIn = true;
		resetCachedCodecCapabilities();
		expect(getScreenShareCodecPreferenceOrder()).toContain('h265');
	});

	it('drops an explicitly requested HEVC preference from the order', () => {
		expect(getScreenShareCodecPreferenceOrder('h265')).not.toContain('h265');
		hevcOptIn = true;
		resetCachedCodecCapabilities();
		expect(getScreenShareCodecPreferenceOrder('h265')[0]).toBe('h265');
	});

	it('stops advertising HEVC encode while still advertising HEVC decode', () => {
		const h265 = buildLocalCodecAdvertisements().find((codec) => codec.name === 'H265');
		expect(h265).toMatchObject({encode: false, decode: true});
		hevcOptIn = true;
		resetCachedCodecCapabilities();
		expect(buildLocalCodecAdvertisements().find((codec) => codec.name === 'H265')).toMatchObject({
			encode: true,
			decode: true,
		});
	});

	it('negotiates away from HEVC even when both ends can encode and decode it', () => {
		const local = [videoAdvertisement('H265', true, true), videoAdvertisement('VP9', true, true)];
		const remote = [[videoAdvertisement('H265', true, true), videoAdvertisement('VP9', true, true)]];
		expect(computeNegotiatedVideoCodec(local, remote, 0, getScreenShareCodecPreferenceOrder()).codec).toBe('vp9');
		hevcOptIn = true;
		resetCachedCodecCapabilities();
		expect(computeNegotiatedVideoCodec(local, remote, 0, getScreenShareCodecPreferenceOrder()).codec).toBe('h265');
	});
});
