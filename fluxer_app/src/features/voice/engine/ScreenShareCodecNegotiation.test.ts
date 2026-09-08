// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	FluxerCodecAdvertisement,
	FluxerSelectProtocolMessage,
} from '@app/features/voice/engine/ScreenShareCodecNegotiation';
import type {HardwareEncodeReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

let av1OptIn = false;
let hevcOptIn = false;
let preferredScreenShareCodec = 'auto';
let gpuReport: HardwareEncodeReport | null = null;
let firefoxBrowser = false;

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
	isFirefoxBrowser: () => firefoxBrowser,
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
	clearScreenShareDecodeFailures: () => undefined,
	getScreenShareDecodeFailures: () => new Set(),
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

const {
	default: ScreenShareCodecNegotiation,
	buildLocalCodecAdvertisements,
	computeNegotiatedVideoCodec,
	getScreenShareCodecPreferenceOrder,
	SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC,
} = await import('./ScreenShareCodecNegotiation');
const {resetCachedCodecCapabilities} = await import('@app/features/voice/utils/CodecCapabilityDetector');
const {RoomEvent} = await import('livekit-client');

class FakeRoom {
	private handlers = new Map<string, Set<(...args: Array<unknown>) => void>>();
	localParticipant = {identity: 'local', publishData: vi.fn(async () => undefined)};
	remoteParticipants = new Map<string, {identity: string}>();

	on(event: string, handler: (...args: Array<unknown>) => void): this {
		if (!this.handlers.has(event)) this.handlers.set(event, new Set());
		this.handlers.get(event)?.add(handler);
		return this;
	}

	off(event: string, handler: (...args: Array<unknown>) => void): this {
		this.handlers.get(event)?.delete(handler);
		return this;
	}

	emit(event: string, ...args: Array<unknown>): void {
		for (const handler of this.handlers.get(event) ?? []) handler(...args);
	}
}

function sessionUpdatePayload(codecs: Array<FluxerCodecAdvertisement>): Uint8Array {
	return new TextEncoder().encode(
		JSON.stringify({
			op: 14,
			d: {video_codec: 'H264', media_session_id: 'peer-session', reason: 'connected', codecs},
		}),
	);
}

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

describe('screen-share codec negotiation on Gecko', () => {
	beforeEach(() => {
		av1OptIn = false;
		hevcOptIn = false;
		preferredScreenShareCodec = 'auto';
		gpuReport = null;
		firefoxBrowser = true;
		resetCachedCodecCapabilities();
	});

	afterEach(() => {
		firefoxBrowser = false;
		resetCachedCodecCapabilities();
	});

	it('ranks VP8 ahead of H.264 because Gecko always software-encodes H.264', () => {
		expect(getScreenShareCodecPreferenceOrder()).toEqual(['vp8', 'h264']);
	});

	it('negotiates VP8 when both ends can encode and decode VP8 and H.264', () => {
		const local = buildLocalCodecAdvertisements();
		const remote = [buildLocalCodecAdvertisements()];
		expect(computeNegotiatedVideoCodec(local, remote, 0, getScreenShareCodecPreferenceOrder()).codec).toBe('vp8');
	});

	it('still lets an explicit H.264 pin win over the Gecko order', () => {
		expect(getScreenShareCodecPreferenceOrder('h264')[0]).toBe('h264');
	});
});

describe('advertising local capabilities over the negotiation topic', () => {
	let room: FakeRoom;

	beforeEach(() => {
		av1OptIn = false;
		hevcOptIn = false;
		preferredScreenShareCodec = 'auto';
		gpuReport = {av1: 'hardware', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'};
		resetCachedCodecCapabilities();
		room = new FakeRoom();
		ScreenShareCodecNegotiation.setSelectionChangeListener(null);
	});

	async function publishAndCaptureSelectProtocol(): Promise<{
		payload: Uint8Array;
		message: FluxerSelectProtocolMessage;
	}> {
		Object.defineProperty(globalThis, 'window', {configurable: true, writable: true, value: {}});
		try {
			ScreenShareCodecNegotiation.bind(room as never);
			await ScreenShareCodecNegotiation.publishLocalCapabilities(room as never, 'manual');
		} finally {
			Reflect.deleteProperty(globalThis, 'window');
		}
		const payloads = (room.localParticipant.publishData.mock.calls as Array<Array<unknown>>).map(
			(call) => call[0] as Uint8Array,
		);
		const payload = payloads.findLast(
			(candidate) => (JSON.parse(new TextDecoder().decode(candidate)) as {op: number}).op === 1,
		);
		if (!payload) throw new Error('no select-protocol message was published');
		return {payload, message: JSON.parse(new TextDecoder().decode(payload)) as FluxerSelectProtocolMessage};
	}

	it('advertises no experiments because nothing on either side reads them', async () => {
		const {message} = await publishAndCaptureSelectProtocol();
		expect(message.op).toBe(1);
		expect(message.d.experiments).toEqual([]);
		ScreenShareCodecNegotiation.dispose();
	});

	it('keeps the empty experiments list acceptable to the receive-side validator', async () => {
		const {payload} = await publishAndCaptureSelectProtocol();
		ScreenShareCodecNegotiation.dispose();
		const peerRoom = new FakeRoom();
		peerRoom.remoteParticipants.set('peer', {identity: 'peer'});
		ScreenShareCodecNegotiation.bind(peerRoom as never);
		const listener = vi.fn();
		ScreenShareCodecNegotiation.setSelectionChangeListener(listener);
		peerRoom.emit(RoomEvent.DataReceived, payload, {identity: 'peer'}, undefined, SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC);
		await new Promise((resolve) => setTimeout(resolve, 0));
		expect(listener).toHaveBeenCalled();
		ScreenShareCodecNegotiation.dispose();
	});
});

describe('reacting to a mid-session codec selection change', () => {
	let room: FakeRoom;

	beforeEach(() => {
		av1OptIn = false;
		hevcOptIn = true;
		preferredScreenShareCodec = 'auto';
		gpuReport = {av1: 'hardware', h265: 'hardware', h264: 'hardware', vp9: 'software', vp8: 'software'};
		resetCachedCodecCapabilities();
		room = new FakeRoom();
		ScreenShareCodecNegotiation.setSelectionChangeListener(null);
	});

	function flush(): Promise<void> {
		return new Promise((resolve) => setTimeout(resolve, 0));
	}

	async function establishBaseline(): Promise<void> {
		ScreenShareCodecNegotiation.bind(room as never);
		room.emit(RoomEvent.ParticipantDisconnected, {identity: 'nobody-was-connected'});
		await flush();
	}

	it('picks the locally preferred codec while no remote participant is known', async () => {
		await establishBaseline();
		expect(ScreenShareCodecNegotiation.getSelectedCodec()).toBe('h265');
		ScreenShareCodecNegotiation.dispose();
	});

	it('notifies the selection-change listener once a peer without HEVC decode is discovered', async () => {
		await establishBaseline();
		expect(ScreenShareCodecNegotiation.getSelectedCodec()).toBe('h265');
		const listener = vi.fn();
		ScreenShareCodecNegotiation.setSelectionChangeListener(listener);
		room.remoteParticipants.set('peer-without-hevc', {identity: 'peer-without-hevc'});
		const payload = sessionUpdatePayload([videoAdvertisement('VP9', true, true)]);
		room.emit(
			RoomEvent.DataReceived,
			payload,
			{identity: 'peer-without-hevc'},
			undefined,
			SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC,
		);
		await flush();
		const negotiated = ScreenShareCodecNegotiation.getSelectedCodec();
		expect(negotiated).not.toBe('h265');
		expect(listener).toHaveBeenCalledTimes(1);
		expect(listener).toHaveBeenCalledWith(room, negotiated, 'data');
		ScreenShareCodecNegotiation.dispose();
	});

	async function withWindowDefined(run: () => Promise<void>): Promise<void> {
		Object.defineProperty(globalThis, 'window', {configurable: true, writable: true, value: {}});
		try {
			await run();
		} finally {
			Reflect.deleteProperty(globalThis, 'window');
		}
	}

	it('stops reusing a live AV1 selection once the AV1 opt-in is turned off mid-share', async () => {
		av1OptIn = true;
		resetCachedCodecCapabilities();
		await establishBaseline();
		expect(ScreenShareCodecNegotiation.getSelectedCodec()).toBe('av1');
		av1OptIn = false;
		resetCachedCodecCapabilities();
		expect(ScreenShareCodecNegotiation.selectScreenShareCodec('auto')).not.toBe('av1');
		expect(ScreenShareCodecNegotiation.selectNativeScreenShareCodec('auto')).not.toBe('av1');
		expect(getScreenShareCodecPreferenceOrder()).toContain(ScreenShareCodecNegotiation.selectScreenShareCodec('auto'));
		ScreenShareCodecNegotiation.dispose();
	});

	it('renegotiates the live share off AV1 when the opt-in toggle refreshes the local capabilities', async () => {
		av1OptIn = true;
		resetCachedCodecCapabilities();
		await establishBaseline();
		expect(ScreenShareCodecNegotiation.getSelectedCodec()).toBe('av1');
		const listener = vi.fn();
		ScreenShareCodecNegotiation.setSelectionChangeListener(listener);
		av1OptIn = false;
		resetCachedCodecCapabilities();
		await withWindowDefined(async () => {
			await ScreenShareCodecNegotiation.publishLocalCapabilities(room as never, 'manual');
		});
		const negotiated = ScreenShareCodecNegotiation.getSelectedCodec();
		expect(negotiated).not.toBe('av1');
		expect(listener).toHaveBeenCalledTimes(1);
		expect(listener).toHaveBeenCalledWith(room, negotiated, 'manual');
		ScreenShareCodecNegotiation.dispose();
	});

	it('does not notify the listener when the recomputed selection is unchanged', async () => {
		await establishBaseline();
		room.remoteParticipants.set('peer-with-hevc', {identity: 'peer-with-hevc'});
		const firstPayload = sessionUpdatePayload([
			videoAdvertisement('H265', true, true),
			videoAdvertisement('VP9', true, true),
		]);
		room.emit(
			RoomEvent.DataReceived,
			firstPayload,
			{identity: 'peer-with-hevc'},
			undefined,
			SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC,
		);
		await flush();
		expect(ScreenShareCodecNegotiation.getSelectedCodec()).toBe('h265');
		const listener = vi.fn();
		ScreenShareCodecNegotiation.setSelectionChangeListener(listener);
		const secondPayload = sessionUpdatePayload([
			videoAdvertisement('H265', true, true),
			videoAdvertisement('VP9', true, true),
		]);
		room.emit(
			RoomEvent.DataReceived,
			secondPayload,
			{identity: 'peer-with-hevc'},
			undefined,
			SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC,
		);
		await flush();
		expect(listener).not.toHaveBeenCalled();
		ScreenShareCodecNegotiation.dispose();
	});
});
