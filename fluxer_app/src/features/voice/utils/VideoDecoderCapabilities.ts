// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {VideoCodec} from 'livekit-client';

const logger = new Logger('VideoDecoderCapabilities');

interface WebCodecsConfig {
	codec: string;
	width: number;
	height: number;
	hardwareAcceleration?: 'no-preference' | 'prefer-hardware' | 'prefer-software';
}

interface CodecProbe {
	codec: VideoCodec;
	configs: Array<WebCodecsConfig>;
}

const CODEC_PROBES: ReadonlyArray<CodecProbe> = [
	{
		codec: 'h265',
		configs: [
			{codec: 'hev1.1.6.L120.B0', width: 1920, height: 1080},
			{codec: 'hvc1.1.6.L120.B0', width: 1920, height: 1080},
			{codec: 'hev1.1.6.L120.B0', width: 1920, height: 1080, hardwareAcceleration: 'prefer-hardware'},
			{codec: 'hvc1.1.6.L120.B0', width: 1920, height: 1080, hardwareAcceleration: 'prefer-hardware'},
		],
	},
	{
		codec: 'h264',
		configs: [
			{codec: 'avc1.42E01F', width: 1920, height: 1080},
			{codec: 'avc1.4D401F', width: 1920, height: 1080},
			{codec: 'avc1.640028', width: 1920, height: 1080},
		],
	},
	{
		codec: 'vp9',
		configs: [{codec: 'vp09.00.31.08', width: 1920, height: 1080}],
	},
	{
		codec: 'av1',
		configs: [{codec: 'av01.0.08M.08', width: 1920, height: 1080}],
	},
];

const SCREEN_SHARE_DECODE_FAILURES_MAX = 1;

let cachedExclusions: Array<VideoCodec> | null = null;
let pendingProbe: Promise<Array<VideoCodec>> | null = null;
const screenShareDecodeFailures = new Set<VideoCodec>();

interface VideoDecoderLike {
	isConfigSupported(config: WebCodecsConfig): Promise<{supported: boolean}>;
}

function getVideoDecoderApi(): VideoDecoderLike | null {
	const vd = (globalThis as Record<string, unknown>).VideoDecoder as VideoDecoderLike | undefined;
	if (vd && typeof vd.isConfigSupported === 'function') return vd;
	return null;
}

function hasRtcReceiverCapability(mimeType: string): boolean {
	const receiver = (globalThis as Record<string, unknown>).RTCRtpReceiver as
		| {getCapabilities?: (kind: 'video') => RTCRtpCapabilities | null}
		| undefined;
	const caps = receiver?.getCapabilities?.('video');
	if (!caps) return false;
	const lower = mimeType.toLowerCase();
	return caps.codecs.some((c) => {
		const candidate = c.mimeType.toLowerCase();
		return candidate === lower || (lower === 'video/av1' && candidate === 'video/av1x');
	});
}

function shouldExcludeByPlatformPolicy(_codec: VideoCodec): boolean {
	return false;
}

function isBaselineWebRtcDecodeCodec(codec: VideoCodec): boolean {
	return codec === 'h264' || codec === 'vp8';
}

function getPlatformPolicyExclusions(): Array<VideoCodec> | null {
	if ((globalThis as Record<string, unknown>).RTCRtpReceiver === undefined) return null;
	const excluded: Array<VideoCodec> = [];
	for (const probe of CODEC_PROBES) {
		if (hasRtcReceiverCapability(`video/${probe.codec}`) && shouldExcludeByPlatformPolicy(probe.codec)) {
			excluded.push(probe.codec);
		}
	}
	return excluded.length > 0 ? excluded : null;
}

async function probeCodecDecode(probe: CodecProbe): Promise<boolean> {
	if (!hasRtcReceiverCapability(`video/${probe.codec}`)) {
		return true;
	}
	if (isBaselineWebRtcDecodeCodec(probe.codec)) {
		return true;
	}
	if (shouldExcludeByPlatformPolicy(probe.codec)) {
		return false;
	}
	const api = getVideoDecoderApi();
	if (!api) {
		return true;
	}
	for (const config of probe.configs) {
		try {
			const result = await api.isConfigSupported(config);
			if (result.supported) return true;
		} catch {}
	}
	return false;
}

async function probeAllCodecs(): Promise<Array<VideoCodec>> {
	const results = await Promise.allSettled(
		CODEC_PROBES.map(async (probe) => {
			const supported = await probeCodecDecode(probe);
			return {codec: probe.codec, supported};
		}),
	);
	const excluded: Array<VideoCodec> = [];
	for (const result of results) {
		if (result.status === 'rejected') {
			logger.warn('Codec decode probe threw unexpectedly', {error: result.reason});
		} else if (!result.value.supported) {
			excluded.push(result.value.codec);
		}
	}
	if (excluded.length > 0) {
		logger.info('Excluding codecs from subscriber decode', {excluded});
	}
	return excluded;
}

export function loadVideoDecoderExclusions(): Promise<Array<VideoCodec>> {
	if (cachedExclusions) return Promise.resolve(cachedExclusions);
	if (pendingProbe) return pendingProbe;
	pendingProbe = probeAllCodecs()
		.then((result) => {
			cachedExclusions = result;
			return result;
		})
		.catch((error) => {
			logger.warn('Video decoder probe failed entirely, not excluding any codecs', {error});
			cachedExclusions = [];
			return [];
		})
		.finally(() => {
			pendingProbe = null;
		});
	return pendingProbe;
}

export function getVideoDecoderExclusionsSync(): Array<VideoCodec> | null {
	return cachedExclusions ?? getPlatformPolicyExclusions();
}

export function markScreenShareDecodeFailure(codec: VideoCodec, reason: string): boolean {
	if (screenShareDecodeFailures.has(codec)) return false;
	if (screenShareDecodeFailures.size >= SCREEN_SHARE_DECODE_FAILURES_MAX) return false;
	screenShareDecodeFailures.add(codec);
	logger.warn('Withdrawing decode advertisement for a codec after a screen share decode stall', {codec, reason});
	return true;
}

export function getScreenShareDecodeFailures(): ReadonlySet<VideoCodec> {
	return screenShareDecodeFailures;
}

export function clearScreenShareDecodeFailures(): void {
	screenShareDecodeFailures.clear();
}

export function resetVideoDecoderExclusions(): void {
	cachedExclusions = null;
	pendingProbe = null;
	screenShareDecodeFailures.clear();
}

if (typeof window !== 'undefined') {
	void loadVideoDecoderExclusions();
}
