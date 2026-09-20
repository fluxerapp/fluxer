// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CodecPreference, ScreenShareEncoderMode} from '@app/features/voice/utils/CodecCapabilityDetector';
import type {VideoCodec} from 'livekit-client';

export const CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['av1', 'h265', 'h264', 'vp9', 'vp8'];
const COMPATIBILITY_CODECS: ReadonlySet<VideoCodec> = new Set(['h264', 'vp9', 'vp8']);
export const LAST_RESORT_VIDEO_CODEC: VideoCodec = 'vp8';
const BASELINE_BROWSER_CODECS: ReadonlySet<VideoCodec> = new Set(['h264', 'vp8']);
export const SCREEN_SHARE_CODEC_ADVERTISEMENT_GRACE_MS = 3_000;
export const SCREEN_SHARE_CODEC_CHANGE_SUPPRESSION_MS = 10_000;
export const VIDEO_CODEC_NAMES: Record<VideoCodec, FluxerVideoCodecName> = {
	av1: 'AV1',
	h265: 'H265',
	h264: 'H264',
	vp9: 'VP9',
	vp8: 'VP8',
};
export const NAME_TO_VIDEO_CODEC: Record<FluxerVideoCodecName, VideoCodec> = {
	AV1: 'av1',
	H265: 'h265',
	H264: 'h264',
	VP9: 'vp9',
	VP8: 'vp8',
};
const VIDEO_CODEC_PROTOCOL_TABLE: Record<
	VideoCodec,
	{
		payloadType: number;
		rtxPayloadType: number;
		priority: number;
	}
> = {
	av1: {payloadType: 101, rtxPayloadType: 102, priority: 5000},
	h265: {payloadType: 105, rtxPayloadType: 106, priority: 4000},
	h264: {payloadType: 103, rtxPayloadType: 104, priority: 3000},
	vp9: {payloadType: 109, rtxPayloadType: 110, priority: 2000},
	vp8: {payloadType: 107, rtxPayloadType: 108, priority: 1000},
};

export type FluxerVideoCodecName = 'AV1' | 'H265' | 'H264' | 'VP9' | 'VP8';
export type FluxerCodecName = 'opus' | FluxerVideoCodecName;
export type FluxerCodecType = 'audio' | 'video';
export type NegotiationReason =
	| 'connected'
	| 'data'
	| 'participant-connected'
	| 'participant-disconnected'
	| 'reconnected'
	| 'manual';
export type ScreenShareCodecBrowser = 'chromium' | 'firefox' | 'other';

export interface FluxerCodecAdvertisement {
	name: FluxerCodecName;
	type: FluxerCodecType;
	payload_type: number;
	rtx_payload_type?: number;
	priority: number;
	encode?: boolean;
	decode?: boolean;
}

export interface CodecNegotiationSelection {
	codec: VideoCodec;
	reason: NegotiationReason;
	candidates: Array<VideoCodec>;
	unknownParticipants: number;
}

export interface ScreenShareCodecProfileEntry {
	allowed: boolean;
	supported: boolean;
	hardware: boolean;
}

export interface ScreenShareCodecProfile {
	browser: ScreenShareCodecBrowser;
	desktop: boolean;
	codecs: Record<VideoCodec, ScreenShareCodecProfileEntry>;
}

export interface ScreenShareCodecRankingInput {
	profile: ScreenShareCodecProfile;
	encoderModeSetting: ScreenShareEncoderMode;
	pin: CodecPreference;
}

export interface ScreenShareCodecRanking {
	order: ReadonlyArray<VideoCodec>;
	hardwareUnavailable: boolean;
}

export function rankScreenShareCodecs(input: ScreenShareCodecRankingInput): ScreenShareCodecRanking {
	const {codecs} = input.profile;
	const isHardware = (codec: VideoCodec): boolean => codecs[codec].supported && codecs[codec].hardware;
	const hardwareAvailable = CODEC_PREFERENCE.some((codec) => isHardware(codec));
	const pin = input.pin !== 'auto' && codecs[input.pin].allowed && codecs[input.pin].supported ? input.pin : null;
	const baselineOnly =
		input.profile.browser === 'firefox' || (input.profile.browser !== 'chromium' && !input.profile.desktop);
	const survives = (codec: VideoCodec): boolean => {
		if (!codecs[codec].allowed || !codecs[codec].supported) return false;
		if (baselineOnly && !BASELINE_BROWSER_CODECS.has(codec)) return false;
		if (codec === 'h265') return pin === 'h265' || (input.encoderModeSetting !== 'software' && isHardware('h265'));
		return true;
	};
	const survivors = CODEC_PREFERENCE.filter(survives);
	const ranked =
		input.encoderModeSetting === 'software'
			? survivors
			: [...survivors.filter(isHardware), ...survivors.filter((codec) => !isHardware(codec))];
	const ordered = pin === null ? ranked : [pin, ...ranked.filter((codec) => codec !== pin)];
	return {
		order: ordered.length > 0 ? ordered : [LAST_RESORT_VIDEO_CODEC],
		hardwareUnavailable: input.encoderModeSetting === 'hardware' && !hardwareAvailable,
	};
}

export function isScreenShareCodecUpgrade(
	order: ReadonlyArray<VideoCodec>,
	publishedCodec: VideoCodec,
	nextCodec: VideoCodec,
): boolean {
	const publishedRank = order.indexOf(publishedCodec);
	const nextRank = order.indexOf(nextCodec);
	if (publishedRank < 0 || nextRank < 0) return false;
	return nextRank < publishedRank;
}

export function buildScreenShareCodecAdvertisements(
	order: ReadonlyArray<VideoCodec>,
	decode: Record<VideoCodec, boolean>,
): Array<FluxerCodecAdvertisement> {
	return [
		{
			name: 'opus',
			type: 'audio',
			payload_type: 120,
			priority: 1000,
			encode: true,
			decode: true,
		},
		...CODEC_PREFERENCE.map((codec) => ({
			name: VIDEO_CODEC_NAMES[codec],
			type: 'video' as const,
			payload_type: VIDEO_CODEC_PROTOCOL_TABLE[codec].payloadType,
			rtx_payload_type: VIDEO_CODEC_PROTOCOL_TABLE[codec].rtxPayloadType,
			priority: VIDEO_CODEC_PROTOCOL_TABLE[codec].priority,
			encode: order.includes(codec),
			decode: decode[codec],
		})),
	];
}

export function countUnknownScreenShareParticipants(
	participants: ReadonlyArray<{identity: string; firstSeenAt: number}>,
	advertised: ReadonlySet<string>,
	now: number,
	graceMs: number = SCREEN_SHARE_CODEC_ADVERTISEMENT_GRACE_MS,
): number {
	return participants.filter(
		(participant) => !advertised.has(participant.identity) && now - participant.firstSeenAt >= graceMs,
	).length;
}

export function getDecodeSet(codecs: ReadonlyArray<FluxerCodecAdvertisement>): Set<VideoCodec> {
	const result = new Set<VideoCodec>();
	for (const codec of codecs) {
		if (codec.type !== 'video' || codec.decode !== true) continue;
		const mapped = NAME_TO_VIDEO_CODEC[codec.name as FluxerVideoCodecName];
		if (mapped) result.add(mapped);
	}
	return result;
}

export function getEncodeSet(codecs: ReadonlyArray<FluxerCodecAdvertisement>): Set<VideoCodec> {
	const result = new Set<VideoCodec>();
	for (const codec of codecs) {
		if (codec.type !== 'video' || codec.encode !== true) continue;
		const mapped = NAME_TO_VIDEO_CODEC[codec.name as FluxerVideoCodecName];
		if (mapped) result.add(mapped);
	}
	return result;
}

function selectCompatibilityFallbackCodec(
	localEncode: ReadonlySet<VideoCodec>,
	remoteDecode: ReadonlyArray<ReadonlySet<VideoCodec>>,
): VideoCodec {
	for (const codec of CODEC_PREFERENCE) {
		if (!COMPATIBILITY_CODECS.has(codec)) continue;
		if (localEncode.has(codec) && remoteDecode.every((decode) => decode.has(codec))) return codec;
	}
	return LAST_RESORT_VIDEO_CODEC;
}

function negotiateVideoCodec(
	localEncode: ReadonlySet<VideoCodec>,
	remoteDecode: ReadonlyArray<ReadonlySet<VideoCodec>>,
	unknownParticipants: number,
	codecPreference: ReadonlyArray<VideoCodec>,
): {codec: VideoCodec; candidates: Array<VideoCodec>} {
	const candidates = codecPreference.filter((codec) => {
		if (!localEncode.has(codec)) return false;
		if (unknownParticipants > 0 && !COMPATIBILITY_CODECS.has(codec)) return false;
		return remoteDecode.every((decode) => decode.has(codec));
	});
	return {codec: candidates[0] ?? selectCompatibilityFallbackCodec(localEncode, remoteDecode), candidates};
}

export function computeNegotiatedVideoCodec(
	localCodecs: ReadonlyArray<FluxerCodecAdvertisement>,
	remoteCodecs: ReadonlyArray<ReadonlyArray<FluxerCodecAdvertisement>>,
	unknownParticipants = 0,
	codecPreference: ReadonlyArray<VideoCodec> = CODEC_PREFERENCE,
): CodecNegotiationSelection {
	const {codec, candidates} = negotiateVideoCodec(
		getEncodeSet(localCodecs),
		remoteCodecs.map((remote) => getDecodeSet(remote)),
		unknownParticipants,
		codecPreference,
	);
	return {codec, reason: 'manual', candidates, unknownParticipants};
}
