// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CodecPreference, ScreenShareEncoderMode} from '@app/features/voice/utils/CodecCapabilityDetector';
import type {VideoCodec} from 'livekit-client';

export const CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['av1', 'h265', 'h264', 'vp9', 'vp8'];
export const SOFTWARE_CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['av1', 'vp9', 'h264', 'vp8'];
export const GECKO_SOFTWARE_CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['vp8', 'h264'];
export const NON_CHROMIUM_CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['h264', 'vp8'];
const COMPATIBILITY_FALLBACK_CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['h264', 'vp9', 'vp8'];
const BASELINE_VIDEO_CODEC: VideoCodec = 'vp8';
export const SCREEN_SHARE_CODEC_ADVERTISEMENT_GRACE_MS = 3_000;
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
export type ScreenShareCodecSubstitution = 'device' | 'viewer';

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

export interface ScreenShareCodecVerdict {
	short: boolean;
	ratio: number;
}

export interface ScreenShareCodecRankingInput {
	profile: ScreenShareCodecProfile;
	encoderModeSetting: ScreenShareEncoderMode;
	pin: CodecPreference;
	verdicts: Partial<Record<VideoCodec, ScreenShareCodecVerdict>>;
}

export interface ScreenShareCodecRanking {
	order: ReadonlyArray<VideoCodec>;
	hardwareUnavailable: boolean;
}

function resolveBaseScreenShareCodecOrder(
	input: ScreenShareCodecRankingInput,
	isHardware: (codec: VideoCodec) => boolean,
	hardwareAvailable: boolean,
): Array<VideoCodec> {
	if (input.profile.browser === 'firefox') return [...GECKO_SOFTWARE_CODEC_PREFERENCE];
	if (input.profile.browser !== 'chromium' && !input.profile.desktop) return [...NON_CHROMIUM_CODEC_PREFERENCE];
	if (input.encoderModeSetting === 'software' || !hardwareAvailable) return [...SOFTWARE_CODEC_PREFERENCE];
	return [
		...CODEC_PREFERENCE.filter((codec) => isHardware(codec)),
		...CODEC_PREFERENCE.filter((codec) => !isHardware(codec)),
	];
}

function demoteShortScreenShareCodecs(
	order: Array<VideoCodec>,
	verdicts: Partial<Record<VideoCodec, ScreenShareCodecVerdict>>,
): Array<VideoCodec> {
	const short = order.filter((codec) => verdicts[codec]?.short === true);
	if (short.length === 0) return order;
	return [
		...order.filter((codec) => verdicts[codec]?.short !== true),
		...short.sort((left, right) => (verdicts[right]?.ratio ?? 0) - (verdicts[left]?.ratio ?? 0)),
	];
}

export function rankScreenShareCodecs(input: ScreenShareCodecRankingInput): ScreenShareCodecRanking {
	const {codecs} = input.profile;
	const isHardware = (codec: VideoCodec): boolean => codecs[codec].supported && codecs[codec].hardware;
	const hardwareAvailable = CODEC_PREFERENCE.some((codec) => isHardware(codec));
	const ranked = demoteShortScreenShareCodecs(
		resolveBaseScreenShareCodecOrder(input, isHardware, hardwareAvailable),
		input.verdicts,
	);
	const pin = input.pin !== 'auto' && codecs[input.pin].allowed && codecs[input.pin].supported ? input.pin : null;
	const pinnedFirst = pin ? [pin, ...ranked.filter((codec) => codec !== pin)] : ranked;
	const order = pinnedFirst.filter(
		(codec) =>
			codecs[codec].allowed && codecs[codec].supported && (codec !== 'h265' || isHardware('h265') || pin === 'h265'),
	);
	return {
		order: order.length > 0 ? order : [BASELINE_VIDEO_CODEC],
		hardwareUnavailable: input.encoderModeSetting === 'hardware' && !hardwareAvailable,
	};
}

export function resolveScreenShareCodecSubstitution(
	pin: CodecPreference,
	order: ReadonlyArray<VideoCodec>,
	negotiated: VideoCodec,
): ScreenShareCodecSubstitution | null {
	if (pin === 'auto' || pin === negotiated) return null;
	return order.includes(pin) ? 'viewer' : 'device';
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
	for (const codec of COMPATIBILITY_FALLBACK_CODEC_PREFERENCE) {
		if (localEncode.has(codec) && remoteDecode.every((decode) => decode.has(codec))) return codec;
	}
	return BASELINE_VIDEO_CODEC;
}

function negotiateVideoCodec(
	localEncode: ReadonlySet<VideoCodec>,
	remoteDecode: ReadonlyArray<ReadonlySet<VideoCodec>>,
	unknownParticipants: number,
	codecPreference: ReadonlyArray<VideoCodec>,
): {codec: VideoCodec; candidates: Array<VideoCodec>} {
	const candidates = codecPreference.filter((codec) => {
		if (!localEncode.has(codec)) return false;
		if (unknownParticipants > 0 && !COMPATIBILITY_FALLBACK_CODEC_PREFERENCE.includes(codec)) return false;
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

export function resolveScreenShareAlternativeCodec(
	order: ReadonlyArray<VideoCodec>,
	current: VideoCodec,
	remoteDecode: ReadonlyArray<ReadonlySet<VideoCodec>>,
	unknownParticipants: number,
): VideoCodec | null {
	const remaining = order.filter((codec) => codec !== current);
	return negotiateVideoCodec(new Set(remaining), remoteDecode, unknownParticipants, remaining).candidates[0] ?? null;
}
