// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearScreenShareDeliveryMemoryKey,
	createScreenShareDeliveryState,
	describeScreenShareDelivery,
	evaluateScreenShareDelivery,
	readScreenShareDeliveryMemory,
	readScreenShareDeliverySample,
	type ScreenShareDeliveryContext,
	type ScreenShareDeliveryCounters,
	type ScreenShareDeliveryDecision,
	type ScreenShareDeliveryMemory,
	type ScreenShareDeliveryMemoryHeader,
	type ScreenShareDeliveryMemoryKey,
	type ScreenShareDeliveryMemoryWrite,
	type ScreenShareDeliveryShareCounters,
	type ScreenShareDeliveryShortCause,
	type ScreenShareDeliveryState,
	type ScreenShareDeliveryTickKind,
	type ScreenShareQualityLimitationReason,
	shouldToastScreenShareNotice,
	updateScreenShareDeliveryMemory,
} from '@app/features/voice/engine/ScreenShareUnderperformance';
import {
	buildScreenShareCodecAdvertisements,
	computeNegotiatedVideoCodec,
	countUnknownScreenShareParticipants,
	type FluxerCodecAdvertisement,
	rankScreenShareCodecs,
	resolveScreenShareAlternativeCodec,
	type ScreenShareCodecProfile,
	type ScreenShareCodecRankingInput,
	type ScreenShareCodecVerdict,
} from '@app/features/voice/utils/ScreenShareCodecSelection';
import {
	buildScreenShareSenderParameters,
	resolveScreenShareLayering,
	resolveScreenShareLevels,
	resolveScreenShareTarget,
	type ScreenShareContext,
	type ScreenShareLayering,
	type ScreenShareLevel,
	type ScreenShareTarget,
	type ScreenShareTargetInput,
	type SupportedScreenShareFrameRate,
} from '@app/features/voice/utils/ScreenShareOptions';
import type {VideoCodec} from 'livekit-client';

export type ProofResolution = ScreenShareTargetInput['storedResolution'];
export type ProofMode = ScreenShareTargetInput['mode'];
export type ProofHintSetting = ScreenShareTargetInput['hintSetting'];
export type ProofPin = ScreenShareCodecRankingInput['pin'];
export type ProofEncoderMode = ScreenShareCodecRankingInput['encoderModeSetting'];
export type ProofVerdicts = ScreenShareCodecRankingInput['verdicts'];
type ProofDimensions = {width: number; height: number};
type ProofDecodeSet = Record<VideoCodec, boolean>;

const PROOF_MODES: ReadonlyArray<ProofMode> = ['custom', 'gaming', 'screenshare'];
export const PROOF_RESOLUTIONS: ReadonlyArray<ProofResolution> = [
	'low_240p',
	'low_480p',
	'medium',
	'high',
	'ultra',
	'source',
];
export const PROOF_STORED_FRAME_RATES: ReadonlyArray<number> = [15, 24, 30, 45, 60, 90, 120];
const PROOF_ENTITLEMENTS: ReadonlyArray<boolean> = [true, false];
const PROOF_CONTEXTS: ReadonlyArray<ScreenShareContext> = ['display', 'app', 'device'];
const PROOF_SOURCES: ReadonlyArray<ProofDimensions | null> = [
	null,
	{width: 800, height: 600},
	{width: 1080, height: 1920},
	{width: 1920, height: 1080},
	{width: 2560, height: 1440},
	{width: 3024, height: 1964},
	{width: 3840, height: 2160},
	{width: 4112, height: 2658},
	{width: 5120, height: 1440},
	{width: 7680, height: 4320},
];
const PROOF_HINT_SETTINGS: ReadonlyArray<ProofHintSetting> = ['auto', 'detail', 'motion', 'text'];
export const PROOF_ALL_CODECS: ReadonlyArray<VideoCodec> = ['av1', 'h265', 'h264', 'vp9', 'vp8'];
export const PROOF_COMPATIBLE_CODECS: ReadonlyArray<VideoCodec> = ['h264', 'vp9', 'vp8'];
export const PROOF_MAX_LEVELS = 8;
export const PROOF_RUNTIME_KEY_COUNT = 340;
const PROOF_SHORT_RATIO = 0.85;
const PROOF_CONNECTION_TARGET_RATIO = 0.85;
const PROOF_TARGET_FILL_RATIO = 0.65;
const PROOF_ENCODE_DUTY_LIMIT = 1;
const PROOF_CPU_REASON_TICKS = 3;
const PROOF_HEALTHY_FRACTION = 0.9;
const PROOF_SHORT_FRACTION = 0.8;
const PROOF_WATCHABLE_TEXT_FPS = 7.5;
const PROOF_MOTION_CAP_RATIO = 0.85;
const PROOF_RESOLUTION_SHORT_RATIO = 0.9;
const PROOF_SOURCE_LIMITED_MIN_FPS = 2;
const PROOF_WINDOW_TICKS = 5;
const PROOF_ACT_TICKS = 4;
const PROOF_ACT_AFTER_RESET_TICKS = 8;
const PROOF_CLEAR_OK_TICKS = 5;
const PROOF_STALL_TICKS = 2;
const PROOF_PROBE_WAIT_TICKS = 33;
const PROOF_PROBE_BUSY_OK_TICKS = 30;
const PROOF_WARMUP_MIN_TICKS = 3;
const PROOF_RAMP_MAX_TICKS = 14;
const PROOF_RAMP_LOOKBACK_TICKS = 3;
const PROOF_RAMP_RISE_RATIO = 1.05;
const PROOF_RAMP_STALL_TICKS = 3;
const PROOF_MAX_UP_PROBES = 4;
const PROOF_MAX_DELIVERY_SWITCHES = 1;
const PROOF_MULTI_LAYER_MIN_VIEWERS = 2;
const PROOF_MULTI_LAYER_ON_TICKS = 5;
const PROOF_MULTI_LAYER_OFF_TICKS = 30;
const PROOF_MAX_LAYERING_CHANGES = 2;
const PROOF_SIMULCAST_LOW_SCALE = 2;
const PROOF_SIMULCAST_LOW_FLOOR_BPS = 150_000;
const PROOF_MAX_MEMORY_ENTRIES = 64;
const PROOF_MEMORY_TTL_MS = 14 * 24 * 60 * 60 * 1000;
const PROOF_TARGET_RAMP: ReadonlyArray<number> = [0.071, 0.071, 0.085, 0.098, 0.103, 0.109, 0.112, 0.739, 0.829, 0.829];
const PROOF_FIRST_TICK_MS = 2500;
const PROOF_TICK_MS = 2000;
const PROOF_ADVERTISEMENT_GRACE_MS = 3000;
export const PROOF_MAX_BITRATE_BPS = 6_000_000;
const PROOF_EPOCH_MS = 1_790_000_000_000;
export const PROOF_SHARE_TICKS = 120;
export const PROOF_VIEWER_SHARE_TICKS = 60;

const IDLE_EXPECTED_FPS = 2;
const IDLE_SENT_RATIO = 0.25;
const FULL_FILL = 0.95;
const JITTER_SPAN = 0.1;
const ENCODE_BIT_COST_FLOOR = 0.35;
const PROOF_FAST_ENCODE_COST = 0.4;
const BUDGET_SPENT_FILL = 0.98;
const APP_LIMITED_FILL = 0.45;
const HEADER_BYTES_SHARE = 0.03;
const KEEPS_UP_DUTY = 0.9;
const OVERLOADED_DUTY = 1.1;

export const PROOF_RUNG_DIMENSIONS: Record<ProofResolution, ProofDimensions> = {
	low_240p: {width: 426, height: 240},
	low_480p: {width: 854, height: 480},
	medium: {width: 1280, height: 720},
	high: {width: 1920, height: 1080},
	ultra: {width: 2560, height: 1440},
	source: {width: 3840, height: 2160},
};

const PROOF_BITRATE_KBPS: Record<ProofResolution, Record<SupportedScreenShareFrameRate, number>> = {
	low_240p: {15: 300, 30: 500, 60: 700, 90: 700, 120: 700},
	low_480p: {15: 1200, 30: 2000, 60: 3000, 90: 3000, 120: 3000},
	medium: {15: 2000, 30: 3000, 60: 4500, 90: 4500, 120: 4500},
	high: {15: 3000, 30: 4500, 60: 6000, 90: 6000, 120: 6000},
	ultra: {15: 4000, 30: 5500, 60: 6000, 90: 6000, 120: 6000},
	source: {15: 4500, 30: 6000, 60: 6000, 90: 6000, 120: 6000},
};

export function proofRungIndex(rung: ProofResolution): number {
	return PROOF_RESOLUTIONS.indexOf(rung);
}

export function proofRungPixels(rung: ProofResolution): number {
	return PROOF_RUNG_DIMENSIONS[rung].width * PROOF_RUNG_DIMENSIONS[rung].height;
}

export function proofFloorRung(pixels: number): ProofResolution {
	let rung: ProofResolution = 'low_240p';
	for (const candidate of PROOF_RESOLUTIONS) {
		if (proofRungPixels(candidate) > pixels) break;
		rung = candidate;
	}
	return rung;
}

export function proofQuantiseFrameRate(frameRate: number): SupportedScreenShareFrameRate {
	if (frameRate >= 120) return 120;
	if (frameRate >= 90) return 90;
	if (frameRate >= 60) return 60;
	if (frameRate >= 30) return 30;
	return 15;
}

export function proofTableBitrate(rung: ProofResolution, frameRate: SupportedScreenShareFrameRate): number {
	return PROOF_BITRATE_KBPS[rung][frameRate] * 1000;
}

export function proofLevelFloorRung(keep: ScreenShareTarget['keep'], levelZero: ScreenShareLevel): number {
	return Math.min(proofRungIndex(levelZero.rung), proofRungIndex(keep === 'resolution' ? 'medium' : 'low_480p'));
}

export function proofLevelFloorRate(keep: ScreenShareTarget['keep'], levelZero: ScreenShareLevel): number {
	return keep === 'resolution' ? 15 : Math.min(levelZero.frameRate, 30);
}

export function forEachProofTargetInput(visit: (input: ScreenShareTargetInput) => void): void {
	for (const mode of PROOF_MODES) {
		for (const storedResolution of PROOF_RESOLUTIONS) {
			for (const storedFrameRate of PROOF_STORED_FRAME_RATES) {
				for (const entitled of PROOF_ENTITLEMENTS) {
					for (const context of PROOF_CONTEXTS) {
						for (const sourceDimensions of PROOF_SOURCES) {
							for (const hintSetting of PROOF_HINT_SETTINGS) {
								visit({mode, storedResolution, storedFrameRate, entitled, context, sourceDimensions, hintSetting});
							}
						}
					}
				}
			}
		}
	}
}

interface ProofRuntimeKey {
	id: string;
	signature: string;
	context: ScreenShareContext;
	deviceReachable: boolean;
	sourceDimensions: ProofDimensions | null;
	target: ScreenShareTarget;
	levels: ReadonlyArray<ScreenShareLevel>;
}

export function describeProofRuntimeKey(target: ScreenShareTarget, levels: ReadonlyArray<ScreenShareLevel>): string {
	const hintClass = target.contentHint === 'motion' ? 'motion' : 'screen';
	const ladder = levels
		.map(
			(level) => `${level.segment}:${level.width}x${level.height}@${level.frameRate}/${level.rung}/${level.maxBitrate}`,
		)
		.join(',');
	return `${target.keep}|${hintClass}|${ladder}`;
}

export function buildProofRuntimeKeys(): Array<ProofRuntimeKey> {
	const bySignature = new Map<string, Omit<ProofRuntimeKey, 'id'>>();
	forEachProofTargetInput((input) => {
		const target = resolveScreenShareTarget(input);
		const levels = resolveScreenShareLevels(target, input.sourceDimensions);
		const signature = describeProofRuntimeKey(target, levels);
		const existing = bySignature.get(signature);
		if (existing) {
			existing.deviceReachable ||= input.context === 'device';
			return;
		}
		bySignature.set(signature, {
			signature,
			context: input.context,
			deviceReachable: input.context === 'device',
			sourceDimensions: input.sourceDimensions,
			target,
			levels,
		});
	});
	return [...bySignature.values()]
		.sort((left, right) => (left.signature < right.signature ? -1 : left.signature > right.signature ? 1 : 0))
		.map((key, index) => ({...key, id: `K${index}`}));
}

function createProofRandom(seed: number): () => number {
	let state = seed >>> 0;
	return () => {
		state = (state + 0x6d2b79f5) >>> 0;
		let value = Math.imul(state ^ (state >>> 15), 1 | state);
		value = (value + Math.imul(value ^ (value >>> 7), 61 | value)) ^ value;
		return ((value ^ (value >>> 14)) >>> 0) / 4_294_967_296;
	};
}

type ProofFractionRow = '720' | '1080' | '1440' | '4k';
type ProofSoftwareCodec = 'h264' | 'vp8' | 'vp9' | 'av1';

const MEASURED_FRACTIONS: Record<
	Exclude<ProofFractionRow, '4k'>,
	Record<ProofSoftwareCodec, [number, number, number]>
> = {
	'720': {h264: [0.95, 0.91, 0.97], vp8: [0.99, 0.97, 0.8], vp9: [0.99, 0.95, 1], av1: [1, 1, 1]},
	'1080': {h264: [0.81, 0.77, 0.83], vp8: [0.96, 0.88, 0.39], vp9: [0.98, 0.92, 0.92], av1: [1, 1, 0.97]},
	'1440': {h264: [0.55, 0.52, 0.59], vp8: [0.94, 0.79, 0.32], vp9: [0.98, 0.78, 0.78], av1: [1, 1, 0.93]},
};
const MEASURED_4K15_FRACTIONS: Record<ProofSoftwareCodec, number> = {h264: 0.57, vp8: 0.86, vp9: 0.97, av1: 0.99};
const FOUR_K30_SCALE_EXTRAPOLATED = 0.5;
const FOUR_K60_SCALE_EXTRAPOLATED = 0.25;

function resolveFractionRow(pixels: number): ProofFractionRow {
	if (pixels <= 1_500_000) return '720';
	if (pixels <= 2_900_000) return '1080';
	if (pixels <= 6_000_000) return '1440';
	return '4k';
}

function resolveFractionCell(codec: ProofSoftwareCodec, row: ProofFractionRow, frameRate: number): number {
	if (row === '4k') {
		if (frameRate <= 15) return MEASURED_4K15_FRACTIONS[codec];
		if (frameRate <= 30) return MEASURED_4K15_FRACTIONS[codec] * FOUR_K30_SCALE_EXTRAPOLATED;
		return MEASURED_4K15_FRACTIONS[codec] * FOUR_K60_SCALE_EXTRAPOLATED;
	}
	const cells = MEASURED_FRACTIONS[row][codec];
	if (frameRate <= 15) return cells[0];
	if (frameRate <= 30) return cells[1];
	return cells[2];
}

function proofSoftwareFraction(codec: VideoCodec, pixels: number, frameRate: number): number {
	const table: ProofSoftwareCodec = codec === 'h265' ? 'h264' : codec;
	const row = resolveFractionRow(pixels);
	if (frameRate > 60) return (resolveFractionCell(table, row, 60) * 60) / frameRate;
	return resolveFractionCell(table, row, frameRate);
}

type ProofProfileId = 'P1' | 'P2' | 'P3' | 'P4' | 'P5' | 'P6' | 'P7' | 'P8' | 'P9' | 'P10' | 'P11' | 'P12' | 'P13';

interface ProofProfile {
	id: ProofProfileId;
	browser: ScreenShareCodecProfile['browser'];
	desktop: boolean;
	senderCodecs: ReadonlyArray<VideoCodec>;
	hardwareCodecs: ReadonlyArray<VideoCodec>;
	mediaSource: boolean;
	targetBitrate: boolean;
	encodeStats: boolean;
	stallingCodecs: ReadonlyArray<VideoCodec>;
	fraction: (codec: VideoCodec, pixels: number, frameRate: number) => number;
}

const SOFTWARE_SENDER_CODECS: ReadonlyArray<VideoCodec> = ['vp8', 'vp9', 'h264', 'av1'];
const ALL_SENDER_CODECS: ReadonlyArray<VideoCodec> = ['vp8', 'vp9', 'h264', 'h265', 'av1'];
const FOUR_K_PIXELS = 3840 * 2160;
const P3_FULL_PIXEL_RATE = 1920 * 1080 * 60;
const P3_PARTIAL_PIXEL_RATE = FOUR_K_PIXELS * 30;

function scaledSoftware(scale: number): ProofProfile['fraction'] {
	return (codec, pixels, frameRate) => proofSoftwareFraction(codec, pixels, frameRate) * scale;
}

const CHROMIUM_DEFAULTS = {
	browser: 'chromium',
	desktop: true,
	mediaSource: true,
	targetBitrate: true,
	encodeStats: true,
	stallingCodecs: [],
} as const satisfies Partial<ProofProfile>;

export const PROOF_PROFILES: ReadonlyArray<ProofProfile> = [
	{
		...CHROMIUM_DEFAULTS,
		id: 'P1',
		senderCodecs: SOFTWARE_SENDER_CODECS,
		hardwareCodecs: [],
		fraction: scaledSoftware(1),
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P2',
		senderCodecs: ALL_SENDER_CODECS,
		hardwareCodecs: ['av1', 'h265', 'h264'],
		fraction: (codec, pixels, frameRate) => {
			if (codec === 'av1' || codec === 'h265' || codec === 'h264') {
				return pixels <= FOUR_K_PIXELS && frameRate <= 60 ? 1 : 0.9;
			}
			return proofSoftwareFraction(codec, pixels, frameRate);
		},
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P3',
		senderCodecs: SOFTWARE_SENDER_CODECS,
		hardwareCodecs: ['h264'],
		fraction: (codec, pixels, frameRate) => {
			if (codec !== 'h264') return proofSoftwareFraction(codec, pixels, frameRate) * 0.8;
			const pixelRate = pixels * frameRate;
			if (pixelRate <= P3_FULL_PIXEL_RATE) return 1;
			if (pixelRate <= P3_PARTIAL_PIXEL_RATE) return 0.7;
			return 0.5;
		},
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P4',
		senderCodecs: ['vp8', 'vp9', 'h264'],
		hardwareCodecs: [],
		fraction: scaledSoftware(1),
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P5',
		senderCodecs: SOFTWARE_SENDER_CODECS,
		hardwareCodecs: [],
		fraction: scaledSoftware(0.6),
	},
	{
		id: 'P6',
		browser: 'firefox',
		desktop: false,
		senderCodecs: SOFTWARE_SENDER_CODECS,
		hardwareCodecs: [],
		mediaSource: false,
		targetBitrate: false,
		encodeStats: false,
		stallingCodecs: [],
		fraction: scaledSoftware(1),
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P7',
		browser: 'other',
		desktop: false,
		encodeStats: false,
		senderCodecs: ALL_SENDER_CODECS,
		hardwareCodecs: [],
		fraction: scaledSoftware(1),
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P8',
		senderCodecs: SOFTWARE_SENDER_CODECS,
		hardwareCodecs: [],
		fraction: scaledSoftware(0.9),
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P9',
		senderCodecs: ALL_SENDER_CODECS,
		hardwareCodecs: [],
		fraction: scaledSoftware(1),
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P10',
		senderCodecs: ALL_SENDER_CODECS,
		hardwareCodecs: ['av1', 'h264'],
		fraction: (codec, pixels, frameRate) => {
			if (codec === 'h264') return 1;
			const software = proofSoftwareFraction(codec, pixels, frameRate);
			return codec === 'av1' ? software * 0.6 : software;
		},
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P11',
		senderCodecs: ['vp8', 'vp9', 'h264', 'h265'],
		hardwareCodecs: ['h264', 'h265'],
		fraction: (codec, pixels, frameRate) =>
			codec === 'h264' || codec === 'h265' ? 1 : proofSoftwareFraction(codec, pixels, frameRate),
	},
	{
		...CHROMIUM_DEFAULTS,
		id: 'P12',
		senderCodecs: SOFTWARE_SENDER_CODECS,
		hardwareCodecs: ['h264'],
		stallingCodecs: ['h264'],
		fraction: (codec, pixels, frameRate) => (codec === 'h264' ? 1 : proofSoftwareFraction(codec, pixels, frameRate)),
	},
];

export const PROOF_STATIC_PROFILES: ReadonlyArray<ProofProfile> = [
	...PROOF_PROFILES,
	{
		...CHROMIUM_DEFAULTS,
		id: 'P13',
		desktop: false,
		senderCodecs: SOFTWARE_SENDER_CODECS,
		hardwareCodecs: [],
		fraction: scaledSoftware(1),
	},
];

function getProofProfiles(ids: ReadonlyArray<ProofProfileId>): Array<ProofProfile> {
	return PROOF_PROFILES.filter((profile) => ids.includes(profile.id));
}

interface ProofOptIns {
	av1: boolean;
	hevc: boolean;
}

export const PROOF_OPT_INS: ReadonlyArray<ProofOptIns> = [
	{av1: false, hevc: false},
	{av1: true, hevc: false},
	{av1: false, hevc: true},
	{av1: true, hevc: true},
];

export function buildProofCodecProfile(
	profile: ProofProfile,
	optIns: ProofOptIns,
	failed: ReadonlySet<VideoCodec>,
): ScreenShareCodecProfile {
	const entry = (codec: VideoCodec) => {
		const supported = profile.senderCodecs.includes(codec);
		const policyAllowed = (codec !== 'av1' || optIns.av1) && (codec !== 'h265' || optIns.hevc);
		return {
			allowed: supported && policyAllowed && !failed.has(codec),
			supported,
			hardware: profile.hardwareCodecs.includes(codec),
		};
	};
	return {
		browser: profile.browser,
		desktop: profile.desktop,
		codecs: {vp8: entry('vp8'), vp9: entry('vp9'), h264: entry('h264'), h265: entry('h265'), av1: entry('av1')},
	};
}

function buildProofHardwareMap(profile: ProofProfile): Partial<Record<VideoCodec, boolean>> {
	const hardware: Partial<Record<VideoCodec, boolean>> = {};
	for (const codec of PROOF_ALL_CODECS) hardware[codec] = profile.hardwareCodecs.includes(codec);
	return hardware;
}

function buildProofDecodeSet(codecs: ReadonlyArray<VideoCodec>): ProofDecodeSet {
	return {
		av1: codecs.includes('av1'),
		h265: codecs.includes('h265'),
		h264: codecs.includes('h264'),
		vp9: codecs.includes('vp9'),
		vp8: codecs.includes('vp8'),
	};
}

const PROOF_FULL_DECODE: ReadonlyArray<VideoCodec> = PROOF_ALL_CODECS;
const LOCAL_DECODE = buildProofDecodeSet(PROOF_FULL_DECODE);

interface ProofViewer {
	identity: string;
	joinedAt: number;
	decode: ReadonlyArray<VideoCodec> | null;
}

interface ProofViewerMix {
	id: string;
	viewers: ReadonlyArray<ProofViewer>;
}

const LONG_AGO = -60_000;
const MOMENTS_AGO = -1_000;

export const PROOF_VIEWER_MIXES: ReadonlyArray<ProofViewerMix> = [
	{id: 'V0', viewers: []},
	{id: 'V1', viewers: [{identity: 'full', joinedAt: LONG_AGO, decode: PROOF_FULL_DECODE}]},
	{id: 'V2', viewers: [{identity: 'avc', joinedAt: LONG_AGO, decode: ['h264', 'vp8']}]},
	{id: 'V3', viewers: [{identity: 'silent', joinedAt: LONG_AGO, decode: null}]},
	{id: 'V4', viewers: [{identity: 'noav1', joinedAt: LONG_AGO, decode: ['h264', 'vp9', 'vp8']}]},
	{
		id: 'V5',
		viewers: [
			{identity: 'vpx', joinedAt: LONG_AGO, decode: ['vp9', 'vp8']},
			{identity: 'avc', joinedAt: LONG_AGO, decode: ['h264', 'vp8']},
		],
	},
	{id: 'V6', viewers: [{identity: 'fresh', joinedAt: MOMENTS_AGO, decode: null}]},
	{id: 'V7', viewers: [{identity: 'gecko', joinedAt: LONG_AGO, decode: ['av1', 'vp9', 'h264', 'vp8']}]},
	{
		id: 'V8',
		viewers: [
			{identity: 'full-a', joinedAt: LONG_AGO, decode: PROOF_FULL_DECODE},
			{identity: 'full-b', joinedAt: LONG_AGO, decode: PROOF_FULL_DECODE},
			{identity: 'silent', joinedAt: LONG_AGO, decode: null},
		],
	},
	{id: 'V9', viewers: [{identity: 'noavc', joinedAt: LONG_AGO, decode: ['vp9', 'vp8']}]},
];

interface ProofRoom {
	remotes: Array<Array<FluxerCodecAdvertisement>>;
	knownDecode: Array<ReadonlyArray<VideoCodec>>;
	unknown: number;
	expectedUnknown: number;
	graceBoundary: boolean;
	pending: boolean;
	signature: string;
}

export function buildProofRoom(viewers: ReadonlyArray<ProofViewer>, now: number): ProofRoom {
	const advertised = new Set<string>();
	const remotes: Array<Array<FluxerCodecAdvertisement>> = [];
	const knownDecode: Array<ReadonlyArray<VideoCodec>> = [];
	let pending = false;
	let expectedUnknown = 0;
	let graceBoundary = false;
	for (const viewer of viewers) {
		if (viewer.decode === null) {
			const waiting = now - viewer.joinedAt < PROOF_ADVERTISEMENT_GRACE_MS;
			graceBoundary ||= now - viewer.joinedAt === PROOF_ADVERTISEMENT_GRACE_MS;
			pending ||= waiting;
			if (!waiting) expectedUnknown += 1;
			continue;
		}
		advertised.add(viewer.identity);
		remotes.push(buildScreenShareCodecAdvertisements([], buildProofDecodeSet(viewer.decode)));
		knownDecode.push(viewer.decode);
	}
	const unknown = countUnknownScreenShareParticipants(
		viewers.map((viewer) => ({identity: viewer.identity, firstSeenAt: viewer.joinedAt})),
		advertised,
		now,
		PROOF_ADVERTISEMENT_GRACE_MS,
	);
	const signature = `${knownDecode.map((decode) => decode.join('+')).join('/')}|${unknown}`;
	return {remotes, knownDecode, unknown, expectedUnknown, graceBoundary, pending, signature};
}

interface ProofSelection {
	order: ReadonlyArray<VideoCodec>;
	codec: VideoCodec;
}

function selectProofCodec(
	codecProfile: ScreenShareCodecProfile,
	encoderModeSetting: ProofEncoderMode,
	pin: ProofPin,
	verdicts: ProofVerdicts,
	room: ProofRoom,
): ProofSelection {
	const ranking = rankScreenShareCodecs({profile: codecProfile, encoderModeSetting, pin, verdicts});
	const local = buildScreenShareCodecAdvertisements(ranking.order, LOCAL_DECODE);
	const negotiated = computeNegotiatedVideoCodec(local, room.remotes, room.unknown, ranking.order);
	return {order: ranking.order, codec: negotiated.codec};
}

function demoteProofVerdicts(verdicts: ProofVerdicts, codec: VideoCodec): ProofVerdicts {
	return {...verdicts, [codec]: {short: true, ratio: 0}};
}

interface ProofCondition {
	id: string;
	uplinkBps: (tick: number) => number | null;
	uplinkShare: (tick: number) => number | null;
	frameDropShare: (tick: number) => number | null;
	stallEveryTicks: number | null;
	appLimitedShare: (tick: number) => number | null;
	appLimitedFill: number;
	cpuReason: (tick: number) => boolean;
	bandwidthReason: (tick: number) => boolean;
	sourceFps: (captureFrameRate: number) => number;
	captureNative: boolean;
	deviceContext: boolean;
	sentShrink: (tick: number) => number;
	applyRejects: boolean;
	applyRejectsAfterStep: boolean;
	stallFromTick: number | null;
	stallEveryPublication: boolean;
	contentScale: (tick: number) => number;
	priorProbes: ReadonlyArray<number>;
}

const PROOF_C2_UPLINK_BPS = 2_500_000;
const PROOF_C3_UPLINK_BPS = 1_500_000;
const PROOF_C3_FROM_TICK = 30;
const PROOF_C3_UNTIL_TICK = 70;
const PROOF_C5_SOURCE_FPS = 24;
const PROOF_C6_SOURCE_FPS = 60;
const PROOF_C10_HEAVY_UNTIL_TICK = 60;
const PROOF_C11_TARGET_SHARE = 0.55;
const PROOF_C12_APP_LIMITED_FILL = 0.665;
const PROOF_C13_CPU_PERIOD = 7;
const PROOF_C13_CPU_TICKS = 3;
const PROOF_C14_DROP_SHARE = 0.63;
const PROOF_C15_UPLINK_SHARE = 0.88;
const PROOF_C15_LATE_UPLINK_SHARE = 0.845;
const PROOF_C15_LATE_FROM_TICK = 60;
const PROOF_C15_DROP_SHARE = 0.7;
const PROOF_C16_STALL_EVERY = 7;
const C10_HEAVY_SCALE = 0.7;
const C10_LIGHT_SCALE = 1.3;
const C10_LIGHT_CAP = 1.02;
const PROOF_C6_SHRINK_FROM_TICK = 40;
const C6_SENT_SHRINK = 0.8;
const C10_PRIOR_PROBES: ReadonlyArray<number> = [0, PROOF_MAX_UP_PROBES - 1, PROOF_MAX_UP_PROBES];

function isProofC3CapActive(tick: number): boolean {
	return tick >= PROOF_C3_FROM_TICK && tick <= PROOF_C3_UNTIL_TICK;
}

const CLEAN_CONDITION: ProofCondition = {
	id: 'c1',
	uplinkBps: () => null,
	uplinkShare: () => null,
	frameDropShare: () => null,
	stallEveryTicks: null,
	appLimitedShare: () => null,
	appLimitedFill: APP_LIMITED_FILL,
	cpuReason: () => false,
	bandwidthReason: () => false,
	sourceFps: (captureFrameRate) => captureFrameRate,
	captureNative: false,
	deviceContext: false,
	sentShrink: () => 1,
	applyRejects: false,
	applyRejectsAfterStep: false,
	stallFromTick: null,
	stallEveryPublication: false,
	contentScale: () => 1,
	priorProbes: [0],
};

const C8_CONDITION: ProofCondition = {...CLEAN_CONDITION, id: 'c8', stallFromTick: 15};

export const PROOF_CONDITIONS: ReadonlyArray<ProofCondition> = [
	CLEAN_CONDITION,
	{...CLEAN_CONDITION, id: 'c2', uplinkBps: () => PROOF_C2_UPLINK_BPS},
	{
		...CLEAN_CONDITION,
		id: 'c3',
		uplinkBps: (tick) => (isProofC3CapActive(tick) ? PROOF_C3_UPLINK_BPS : null),
		bandwidthReason: isProofC3CapActive,
	},
	{...CLEAN_CONDITION, id: 'c4', sourceFps: () => 1},
	{...CLEAN_CONDITION, id: 'c5', sourceFps: (captureFrameRate) => Math.min(captureFrameRate, PROOF_C5_SOURCE_FPS)},
	{
		...CLEAN_CONDITION,
		id: 'c6',
		sourceFps: () => PROOF_C6_SOURCE_FPS,
		captureNative: true,
		deviceContext: true,
		sentShrink: (tick) => (tick >= PROOF_C6_SHRINK_FROM_TICK ? C6_SENT_SHRINK : 1),
	},
	{...CLEAN_CONDITION, id: 'c7', applyRejects: true},
	C8_CONDITION,
	{...CLEAN_CONDITION, id: 'c9', stallFromTick: 5, stallEveryPublication: true},
	{
		...CLEAN_CONDITION,
		id: 'c10',
		contentScale: (tick) => (tick <= PROOF_C10_HEAVY_UNTIL_TICK ? C10_HEAVY_SCALE : C10_LIGHT_SCALE),
		priorProbes: C10_PRIOR_PROBES,
	},
	{...CLEAN_CONDITION, id: 'c11', appLimitedShare: () => PROOF_C11_TARGET_SHARE},
	{
		...CLEAN_CONDITION,
		id: 'c12',
		appLimitedShare: () => PROOF_C11_TARGET_SHARE,
		appLimitedFill: PROOF_C12_APP_LIMITED_FILL,
	},
	{
		...CLEAN_CONDITION,
		id: 'c13',
		uplinkBps: () => PROOF_C2_UPLINK_BPS,
		cpuReason: (tick) => tick % PROOF_C13_CPU_PERIOD < PROOF_C13_CPU_TICKS,
	},
	{...CLEAN_CONDITION, id: 'c14', frameDropShare: () => PROOF_C14_DROP_SHARE},
	{
		...CLEAN_CONDITION,
		id: 'c15',
		uplinkShare: (tick) => (tick < PROOF_C15_LATE_FROM_TICK ? PROOF_C15_UPLINK_SHARE : PROOF_C15_LATE_UPLINK_SHARE),
		frameDropShare: () => PROOF_C15_DROP_SHARE,
	},
	{
		...CLEAN_CONDITION,
		id: 'c16',
		uplinkBps: () => PROOF_C2_UPLINK_BPS,
		stallEveryTicks: PROOF_C16_STALL_EVERY,
	},
];

const LATE_REJECT_CONDITION: ProofCondition = {...CLEAN_CONDITION, id: 'c7-late', applyRejectsAfterStep: true};

function proofEncodeBitCost(targetShare: number): number {
	return ENCODE_BIT_COST_FLOOR + (1 - ENCODE_BIT_COST_FLOOR) * Math.min(1, targetShare);
}

function proofEncodeDuty(fraction: number, targetShare: number): number {
	const cost = fraction < 1 ? 1 / fraction : PROOF_FAST_ENCODE_COST;
	return proofEncodeBitCost(targetShare) * cost;
}

function proofTickFill(
	condition: ProofCondition,
	appLimited: boolean,
	targetShare: number,
	delivered: number,
	demanded: number,
	frameRate: number,
): number {
	if (appLimited) return condition.appLimitedFill;
	if (targetShare < 1) return FULL_FILL;
	if (delivered < demanded) return BUDGET_SPENT_FILL;
	const deliveredShare = delivered / frameRate;
	return deliveredShare >= PROOF_SHORT_RATIO ? FULL_FILL : deliveredShare;
}

function proofModelFraction(
	profile: ProofProfile,
	condition: ProofCondition,
	codec: VideoCodec,
	level: ScreenShareLevel,
	tick: number,
): number {
	const base = profile.fraction(codec, level.width * level.height, level.frameRate);
	const scale = condition.contentScale(tick);
	return scale > 1 ? Math.min(C10_LIGHT_CAP, base * scale) : base * scale;
}

interface ProofCodecPolicy {
	id: 'auto-off' | 'auto-on' | 'pinned';
	optIns: ProofOptIns;
	pinned: boolean;
}

export const PROOF_CODEC_POLICIES: ReadonlyArray<ProofCodecPolicy> = [
	{id: 'auto-off', optIns: {av1: false, hevc: false}, pinned: false},
	{id: 'auto-on', optIns: {av1: true, hevc: true}, pinned: false},
	{id: 'pinned', optIns: {av1: true, hevc: true}, pinned: true},
];

interface ProofViewerScript {
	id: string;
	viewersAt: (tick: number) => ReadonlyArray<ProofViewer>;
	pausedAt: (tick: number) => boolean;
}

function proofTickTime(tick: number): number {
	return PROOF_FIRST_TICK_MS + (tick - 1) * PROOF_TICK_MS;
}

const PROOF_F4_JOIN_TICK = 20;
const PROOF_F4_ADVERTISE_TICK = 21;
const PROOF_F4_DECODE: ReadonlyArray<VideoCodec> = ['h264', 'vp9', 'vp8'];
const PROOF_F7_LEAVE_TICK = 40;
const PROOF_F8_PAUSE_FROM_TICK = 5;
const PROOF_F8_RESUME_TICK = 60;

const FULL_VIEWER: ReadonlyArray<ProofViewer> = [{identity: 'full', joinedAt: LONG_AGO, decode: PROOF_FULL_DECODE}];
const NO_VIEWERS: ReadonlyArray<ProofViewer> = [];
const AVC_VIEWER: ReadonlyArray<ProofViewer> = [{identity: 'avc', joinedAt: LONG_AGO, decode: ['h264', 'vp8']}];
const SILENT_VIEWER: ReadonlyArray<ProofViewer> = [{identity: 'silent', joinedAt: LONG_AGO, decode: null}];
const SPLIT_VIEWERS: ReadonlyArray<ProofViewer> = [
	{identity: 'vpx', joinedAt: LONG_AGO, decode: ['vp9', 'vp8']},
	{identity: 'avc', joinedAt: LONG_AGO, decode: ['h264', 'vp8']},
];
const F4_SILENT_JOINER: ReadonlyArray<ProofViewer> = [
	{identity: 'joiner', joinedAt: proofTickTime(PROOF_F4_JOIN_TICK), decode: null},
];
const F4_ADVERTISED_JOINER: ReadonlyArray<ProofViewer> = [
	{identity: 'joiner', joinedAt: proofTickTime(PROOF_F4_JOIN_TICK), decode: PROOF_F4_DECODE},
];
const PROOF_F5_JOINED_AT = proofTickTime(PROOF_F4_JOIN_TICK) - PROOF_TICK_MS / 2;
const F5_QUIET_JOINER: ReadonlyArray<ProofViewer> = [{identity: 'quiet', joinedAt: PROOF_F5_JOINED_AT, decode: null}];
const SILENT_AND_AV1_VIEWERS: ReadonlyArray<ProofViewer> = [
	{identity: 'silent', joinedAt: LONG_AGO, decode: null},
	{identity: 'av1', joinedAt: LONG_AGO, decode: ['av1', 'vp8']},
];
const NEVER_PAUSED = () => false;

const PROOF_V1_SCRIPT: ProofViewerScript = {id: 'V1', viewersAt: () => FULL_VIEWER, pausedAt: NEVER_PAUSED};

function isProofF8Paused(tick: number): boolean {
	return tick >= PROOF_F8_PAUSE_FROM_TICK && tick < PROOF_F8_RESUME_TICK;
}

export const PROOF_VIEWER_SCRIPTS: ReadonlyArray<ProofViewerScript> = [
	{id: 'F1', viewersAt: () => FULL_VIEWER, pausedAt: NEVER_PAUSED},
	{id: 'F2', viewersAt: () => AVC_VIEWER, pausedAt: NEVER_PAUSED},
	{id: 'F3', viewersAt: () => SILENT_VIEWER, pausedAt: NEVER_PAUSED},
	{
		id: 'F4',
		viewersAt: (tick) =>
			tick < PROOF_F4_JOIN_TICK ? NO_VIEWERS : tick < PROOF_F4_ADVERTISE_TICK ? F4_SILENT_JOINER : F4_ADVERTISED_JOINER,
		pausedAt: NEVER_PAUSED,
	},
	{
		id: 'F5',
		viewersAt: (tick) => (tick < PROOF_F4_JOIN_TICK ? NO_VIEWERS : F5_QUIET_JOINER),
		pausedAt: NEVER_PAUSED,
	},
	{id: 'F6', viewersAt: () => SPLIT_VIEWERS, pausedAt: NEVER_PAUSED},
	{id: 'F7', viewersAt: (tick) => (tick < PROOF_F7_LEAVE_TICK ? FULL_VIEWER : NO_VIEWERS), pausedAt: NEVER_PAUSED},
	{id: 'F8', viewersAt: (tick) => (isProofF8Paused(tick) ? NO_VIEWERS : FULL_VIEWER), pausedAt: isProofF8Paused},
	{id: 'F9', viewersAt: () => SILENT_AND_AV1_VIEWERS, pausedAt: NEVER_PAUSED},
];

const PROOF_LAYERING_LEAVE_TICK = 60;
const PROOF_LAYERING_FLAP_TICKS = 3;
const PROOF_LAYERING_DEGRADE_FROM_TICK = 40;
const LAYERING_SLOW_SCALE = 0.5;

const PROOF_LAYERING_CONDITIONS: ReadonlyArray<ProofCondition> = [
	CLEAN_CONDITION,
	{
		...CLEAN_CONDITION,
		id: 'c11',
		sentShrink: (tick) => (tick >= PROOF_LAYERING_DEGRADE_FROM_TICK ? C6_SENT_SHRINK : 1),
	},
	{
		...CLEAN_CONDITION,
		id: 'c12',
		contentScale: (tick) => (tick >= PROOF_LAYERING_DEGRADE_FROM_TICK ? LAYERING_SLOW_SCALE : 1),
	},
];
const LAYERING_ONE_VIEWER: ReadonlyArray<ProofViewer> = [
	{identity: 'w1', joinedAt: LONG_AGO, decode: PROOF_FULL_DECODE},
];
const LAYERING_TWO_VIEWERS: ReadonlyArray<ProofViewer> = [
	...LAYERING_ONE_VIEWER,
	{identity: 'w2', joinedAt: LONG_AGO, decode: PROOF_FULL_DECODE},
];

export const PROOF_LAYERING_SCRIPTS: ReadonlyArray<ProofViewerScript> = [
	{id: 'L1', viewersAt: () => LAYERING_ONE_VIEWER, pausedAt: NEVER_PAUSED},
	{id: 'L2', viewersAt: () => LAYERING_TWO_VIEWERS, pausedAt: NEVER_PAUSED},
	{
		id: 'L3',
		viewersAt: (tick) => (tick < PROOF_LAYERING_LEAVE_TICK ? LAYERING_TWO_VIEWERS : LAYERING_ONE_VIEWER),
		pausedAt: NEVER_PAUSED,
	},
	{
		id: 'L4',
		viewersAt: (tick) =>
			Math.floor((tick - 1) / PROOF_LAYERING_FLAP_TICKS) % 2 === 0 ? LAYERING_TWO_VIEWERS : LAYERING_ONE_VIEWER,
		pausedAt: NEVER_PAUSED,
	},
];

function buildProofMemoryHeader(profile: ProofProfile, generation: number): ScreenShareDeliveryMemoryHeader {
	return {
		chromiumMajor: 140 + generation,
		gpuKey: profile.id,
		encoderMode: 'auto',
		hardwareAccelerationDisabled: profile.id === 'P9',
	};
}

interface ProofShareInput {
	key: ProofRuntimeKey;
	keyIndex: number;
	profile: ProofProfile;
	condition: ProofCondition;
	policy: ProofCodecPolicy;
	script: ProofViewerScript;
	tickCount: number;
	seed: number;
	memory: ScreenShareDeliveryMemory | null;
	header: ScreenShareDeliveryMemoryHeader;
	now: number;
	failed: Set<VideoCodec>;
	failCurrentCodec: boolean;
	adaptive: boolean;
}

interface ProofEvent {
	tick: number;
	kind: string;
	from: number;
	to: number;
	codec: VideoCodec;
	previousCodec: VideoCodec;
}

interface ProofShareResult {
	violations: Array<string>;
	tallies: Record<string, number>;
	startCodec: VideoCodec;
	startLevelIndex: number;
	startProbeBlocked: boolean;
	startOrder: ReadonlyArray<VideoCodec>;
	memoryFreeOrder: ReadonlyArray<VideoCodec>;
	startVerdicts: ProofVerdicts;
	endCodec: VideoCodec;
	events: Array<ProofEvent>;
	publications: number;
	republishes: number;
	steps: number;
	probes: number;
	failedProbes: number;
	switches: number;
	recoveries: number;
	stopped: boolean;
	notices: number;
	memoryWrites: Array<string>;
	noticeTicks: Map<string, Array<number>>;
	clearTicks: Array<number>;
	finalNotice: string | null;
	finalLevelIndex: number;
	sourceReports: Array<{frameRate: number; sourceFrameRate: number | null}>;
	pausedWindowTicks: number;
	passedProbeCycles: number;
	stalledWindowTicks: number;
	pausedDecisions: number;
	resumedWarmUp: boolean;
	decodeViolations: number;
	silentIncompatibleTicks: number;
	silentIncompatibleAlternatives: number;
	applyFailedRaises: number;
	layerings: Array<{tick: number; codec: VideoCodec; multiLayer: boolean}>;
	multiLayerTicks: number;
	memory: ScreenShareDeliveryMemory | null;
	ticksRun: number;
}

const CODEC_MIME: Record<VideoCodec, string> = {
	av1: 'video/AV1',
	h265: 'video/H265',
	h264: 'video/H264',
	vp9: 'video/VP9',
	vp8: 'video/VP8',
};
const WINDOW_NOTICES = new Set([
	'device-framerate',
	'device-resolution',
	'device-crossover',
	'device-short',
	'connection-short',
]);
const HOLD_NOTICES = new Set([...WINDOW_NOTICES, 'pinned-codec-short']);
const TRANSITIONS = new Set([
	'step',
	'probe',
	'revert-probe',
	'switch-codec',
	'republish-layering',
	'recover-stalled',
	'stop-stalled',
]);
const MAX_PUBLICATIONS = 12;
const PUBLICATION_ENDS = new Set([
	'switch-codec',
	'recover-stalled',
	'stop-stalled',
	'republish',
	'republish-layering',
]);

interface ProofStats {
	report: RTCStatsReport;
	codec: {type: 'codec'; id: string; mimeType: string};
	source: {
		type: 'media-source';
		id: string;
		kind: 'video';
		frames: number;
		framesPerSecond: number;
		width: number;
		height: number;
	};
	outbound: {
		type: 'outbound-rtp';
		id: string;
		kind: 'video';
		codecId: string;
		mediaSourceId: string | undefined;
		active: boolean;
		framesEncoded: number;
		bytesSent: number;
		headerBytesSent: number;
		totalEncodeTime: number | undefined;
		frameWidth: number;
		frameHeight: number;
		targetBitrate: number | undefined;
		qualityLimitationReason: string;
		encoderImplementation: string;
		timestamp: number;
	};
}

function createProofStats(profile: ProofProfile): ProofStats {
	const codec = {type: 'codec' as const, id: 'codec', mimeType: 'video/VP8'};
	const source = {
		type: 'media-source' as const,
		id: 'source',
		kind: 'video' as const,
		frames: 0,
		framesPerSecond: 0,
		width: 0,
		height: 0,
	};
	const outbound = {
		type: 'outbound-rtp' as const,
		id: 'outbound',
		kind: 'video' as const,
		codecId: 'codec',
		mediaSourceId: profile.mediaSource ? 'source' : undefined,
		active: true,
		framesEncoded: 0,
		bytesSent: 0,
		headerBytesSent: 0,
		totalEncodeTime: profile.encodeStats ? 0 : undefined,
		frameWidth: 0,
		frameHeight: 0,
		targetBitrate: undefined as number | undefined,
		qualityLimitationReason: 'none',
		encoderImplementation: 'proof',
		timestamp: 0,
	};
	const entries: Array<[string, unknown]> = [
		['codec', codec],
		['outbound', outbound],
	];
	if (profile.mediaSource) entries.push(['source', source]);
	return {report: new Map(entries) as unknown as RTCStatsReport, codec, source, outbound};
}

function floorEven(value: number): number {
	return Math.max(2, Math.floor(value / 2) * 2);
}

function buildProofEncodings(
	layering: ScreenShareLayering,
	levelZero: ScreenShareLevel,
): Array<RTCRtpEncodingParameters> {
	if (!layering.simulcast) return [{active: true}];
	return [
		{
			rid: 'q',
			active: true,
			maxBitrate: Math.max(
				PROOF_SIMULCAST_LOW_FLOOR_BPS,
				Math.floor(levelZero.maxBitrate / (PROOF_SIMULCAST_LOW_SCALE * PROOF_SIMULCAST_LOW_SCALE)),
			),
			scaleResolutionDownBy: PROOF_SIMULCAST_LOW_SCALE,
		},
		{rid: 'h', active: true, maxBitrate: levelZero.maxBitrate},
	];
}

interface ProofMove {
	level: number;
	kind: string;
}

const PROOF_FAILED_PROBE_CYCLE = 'step probe revert-probe';
const PROOF_PASSED_PROBE_CYCLES: ReadonlyArray<string> = ['step probe step', 'probe step probe'];

function findProofOscillations(moves: ReadonlyArray<ProofMove>): Array<string> {
	const sequence = moves.filter((move, index) => index === 0 || moves[index - 1].level !== move.level);
	const cycles: Array<string> = [];
	for (let index = 3; index < sequence.length; index++) {
		const first = sequence[index - 3].level;
		const second = sequence[index - 2].level;
		if (first === second || sequence[index - 1].level !== first || sequence[index].level !== second) continue;
		cycles.push(
			sequence
				.slice(index - 2, index + 1)
				.map((move) => move.kind)
				.join(' '),
		);
	}
	return cycles;
}

function lowerMedian(values: ReadonlyArray<number>): number {
	const sorted = [...values].sort((first, second) => first - second);
	return sorted[Math.floor((sorted.length - 1) / 2)];
}

function toVerdicts(entries: ReturnType<typeof readScreenShareDeliveryMemory>): ProofVerdicts {
	const verdicts: Partial<Record<VideoCodec, ScreenShareCodecVerdict>> = {};
	for (const codec of PROOF_ALL_CODECS) {
		const entry = entries[codec];
		if (entry) verdicts[codec] = {short: entry.short, ratio: entry.ratio};
	}
	return verdicts;
}

interface ProofWindowTick {
	kind: ScreenShareDeliveryTickKind;
	encodedFps: number;
	ratio: number;
	cpu: boolean;
	duty: number | null;
	targetRatio: number | null;
	fill: number | null;
	sourceFps: number | null;
}

function proofWindowMedian(values: ReadonlyArray<number | null>): number | null {
	const known = values.filter((value): value is number => value !== null);
	return known.length === values.length ? lowerMedian(known) : null;
}

function classifyProofWindow(window: ReadonlyArray<ProofWindowTick>): ScreenShareDeliveryShortCause {
	if (window.filter((tick) => tick.cpu).length >= PROOF_CPU_REASON_TICKS) return 'cpu';
	const measured = window.filter((tick) => tick.kind !== 'stalled');
	if (measured.length === 0) return 'stalled';
	const duty = proofWindowMedian(measured.map((tick) => tick.duty));
	if (duty !== null && duty >= PROOF_ENCODE_DUTY_LIMIT) return 'duty';
	const targetRatio = proofWindowMedian(measured.map((tick) => tick.targetRatio));
	if (targetRatio === null) return 'ratio-unmeasured';
	if (targetRatio >= PROOF_CONNECTION_TARGET_RATIO) return 'ratio';
	const fill = proofWindowMedian(measured.map((tick) => tick.fill));
	if (fill === null || fill < PROOF_TARGET_FILL_RATIO) return 'fill';
	return duty === null ? 'connection-unmeasured' : 'connection';
}

interface ProofExpectation {
	decision: string;
	levelIndex: number | null;
	write: string | null;
	notice: string | null | undefined;
}

const PROOF_HOLD: ProofExpectation = {decision: 'none', levelIndex: null, write: null, notice: undefined};

function resolveProofRampExit(
	samples: number,
	targets: ReadonlyArray<number | null>,
	maxBitrate: number,
): string | null {
	if (samples < PROOF_WARMUP_MIN_TICKS) return null;
	if (samples >= PROOF_RAMP_MAX_TICKS) return 'E3 ramp cap exit';
	const current = targets[targets.length - 1];
	if (current === null) return null;
	if (current >= PROOF_CONNECTION_ANY_THROTTLE * maxBitrate) return 'E3 settle exit';
	const earlier =
		targets.length > PROOF_RAMP_LOOKBACK_TICKS ? targets[targets.length - 1 - PROOF_RAMP_LOOKBACK_TICKS] : null;
	return earlier !== null && current <= PROOF_RAMP_RISE_RATIO * earlier ? 'E3 plateau exit' : null;
}

function resolveProofNextLevel(
	levels: ReadonlyArray<ScreenShareLevel>,
	keep: ScreenShareTarget['keep'],
	from: number,
	delivered: number,
): number | null {
	const lastSegmentZero = levels.findLastIndex((level) => level.segment === 0);
	if (from < lastSegmentZero) return from + 1;
	if (keep === 'resolution') {
		if (from + 1 >= levels.length) return null;
		return from === lastSegmentZero && delivered >= PROOF_WATCHABLE_TEXT_FPS ? null : from + 1;
	}
	const cap = levels.findIndex((level, index) => index > from && level.frameRate <= delivered);
	if (cap < 0) return null;
	return levels[cap].frameRate >= PROOF_MOTION_CAP_RATIO * delivered ? cap : null;
}

function proofLevelNoticeKind(levels: ReadonlyArray<ScreenShareLevel>, index: number): string | null {
	if (index === 0) return null;
	const slower = levels[index].frameRate < levels[0].frameRate;
	const smaller = levels[index].width * levels[index].height < levels[0].width * levels[0].height;
	if (slower && smaller) return 'device-crossover';
	return slower ? 'device-framerate' : 'device-resolution';
}

function describeProofWrite(write: ScreenShareDeliveryMemoryWrite | null): string | null {
	if (write === null) return null;
	if (write.kind === 'short') return `short ${write.ratio}`;
	if (write.kind === 'settled') return `settled ${write.settledIndex}`;
	return write.kind;
}

function resolveProofSourceRate(deliveryWindow: ReadonlyArray<ProofWindowTick>): number | null {
	const rates = deliveryWindow.map((tick) => tick.sourceFps).filter((rate): rate is number => rate !== null);
	return rates.length === deliveryWindow.length ? lowerMedian(rates) : null;
}

function isProofSourceRateLimited(rate: number | null, level: ScreenShareLevel): rate is number {
	return rate !== null && rate >= PROOF_SOURCE_LIMITED_MIN_FPS && rate < PROOF_SHORT_RATIO * level.frameRate;
}

function runProofShare(input: ProofShareInput): ProofShareResult {
	const {key, profile, condition, policy, script, adaptive} = input;
	const levels = key.levels;
	const levelZero = levels[0];
	const levelZeroPixels = levelZero.width * levelZero.height;
	const keep = key.target.keep;
	const context: ScreenShareContext = condition.deviceContext && key.deviceReachable ? 'device' : key.context;
	const floorRung = proofLevelFloorRung(keep, levelZero);
	const floorRate = proofLevelFloorRate(keep, levelZero);
	const random = createProofRandom(input.seed);
	const violations: Array<string> = [];
	const label = `${key.id}/${profile.id}/${condition.id}/${policy.id}/${script.id}/${context}/${adaptive ? 'treatment' : 'control'}`;
	const fail = (tick: number, message: string) => {
		if (violations.length < 8) violations.push(`${label}@${tick} ${message}`);
	};
	const tallies: Record<string, number> = {};
	const tally = (name: string) => {
		tallies[name] = (tallies[name] ?? 0) + 1;
	};
	const failed = input.failed;
	const encoderModeSetting: ProofEncoderMode = 'auto';
	const hardwareMap = buildProofHardwareMap(profile);
	const memoryKeyAt = (offset: number): ScreenShareDeliveryMemoryKey => ({
		header: input.header,
		memoryKey: key.target.memoryKey,
		hardware: hardwareMap,
		now: input.now + offset,
	});
	const currentProfile = () => buildProofCodecProfile(profile, policy.optIns, failed);
	let verdicts = toVerdicts(readScreenShareDeliveryMemory(input.memory, memoryKeyAt(0)));
	const pin: ProofPin = policy.pinned
		? rankScreenShareCodecs({profile: currentProfile(), encoderModeSetting, pin: 'auto', verdicts: {}}).order[0]
		: 'auto';
	let viewers = script.viewersAt(1);
	let room = buildProofRoom(viewers, proofTickTime(1));
	const memoryFree = selectProofCodec(currentProfile(), encoderModeSetting, pin, {}, room);
	const initial = selectProofCodec(currentProfile(), encoderModeSetting, pin, verdicts, room);
	let memory = input.memory;
	const shareCounters: ScreenShareDeliveryShareCounters = {
		switches: 0,
		probes: condition.priorProbes[input.keyIndex % condition.priorProbes.length],
		layerings: 0,
		probeBlocked: false,
		toastShown: false,
	};
	const result: ProofShareResult = {
		violations,
		tallies,
		startCodec: initial.codec,
		startLevelIndex: 0,
		startProbeBlocked: false,
		startOrder: initial.order,
		memoryFreeOrder: memoryFree.order,
		startVerdicts: {...verdicts},
		endCodec: initial.codec,
		events: [],
		publications: 0,
		republishes: 0,
		steps: 0,
		probes: 0,
		failedProbes: 0,
		switches: 0,
		recoveries: 0,
		stopped: false,
		notices: 0,
		memoryWrites: [],
		noticeTicks: new Map(),
		clearTicks: [],
		finalNotice: null,
		finalLevelIndex: 0,
		sourceReports: [],
		pausedWindowTicks: 0,
		passedProbeCycles: 0,
		stalledWindowTicks: 0,
		pausedDecisions: 0,
		resumedWarmUp: false,
		decodeViolations: 0,
		silentIncompatibleTicks: 0,
		silentIncompatibleAlternatives: 0,
		applyFailedRaises: 0,
		layerings: [],
		multiLayerTicks: 0,
		memory,
		ticksRun: 0,
	};
	const stats = createProofStats(profile);
	const capture = condition.captureNative
		? (key.sourceDimensions ?? {width: levelZero.width, height: levelZero.height})
		: {width: levelZero.width, height: levelZero.height};
	let codec = initial.codec;
	let order = initial.order;
	let state: ScreenShareDeliveryState = createScreenShareDeliveryState(0);
	let counters: ScreenShareDeliveryCounters | null = null;
	let encodeTimeTotal = 0;
	let queuedBits = 0;
	let accepting = true;
	let rejectAll = false;
	let appliedScale = 1;
	let pendingScale: number | null = null;
	let lastApplyFailed = false;
	let alternative: VideoCodec | null = null;
	let alternativeStale = true;
	let probeFailed = false;
	let probeOrigin: number | null = null;
	let stepsThisPublication = 0;
	let probesPassedThisPublication = 0;
	let publicationTicks = 0;
	let rampTick = 0;
	let reapplied = false;
	let pinnedShown = false;
	let lastResetTick = 0;
	let wasPaused = false;
	let stallFrom: number | null = condition.stallFromTick;
	let stalledRun = 0;
	let warmedUp = false;
	let rampSamples = 0;
	let okRun = 0;
	let busyOkRun = 0;
	let settledSinceReset = false;
	let multiLayerRequest = false;
	let multiLayerRun = 0;
	let singleLayerRun = 0;
	let monitoredMaxBitrate = levelZero.maxBitrate;
	const rampTargets: Array<number | null> = [];
	const publicationMoves: Array<ProofMove> = [];
	const deliveryWindow: Array<ProofWindowTick> = [];

	const resetWindows = (tick: number) => {
		stalledRun = 0;
		deliveryWindow.length = 0;
		rampTargets.length = 0;
		rampSamples = 0;
		warmedUp = false;
		okRun = 0;
		busyOkRun = 0;
		multiLayerRun = 0;
		singleLayerRun = 0;
		settledSinceReset = false;
		lastResetTick = tick;
	};

	const layeringFor = (senderCodec: VideoCodec, requested: boolean): ScreenShareLayering =>
		resolveScreenShareLayering({
			codec: senderCodec,
			svcSetting: 'auto',
			levelPixels: levelZeroPixels,
			multiLayer: requested,
		});

	const buildParameters = (
		tick: number,
		level: ScreenShareLevel,
		senderCodec: VideoCodec,
		role: string,
		layering: ScreenShareLayering,
	) => {
		const encodings = buildProofEncodings(layering, levelZero);
		const parameters = buildScreenShareSenderParameters({
			encodings,
			level,
			capture,
			scalabilityMode: layering.scalabilityMode,
		});
		if (parameters.degradationPreference !== 'maintain-resolution') fail(tick, `R9 ${role} preference`);
		const total = proofTableBitrate(level.rung, level.frameRate);
		const weights = encodings.map((encoding) => encoding.maxBitrate ?? 0);
		const weightSum = weights.reduce((sum, weight) => sum + weight, 0);
		const baseScale = Math.min(...encodings.map((encoding) => encoding.scaleResolutionDownBy ?? 1));
		const levelScale = Math.max(1, Math.sqrt((capture.width * capture.height) / (level.width * level.height)));
		const expectedMode = layering.scalabilityMode;
		for (const [index, encoding] of parameters.encodings.entries()) {
			if ((encoding.scaleResolutionDownBy ?? 1) < 1) fail(tick, `R9 ${role} scale below 1`);
			if (encoding.scalabilityMode !== expectedMode) {
				fail(tick, `R9 ${role} layering ${encoding.scalabilityMode} on ${senderCodec}`);
			}
			const expectedBitrate = encodings.length === 1 ? total : Math.floor((weights[index] / weightSum) * total);
			if (encoding.maxBitrate !== expectedBitrate) {
				fail(tick, `R8 ${role} bitrate ${encoding.maxBitrate} at ${level.rung}@${level.frameRate}`);
			}
			if (encoding.maxFramerate !== level.frameRate) fail(tick, `R8 ${role} frame rate`);
			const expectedScale = ((encodings[index].scaleResolutionDownBy ?? 1) / baseScale) * levelScale;
			if (encoding.scaleResolutionDownBy !== expectedScale) {
				fail(tick, `R17 ${role} scale ${encoding.scaleResolutionDownBy}, expected ${expectedScale}`);
			}
		}
		const top = parameters.encodings.length - 1;
		if (parameters.monitoredMaxBitrate !== parameters.encodings[top].maxBitrate) {
			fail(tick, `R17 ${role} monitored bitrate ${parameters.monitoredMaxBitrate}`);
		}
		return parameters;
	};

	const applyLevel = (tick: number, levelIndex: number, kind: string) => {
		const level = levels[levelIndex];
		if (level.width * level.height > levelZeroPixels || level.frameRate > levelZero.frameRate) {
			fail(tick, 'R4 level above level 0');
		}
		if (proofRungIndex(level.rung) < floorRung) fail(tick, `R4 rung ${level.rung} below the floor`);
		if (level.frameRate < floorRate) fail(tick, `R4 rate ${level.frameRate} below the floor`);
		const primary = buildParameters(tick, level, codec, 'primary', layeringFor(codec, multiLayerRequest));
		if (codec !== 'h264' && codec !== 'vp8' && order.includes('h264')) {
			const backup = buildParameters(tick, level, 'h264', 'backup', layeringFor('h264', false));
			if (backup.encodings.length !== 1) fail(tick, 'R18 backup sender carries more than one encoding');
		}
		publicationMoves.push({level: levelIndex, kind});
		if (!accepting) {
			lastApplyFailed = true;
			if (stepsThisPublication > 0) tally('R13 apply failed after a step');
			return;
		}
		const top = primary.encodings.length - 1;
		pendingScale = primary.encodings[top].scaleResolutionDownBy ?? 1;
		monitoredMaxBitrate = primary.monitoredMaxBitrate;
	};

	const checkAlternation = (tick: number) => {
		for (const cycle of findProofOscillations(publicationMoves)) {
			if (PROOF_PASSED_PROBE_CYCLES.includes(cycle)) result.passedProbeCycles += 1;
			else if (cycle !== PROOF_FAILED_PROBE_CYCLE) fail(tick, `R6 oscillation ${cycle}`);
		}
		publicationMoves.length = 0;
	};

	const startPublication = (tick: number, nextCodec: VideoCodec, nextOrder: ReadonlyArray<VideoCodec>) => {
		checkAlternation(tick);
		result.publications += 1;
		codec = nextCodec;
		order = nextOrder;
		const entry = readScreenShareDeliveryMemory(memory, memoryKeyAt(proofTickTime(Math.max(tick, 1))))[codec];
		const startLevel = Math.min(entry?.settledIndex ?? 0, levels.length - 1);
		if (result.publications === 1) {
			result.startLevelIndex = startLevel;
			const blockedUntil = entry?.probeBlockedUntil ?? null;
			if (blockedUntil !== null && blockedUntil > input.now) {
				shareCounters.probeBlocked = true;
				result.startProbeBlocked = true;
			}
		}
		state = createScreenShareDeliveryState(startLevel);
		counters = null;
		stats.outbound.framesEncoded = 0;
		stats.outbound.bytesSent = 0;
		stats.outbound.headerBytesSent = 0;
		encodeTimeTotal = 0;
		queuedBits = 0;
		stats.outbound.totalEncodeTime = profile.encodeStats ? 0 : undefined;
		stats.source.frames = 0;
		stats.codec.mimeType = CODEC_MIME[codec];
		accepting = true;
		appliedScale = 1;
		pendingScale = null;
		lastApplyFailed = false;
		alternativeStale = true;
		stepsThisPublication = 0;
		probesPassedThisPublication = 0;
		probeOrigin = null;
		publicationTicks = 0;
		rampTick = 0;
		reapplied = false;
		pinnedShown = false;
		resetWindows(tick);
		applyLevel(tick, startLevel, 'start');
		const rejecting = condition.applyRejects || rejectAll;
		accepting = !rejecting;
		lastApplyFailed = rejecting;
		if (condition.stallEveryPublication && condition.stallFromTick !== null) stallFrom = tick + condition.stallFromTick;
	};

	const reselect = () => selectProofCodec(currentProfile(), encoderModeSetting, pin, verdicts, room);

	startPublication(0, initial.codec, initial.order);
	if (input.failCurrentCodec) failed.add(initial.codec);

	for (let tick = 1; tick <= input.tickCount; tick++) {
		result.ticksRun = tick;
		publicationTicks += 1;
		const nowMs = proofTickTime(tick);
		if (pendingScale !== null) {
			appliedScale = pendingScale;
			pendingScale = null;
		}
		if (state.warmedUp !== warmedUp) fail(tick, `E3 warmed up ${state.warmedUp}, expected ${warmedUp}`);
		const nextViewers = script.viewersAt(tick);
		if (nextViewers !== viewers || room.pending) {
			viewers = nextViewers;
			const nextRoom = buildProofRoom(viewers, nowMs);
			if (nextRoom.unknown !== nextRoom.expectedUnknown) {
				fail(tick, `H counted ${nextRoom.unknown} silent viewers, expected ${nextRoom.expectedUnknown}`);
			}
			if (nextRoom.graceBoundary) tally('H grace boundary');
			const changed = nextRoom.signature !== room.signature;
			room = nextRoom;
			if (changed) {
				alternativeStale = true;
				const selection = reselect();
				if (selection.codec !== codec) {
					result.republishes += 1;
					result.events.push({
						tick,
						kind: 'republish',
						from: state.levelIndex,
						to: 0,
						codec: selection.codec,
						previousCodec: codec,
					});
					startPublication(tick, selection.codec, selection.order);
					continue;
				}
			}
		}
		if (room.knownDecode.some((decode) => !decode.includes(codec))) {
			if (order.some((candidate) => room.knownDecode.every((decode) => decode.includes(candidate)))) {
				result.decodeViolations += 1;
			}
		}
		if (room.unknown > 0 && !PROOF_COMPATIBLE_CODECS.includes(codec)) {
			if (order.some((candidate) => PROOF_COMPATIBLE_CODECS.includes(candidate))) result.silentIncompatibleTicks += 1;
		}
		if (alternativeStale) {
			alternativeStale = false;
			const ranking = rankScreenShareCodecs({
				profile: currentProfile(),
				encoderModeSetting,
				pin,
				verdicts: demoteProofVerdicts(verdicts, codec),
			});
			alternative = resolveScreenShareAlternativeCodec(
				ranking.order,
				codec,
				room.knownDecode.map((decode) => new Set(decode)),
				room.unknown,
			);
			if (room.unknown > 0 && alternative !== null && !PROOF_COMPATIBLE_CODECS.includes(alternative)) {
				result.silentIncompatibleAlternatives += 1;
			}
			if (codec === 'vp8' && room.unknown > 0 && ranking.order.includes('av1')) {
				if (room.knownDecode.some((decode) => decode.includes('av1'))) tally('R14 VP8 fallback with a silent viewer');
			}
		}

		const level = levels[state.levelIndex];
		const levelPixels = level.width * level.height;
		const multiLayerActive: boolean = layeringFor(codec, multiLayerRequest).multiLayer;
		const paused = script.pausedAt(tick);
		const resumed = wasPaused && !paused;
		rampTick = paused ? 0 : rampTick + 1;
		const rampFactor = PROOF_TARGET_RAMP[rampTick - 1] ?? 1;
		const seconds = (tick === 1 ? PROOF_FIRST_TICK_MS : PROOF_TICK_MS) / 1000;
		const uplink = condition.uplinkBps(tick);
		const uplinkShare = condition.uplinkShare(tick);
		const appLimitedShare = condition.appLimitedShare(tick);
		const rampedTarget = monitoredMaxBitrate * rampFactor;
		const allowances = [rampedTarget];
		if (uplink !== null) allowances.push(uplink);
		if (uplinkShare !== null) allowances.push(uplinkShare * monitoredMaxBitrate);
		if (appLimitedShare !== null) allowances.push(appLimitedShare * monitoredMaxBitrate);
		const target = Math.min(...allowances);
		const targetShare = target / monitoredMaxBitrate;
		const fraction = proofModelFraction(profile, condition, codec, level, tick);
		const sourceFps = condition.sourceFps(levelZero.frameRate);
		const demanded = Math.min(sourceFps, level.frameRate);
		const capacity = fraction * level.frameRate * targetShare;
		const dropShare = condition.frameDropShare(tick);
		const delivered = Math.min(demanded, capacity, dropShare === null ? demanded : dropShare * level.frameRate);
		const bandwidth = condition.bandwidthReason(tick);
		const capped = uplink !== null || uplinkShare !== null || appLimitedShare !== null;
		const overused = !capped && rampFactor === 1 && capacity < demanded;
		const cpu = !bandwidth && (overused || condition.cpuReason(tick));
		const stalled =
			profile.stallingCodecs.includes(codec) ||
			(condition.stallEveryTicks !== null && tick % condition.stallEveryTicks === 0) ||
			(stallFrom !== null && tick >= stallFrom && (condition.stallEveryPublication || result.publications === 1));
		const frameJitter = 1 + (random() - 0.5) * JITTER_SPAN;
		const sourceJitter = 1 + (random() - 0.5) * JITTER_SPAN;
		const shrink = condition.sentShrink(tick);
		let encodedDelta = 0;
		let sourceDelta = 0;
		let bytesDelta = 0;
		let headerDelta = 0;
		let encodeTimeDelta = 0;
		stats.outbound.timestamp = input.now + nowMs;
		stats.outbound.active = !paused;
		if (!paused) {
			sourceDelta = Math.max(1, Math.round(sourceFps * seconds * sourceJitter));
			encodedDelta = stalled ? 0 : Math.max(1, Math.round(delivered * seconds * frameJitter));
			const fill = proofTickFill(
				condition,
				appLimitedShare !== null,
				targetShare,
				delivered,
				demanded,
				level.frameRate,
			);
			const encodedBits = stalled ? 0 : fill * target * seconds * frameJitter;
			const sentBits = stalled ? queuedBits : encodedBits;
			queuedBits = encodedBits;
			bytesDelta = Math.round((sentBits * (1 - HEADER_BYTES_SHARE)) / 8);
			headerDelta = Math.round((sentBits * HEADER_BYTES_SHARE) / 8);
			encodeTimeDelta = (proofEncodeDuty(fraction, targetShare) / level.frameRate) * encodedDelta * frameJitter;
			encodeTimeTotal += encodeTimeDelta;
			stats.source.frames += sourceDelta;
			stats.source.framesPerSecond = sourceFps;
			stats.source.width = capture.width;
			stats.source.height = capture.height;
			stats.outbound.framesEncoded += encodedDelta;
			stats.outbound.bytesSent += bytesDelta;
			stats.outbound.headerBytesSent += headerDelta;
			stats.outbound.totalEncodeTime = profile.encodeStats ? encodeTimeTotal : undefined;
			stats.outbound.frameWidth = floorEven((capture.width / appliedScale) * shrink);
			stats.outbound.frameHeight = floorEven((capture.height / appliedScale) * shrink);
			stats.outbound.targetBitrate = profile.targetBitrate ? target : undefined;
			stats.outbound.qualityLimitationReason = bandwidth ? 'bandwidth' : cpu ? 'cpu' : 'none';
		}
		const hadCounters = counters !== null;
		const sample = readScreenShareDeliverySample(stats.report, counters);
		counters = sample.counters ?? counters;
		const ctx: ScreenShareDeliveryContext = {
			adaptive,
			levels,
			keep,
			context,
			codec,
			pinned: policy.pinned,
			alternativeCodec: alternative,
			codecFailed: failed.has(codec),
			lastApplyFailed,
			paused,
			viewers: viewers.length,
			multiLayer: multiLayerActive,
			multiLayerAvailable: layeringFor(codec, true).multiLayer,
			monitoredMaxBitrate,
			shareCounters,
		};
		const fromIndex = state.levelIndex;
		const warmBefore = warmedUp;
		const applyFailedBefore = state.applyFailed;
		const noticeBefore = state.notice;
		const evaluation = evaluateScreenShareDelivery(state, sample, ctx);
		const decision = evaluation.decision;
		const notice = evaluation.notice;
		if (!applyFailedBefore && state.applyFailed) result.applyFailedRaises += 1;
		if (adaptive && lastApplyFailed && !state.applyFailed) fail(tick, 'E10 apply failure not held');
		if (!applyFailedBefore && state.applyFailed && notice?.kind !== 'apply-failed') {
			fail(tick, `E10 apply failure shows ${notice?.kind}`);
		}

		if (paused) {
			wasPaused = true;
			if (evaluation.kind !== 'inactive') fail(tick, 'R14 paused tick is not inactive');
			if (decision.kind !== 'none') result.pausedDecisions += 1;
			if (!evaluation.warmingUp) result.pausedWindowTicks += 1;
		} else if (wasPaused) {
			wasPaused = false;
			if (evaluation.warmingUp) result.resumedWarmUp = true;
		}

		const encodedFps = encodedDelta / seconds;
		const measuredSource = profile.mediaSource ? sourceDelta / seconds : null;
		const expectedRate = cpu || measuredSource === null ? level.frameRate : Math.min(level.frameRate, measuredSource);
		const ratio = encodedFps / expectedRate;
		const sentBps = (bytesDelta * 8) / seconds;
		const sentPixels = stats.outbound.frameWidth * stats.outbound.frameHeight;
		const expectedPixels =
			context === 'device'
				? levelPixels
				: profile.mediaSource
					? Math.min(levelPixels, capture.width * capture.height)
					: null;
		let tickKind: ScreenShareDeliveryTickKind = 'ok';
		if (paused) tickKind = 'inactive';
		else if (resumed && hadCounters) tickKind = 'gap';
		else if (encodedDelta === 0 && profile.mediaSource) tickKind = 'stalled';
		else if (!hadCounters) tickKind = 'gap';
		else if (expectedRate < IDLE_EXPECTED_FPS) tickKind = 'idle';
		else if (measuredSource === null && sentBps < IDLE_SENT_RATIO * monitoredMaxBitrate) tickKind = 'idle';
		else if (ratio < PROOF_SHORT_RATIO) tickKind = 'short';
		else if (multiLayerActive) tickKind = 'ok';
		else if (expectedPixels !== null && sentPixels < PROOF_RESOLUTION_SHORT_RATIO * expectedPixels) {
			tickKind = 'resolution-short';
		}
		if (evaluation.kind !== tickKind) fail(tick, `D tick read as ${evaluation.kind}, expected ${tickKind}`);
		const measured = tickKind !== 'inactive' && tickKind !== 'gap';

		if (tickKind === 'stalled') stalledRun += 1;
		else stalledRun = 0;
		const requiredStall = publicationTicks === 1 ? 1 : warmBefore ? PROOF_STALL_TICKS : PROOF_RAMP_STALL_TICKS;
		const stallDue = tickKind === 'stalled' && stalledRun >= requiredStall;
		const stallDecision = decision.kind === 'recover-stalled' || decision.kind === 'stop-stalled';
		if (stallDecision !== stallDue) fail(tick, `R1 ${decision.kind} after ${stalledRun} stalled ticks`);
		if (stallDue) {
			const expectedStall: string = alternative !== null && !ctx.codecFailed ? 'recover-stalled' : 'stop-stalled';
			if (decision.kind !== expectedStall) fail(tick, `E1 ${decision.kind}, expected ${expectedStall}`);
			if (ctx.codecFailed) tally('E1 failed codec stops');
		}

		let pushed = false;
		if (measured && !stallDue) {
			if (!warmBefore) {
				rampSamples += 1;
				rampTargets.push(profile.targetBitrate ? target : null);
				if (rampTargets.length > PROOF_RAMP_LOOKBACK_TICKS + 1) rampTargets.shift();
				const exit = resolveProofRampExit(rampSamples, rampTargets, monitoredMaxBitrate);
				warmedUp = exit !== null;
				if (exit !== null) tally(exit);
			} else if (adaptive) {
				const manyViewers = viewers.length >= PROOF_MULTI_LAYER_MIN_VIEWERS;
				multiLayerRun = manyViewers ? multiLayerRun + 1 : 0;
				singleLayerRun = manyViewers ? 0 : singleLayerRun + 1;
				const busy = measuredSource === null || measuredSource >= PROOF_SHORT_RATIO * level.frameRate;
				okRun = tickKind === 'ok' ? okRun + 1 : 0;
				busyOkRun = tickKind === 'ok' && busy ? busyOkRun + 1 : 0;
				if (tickKind !== 'idle') {
					pushed = true;
					deliveryWindow.push({
						kind: tickKind,
						encodedFps,
						ratio: tickKind === 'stalled' ? 0 : ratio,
						cpu,
						duty:
							encodedDelta === 0 || !profile.encodeStats ? null : (encodeTimeDelta / encodedDelta) * level.frameRate,
						targetRatio: profile.targetBitrate ? target / monitoredMaxBitrate : null,
						fill: profile.targetBitrate ? ((bytesDelta + headerDelta) * 8) / seconds / target : null,
						sourceFps: measuredSource,
					});
					if (deliveryWindow.length > PROOF_WINDOW_TICKS) deliveryWindow.shift();
				}
			}
		}

		const noticeAtClear =
			adaptive && lastApplyFailed && !applyFailedBefore ? 'apply-failed' : (noticeBefore?.kind ?? null);
		const clearDue =
			measured &&
			!stallDue &&
			warmBefore &&
			tickKind === 'ok' &&
			okRun >= PROOF_CLEAR_OK_TICKS &&
			fromIndex === 0 &&
			noticeAtClear !== null &&
			noticeAtClear !== 'apply-failed';
		if (clearDue !== (noticeAtClear !== null && notice === null)) {
			fail(tick, `E11 ${noticeAtClear} ${clearDue ? 'kept' : 'cleared'} after ${okRun} ok ticks at level ${fromIndex}`);
		}
		if (clearDue) tally('E11 notice cleared');

		const windowFull = deliveryWindow.length === PROOF_WINDOW_TICKS;
		const countWindow = (test: (entry: ProofWindowTick) => boolean) => deliveryWindow.filter(test).length;
		const shortTicks = countWindow((entry) => entry.kind === 'short' || entry.kind === 'stalled');
		const resolutionTicks = countWindow((entry) => entry.kind === 'resolution-short');
		const shortWindow = windowFull && shortTicks >= PROOF_ACT_TICKS;
		const cpuMajority = countWindow((entry) => entry.cpu) * 2 > deliveryWindow.length;
		const windowCause = shortWindow ? classifyProofWindow(deliveryWindow) : null;
		if (windowCause !== null) tally(`E4 ${windowCause}`);
		if (shortWindow && deliveryWindow.some((entry) => entry.kind === 'stalled')) result.stalledWindowTicks += 1;
		const connectionWindow = windowCause === 'connection' || windowCause === 'connection-unmeasured';
		if (multiLayerActive) result.multiLayerTicks += 1;
		const writable = profile.targetBitrate || cpuMajority;

		const expectWindow = (): ProofExpectation => {
			const deliveredRate = lowerMedian(deliveryWindow.map((entry) => entry.encodedFps));
			if (shortTicks >= PROOF_ACT_TICKS) {
				if (connectionWindow) {
					probeOrigin = null;
					return {decision: 'none', levelIndex: null, write: null, notice: 'connection-short'};
				}
				if (probeOrigin !== null) {
					const write = writable ? 'probe-blocked' : null;
					return {
						decision: 'revert-probe',
						levelIndex: probeOrigin,
						write,
						notice: proofLevelNoticeKind(levels, probeOrigin),
					};
				}
				const write =
					fromIndex === 0 && writable ? `short ${lowerMedian(deliveryWindow.map((entry) => entry.ratio))}` : null;
				if (write !== null) tally('K short write');
				const replacement =
					fromIndex === 0 && shareCounters.switches < PROOF_MAX_DELIVERY_SWITCHES ? alternative : null;
				if (replacement !== null && !policy.pinned) {
					return {decision: 'switch-codec', levelIndex: null, write, notice: 'codec-switched'};
				}
				if (replacement !== null && !pinnedShown) {
					pinnedShown = true;
					return {decision: 'none', levelIndex: null, write, notice: 'pinned-codec-short'};
				}
				const next = resolveProofNextLevel(levels, keep, fromIndex, deliveredRate);
				const lastSegmentZero = levels.findLastIndex((entry) => entry.segment === 0);
				if (fromIndex >= lastSegmentZero && fromIndex + 1 < levels.length) {
					const shape = keep === 'resolution' ? 'text' : 'motion';
					tally(`E5 ${shape} ${next === null ? 'hold' : 'move'} past segment 0`);
				}
				if (next === null) return {decision: 'none', levelIndex: null, write, notice: 'device-short'};
				return {decision: 'step', levelIndex: next, write, notice: proofLevelNoticeKind(levels, next)};
			}
			if (resolutionTicks >= PROOF_ACT_TICKS) {
				if (!reapplied) {
					reapplied = true;
					tally('E7 reapply');
					return {decision: 'reapply-level', levelIndex: null, write: null, notice: undefined};
				}
				const kind = context === 'device' ? 'capture-resolution' : 'browser-resolution';
				tally(`E7 ${kind}`);
				return {decision: 'none', levelIndex: null, write: null, notice: kind};
			}
			if (shortTicks > 0 || resolutionTicks > 0) return PROOF_HOLD;
			if (probeOrigin !== null) {
				probeOrigin = null;
				probesPassedThisPublication += 1;
			}
			const write = !settledSinceReset && writable ? `settled ${fromIndex}` : null;
			settledSinceReset ||= write !== null;
			if (write !== null) tally('K settled write');
			const ready = fromIndex > 0 && busyOkRun >= PROOF_PROBE_BUSY_OK_TICKS && !shareCounters.probeBlocked;
			const probeable = ready && shareCounters.probes < PROOF_MAX_UP_PROBES;
			if (ready) tally(probeable ? 'E8 probe allowed' : 'E8 probe budget spent');
			if (ready && shareCounters.probes === PROOF_MAX_UP_PROBES - 1) tally('E8 last probe in budget');
			return {
				decision: probeable ? 'probe' : 'none',
				levelIndex: probeable ? fromIndex - 1 : null,
				write,
				notice: undefined,
			};
		};
		const expectCrossing = (): boolean | null => {
			if (state.applyFailed) return null;
			if (fromIndex !== 0 || probeOrigin !== null) return null;
			if (deliveryWindow.length < PROOF_WINDOW_TICKS) return null;
			if (shareCounters.layerings >= PROOF_MAX_LAYERING_CHANGES) return null;
			if (multiLayerActive && !layeringFor(codec, true).multiLayer) return false;
			if (!multiLayerActive && layeringFor(codec, true).multiLayer && multiLayerRun >= PROOF_MULTI_LAYER_ON_TICKS) {
				return true;
			}
			if (multiLayerActive && singleLayerRun >= PROOF_MULTI_LAYER_OFF_TICKS) return false;
			return null;
		};
		const expected = pushed && windowFull && !state.applyFailed ? expectWindow() : PROOF_HOLD;
		const crossing =
			!measured || stallDue || expected.decision !== 'none' || expected.notice === 'pinned-codec-short'
				? null
				: expectCrossing();
		const expectedDecision = crossing === null ? expected.decision : 'republish-layering';
		const expectedCause =
			pushed && windowFull && !state.applyFailed && shortTicks >= PROOF_ACT_TICKS ? windowCause : null;
		if (evaluation.shortCause !== expectedCause) {
			fail(tick, `E4 cause ${evaluation.shortCause}, expected ${expectedCause}`);
		}
		if (!adaptive) {
			tally('X control tick');
			if (state.levelIndex !== 0) fail(tick, `X1 control left level 0 for ${state.levelIndex}`);
			if (notice !== null) fail(tick, `X2 control raised ${notice.kind}`);
			if (evaluation.memoryWrite !== null) fail(tick, 'X3 control wrote memory');
			if (state.window.length !== 0) fail(tick, 'X4 control pushed a window tick');
			if (state.applyFailed) fail(tick, 'X5 control held an apply failure');
			if (evaluation.shortCause !== null) fail(tick, `X7 control classified ${evaluation.shortCause}`);
			if (!stallDecision && decision.kind !== 'none') fail(tick, `X6 control decided ${decision.kind}`);
		}
		if (!stallDue) {
			const decisionLevel = 'levelIndex' in decision ? decision.levelIndex : null;
			if (decision.kind !== expectedDecision || decisionLevel !== expected.levelIndex) {
				fail(tick, `E decided ${decision.kind} ${decisionLevel}, expected ${expectedDecision} ${expected.levelIndex}`);
			}
			if (decision.kind === 'republish-layering' && decision.multiLayer !== crossing) {
				fail(tick, `R17 crossed to ${decision.multiLayer}, expected ${crossing}`);
			}
			if (decision.kind === 'switch-codec' && decision.codec !== alternative) {
				fail(tick, `E5a switched to ${decision.codec}, expected ${alternative}`);
			}
			const write = describeProofWrite(evaluation.memoryWrite);
			if (write !== expected.write) fail(tick, `K wrote ${write}, expected ${expected.write}`);
			if (expected.notice !== undefined && (notice?.kind ?? null) !== expected.notice) {
				fail(tick, `E notice ${notice?.kind}, expected ${expected.notice}`);
			}
		} else if (evaluation.memoryWrite !== null) {
			fail(tick, 'K write on a stall decision');
		}
		if (pushed && windowFull) {
			const reset = decision.kind !== 'none' || expected.notice === 'pinned-codec-short';
			const sourceRate = resolveProofSourceRate(deliveryWindow);
			const limited = !reset && isProofSourceRateLimited(sourceRate, level);
			const cpuWindow = deliveryWindow.some((entry) => entry.cpu);
			if (limited && cpuWindow) tally('E12 cpu window not source limited');
			const expectedSource = limited && !cpuWindow ? sourceRate : null;
			if (expectedSource !== null) tally('E12 source limited');
			const reported = describeScreenShareDelivery(state, levels).sourceFrameRate;
			if (reported !== expectedSource) fail(tick, `E12 source rate ${reported}, expected ${expectedSource}`);
		}

		if (connectionWindow && (decision.kind === 'step' || decision.kind === 'switch-codec')) {
			fail(tick, `R11 ${decision.kind} on a connection window`);
		}
		if (connectionWindow && notice?.kind !== 'connection-short' && !TRANSITIONS.has(decision.kind)) {
			fail(tick, `R11 connection window shows ${notice?.kind}`);
		}
		if (evaluation.noticeChanged && notice !== null) {
			result.notices += 1;
			const ticks = result.noticeTicks.get(notice.kind);
			if (ticks) ticks.push(tick);
			else result.noticeTicks.set(notice.kind, [tick]);
			if (WINDOW_NOTICES.has(notice.kind) && !shortWindow) fail(tick, `R2 ${notice.kind} without a short window`);
			const toast = shouldToastScreenShareNotice(shareCounters, notice);
			if (toast !== (result.notices === 1)) fail(tick, `R12 toast ${toast} on notice ${result.notices}`);
			if (toast) {
				shareCounters.toastShown = true;
			}
		}
		if (evaluation.noticeChanged && notice === null) result.clearTicks.push(tick);
		result.finalNotice = notice?.kind ?? null;
		if (
			shortWindow &&
			!state.applyFailed &&
			tick - lastResetTick >= PROOF_ACT_AFTER_RESET_TICKS &&
			!(notice !== null && HOLD_NOTICES.has(notice.kind)) &&
			!TRANSITIONS.has(decision.kind)
		) {
			fail(tick, 'R2 short window without a notice or a transition');
		}
		if (evaluation.memoryWrite !== null) {
			const write = evaluation.memoryWrite;
			result.memoryWrites.push(`${write.kind}:${codec}:${write.kind === 'settled' ? write.settledIndex : ''}`);
			if (write.kind === 'short' && !shortWindow) fail(tick, 'K short write without a short window');
			if (!profile.targetBitrate && !cpuMajority) fail(tick, 'K write without a target or a cpu majority');
			if (write.kind !== 'settled' && connectionWindow) fail(tick, 'R11 memory write on a connection window');
			memory = updateScreenShareDeliveryMemory(memory, memoryKeyAt(nowMs), codec, write);
			if (Object.keys(memory.entries).length > PROOF_MAX_MEMORY_ENTRIES) fail(tick, 'R15 memory above its cap');
		}
		if (!evaluation.warmingUp && windowFull && evaluation.kind !== 'inactive') {
			result.sourceReports.push({
				frameRate: level.frameRate,
				sourceFrameRate: describeScreenShareDelivery(state, levels).sourceFrameRate,
			});
		}
		if (!measured || expected.notice === 'pinned-codec-short') resetWindows(tick);
		if (decision.kind !== 'none') {
			result.events.push({
				tick,
				kind: decision.kind,
				from: fromIndex,
				to: state.levelIndex,
				codec,
				previousCodec: codec,
			});
		}

		switch (decision.kind) {
			case 'none':
				break;
			case 'reapply-level':
				resetWindows(tick);
				applyLevel(tick, state.levelIndex, 'reapply-level');
				break;
			case 'step': {
				result.steps += 1;
				stepsThisPublication += 1;
				if (condition.applyRejects) fail(tick, 'R13 step after an apply failure');
				if (stepsThisPublication > PROOF_MAX_LEVELS - 1 + probesPassedThisPublication) fail(tick, 'R5 too many steps');
				const from = levels[fromIndex];
				const to = levels[decision.levelIndex];
				if (decision.levelIndex <= fromIndex) fail(tick, 'R3 step does not move down');
				if (from.segment === 0 && to.segment === 0 && decision.levelIndex !== fromIndex + 1) {
					fail(tick, 'R3 step skips inside segment 0');
				}
				const deliveredRate = lowerMedian(deliveryWindow.map((entry) => entry.encodedFps));
				if (keep === 'resolution') {
					if (decision.levelIndex !== fromIndex + 1) fail(tick, 'R3 kept resolution skips a level');
					if (from.segment === 0 && to.segment === 1 && !(deliveredRate < PROOF_WATCHABLE_TEXT_FPS)) {
						fail(tick, `R3 crossed to segment 1 at ${deliveredRate} FPS`);
					}
				} else if (to.segment === 1) {
					if (to.frameRate > deliveredRate) fail(tick, 'R3 motion cap above the delivered rate');
					if (to.frameRate < PROOF_MOTION_CAP_RATIO * deliveredRate) fail(tick, 'R3 motion cap too far below');
				}
				probeOrigin = null;
				resetWindows(tick);
				applyLevel(tick, decision.levelIndex, decision.kind);
				if (condition.applyRejectsAfterStep) {
					rejectAll = true;
					accepting = false;
				}
				break;
			}
			case 'probe':
				result.probes += 1;
				shareCounters.probes += 1;
				if (probeFailed) fail(tick, 'R6 probe after a failed probe');
				if (tick - lastResetTick < PROOF_PROBE_WAIT_TICKS)
					fail(tick, `R6 probe ${tick - lastResetTick} ticks after a change`);
				if (shareCounters.probeBlocked) fail(tick, 'R15 probe while blocked');
				if (shareCounters.probes > PROOF_MAX_UP_PROBES) fail(tick, 'R5 too many probes');
				if (decision.levelIndex !== fromIndex - 1) fail(tick, 'R3 probe moves more than one level');
				probeOrigin = fromIndex;
				resetWindows(tick);
				applyLevel(tick, decision.levelIndex, decision.kind);
				break;
			case 'revert-probe':
				result.failedProbes += 1;
				probeFailed = true;
				shareCounters.probeBlocked = true;
				if (result.failedProbes > 1) fail(tick, 'R5 more than one failed probe');
				if (probeOrigin !== decision.levelIndex) fail(tick, 'R6 revert to a level the probe did not leave');
				probeOrigin = null;
				resetWindows(tick);
				applyLevel(tick, decision.levelIndex, decision.kind);
				break;
			case 'switch-codec': {
				result.switches += 1;
				shareCounters.switches += 1;
				if (policy.pinned) fail(tick, 'R7 pinned codec replaced');
				if (result.switches > PROOF_MAX_DELIVERY_SWITCHES) fail(tick, 'R5 second delivery switch');
				if (fromIndex !== 0) fail(tick, 'E5a switch away from a lower level');
				verdicts = demoteProofVerdicts(verdicts, codec);
				const selection = reselect();
				if (selection.codec !== decision.codec)
					fail(tick, `switch to ${decision.codec} but negotiation gives ${selection.codec}`);
				startPublication(tick, selection.codec, selection.order);
				break;
			}
			case 'republish-layering': {
				result.layerings.push({tick, codec, multiLayer: decision.multiLayer});
				shareCounters.layerings += 1;
				if (decision.multiLayer === multiLayerActive) fail(tick, 'R17 layering republish without a change');
				if (shareCounters.layerings > PROOF_MAX_LAYERING_CHANGES) fail(tick, 'R5 too many layering changes');
				if (fromIndex !== 0) fail(tick, 'R17 layering republish away from level 0');
				multiLayerRequest = decision.multiLayer;
				startPublication(tick, codec, order);
				break;
			}
			case 'recover-stalled': {
				result.recoveries += 1;
				if (failed.has(codec)) fail(tick, 'R1 recovered a codec that already failed');
				if (codec !== 'vp8') failed.add(codec);
				const selection = reselect();
				if (selection.codec === codec || result.publications >= MAX_PUBLICATIONS) {
					result.stopped = true;
					break;
				}
				startPublication(tick, selection.codec, selection.order);
				break;
			}
			case 'stop-stalled':
				result.stopped = true;
				break;
		}
		if (result.stopped) break;
	}
	checkAlternation(result.ticksRun);
	result.endCodec = codec;
	result.finalLevelIndex = state.levelIndex;
	result.memory = memory;
	return result;
}

interface ProofStageOutcome {
	traces: number;
	ticks: number;
	violations: Array<string>;
	applied: Record<string, number>;
}

function createProofStageOutcome(): ProofStageOutcome {
	return {traces: 0, ticks: 0, violations: [], applied: {}};
}

function recordProofTrace(outcome: ProofStageOutcome, result: ProofShareResult): void {
	outcome.traces += 1;
	outcome.ticks += result.ticksRun;
	recordProofFindings(outcome, result);
}

function recordProofFindings(outcome: ProofStageOutcome, result: ProofShareResult): void {
	if (outcome.violations.length < 40) outcome.violations.push(...result.violations);
	for (const [rule, count] of Object.entries(result.tallies)) {
		outcome.applied[rule] = (outcome.applied[rule] ?? 0) + count;
	}
}

function createProofCheck(outcome: ProofStageOutcome, label: string) {
	return {
		applies(rule: string): void {
			outcome.applied[rule] = (outcome.applied[rule] ?? 0) + 1;
		},
		fail(message: string): void {
			if (outcome.violations.length < 40) outcome.violations.push(`${label} ${message}`);
		},
	};
}

function proofSeed(...parts: ReadonlyArray<number>): number {
	let seed = 0x811c9dc5;
	for (const part of parts) {
		seed = Math.imul(seed ^ part, 0x01000193) >>> 0;
	}
	return seed;
}

export const PROOF_RUNTIME_TRACES = 195_840;
export const PROOF_RUNTIME_TRACES_PER_FILE = 48_960;
export const PROOF_RUNTIME_RULES: ReadonlyArray<string> = [
	'E1 failed codec stops',
	'E3 plateau exit',
	'E3 settle exit',
	'E4 connection',
	'E4 cpu',
	'E4 duty',
	'E4 fill',
	'E4 ratio',
	'E5 motion hold past segment 0',
	'E5 motion move past segment 0',
	'E5 text hold past segment 0',
	'E5 text move past segment 0',
	'E7 browser-resolution',
	'E7 capture-resolution',
	'E7 reapply',
	'E8 last probe in budget',
	'E8 probe allowed',
	'E8 probe budget spent',
	'E11 notice cleared',
	'E12 cpu window not source limited',
	'E12 source limited',
	'K settled write',
	'K short write',
	'R1 c8',
	'R1 c9',
	'R1 stalled tick inside a short window',
	'R7',
	'R10 800x600',
	'R10 c4',
	'R10 c5',
	'R10 c6',
	'R11 c2 device',
	'R11 c2 hold',
	'R11 c2 notice',
	'R11 c3 hold',
	'R11 c3 notice',
	'R11 c11 device',
	'R13',
	'R13 apply failed after a step',
	'R16',
];

const PROOF_EXTRA_RUNTIME_TRACES: ReadonlyArray<{condition: ProofCondition; failCurrentCodec: boolean}> = [
	{condition: C8_CONDITION, failCurrentCodec: true},
	{condition: LATE_REJECT_CONDITION, failCurrentCodec: false},
];
const PROOF_CONNECTION_THROTTLE = 0.7;
const PROOF_CONNECTION_ANY_THROTTLE = 0.85;

function hasWindowNotice(result: ProofShareResult): boolean {
	for (const kind of result.noticeTicks.keys()) {
		if (WINDOW_NOTICES.has(kind)) return true;
	}
	return false;
}

function checkRuntimeTrace(
	outcome: ProofStageOutcome,
	key: ProofRuntimeKey,
	profile: ProofProfile,
	condition: ProofCondition,
	policy: ProofCodecPolicy,
	result: ProofShareResult,
): void {
	recordProofTrace(outcome, result);
	const check = createProofCheck(outcome, `${key.id}/${profile.id}/${condition.id}/${policy.id}`);
	for (let cycle = 0; cycle < result.passedProbeCycles; cycle++) check.applies('R6 passed probe cycle');
	const levelZero = key.levels[0];
	const startFraction = profile.fraction(result.startCodec, levelZero.width * levelZero.height, levelZero.frameRate);
	const startStalls = profile.stallingCodecs.includes(result.startCodec);
	const healthy = startFraction >= PROOF_HEALTHY_FRACTION && !startStalls;
	if (result.stalledWindowTicks > 0) check.applies('R1 stalled tick inside a short window');
	if (result.decodeViolations > 0 || result.silentIncompatibleTicks > 0) check.fail('R14 undecodable codec for V1');
	if (result.republishes > 0) check.fail('V1 caused a republish');
	const stalls = condition.stallFromTick !== null || condition.stallEveryTicks !== null;
	if (result.recoveries > 0 && !stalls && profile.id !== 'P12') {
		check.fail('R1 recovery without a stall');
	}
	if (key.sourceDimensions?.width === 800 && key.target.keep === 'resolution') {
		check.applies('R10 800x600');
		if (result.events.some((event) => event.kind === 'step' && key.levels[event.to].segment === 1)) {
			check.fail('R10 800x600 source stepped resolution');
		}
	}
	switch (condition.id) {
		case 'c1':
			if (healthy) {
				check.applies('R16');
				if (result.steps + result.switches + result.probes + result.notices > 0 || result.finalLevelIndex !== 0) {
					check.fail(`R16 healthy target left level 0 or set a notice ${result.events.map((event) => event.kind)}`);
				}
			}
			break;
		case 'c2':
		case 'c3': {
			const cap = condition.id === 'c2' ? PROOF_C2_UPLINK_BPS : PROOF_C3_UPLINK_BPS;
			const throttle = cap / levelZero.maxBitrate;
			if (!profile.targetBitrate) {
				check.applies('K write rule without a target');
				break;
			}
			const settledBeforeCap = condition.id === 'c2' || healthy;
			const cappedDuty = proofEncodeDuty(startFraction, Math.min(1, throttle));
			if (
				throttle < PROOF_CONNECTION_ANY_THROTTLE &&
				settledBeforeCap &&
				profile.encodeStats &&
				cappedDuty >= OVERLOADED_DUTY
			) {
				check.applies(`R11 ${condition.id} device`);
				const firstMove = result.events.find(
					(event) => event.kind === 'step' || event.kind === 'probe' || PUBLICATION_ENDS.has(event.kind),
				);
				const early = (result.noticeTicks.get('connection-short') ?? []).filter(
					(tick) => firstMove === undefined || tick < firstMove.tick,
				);
				if (early.length > 0)
					check.fail(`R11 an encoder that cannot keep up under the cap read as connection ${early}`);
			}
			if (cappedDuty > KEEPS_UP_DUTY || result.publications > 1) break;
			if (throttle < PROOF_CONNECTION_ANY_THROTTLE && settledBeforeCap) {
				check.applies(`R11 ${condition.id} hold`);
				if (result.steps + result.switches > 0) check.fail('R11 stepped or switched under a capped uplink');
				if (result.memoryWrites.some((write) => write.startsWith('short'))) check.fail('R11 short write under a cap');
			}
			if (throttle <= PROOF_CONNECTION_THROTTLE && settledBeforeCap && !result.stopped) {
				check.applies(`R11 ${condition.id} notice`);
				const ticks = result.noticeTicks.get('connection-short') ?? [];
				if (ticks.length === 0) check.fail('R11 no connection notice');
				if (condition.id === 'c2' && result.memoryWrites.length > 0) check.fail('R11 memory write under a cap');
				if (condition.id === 'c3') {
					const first = PROOF_C3_FROM_TICK + PROOF_ACT_TICKS - 1;
					const last = PROOF_C3_UNTIL_TICK + PROOF_WINDOW_TICKS - PROOF_ACT_TICKS;
					if (ticks.some((tick) => tick < first || tick > last)) check.fail(`R11 connection notice at ${ticks}`);
					const cleared = result.clearTicks.some(
						(tick) => tick > PROOF_C3_UNTIL_TICK && tick <= PROOF_C3_UNTIL_TICK + PROOF_CLEAR_OK_TICKS,
					);
					if (!cleared) check.fail(`R11 notice not cleared after the cap ${result.clearTicks}`);
				}
			}
			break;
		}
		case 'c11': {
			if (!profile.targetBitrate) break;
			check.applies('R11 c11 device');
			const notices = result.noticeTicks.get('connection-short') ?? [];
			if (notices.length > 0) check.fail(`R11 an application limited target read as connection ${notices}`);
			break;
		}
		case 'c4':
			check.applies('R10 c4');
			if (result.steps + result.switches + result.probes > 0 || hasWindowNotice(result)) {
				check.fail('R10 idle content acted');
			}
			break;
		case 'c5': {
			if (!profile.mediaSource || startStalls) break;
			const demanded = Math.min(levelZero.frameRate, PROOF_C5_SOURCE_FPS);
			if (startFraction * levelZero.frameRate < demanded) break;
			check.applies('R10 c5');
			if (result.steps + result.switches > 0) check.fail('R10 source-capped content stepped');
			const reports = result.sourceReports;
			if (reports.length === 0) check.fail('R10 no source report');
			for (const report of reports) {
				const limited = report.frameRate > PROOF_C5_SOURCE_FPS / PROOF_SHORT_RATIO;
				if (
					limited &&
					(report.sourceFrameRate === null || Math.abs(report.sourceFrameRate - PROOF_C5_SOURCE_FPS) > 1.5)
				) {
					check.fail(`R10 source rate ${report.sourceFrameRate} at ${report.frameRate}`);
					break;
				}
				if (!limited && report.sourceFrameRate !== null) {
					check.fail(`R10 source rate reported at ${report.frameRate}`);
					break;
				}
			}
			break;
		}
		case 'c6': {
			check.applies('R10 c6');
			const reapplies = result.events.filter((event) => event.kind === 'reapply-level');
			if (reapplies.length > result.publications) check.fail(`R10 ${reapplies.length} reapplies`);
			if (reapplies.some((event) => event.tick < PROOF_C6_SHRINK_FROM_TICK)) {
				check.fail('R10 native capture read as a resolution shortfall');
			}
			break;
		}
		case 'c7':
			check.applies('R13');
			if (result.steps + result.probes + result.switches > 0) check.fail('R13 acted after an apply failure');
			if (result.applyFailedRaises !== result.publications) {
				check.fail(`R13 raised ${result.applyFailedRaises} times in ${result.publications} publications`);
			}
			if (!result.stopped && result.finalNotice !== 'apply-failed') check.fail('R13 apply-failed notice lost');
			break;
		case 'c8':
		case 'c9': {
			if (!profile.mediaSource) {
				check.applies('R1 stall without source stats');
				if (result.recoveries > 0 || result.stopped) check.fail('R1 acted on a stall it cannot observe');
				if (healthy && hasWindowNotice(result)) check.fail('R1 unobservable stall set a delivery notice');
				break;
			}
			if (condition.id === 'c9') {
				check.applies('R1 c9');
				if (!result.stopped) check.fail('R1 every codec stalls but the share kept running');
				const nonVp8 = new Set(result.startOrder.filter((codec) => codec !== 'vp8'));
				if (result.recoveries > nonVp8.size + 1) check.fail(`R1 ${result.recoveries} recoveries`);
				break;
			}
			if (condition.stallFromTick === null) break;
			const stallFrom = condition.stallFromTick;
			const firstEnd = result.events.find((event) => PUBLICATION_ENDS.has(event.kind));
			if (firstEnd !== undefined && firstEnd.tick < stallFrom) break;
			check.applies('R1 c8');
			const latest = stallFrom + 1 + PROOF_RAMP_STALL_TICKS;
			if (firstEnd === undefined || firstEnd.tick > latest) {
				check.fail(`R1 first codec stall ended its publication at ${firstEnd?.tick}`);
			}
			break;
		}
	}
}

export function runProofRuntimeStage(profileIds: ReadonlyArray<ProofProfileId>): ProofStageOutcome {
	const outcome = createProofStageOutcome();
	const keys = buildProofRuntimeKeys();
	const profiles = getProofProfiles(profileIds);
	for (const [keyIndex, key] of keys.entries()) {
		for (const profile of profiles) {
			const header = buildProofMemoryHeader(profile, 0);
			for (const [conditionIndex, condition] of PROOF_CONDITIONS.entries()) {
				const seed = proofSeed(keyIndex, PROOF_PROFILES.indexOf(profile), conditionIndex);
				const byPolicy = new Map<ProofCodecPolicy['id'], ProofShareResult>();
				for (const policy of PROOF_CODEC_POLICIES) {
					const result = runProofShare({
						key,
						keyIndex,
						profile,
						condition,
						policy,
						script: PROOF_V1_SCRIPT,
						tickCount: PROOF_SHARE_TICKS,
						seed,
						memory: null,
						header,
						now: PROOF_EPOCH_MS,
						failed: new Set(),
						failCurrentCodec: false,
						adaptive: true,
					});
					checkRuntimeTrace(outcome, key, profile, condition, policy, result);
					byPolicy.set(policy.id, result);
				}
				const auto = byPolicy.get('auto-on');
				const pinned = byPolicy.get('pinned');
				if (!auto || !pinned || auto.startCodec !== pinned.startCodec) continue;
				const autoSwitch = auto.events.find((event) => event.kind === 'switch-codec');
				if (!autoSwitch) continue;
				const check = createProofCheck(outcome, `${key.id}/${profile.id}/${condition.id}/pinned`);
				check.applies('R7');
				if (!(pinned.noticeTicks.get('pinned-codec-short') ?? []).includes(autoSwitch.tick)) {
					check.fail(`R7 no pinned notice at tick ${autoSwitch.tick}`);
				}
			}
			for (const [extraIndex, extra] of PROOF_EXTRA_RUNTIME_TRACES.entries()) {
				const result = runProofShare({
					key,
					keyIndex,
					profile,
					condition: extra.condition,
					policy: PROOF_CODEC_POLICIES[1],
					script: PROOF_V1_SCRIPT,
					tickCount: PROOF_SHARE_TICKS,
					seed: proofSeed(keyIndex, PROOF_PROFILES.indexOf(profile), 50 + extraIndex),
					memory: null,
					header,
					now: PROOF_EPOCH_MS,
					failed: new Set(),
					failCurrentCodec: extra.failCurrentCodec,
					adaptive: true,
				});
				recordProofFindings(outcome, result);
			}
		}
	}
	return outcome;
}

export const PROOF_CONTROL_PROFILE_IDS: ReadonlyArray<ProofProfileId> = ['P2', 'P6'];
export const PROOF_CONTROL_TRACES =
	PROOF_RUNTIME_KEY_COUNT * PROOF_CONTROL_PROFILE_IDS.length * PROOF_CONDITIONS.length;
const PROOF_ARM_INDEPENDENT_EVENTS: ReadonlySet<string> = new Set(['recover-stalled', 'stop-stalled', 'republish']);

function firstProofArmActionTick(result: ProofShareResult): number {
	for (const event of result.events) {
		if (!PROOF_ARM_INDEPENDENT_EVENTS.has(event.kind)) return event.tick;
	}
	return Number.POSITIVE_INFINITY;
}

function describeProofEventsBefore(result: ProofShareResult, limit: number): string {
	return result.events
		.filter((event) => event.tick < limit)
		.map((event) => `${event.tick}:${event.kind}:${event.codec}`)
		.join('|');
}

export function runProofControlStage(): ProofStageOutcome {
	const outcome = createProofStageOutcome();
	const keys = buildProofRuntimeKeys();
	const profiles = getProofProfiles(PROOF_CONTROL_PROFILE_IDS);
	const policy = PROOF_CODEC_POLICIES[1];
	let treatmentActions = 0;
	let controlStalls = 0;
	for (const [keyIndex, key] of keys.entries()) {
		for (const profile of profiles) {
			const header = buildProofMemoryHeader(profile, 0);
			for (const [conditionIndex, condition] of PROOF_CONDITIONS.entries()) {
				const seed = proofSeed(keyIndex, PROOF_PROFILES.indexOf(profile), conditionIndex);
				const share = (adaptive: boolean) =>
					runProofShare({
						key,
						keyIndex,
						profile,
						condition,
						policy,
						script: PROOF_V1_SCRIPT,
						tickCount: PROOF_SHARE_TICKS,
						seed,
						memory: null,
						header,
						now: PROOF_EPOCH_MS,
						failed: new Set(),
						failCurrentCodec: false,
						adaptive,
					});
				const control = share(false);
				const treatment = share(true);
				recordProofTrace(outcome, control);
				const check = createProofCheck(outcome, `${key.id}/${profile.id}/${condition.id}/control`);
				check.applies('X control never adapts');
				if (control.steps + control.probes + control.failedProbes + control.switches + control.notices > 0) {
					check.fail(
						`X control ran ${control.steps} steps, ${control.probes} probes, ${control.switches} switches and ${control.notices} notices`,
					);
				}
				if (control.memoryWrites.length > 0) check.fail(`X control wrote ${control.memoryWrites.join(',')}`);
				if (control.layerings.length > 0) check.fail(`X control changed layering ${control.layerings.length} times`);
				if (control.finalLevelIndex !== 0) check.fail(`X control settled at level ${control.finalLevelIndex}`);
				if (control.finalNotice !== null) check.fail(`X control ended showing ${control.finalNotice}`);
				check.applies('X arms run the same events up to the tick treatment first acts');
				const divergence = firstProofArmActionTick(treatment);
				const controlPrefix = describeProofEventsBefore(control, divergence);
				const treatmentPrefix = describeProofEventsBefore(treatment, divergence);
				if (controlPrefix !== treatmentPrefix) {
					check.fail(`X control ran ${controlPrefix}, treatment ran ${treatmentPrefix}`);
				}
				treatmentActions += treatment.steps + treatment.probes + treatment.switches + treatment.notices;
				controlStalls += control.recoveries + (control.stopped ? 1 : 0);
			}
		}
	}
	const check = createProofCheck(outcome, 'control arm');
	check.applies('X treatment acts where control holds');
	if (treatmentActions === 0) check.fail('X treatment never acted, so every control pin is vacuous');
	check.applies('X control keeps stall recovery');
	if (controlStalls === 0) check.fail('X control never recovered or stopped a stalled share');
	return outcome;
}

export const PROOF_VIEWER_TRACES = 36_720;
export const PROOF_MEMORY_SEQUENCES = 8_160;
export const PROOF_MEMORY_SHARES = 3;
const PROOF_HEADER_CHANGE_EVERY = 4;
const SHARE_SPACING_MS = PROOF_SHARE_TICKS * PROOF_TICK_MS + PROOF_FIRST_TICK_MS;
const AUTO_POLICIES = PROOF_CODEC_POLICIES.filter((policy) => !policy.pinned);

function isHealthyAtTarget(profile: ProofProfile, codec: VideoCodec, level: ScreenShareLevel): boolean {
	return (
		!profile.stallingCodecs.includes(codec) &&
		profile.fraction(codec, level.width * level.height, level.frameRate) >= PROOF_HEALTHY_FRACTION
	);
}

function isShortAtTarget(profile: ProofProfile, codec: VideoCodec, level: ScreenShareLevel): boolean {
	return (
		!profile.stallingCodecs.includes(codec) &&
		profile.fraction(codec, level.width * level.height, level.frameRate) <= PROOF_SHORT_FRACTION
	);
}

function checkViewerTrace(
	outcome: ProofStageOutcome,
	key: ProofRuntimeKey,
	profile: ProofProfile,
	script: ProofViewerScript,
	result: ProofShareResult,
): void {
	recordProofTrace(outcome, result);
	const check = createProofCheck(outcome, `${key.id}/${profile.id}/${script.id}`);
	check.applies('R14 decode');
	if (result.decodeViolations > 0)
		check.fail(`R14 ${result.decodeViolations} ticks on a codec an advertised viewer lacks`);
	if (result.silentIncompatibleTicks > 0) {
		check.fail(`R14 ${result.silentIncompatibleTicks} ticks outside h264, vp9 and vp8 with a silent viewer`);
	}
	if (result.silentIncompatibleAlternatives > 0) {
		check.fail(`R14 ${result.silentIncompatibleAlternatives} fallbacks outside h264, vp9 and vp8 with a silent viewer`);
	}
	const republishes = result.events.filter((event) => event.kind === 'republish');
	switch (script.id) {
		case 'F1':
		case 'F7':
			if (republishes.length > 0) check.fail('R14 republish without a decode change');
			if (isHealthyAtTarget(profile, result.startCodec, key.levels[0])) {
				check.applies('R16 viewers');
				if (result.steps + result.switches + result.probes + result.notices > 0) check.fail('R16 healthy target acted');
			}
			break;
		case 'F4':
			check.applies('R14 F4');
			for (const event of republishes) {
				check.applies('R14 F4 republish');
				if (event.tick !== PROOF_F4_ADVERTISE_TICK) check.fail(`R14 F4 republished at ${event.tick}`);
				if (PROOF_F4_DECODE.includes(event.previousCodec)) {
					check.fail(`R14 F4 republished away from ${event.previousCodec}, which the joiner decodes`);
				}
			}
			if (republishes.length === 0 && result.ticksRun >= PROOF_F4_ADVERTISE_TICK) {
				const published = result.events.filter(
					(event) => event.tick < PROOF_F4_ADVERTISE_TICK && PUBLICATION_ENDS.has(event.kind),
				);
				const current = published.length > 0 ? published[published.length - 1].codec : result.startCodec;
				if (result.endCodec === current && !PROOF_F4_DECODE.includes(current) && !result.stopped) {
					check.fail(`R14 F4 kept ${current} for a joiner that cannot decode it`);
				}
			}
			break;
		case 'F5': {
			check.applies('R14 F5');
			const joinedBefore = proofTickTime(PROOF_F4_JOIN_TICK) - PROOF_F5_JOINED_AT;
			const graceTick = PROOF_F4_JOIN_TICK + Math.ceil((PROOF_ADVERTISEMENT_GRACE_MS - joinedBefore) / PROOF_TICK_MS);
			for (const event of republishes) {
				check.applies('R14 F5 republish');
				if (event.tick !== graceTick) check.fail(`R14 F5 republished at ${event.tick}, grace ends at ${graceTick}`);
				if (PROOF_COMPATIBLE_CODECS.includes(event.previousCodec)) check.fail('R14 F5 left a compatible codec');
			}
			break;
		}
		case 'F8':
			check.applies('R14 F8');
			if (result.pausedWindowTicks > 0) check.fail(`R14 F8 counted ${result.pausedWindowTicks} paused ticks`);
			if (result.pausedDecisions > 0) check.fail('R14 F8 acted while paused');
			if (!result.stopped && result.ticksRun >= PROOF_F8_RESUME_TICK && !result.resumedWarmUp) {
				check.fail('R14 F8 skipped warm-up on resume');
			}
			break;
		case 'F9':
			check.applies('R14 F9');
			if (result.startCodec !== 'vp8') check.fail(`R14 F9 started on ${result.startCodec}`);
			break;
	}
}

export function runProofViewerStage(): ProofStageOutcome {
	const outcome = createProofStageOutcome();
	const keys = buildProofRuntimeKeys();
	const policy = PROOF_CODEC_POLICIES[1];
	for (const [keyIndex, key] of keys.entries()) {
		for (const [profileIndex, profile] of PROOF_PROFILES.entries()) {
			const header = buildProofMemoryHeader(profile, 0);
			for (const [scriptIndex, script] of PROOF_VIEWER_SCRIPTS.entries()) {
				const result = runProofShare({
					key,
					keyIndex,
					profile,
					condition: CLEAN_CONDITION,
					policy,
					script,
					tickCount: PROOF_VIEWER_SHARE_TICKS,
					seed: proofSeed(keyIndex, profileIndex, 100 + scriptIndex),
					memory: null,
					header,
					now: PROOF_EPOCH_MS,
					failed: new Set(),
					failCurrentCodec: false,
					adaptive: true,
				});
				checkViewerTrace(outcome, key, profile, script, result);
			}
		}
	}
	return outcome;
}

export const PROOF_LAYERING_PROFILE_IDS: ReadonlyArray<ProofProfileId> = ['P2', 'P3', 'P4', 'P6', 'P11'];
export const PROOF_LAYERING_TRACES =
	PROOF_RUNTIME_KEY_COUNT *
	PROOF_LAYERING_PROFILE_IDS.length *
	PROOF_LAYERING_CONDITIONS.length *
	PROOF_LAYERING_SCRIPTS.length;

function checkLayeringTrace(
	outcome: ProofStageOutcome,
	key: ProofRuntimeKey,
	profile: ProofProfile,
	script: ProofViewerScript,
	result: ProofShareResult,
): void {
	recordProofTrace(outcome, result);
	const check = createProofCheck(outcome, `${key.id}/${profile.id}/${script.id}`);
	const levelZero = key.levels[0];
	const admits = (codec: VideoCodec) =>
		codec === 'av1' || (codec === 'h264' && levelZero.width * levelZero.height <= proofRungPixels('medium'));
	const available = admits(result.startCodec);
	check.applies(available ? `R17 ${result.startCodec} carries layers` : `R17 ${result.startCodec} stays single`);
	if (result.layerings.length > PROOF_MAX_LAYERING_CHANGES) {
		check.fail(`R17 ${result.layerings.length} layering changes`);
	}
	for (const [index, change] of result.layerings.entries()) {
		if (change.multiLayer !== (index % 2 === 0)) check.fail(`R17 change ${index} went to ${change.multiLayer}`);
		if (change.multiLayer && !admits(change.codec)) check.fail(`R17 crossed on ${change.codec}`);
	}
	if (result.multiLayerTicks > 0 && result.layerings.length === 0)
		check.fail('R17 multi-layer ticks without a crossing');
	if (result.stopped || result.switches > 0 || result.republishes > 0) return;
	if (!available) {
		if (result.layerings.length > 0) check.fail('R17 crossed on a codec that cannot carry layers');
		return;
	}
	if (result.steps > 0 || result.finalLevelIndex !== 0) return;
	if (!isHealthyAtTarget(profile, result.startCodec, levelZero)) return;
	const crossings = result.layerings.map((change) => change.multiLayer).join(' ');
	switch (script.id) {
		case 'L1':
			check.applies('R17 one viewer stays single');
			if (crossings !== '') check.fail(`R17 one viewer crossed ${crossings}`);
			break;
		case 'L2':
			check.applies('R17 a second viewer turns it on');
			if (crossings !== 'true') check.fail(`R17 two viewers crossed ${crossings}`);
			if (result.multiLayerTicks === 0) check.fail('R17 no multi-layer ticks after the crossing');
			break;
		case 'L3': {
			check.applies('R17 the last viewer leaving turns it off');
			if (crossings !== 'true false') check.fail(`R17 a leaving viewer crossed ${crossings}`);
			const off = result.layerings[1];
			if (off !== undefined && off.tick < PROOF_LAYERING_LEAVE_TICK + PROOF_MULTI_LAYER_OFF_TICKS - 1) {
				check.fail(`R17 turned off at ${off.tick}`);
			}
			break;
		}
		case 'L4':
			check.applies('R17 a flapping count does not cross');
			if (crossings !== '') check.fail(`R17 a flapping count crossed ${crossings}`);
			break;
	}
}

export function runProofLayeringStage(): ProofStageOutcome {
	const outcome = createProofStageOutcome();
	const keys = buildProofRuntimeKeys();
	const profiles = getProofProfiles(PROOF_LAYERING_PROFILE_IDS);
	const policy = PROOF_CODEC_POLICIES[1];
	for (const [keyIndex, key] of keys.entries()) {
		for (const profile of profiles) {
			const header = buildProofMemoryHeader(profile, 0);
			for (const [conditionIndex, condition] of PROOF_LAYERING_CONDITIONS.entries()) {
				for (const [scriptIndex, script] of PROOF_LAYERING_SCRIPTS.entries()) {
					const result = runProofShare({
						key,
						keyIndex,
						profile,
						condition,
						policy,
						script,
						tickCount: PROOF_SHARE_TICKS,
						seed: proofSeed(keyIndex, PROOF_PROFILES.indexOf(profile), 200 + conditionIndex * 10 + scriptIndex),
						memory: null,
						header,
						now: PROOF_EPOCH_MS,
						failed: new Set(),
						failCurrentCodec: false,
						adaptive: true,
					});
					checkLayeringTrace(outcome, key, profile, script, result);
				}
			}
		}
	}
	return outcome;
}

function lastSettledIndex(shares: ReadonlyArray<ProofShareResult>, codec: VideoCodec): number | null {
	let settled: number | null = null;
	for (const share of shares) {
		for (const write of share.memoryWrites) {
			const [kind, writeCodec, index] = write.split(':');
			if (kind === 'settled' && writeCodec === codec) settled = Number(index);
		}
	}
	return settled;
}

function sameShare(left: ProofShareResult, right: ProofShareResult): boolean {
	const describe = (share: ProofShareResult) =>
		JSON.stringify([
			share.startCodec,
			share.startLevelIndex,
			share.events.map((event) => `${event.tick}:${event.kind}:${event.to}:${event.codec}`),
			share.memoryWrites,
		]);
	return describe(left) === describe(right);
}

function checkMemorySequence(
	outcome: ProofStageOutcome,
	key: ProofRuntimeKey,
	profile: ProofProfile,
	policy: ProofCodecPolicy,
	shares: ReadonlyArray<ProofShareResult>,
	headerChanged: boolean,
): void {
	const check = createProofCheck(outcome, `${key.id}/${profile.id}/${policy.id}`);
	const levelZero = key.levels[0];
	const order = shares[0].memoryFreeOrder;
	for (const [index, share] of shares.entries()) {
		recordProofTrace(outcome, share);
		if (share.switches > PROOF_MAX_DELIVERY_SWITCHES) check.fail(`R15 share ${index + 1} switched twice`);
		const history = headerChanged && index === 2 ? [] : shares.slice(0, index);
		const expectedStart = lastSettledIndex(history, share.startCodec) ?? 0;
		if (share.startLevelIndex !== Math.min(expectedStart, key.levels.length - 1)) {
			check.fail(`R15 share ${index + 1} started at ${share.startLevelIndex}, memory says ${expectedStart}`);
		}
		if (share.startLevelIndex > 0) check.applies('R15 starts at a settled level');
		const previous = history[history.length - 1];
		if (previous?.memoryWrites.some((write) => write.startsWith(`probe-blocked:${share.startCodec}:`))) {
			check.applies('R15 probe block');
			if (!share.startProbeBlocked || share.probes > 0) check.fail(`R15 share ${index + 1} probed a blocked level`);
		}
		const verdicts = share.startVerdicts;
		const startVerdict = verdicts[share.startCodec];
		if (startVerdict?.short) {
			check.applies('R15 best recorded ratio');
			const unshortened = share.startOrder.some((codec) => verdicts[codec]?.short !== true);
			if (unshortened) check.fail(`R15 share ${index + 1} started on a short codec while another was not short`);
			for (const codec of share.startOrder) {
				if ((verdicts[codec]?.ratio ?? 0) > startVerdict.ratio) {
					check.fail(`R15 share ${index + 1} skipped ${codec} with a better ratio`);
				}
			}
		}
	}
	if (headerChanged) {
		check.applies('R15 header change');
		if (!sameShare(shares[0], shares[2])) check.fail('R15 header change did not restore share 1 behaviour');
	}
	if (order.length < 2 || profile.stallingCodecs.length > 0) return;
	if (isHealthyAtTarget(profile, order[0], levelZero)) {
		check.applies('R15 first codec holds');
		for (const [index, share] of shares.entries()) {
			if (share.startCodec !== order[0] || share.startLevelIndex !== 0 || share.switches > 0) {
				check.fail(`R15 share ${index + 1} left the healthy first codec`);
			}
		}
	} else if (isShortAtTarget(profile, order[0], levelZero) && isHealthyAtTarget(profile, order[1], levelZero)) {
		check.applies('R15 converges on the second codec');
		if (shares[0].switches !== 1 || shares[0].endCodec !== order[1]) check.fail('R15 share 1 did not switch once');
		for (const share of shares.slice(1, headerChanged ? 2 : PROOF_MEMORY_SHARES)) {
			if (share.startCodec !== order[1] || share.startLevelIndex !== 0 || share.switches > 0) {
				check.fail(`R15 later share started on ${share.startCodec} at ${share.startLevelIndex}`);
			}
		}
	}
}

export function runProofMemoryStage(): ProofStageOutcome {
	const outcome = createProofStageOutcome();
	const keys = buildProofRuntimeKeys();
	let sequence = 0;
	for (const [profileIndex, profile] of PROOF_PROFILES.entries()) {
		const header = buildProofMemoryHeader(profile, 0);
		const changedHeader = buildProofMemoryHeader(profile, 1);
		for (const policy of AUTO_POLICIES) {
			let longLived: ScreenShareDeliveryMemory | null = null;
			for (const [keyIndex, key] of keys.entries()) {
				const headerChanged = sequence % PROOF_HEADER_CHANGE_EVERY === PROOF_HEADER_CHANGE_EVERY - 1;
				sequence += 1;
				const failed = new Set<VideoCodec>();
				const seed = proofSeed(keyIndex, profileIndex, 200);
				const shares: Array<ProofShareResult> = [];
				let memory: ScreenShareDeliveryMemory | null = null;
				for (let share = 0; share < PROOF_MEMORY_SHARES; share++) {
					const result = runProofShare({
						key,
						keyIndex,
						profile,
						condition: CLEAN_CONDITION,
						policy,
						script: PROOF_V1_SCRIPT,
						tickCount: PROOF_SHARE_TICKS,
						seed,
						memory,
						header: headerChanged && share === 2 ? changedHeader : header,
						now: PROOF_EPOCH_MS + share * SHARE_SPACING_MS,
						failed: headerChanged && share === 2 ? new Set() : failed,
						failCurrentCodec: false,
						adaptive: true,
					});
					shares.push(result);
					memory = result.memory;
				}
				checkMemorySequence(outcome, key, profile, policy, shares, headerChanged);
				const check = createProofCheck(outcome, `${key.id}/${profile.id}/${policy.id} long-lived memory`);
				longLived = replayProofMemory(check, longLived, key, profile, header, shares[0], keyIndex);
				check.applies('R15 memory cap');
				const size = Object.keys(longLived.entries).length;
				if (size > PROOF_MAX_MEMORY_ENTRIES) check.fail(`R15 memory holds ${size} entries`);
				if (size === PROOF_MAX_MEMORY_ENTRIES) check.applies('R15 memory full');
				checkFullQualityReset(check, longLived, key, profile, header, keyIndex);
				checkProofMemoryExpiry(check, longLived, key, profile, header, keyIndex);
			}
		}
	}
	return outcome;
}

function checkFullQualityReset(
	check: ReturnType<typeof createProofCheck>,
	memory: ScreenShareDeliveryMemory,
	key: ProofRuntimeKey,
	profile: ProofProfile,
	header: ScreenShareDeliveryMemoryHeader,
	keyIndex: number,
): void {
	const hardware = buildProofHardwareMap(profile);
	const memoryKey = {header, memoryKey: key.target.memoryKey, hardware, now: PROOF_EPOCH_MS + keyIndex * 60_000};
	const cleared = clearScreenShareDeliveryMemoryKey(memory, memoryKey);
	const suffix = `|${key.target.memoryKey}`;
	const kept = Object.keys(memory.entries).filter((entry) => !entry.endsWith(suffix));
	check.applies('K full quality reset');
	if (Object.keys(readScreenShareDeliveryMemory(cleared, memoryKey)).length > 0) check.fail('K reset kept a verdict');
	if (Object.keys(cleared.entries).sort().join() !== kept.sort().join()) check.fail('K reset touched another key');
}

function checkProofMemoryExpiry(
	check: ReturnType<typeof createProofCheck>,
	memory: ScreenShareDeliveryMemory,
	key: ProofRuntimeKey,
	profile: ProofProfile,
	header: ScreenShareDeliveryMemoryHeader,
	keyIndex: number,
): void {
	const hardware = buildProofHardwareMap(profile);
	const written = PROOF_EPOCH_MS + keyIndex * 60_000;
	const keyAt = (now: number, memoryKey = key.target.memoryKey) => ({header, memoryKey, hardware, now});
	const countWritten = (stored: ScreenShareDeliveryMemory, now: number) =>
		Object.values(readScreenShareDeliveryMemory(stored, keyAt(now))).filter((entry) => entry.at === written).length;
	const live = countWritten(memory, written);
	if (live === 0) return;
	check.applies('K memory expiry');
	const lastDay = written + PROOF_MEMORY_TTL_MS;
	const expired = lastDay + 1;
	if (countWritten(memory, lastDay) !== live) check.fail('K entry dropped before 14 days');
	if (Object.keys(readScreenShareDeliveryMemory(memory, keyAt(expired))).length > 0) {
		check.fail('K entry kept past 14 days');
	}
	const otherKey = `${key.target.memoryKey}|other`;
	const kept = updateScreenShareDeliveryMemory(memory, keyAt(lastDay, otherKey), 'vp8', {kind: 'probe-blocked'});
	if (countWritten(kept, lastDay) !== live) check.fail('K write dropped a live entry');
	const pruned = updateScreenShareDeliveryMemory(memory, keyAt(expired, otherKey), 'vp8', {kind: 'probe-blocked'});
	if (Object.values(pruned.entries).some((entry) => entry.at <= written)) check.fail('K write kept an expired entry');
}

function replayProofMemory(
	check: ReturnType<typeof createProofCheck>,
	memory: ScreenShareDeliveryMemory | null,
	key: ProofRuntimeKey,
	profile: ProofProfile,
	header: ScreenShareDeliveryMemoryHeader,
	share: ProofShareResult,
	keyIndex: number,
): ScreenShareDeliveryMemory {
	const hardware = buildProofHardwareMap(profile);
	const now = PROOF_EPOCH_MS + keyIndex * 60_000;
	let next: ScreenShareDeliveryMemory = memory ?? {header, entries: {}};
	for (const write of share.memoryWrites) {
		const [kind, codec, index] = write.split(':') as [string, VideoCodec, string];
		const entryKey = {header, memoryKey: key.target.memoryKey, hardware, now};
		next = updateScreenShareDeliveryMemory(
			next,
			entryKey,
			codec,
			kind === 'settled'
				? {kind: 'settled', settledIndex: Number(index)}
				: kind === 'short'
					? {kind: 'short', ratio: 0.5}
					: {kind: 'probe-blocked'},
		);
		const written = `${codec}|${hardware[codec] ? 'hardware' : 'software'}|${key.target.memoryKey}`;
		if (next.entries[written]?.at !== now) check.fail(`R15 newest entry ${written} was evicted`);
	}
	return next;
}

type ProofCaptureRow = readonly [
	timestamp: number,
	framesEncoded: number,
	bytesSent: number,
	headerBytesSent: number,
	totalEncodeTime: number | null,
	targetBitrate: number | null,
	reason: ScreenShareQualityLimitationReason,
	sourceFrames: number,
	sourceFps: number,
	active: boolean,
];

const CPU_LIMITED_4K15: ReadonlyArray<ProofCaptureRow> = [
	[0, 2, 129186, 3108, null, 280000, 'none', 36, 15, true],
	[2006, 5, 305462, 6780, null, 320833, 'none', 66, 15, true],
	[4010, 5, 490755, 10668, null, 383333, 'none', 96, 15, true],
	[6013, 6, 622765, 13428, null, 440833, 'none', 127, 15, true],
	[8015, 7, 691179, 22202, null, 465000, 'none', 156, 15, true],
	[10017, 8, 780200, 28295, null, 490833, 'none', 186, 15, true],
	[12019, 9, 872977, 34484, null, 503333, 'none', 217, 16, true],
	[14022, 10, 1047119, 42082, null, 3325000, 'none', 246, 14, true],
	[16025, 22, 1546390, 52789, null, 3729167, 'none', 276, 15, true],
	[18028, 37, 2133594, 65029, null, 3729167, 'none', 306, 14, true],
	[20031, 62, 2966887, 82760, null, 4500000, 'none', 336, 14, true],
	[22033, 92, 4154160, 107528, null, 4500000, 'none', 366, 15, true],
	[24036, 115, 5255245, 130424, null, 4426506, 'none', 396, 15, true],
	[26039, 140, 6355838, 153627, null, 4500000, 'none', 427, 15, true],
	[28042, 169, 7471522, 176907, null, 4500000, 'none', 456, 15, true],
	[30045, 200, 8636367, 201478, null, 4362962, 'none', 487, 16, true],
	[32048, 230, 9758680, 224950, null, 4500000, 'none', 517, 15, true],
	[34051, 257, 10883847, 248398, null, 4494444, 'none', 546, 14, true],
	[36054, 279, 12064581, 273233, null, 4375770, 'none', 577, 16, true],
	[38057, 303, 13113230, 295121, null, 4500000, 'none', 607, 15, true],
	[40060, 332, 14230269, 318684, null, 4500000, 'none', 636, 14, true],
	[42063, 363, 15381555, 342732, null, 4500000, 'none', 667, 15, true],
	[44066, 393, 16494637, 365988, null, 4424964, 'none', 697, 15, true],
	[46068, 422, 17686730, 391111, null, 4432499, 'none', 727, 15, true],
	[48070, 446, 18792500, 414175, null, 4440221, 'cpu', 756, 15, true],
	[50073, 461, 19497511, 429106, null, 4500000, 'cpu', 774, 10, true],
	[52076, 479, 20539912, 450730, null, 4500000, 'cpu', 791, 9, true],
	[54078, 497, 21505872, 470794, null, 4500000, 'cpu', 810, 10, true],
	[56080, 515, 22437606, 490445, null, 4500000, 'cpu', 828, 10, true],
	[58083, 533, 23548278, 513461, null, 4500000, 'cpu', 846, 9, true],
	[60086, 551, 24680311, 537240, null, 4382082, 'cpu', 864, 10, true],
];

const CAPPED_2MBPS: ReadonlyArray<ProofCaptureRow> = [
	[0, 75, 520539, 16656, null, 1440405, 'none', 75, 29, true],
	[2006, 135, 895464, 27960, null, 1760559, 'none', 135, 30, true],
	[4009, 196, 1333290, 41016, null, 1902432, 'none', 196, 31, true],
	[6012, 255, 1796235, 54744, null, 1851931, 'none', 255, 31, true],
	[8015, 316, 2259123, 68504, null, 1831941, 'none', 316, 31, true],
	[10018, 376, 2726645, 82200, null, 1860277, 'none', 376, 30, true],
	[12023, 436, 3183661, 95736, null, 1840059, 'none', 436, 30, true],
	[14025, 496, 3647196, 109528, null, 1841855, 'none', 496, 30, true],
	[16029, 556, 4114312, 123352, null, 1835270, 'none', 556, 30, true],
	[18031, 616, 4584658, 137272, null, 1810131, 'none', 616, 30, true],
	[20040, 677, 5036017, 150616, null, 1864276, 'none', 677, 31, true],
	[22043, 736, 5495991, 164312, null, 1862810, 'none', 736, 30, true],
	[24048, 797, 5965337, 178200, null, 1844453, 'none', 797, 31, true],
	[26056, 857, 6427879, 192024, null, 1819481, 'none', 857, 30, true],
	[28059, 916, 6887400, 205528, null, 1854871, 'none', 916, 28, true],
	[30062, 977, 7360423, 219576, null, 1834706, 'none', 977, 30, true],
	[32064, 1037, 7814344, 232984, null, 1868472, 'none', 1037, 29, true],
	[34067, 1097, 8285150, 246904, null, 1869645, 'none', 1098, 31, true],
	[36070, 1157, 8744546, 260472, null, 1853878, 'none', 1157, 30, true],
	[38073, 1217, 9213407, 274456, null, 1851639, 'none', 1217, 29, true],
	[40075, 1278, 9692179, 288728, null, 1834499, 'none', 1278, 31, true],
	[42077, 1337, 10146432, 302168, null, 1804368, 'none', 1337, 29, true],
	[44079, 1398, 10602879, 315736, null, 1858826, 'none', 1398, 30, true],
	[46083, 1457, 11061265, 329336, null, 1864706, 'none', 1458, 30, true],
	[48087, 1518, 11531502, 343320, null, 1859807, 'none', 1518, 29, true],
	[50090, 1578, 11995203, 357112, null, 1847778, 'none', 1578, 30, true],
	[52092, 1638, 12469008, 371160, null, 1850641, 'none', 1638, 30, true],
	[54095, 1698, 12930416, 384856, null, 1803644, 'none', 1698, 29, true],
	[56098, 1758, 13379246, 398200, null, 1848624, 'none', 1758, 30, true],
	[58100, 1818, 13851843, 412248, null, 1863988, 'none', 1818, 29, true],
	[60103, 1878, 14314049, 426072, null, 1860922, 'none', 1879, 31, true],
];

const CAPPED_600KBPS: ReadonlyArray<ProofCaptureRow> = [
	[0, 22, 187463, 5832, null, null, 'none', 75, 29, true],
	[2012, 37, 309915, 9488, null, 482456, 'none', 136, 31, true],
	[4014, 57, 432214, 13072, null, 493897, 'none', 196, 30, true],
	[6015, 69, 557528, 16752, null, 490255, 'none', 256, 30, true],
	[8019, 83, 677185, 20272, null, 486148, 'none', 316, 30, true],
	[10022, 98, 799063, 23824, null, 499326, 'none', 376, 29, true],
	[12024, 115, 920931, 27472, null, 483544, 'none', 437, 31, true],
	[14027, 136, 1046172, 31248, null, 496048, 'none', 496, 29, true],
	[16031, 156, 1170697, 35056, null, 481940, 'none', 557, 30, true],
	[18034, 168, 1291106, 38544, null, 500875, 'none', 616, 30, true],
	[20037, 182, 1413258, 42096, null, 496599, 'none', 677, 30, true],
	[22040, 197, 1536196, 45744, null, 465660, 'none', 737, 31, true],
	[24042, 214, 1654048, 49168, null, 486745, 'none', 797, 31, true],
	[26044, 234, 1779687, 52912, null, 502729, 'none', 857, 30, true],
	[28048, 253, 1903382, 56592, null, 499520, 'none', 917, 31, true],
	[30051, 266, 2029698, 60336, null, 492012, 'none', 977, 30, true],
	[32058, 281, 2155497, 64048, null, 513598, 'none', 1038, 31, true],
	[34060, 296, 2282229, 67728, null, 493685, 'none', 1097, 30, true],
	[36064, 314, 2407379, 71408, null, 486061, 'none', 1158, 30, true],
	[38066, 334, 2531809, 75152, null, 500051, 'none', 1218, 31, true],
	[40070, 353, 2658680, 78928, null, 503117, 'none', 1278, 30, true],
	[42072, 365, 2782761, 82544, null, 488727, 'none', 1338, 31, true],
	[44076, 379, 2899598, 86032, null, 468875, 'none', 1398, 31, true],
	[46079, 393, 3017663, 89552, null, 481662, 'none', 1458, 30, true],
	[48083, 411, 3142203, 93232, null, 486593, 'none', 1518, 31, true],
	[50085, 430, 3262178, 96816, null, 493975, 'none', 1578, 29, true],
	[52088, 448, 3389974, 100624, null, 502622, 'none', 1638, 30, true],
	[54090, 460, 3512625, 104176, null, 489124, 'none', 1698, 30, true],
	[56093, 475, 3635458, 107824, null, 485257, 'none', 1758, 30, true],
	[58096, 490, 3762192, 111632, null, 499227, 'none', 1818, 30, true],
	[60100, 509, 3888936, 115472, null, 505964, 'none', 1878, 30, true],
];

const VIEWERLESS: ReadonlyArray<ProofCaptureRow> = [
	[0, 67, 510637, 15924, null, 2046739, 'none', 73, 30, true],
	[2005, 120, 1020845, 30868, null, 2724124, 'none', 133, 29, true],
	[4008, 180, 1727377, 51412, null, 3000000, 'none', 194, 31, true],
	[6011, 237, 2429327, 72079, null, 3000000, 'none', 253, 29, true],
	[8013, 282, 2997753, 88495, null, 3000000, 'none', 315, 31, false],
	[10017, 282, 2997753, 88495, null, 3000000, 'none', 375, 30, false],
	[12020, 282, 3006793, 89066, null, 3000000, 'none', 437, 31, false],
	[14024, 282, 3006793, 89066, null, 3000000, 'none', 497, 30, false],
	[16026, 282, 3006793, 89066, null, 3000000, 'none', 558, 30, false],
	[18030, 282, 3006793, 89066, null, 3000000, 'none', 618, 29, false],
	[20033, 282, 3006793, 89066, null, 3000000, 'none', 679, 30, false],
	[22036, 282, 3006793, 89066, null, 3000000, 'none', 740, 30, false],
	[24039, 282, 3006793, 89066, null, 3000000, 'none', 801, 31, false],
	[26041, 282, 3006793, 89066, null, 3000000, 'none', 861, 30, false],
	[28044, 282, 3006793, 89066, null, 3000000, 'none', 922, 30, false],
	[30046, 282, 3006793, 89066, null, 3000000, 'none', 983, 31, false],
	[32049, 282, 3006793, 89066, null, 3000000, 'none', 1043, 30, false],
	[34052, 282, 3006793, 89066, null, 3000000, 'none', 1104, 31, false],
	[36056, 282, 3006793, 89066, null, 3000000, 'none', 1165, 30, false],
	[38059, 282, 3006793, 89066, null, 3000000, 'none', 1225, 29, false],
	[40062, 282, 3006793, 89066, null, 3000000, 'none', 1286, 30, false],
	[42065, 282, 3006793, 89066, null, 3000000, 'none', 1347, 30, false],
	[44067, 282, 3006793, 89066, null, 3000000, 'none', 1407, 29, false],
	[46070, 282, 3006793, 89066, null, 3000000, 'none', 1468, 30, false],
	[48073, 282, 3006793, 89066, null, 3000000, 'none', 1529, 30, false],
	[50076, 282, 3006793, 89066, null, 3000000, 'none', 1590, 31, false],
	[52079, 282, 3006793, 89066, null, 3000000, 'none', 1650, 30, false],
	[54082, 282, 3006793, 89066, null, 3000000, 'none', 1711, 30, false],
	[56085, 282, 3006793, 89066, null, 3000000, 'none', 1772, 31, false],
	[58088, 282, 3006793, 89066, null, 3000000, 'none', 1833, 30, false],
	[60090, 282, 3006793, 89066, null, 3000000, 'none', 1893, 30, false],
];

type ProofCaptureInput = ScreenShareTargetInput & {sourceDimensions: ProofDimensions};

const CPU_AND_CAP_1080P60: ReadonlyArray<ProofCaptureRow> = [
	[0, 0, 0, 0, 0, null, 'none', 1, 0, true],
	[1506, 31, 596063, 18932, 0.766, 2832518, 'none', 91, 60, true],
	[3009, 52, 1086977, 33556, 1.286, 2954937, 'none', 181, 60, true],
	[4511, 86, 1789046, 54644, 2.115, 4634849, 'none', 271, 60, true],
	[6009, 132, 2767211, 83700, 3.228, 5429641, 'none', 361, 60, true],
	[7517, 182, 3816489, 115343, 4.433, 4947472, 'none', 451, 60, true],
	[9020, 229, 4822742, 145263, 5.556, 5567682, 'none', 542, 61, true],
	[10523, 280, 5884526, 177039, 6.792, 6000000, 'none', 631, 59, true],
	[12010, 330, 6928815, 208207, 8.005, 6000000, 'none', 720, 59, true],
	[13529, 378, 7947540, 238954, 9.186, 5700331, 'none', 812, 60, true],
	[15033, 430, 9021305, 271018, 10.443, 6000000, 'none', 902, 60, true],
	[16536, 479, 10062656, 302186, 11.642, 6000000, 'none', 993, 61, true],
	[18009, 523, 11014948, 330821, 12.718, 5014948, 'none', 1081, 60, true],
	[19544, 570, 12003339, 360325, 13.861, 5494898, 'none', 1173, 60, true],
	[21106, 622, 13088747, 392677, 15.13, 6000000, 'none', 1266, 59, true],
	[22665, 633, 13619997, 408165, 15.422, 357261, 'none', 1359, 59, true],
	[24170, 635, 13639405, 413584, 15.473, 232099, 'none', 1451, 61, true],
	[25674, 635, 13654108, 422207, 15.473, 415000, 'none', 1541, 60, true],
	[27179, 638, 13691043, 427038, 15.553, 402000, 'none', 1631, 60, true],
	[28683, 641, 13746273, 428702, 15.631, 347000, 'none', 1722, 61, true],
	[30187, 644, 13804731, 430462, 15.707, 289908, 'none', 1811, 59, true],
	[31691, 647, 13852713, 431934, 15.783, 170149, 'none', 1902, 60, true],
	[33195, 649, 13884651, 443114, 15.834, 163643, 'none', 1992, 60, true],
	[34699, 651, 13917276, 487971, 15.885, 260831, 'none', 2083, 60, true],
	[36219, 656, 14029965, 497589, 16.013, 490257, 'none', 2173, 59, true],
	[37726, 660, 14099108, 499701, 16.114, 339779, 'none', 2264, 59, true],
	[39230, 664, 14162512, 501621, 16.216, 328109, 'none', 2354, 60, true],
	[40734, 669, 14245249, 504149, 16.346, 371850, 'none', 2445, 60, true],
	[42243, 674, 14319303, 506421, 16.48, 415574, 'none', 2535, 59, true],
	[43748, 679, 14399913, 509168, 16.607, 777722, 'none', 2626, 60, true],
	[45252, 685, 14508894, 512747, 16.764, 282944, 'none', 2716, 60, true],
	[46758, 688, 14558242, 514251, 16.843, 282944, 'none', 2806, 59, true],
	[48267, 692, 14615103, 515979, 16.951, 378386, 'none', 2897, 60, true],
	[49773, 697, 14707648, 519361, 17.083, 494571, 'none', 2987, 60, true],
	[51280, 702, 14794493, 521985, 17.215, 494571, 'none', 3076, 59, true],
	[52786, 709, 14893085, 524961, 17.417, 878807, 'none', 3167, 59, true],
	[54290, 714, 15022602, 528865, 17.556, 285369, 'none', 3258, 60, true],
	[55796, 717, 15070522, 530337, 17.634, 373349, 'none', 3348, 59, true],
	[57306, 724, 15162531, 533436, 17.819, 746000, 'none', 3439, 60, true],
	[58809, 728, 15289301, 537244, 17.924, 389119, 'none', 3529, 60, true],
	[60314, 732, 15436687, 541532, 18.028, 390405, 'none', 3620, 60, true],
	[61818, 738, 15570183, 545500, 18.183, 356010, 'none', 3710, 60, true],
	[63321, 742, 15634454, 547484, 18.291, 363292, 'none', 3800, 60, true],
	[64825, 747, 15707002, 549692, 18.421, 548730, 'none', 3890, 60, true],
	[66329, 752, 15791823, 552284, 18.563, 439494, 'none', 3980, 59, true],
	[67834, 757, 15875913, 554844, 18.691, 470898, 'none', 4071, 60, true],
	[69338, 764, 15987640, 558519, 18.876, 470003, 'none', 4161, 60, true],
	[70844, 768, 16068365, 560919, 18.983, 511169, 'none', 4251, 59, true],
	[72348, 778, 16209824, 565175, 19.245, 431740, 'none', 4342, 60, true],
	[73853, 780, 16317425, 568375, 19.296, 223982, 'none', 4432, 60, true],
];

const CAPPED_500KBPS_1080P30: ReadonlyArray<ProofCaptureRow> = [
	[0, 0, 0, 0, 0, null, 'none', 0, 0, true],
	[1506, 13, 253486, 8724, 0.334, 4460000, 'none', 44, 30, true],
	[3009, 49, 1011648, 31316, 1.234, 2912552, 'none', 90, 31, true],
	[4514, 81, 1688331, 51476, 2.032, 4566185, 'none', 135, 29, true],
	[5999, 124, 2602660, 78708, 3.106, 5755099, 'none', 178, 30, true],
	[7522, 170, 3582366, 108143, 4.251, 5559512, 'none', 224, 30, true],
	[9025, 215, 4538008, 136559, 5.374, 6000000, 'none', 269, 30, true],
	[10529, 260, 5490761, 165071, 6.506, 6000000, 'none', 314, 30, true],
	[12000, 304, 6431756, 193386, 7.6, 5557406, 'none', 357, 29, true],
	[13535, 350, 7402892, 222346, 8.75, 6000000, 'none', 404, 30, true],
	[15038, 396, 8372990, 251178, 9.906, 6000000, 'none', 450, 31, true],
	[16544, 440, 9304724, 279173, 11.003, 5234692, 'none', 494, 30, true],
	[18000, 485, 10264813, 307813, 12.114, 5776250, 'none', 538, 30, true],
	[19551, 531, 11240088, 336901, 13.262, 6000000, 'none', 584, 30, true],
	[21055, 576, 12196047, 365413, 14.388, 6000000, 'none', 630, 30, true],
	[22559, 604, 13026219, 389888, 15.081, 160242, 'none', 674, 29, true],
	[24062, 605, 13161316, 393792, 15.107, 68333, 'none', 720, 30, true],
	[25565, 605, 13306645, 398048, 15.107, 85000, 'none', 765, 30, true],
	[27072, 605, 13338841, 400989, 15.107, 109167, 'none', 810, 30, true],
	[28575, 606, 13346730, 409169, 15.132, 74167, 'none', 855, 30, true],
	[30078, 606, 13359371, 414049, 15.132, 145833, 'none', 900, 30, true],
	[31581, 607, 13375243, 423019, 15.161, 139167, 'none', 946, 30, true],
	[33083, 608, 13393176, 423846, 15.187, 139167, 'none', 991, 31, true],
	[34586, 609, 13407694, 424577, 15.213, 139167, 'none', 1036, 30, true],
	[36089, 611, 13428387, 428896, 15.266, 139167, 'none', 1081, 31, true],
	[37593, 612, 13449938, 429536, 15.296, 139167, 'none', 1126, 30, true],
	[39099, 613, 13470400, 432974, 15.323, 139167, 'none', 1171, 30, true],
	[40607, 614, 13490019, 433550, 15.349, 139167, 'none', 1216, 30, true],
	[42109, 615, 13508020, 434094, 15.375, 139167, 'none', 1261, 30, true],
	[43612, 616, 13527786, 438664, 15.404, 139167, 'none', 1307, 31, true],
	[45115, 617, 13544569, 442855, 15.434, 139167, 'none', 1351, 29, true],
	[46618, 618, 13560971, 450442, 15.459, 144167, 'none', 1396, 29, true],
	[48122, 620, 13584491, 454857, 15.516, 144167, 'none', 1441, 30, true],
	[49629, 621, 13606798, 462604, 15.545, 150000, 'none', 1486, 29, true],
	[51132, 622, 13629557, 478841, 15.571, 210000, 'none', 1531, 30, true],
	[52636, 625, 13677839, 489369, 15.651, 330833, 'none', 1577, 31, true],
	[54141, 629, 13741489, 491289, 15.759, 361667, 'none', 1623, 31, true],
	[55649, 633, 13810987, 493401, 15.87, 417500, 'none', 1667, 29, true],
	[57153, 638, 13885900, 495673, 16.013, 426226, 'none', 1713, 30, true],
	[58657, 643, 13974481, 498361, 16.152, 464242, 'none', 1757, 29, true],
	[60160, 648, 14052035, 500729, 16.29, 184983, 'none', 1803, 30, true],
	[61663, 650, 14090403, 501881, 16.346, 184983, 'none', 1847, 29, true],
	[63167, 652, 14125981, 502969, 16.4, 301164, 'none', 1893, 30, true],
	[64670, 655, 14172176, 504377, 16.487, 333128, 'none', 1938, 30, true],
	[66176, 658, 14228082, 506041, 16.564, 432000, 'none', 1981, 28, true],
	[67680, 663, 14312025, 508601, 16.702, 322719, 'none', 2026, 30, true],
	[69183, 667, 14383073, 511060, 16.808, 313968, 'none', 2071, 30, true],
	[70686, 670, 14435834, 512660, 16.893, 282844, 'none', 2116, 29, true],
	[72190, 673, 14488588, 514260, 16.974, 291532, 'none', 2162, 31, true],
	[73693, 677, 14552612, 516495, 17.084, 381187, 'none', 2206, 29, true],
];

interface ProofCapture {
	id: string;
	input: ProofCaptureInput;
	pinned: boolean;
	rows: ReadonlyArray<ProofCaptureRow>;
	warmUpEndsAt: number;
	decisions: ReadonlyArray<string>;
	notices: ReadonlyArray<string>;
	memoryWrites: ReadonlyArray<string>;
	inactiveFrom: number | null;
	finalLevelIndex: number;
	finalNotice: string | null;
	holdingShort: boolean;
}

const CAPTURE_4K15_INPUT: ProofCaptureInput = {
	mode: 'screenshare',
	storedResolution: 'medium',
	storedFrameRate: 30,
	entitled: true,
	context: 'display',
	sourceDimensions: {width: 3840, height: 2160},
	hintSetting: 'auto',
};
const CAPTURE_720P_MOTION_INPUT: ProofCaptureInput = {
	mode: 'custom',
	storedResolution: 'medium',
	storedFrameRate: 30,
	entitled: true,
	context: 'display',
	sourceDimensions: {width: 1280, height: 720},
	hintSetting: 'motion',
};
const CAPTURE_1080P60_MOTION_INPUT: ProofCaptureInput = {
	mode: 'custom',
	storedResolution: 'high',
	storedFrameRate: 60,
	entitled: true,
	context: 'display',
	sourceDimensions: {width: 1920, height: 1080},
	hintSetting: 'motion',
};
const CAPTURE_1080P30_MOTION_INPUT: ProofCaptureInput = {
	mode: 'custom',
	storedResolution: 'high',
	storedFrameRate: 30,
	entitled: true,
	context: 'display',
	sourceDimensions: {width: 1920, height: 1080},
	hintSetting: 'motion',
};

export const PROOF_CAPTURES: ReadonlyArray<ProofCapture> = [
	{
		id: 'CL b2 processor load with the uplink capped at 1 Mbps',
		input: CAPTURE_1080P60_MOTION_INPUT,
		pinned: false,
		rows: CPU_AND_CAP_1080P60,
		warmUpEndsAt: 4,
		decisions: [
			'9 switch-codec vp9',
			'18 switch-codec vp9',
			'27 switch-codec vp9',
			'36 switch-codec vp9',
			'45 switch-codec vp9',
		],
		notices: ['9 codec-switched'],
		memoryWrites: ['9 short 0.556', '18 short 0.033', '27 short 0.044', '36 short 0.056', '45 short 0.056'],
		inactiveFrom: null,
		finalLevelIndex: 0,
		finalNotice: '{"kind":"codec-switched","codec":"vp9","previousCodec":"h264"}',
		holdingShort: false,
	},
	{
		id: 'CL c1 uplink capped at 500 kbps at 1080p30',
		input: CAPTURE_1080P30_MOTION_INPUT,
		pinned: false,
		rows: CAPPED_500KBPS_1080P30,
		warmUpEndsAt: 3,
		decisions: ['18 recover-stalled'],
		notices: [
			'28 connection-short',
			'37 connection-short',
			'38 connection-short',
			'43 connection-short',
			'49 connection-short',
		],
		memoryWrites: ['8 settled 0'],
		inactiveFrom: null,
		finalLevelIndex: 0,
		finalNotice: '{"kind":"connection-short","width":1920,"height":1080,"frameRate":30,"deliveredFrameRate":3}',
		holdingShort: true,
	},
	{
		id: 'U0 c1 software H.264 at 4K15',
		input: CAPTURE_4K15_INPUT,
		pinned: false,
		rows: CPU_LIMITED_4K15,
		warmUpEndsAt: 10,
		decisions: ['27 switch-codec vp9'],
		notices: ['27 codec-switched'],
		memoryWrites: ['27 short 0.599'],
		inactiveFrom: null,
		finalLevelIndex: 0,
		finalNotice: '{"kind":"codec-switched","codec":"vp9","previousCodec":"h264"}',
		holdingShort: false,
	},
	{
		id: 'U0 c1 pinned to software H.264 at 4K15',
		input: CAPTURE_4K15_INPUT,
		pinned: true,
		rows: CPU_LIMITED_4K15,
		warmUpEndsAt: 10,
		decisions: [],
		notices: ['27 pinned-codec-short'],
		memoryWrites: ['27 short 0.599'],
		inactiveFrom: null,
		finalLevelIndex: 0,
		finalNotice: '{"kind":"pinned-codec-short","codec":"h264"}',
		holdingShort: true,
	},
	{
		id: 'U0 c4 uplink capped at 2 Mbps',
		input: CAPTURE_720P_MOTION_INPUT,
		pinned: false,
		rows: CAPPED_2MBPS,
		warmUpEndsAt: 4,
		decisions: [],
		notices: [],
		memoryWrites: ['9 settled 0'],
		inactiveFrom: null,
		finalLevelIndex: 0,
		finalNotice: null,
		holdingShort: false,
	},
	{
		id: 'U0 c6 uplink capped at 600 kbps',
		input: CAPTURE_720P_MOTION_INPUT,
		pinned: false,
		rows: CAPPED_600KBPS,
		warmUpEndsAt: 4,
		decisions: [],
		notices: [
			'9 connection-short',
			'11 connection-short',
			'14 connection-short',
			'17 connection-short',
			'20 connection-short',
			'23 connection-short',
			'26 connection-short',
			'29 connection-short',
		],
		memoryWrites: [],
		inactiveFrom: null,
		finalLevelIndex: 0,
		finalNotice: '{"kind":"connection-short","width":1280,"height":720,"frameRate":30,"deliveredFrameRate":7}',
		holdingShort: true,
	},
	{
		id: 'U0 c5 viewerless share',
		input: {...CAPTURE_720P_MOTION_INPUT, hintSetting: 'auto'},
		pinned: false,
		rows: VIEWERLESS,
		warmUpEndsAt: 3,
		decisions: [],
		notices: [],
		memoryWrites: [],
		inactiveFrom: 4,
		finalLevelIndex: 0,
		finalNotice: null,
		holdingShort: false,
	},
];

function buildProofCaptureStats(row: ProofCaptureRow, width: number, height: number): RTCStatsReport {
	const [
		timestamp,
		framesEncoded,
		bytesSent,
		headerBytesSent,
		totalEncodeTime,
		targetBitrate,
		reason,
		sourceFrames,
		sourceFps,
		active,
	] = row;
	return new Map<string, unknown>([
		['codec', {type: 'codec', id: 'codec', mimeType: 'video/VP9'}],
		[
			'source',
			{
				type: 'media-source',
				id: 'source',
				kind: 'video',
				frames: sourceFrames,
				framesPerSecond: sourceFps,
				width,
				height,
			},
		],
		[
			'outbound',
			{
				type: 'outbound-rtp',
				id: 'outbound',
				kind: 'video',
				codecId: 'codec',
				mediaSourceId: 'source',
				active,
				framesEncoded,
				bytesSent,
				headerBytesSent,
				totalEncodeTime: totalEncodeTime ?? undefined,
				frameWidth: active ? width : undefined,
				frameHeight: active ? height : undefined,
				targetBitrate: targetBitrate ?? undefined,
				qualityLimitationReason: reason,
				encoderImplementation: 'libvpx',
				timestamp,
			},
		],
	]) as unknown as RTCStatsReport;
}

function describeProofCaptureDecision(decision: ScreenShareDeliveryDecision): string {
	if (decision.kind === 'switch-codec') return `${decision.kind} ${decision.codec}`;
	if (decision.kind === 'step' || decision.kind === 'probe' || decision.kind === 'revert-probe') {
		return `${decision.kind} ${decision.levelIndex}`;
	}
	return decision.kind;
}

function describeProofCaptureWrite(write: ScreenShareDeliveryMemoryWrite): string {
	if (write.kind === 'short') return `short ${write.ratio.toFixed(3)}`;
	if (write.kind === 'settled') return `settled ${write.settledIndex}`;
	return write.kind;
}

const PROOF_SAMPLE_WIDTH = 1280;
const PROOF_SAMPLE_HEIGHT = 720;

function checkProofCaptureSamples(outcome: ProofStageOutcome): void {
	const check = createProofCheck(outcome, 'U0 sample');
	const read = (row: ProofCaptureRow, counters: ScreenShareDeliveryCounters | null) =>
		readScreenShareDeliverySample(buildProofCaptureStats(row, PROOF_SAMPLE_WIDTH, PROOF_SAMPLE_HEIGHT), counters);
	check.applies('D sample deltas');
	const first = read([1000, 30, 100_000, 3_000, 1, 2_000_000, 'none', 30, 30, true], null);
	const second = read([3000, 90, 350_000, 10_500, 3.4, 2_000_000, 'none', 90, 30, true], first.counters);
	if (second.dtMs !== 2000 || second.encodedFps !== 30 || second.sourceFps !== 30 || second.sentBps !== 1_000_000) {
		check.fail(`D read ${second.dtMs} ms, ${second.encodedFps} FPS, ${second.sentBps} bps`);
	}
	check.applies('D encode time and header bytes');
	if (second.encodeSecondsPerFrame !== 0.04 || second.fill !== 0.515) {
		check.fail(`D read ${second.encodeSecondsPerFrame} s per frame and a fill of ${second.fill}`);
	}
	check.applies('D paused encoding');
	const paused = read([1000, 30, 100_000, 3_000, 1, 2_000_000, 'none', 30, 30, false], null);
	if (paused.active || paused.counters !== null) check.fail('D paused encoding kept a baseline');
	check.applies('D counters reset');
	const republished = read([3000, 2, 900, 30, 0.1, 2_000_000, 'none', 4, 30, true], {
		timestamp: 1000,
		framesEncoded: 300,
		bytesSent: 500_000,
		headerBytesSent: 15_000,
		totalEncodeTime: 6,
		sourceFrames: 300,
	});
	if (!republished.countersReset || republished.encodedFps !== null)
		check.fail('D counters that went backwards read as rates');
}

export function runProofCaptureStage(): ProofStageOutcome {
	const outcome = createProofStageOutcome();
	checkProofCaptureSamples(outcome);
	for (const capture of PROOF_CAPTURES) {
		const check = createProofCheck(outcome, capture.id);
		const source = capture.input.sourceDimensions;
		const target = resolveScreenShareTarget(capture.input);
		const levels = resolveScreenShareLevels(target, source);
		const state = createScreenShareDeliveryState();
		const context: ScreenShareDeliveryContext = {
			adaptive: true,
			levels,
			keep: target.keep,
			context: capture.input.context,
			codec: 'h264',
			pinned: capture.pinned,
			alternativeCodec: 'vp9',
			codecFailed: false,
			lastApplyFailed: false,
			paused: false,
			viewers: 1,
			multiLayer: false,
			multiLayerAvailable: false,
			monitoredMaxBitrate: levels[0].maxBitrate,
			shareCounters: {switches: 0, probes: 0, layerings: 0, probeBlocked: false, toastShown: false},
		};
		let counters: ScreenShareDeliveryCounters | null = null;
		const decisions: Array<string> = [];
		const notices: Array<string> = [];
		const memoryWrites: Array<string> = [];
		let warmUpEndsAt = -1;
		let inactiveFrom: number | null = null;
		let activeAfterInactive = false;
		for (const [index, row] of capture.rows.entries()) {
			const sample = readScreenShareDeliverySample(buildProofCaptureStats(row, source.width, source.height), counters);
			counters = sample.counters;
			context.monitoredMaxBitrate = levels[state.levelIndex].maxBitrate;
			const evaluation = evaluateScreenShareDelivery(state, sample, context);
			if (warmUpEndsAt < 0 && !evaluation.warmingUp) warmUpEndsAt = index;
			if (evaluation.decision.kind !== 'none') {
				decisions.push(`${index} ${describeProofCaptureDecision(evaluation.decision)}`);
			}
			if (evaluation.noticeChanged) notices.push(`${index} ${evaluation.notice?.kind ?? 'cleared'}`);
			if (evaluation.memoryWrite !== null) {
				memoryWrites.push(`${index} ${describeProofCaptureWrite(evaluation.memoryWrite)}`);
			}
			if (evaluation.kind === 'inactive') {
				if (inactiveFrom === null) inactiveFrom = index;
			} else if (inactiveFrom !== null) {
				activeAfterInactive = true;
			}
		}
		outcome.traces += 1;
		outcome.ticks += capture.rows.length;
		check.applies(capture.id);
		const description = describeScreenShareDelivery(state, levels);
		if (warmUpEndsAt !== capture.warmUpEndsAt) check.fail(`U0 warm-up ended at ${warmUpEndsAt}`);
		if (decisions.join(', ') !== capture.decisions.join(', ')) check.fail(`U0 decided ${decisions.join(', ')}`);
		if (notices.join(', ') !== capture.notices.join(', ')) check.fail(`U0 noticed ${notices.join(', ')}`);
		if (memoryWrites.join(', ') !== capture.memoryWrites.join(', ')) check.fail(`U0 wrote ${memoryWrites.join(', ')}`);
		if (inactiveFrom !== capture.inactiveFrom) check.fail(`U0 went inactive at ${inactiveFrom}`);
		if (activeAfterInactive) check.fail('U0 came back from an inactive share');
		if (state.levelIndex !== capture.finalLevelIndex) check.fail(`U0 ended at level ${state.levelIndex}`);
		const finalNotice = state.notice === null ? null : JSON.stringify(state.notice);
		if (finalNotice !== capture.finalNotice) check.fail(`U0 ended on ${finalNotice}`);
		if (description.holdingShort !== capture.holdingShort) check.fail(`U0 holdingShort ${description.holdingShort}`);
	}
	return outcome;
}

export function reportProofStage(label: string, outcome: ProofStageOutcome): void {
	console.info(`${label}: ${outcome.traces} traces, ${outcome.ticks} ticks`, outcome.applied);
}
