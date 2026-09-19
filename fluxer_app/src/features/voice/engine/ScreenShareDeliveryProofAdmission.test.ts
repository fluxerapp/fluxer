// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {
	buildProofCodecProfile,
	buildProofRoom,
	buildProofRuntimeKeys,
	describeProofRuntimeKey,
	forEachProofTargetInput,
	PROOF_ALL_CODECS,
	PROOF_COMPATIBLE_CODECS,
	PROOF_MAX_BITRATE_BPS,
	PROOF_MAX_LEVELS,
	PROOF_OPT_INS,
	PROOF_RESOLUTIONS,
	PROOF_RUNG_DIMENSIONS,
	PROOF_RUNTIME_KEY_COUNT,
	PROOF_STATIC_PROFILES,
	PROOF_STORED_FRAME_RATES,
	PROOF_VIEWER_MIXES,
	type ProofEncoderMode,
	type ProofHintSetting,
	type ProofMode,
	type ProofPin,
	type ProofResolution,
	type ProofVerdicts,
	proofFloorRung,
	proofLevelFloorRate,
	proofLevelFloorRung,
	proofQuantiseFrameRate,
	proofRungIndex,
	proofRungPixels,
	proofTableBitrate,
} from '@app/features/voice/engine/ScreenShareDeliveryProofHarness';
import type {
	ScreenShareDeliveryDescription,
	ScreenShareDeliveryEvaluation,
	ScreenShareDeliveryMemoryHeader,
} from '@app/features/voice/engine/ScreenShareUnderperformance';
import {
	buildScreenShareCodecAdvertisements,
	computeNegotiatedVideoCodec,
	rankScreenShareCodecs,
	resolveScreenShareCodecSubstitution,
} from '@app/features/voice/utils/ScreenShareCodecSelection';
import {
	buildScreenShareOptions,
	buildScreenShareSenderParameters,
	resolveEffectiveScreenShareDimensions,
	resolveScreenShareLayering,
	resolveScreenShareLevels,
	resolveScreenShareQualityPick,
	resolveScreenShareSenderCodec,
	resolveScreenShareTarget,
	type ScreenShareBuildConfig,
	type ScreenShareContext,
	type ScreenShareLevel,
	type ScreenShareQualityInput,
	type ScreenShareQualityPick,
	type ScreenShareTarget,
	type ScreenShareTargetInput,
	type SupportedScreenShareFrameRate,
} from '@app/features/voice/utils/ScreenShareOptions';
import type {VideoCodec} from 'livekit-client';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getScreenShareAv1OptIn: () => false,
		getScreenShareHevcOptIn: () => false,
		getPreferredVideoCodec: () => 'auto',
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
	getGpuEncoderReportSync: () => null,
}));

vi.mock('@app/features/voice/utils/NativeHardwareEncoderCapabilities', () => ({
	getNativeHardwareEncoderCapabilitiesSync: () => null,
	hasNativeHardwareEncoder: () => false,
	resetNativeHardwareEncoderCapabilities: () => undefined,
}));

vi.mock('@app/features/voice/utils/OpenH264Status', () => ({
	getOpenH264StatusSync: () => null,
	resetOpenH264Status: () => undefined,
}));

const proofStorage = {items: new Map<string, string>(), throws: false};

vi.mock('@app/features/platform/state/PersistentStorage', () => ({
	default: {
		getItem: (key: string) => {
			if (proofStorage.throws) throw new Error('storage unavailable');
			return proofStorage.items.get(key) ?? null;
		},
		setItem: (key: string, value: string) => {
			if (proofStorage.throws) throw new Error('storage unavailable');
			proofStorage.items.set(key, value);
		},
	},
}));

Object.defineProperty(globalThis, 'RTCRtpSender', {
	configurable: true,
	writable: true,
	value: {
		getCapabilities: () => ({
			codecs: ['video/VP8', 'video/VP9', 'video/H264', 'video/H265', 'video/AV1'].map((mimeType) => ({mimeType})),
		}),
	},
});

const {findVideoPublishCodecPolicyViolation, resolveScreenShareEncoderVerificationAction} = await import(
	'@app/features/voice/utils/CodecCapabilityDetector'
);
const {default: ScreenShareDelivery} = await import('@app/features/voice/state/ScreenShareDelivery');

const MAX_FAILURES = 40;
const FREE_RESOLUTIONS: ReadonlyArray<ProofResolution> = ['low_480p', 'medium'];
const PREMIUM_RESOLUTIONS: ReadonlyArray<ProofResolution> = ['high', 'ultra', 'source'];
const SOFTWARE_ORDER: ReadonlyArray<VideoCodec> = ['av1', 'vp9', 'h264', 'vp8'];
const PREFERENCE_ORDER: ReadonlyArray<VideoCodec> = ['av1', 'h265', 'h264', 'vp9', 'vp8'];
const GECKO_ORDER: ReadonlyArray<VideoCodec> = ['vp8', 'h264'];
const WEB_ORDER: ReadonlyArray<VideoCodec> = ['h264', 'vp8'];
const LOCAL_DECODE = {av1: true, h265: true, h264: true, vp9: true, vp8: true};

function createFailures() {
	const failures: Array<string> = [];
	return {
		failures,
		fail(label: string, message: string): void {
			if (failures.length < MAX_FAILURES) failures.push(`${label} ${message}`);
		},
	};
}

function describeInput(input: ScreenShareTargetInput): string {
	const source = input.sourceDimensions ? `${input.sourceDimensions.width}x${input.sourceDimensions.height}` : 'none';
	return `${input.mode}/${input.storedResolution}@${input.storedFrameRate}/${input.entitled ? 'premium' : 'free'}/${input.context}/${source}/${input.hintSetting}`;
}

function expectedQuality(input: ScreenShareQualityInput): {
	mode: ProofMode;
	resolution: ProofResolution;
	frameRate: SupportedScreenShareFrameRate;
} {
	const mode = input.context === 'device' && input.mode === 'screenshare' ? 'gaming' : input.mode;
	if (mode === 'gaming') {
		return input.entitled ? {mode, resolution: 'ultra', frameRate: 60} : {mode, resolution: 'medium', frameRate: 30};
	}
	if (mode === 'screenshare') {
		return input.entitled ? {mode, resolution: 'source', frameRate: 15} : {mode, resolution: 'medium', frameRate: 30};
	}
	let resolution = input.storedResolution;
	if (!input.entitled && !FREE_RESOLUTIONS.includes(resolution)) resolution = 'medium';
	if (input.context === 'device' && resolution === 'source') resolution = input.entitled ? 'ultra' : 'medium';
	const quantised = proofQuantiseFrameRate(input.storedFrameRate);
	return {mode, resolution, frameRate: !input.entitled && quantised > 30 ? 30 : quantised};
}

function expectedHint(mode: ProofMode, hintSetting: ProofHintSetting): ScreenShareTarget['contentHint'] {
	if (mode === 'screenshare') return 'text';
	if (mode === 'gaming') return 'motion';
	return hintSetting === 'auto' ? undefined : hintSetting;
}

function levelPixels(level: {width: number; height: number}): number {
	return level.width * level.height;
}

function describeLadder(levels: ReadonlyArray<ScreenShareLevel>): string {
	return levels.map((level) => `${level.width}x${level.height}@${level.frameRate}`).join(' ');
}

const LADDER_EXAMPLES: ReadonlyArray<{input: ScreenShareTargetInput; ladder: string}> = [
	{
		input: {
			mode: 'gaming',
			storedResolution: 'medium',
			storedFrameRate: 30,
			entitled: true,
			context: 'display',
			sourceDimensions: {width: 2560, height: 1440},
			hintSetting: 'auto',
		},
		ladder: '2560x1440@60 1920x1080@60 1280x720@60 852x480@60 852x480@30',
	},
	{
		input: {
			mode: 'screenshare',
			storedResolution: 'medium',
			storedFrameRate: 30,
			entitled: true,
			context: 'display',
			sourceDimensions: {width: 5120, height: 1440},
			hintSetting: 'auto',
		},
		ladder: '5120x1440@15 3620x1018@15 2714x762@15 1810x508@15',
	},
	{
		input: {
			mode: 'custom',
			storedResolution: 'source',
			storedFrameRate: 120,
			entitled: true,
			context: 'display',
			sourceDimensions: null,
			hintSetting: 'detail',
		},
		ladder: '3840x2160@120 3840x2160@90 3840x2160@60 3840x2160@30 3840x2160@15 2560x1440@15 1920x1080@15 1280x720@15',
	},
];

const FIT_EXAMPLES: ReadonlyArray<{
	resolution: ProofResolution;
	source: {width: number; height: number};
	fit: {width: number; height: number; rung: ProofResolution};
}> = [
	{resolution: 'source', source: {width: 5120, height: 1440}, fit: {width: 5120, height: 1440, rung: 'ultra'}},
	{resolution: 'source', source: {width: 4112, height: 2658}, fit: {width: 3582, height: 2314, rung: 'source'}},
	{resolution: 'high', source: {width: 1080, height: 1920}, fit: {width: 1080, height: 1920, rung: 'high'}},
	{resolution: 'source', source: {width: 800, height: 600}, fit: {width: 800, height: 600, rung: 'low_480p'}},
	{resolution: 'high', source: {width: 5120, height: 1440}, fit: {width: 2714, height: 762, rung: 'high'}},
];

const BITRATE_EXAMPLES: ReadonlyArray<{source: {width: number; height: number}; frameRate: number; bps: number}> = [
	{source: {width: 1920, height: 1080}, frameRate: 15, bps: 3_000_000},
	{source: {width: 1080, height: 1920}, frameRate: 30, bps: 4_500_000},
	{source: {width: 3840, height: 2160}, frameRate: 15, bps: 4_500_000},
	{source: {width: 5120, height: 1440}, frameRate: 15, bps: 4_000_000},
];

const OPTION_SHAPES: ReadonlyArray<Pick<ScreenShareBuildConfig, 'includeAudio' | 'preferredDisplaySurface'>> = [
	{includeAudio: true, preferredDisplaySurface: 'monitor'},
	{includeAudio: true, preferredDisplaySurface: 'window'},
	{includeAudio: false, preferredDisplaySurface: 'monitor'},
	{includeAudio: false},
];

describe('screen share delivery proof, admission', () => {
	it('stage A holds for every target and ladder', () => {
		const {failures, fail} = createFailures();
		const signatures = new Set<string>();
		let scenarios = 0;
		forEachProofTargetInput((input) => {
			scenarios += 1;
			const label = describeInput(input);
			const target = resolveScreenShareTarget(input);
			const levels = resolveScreenShareLevels(target, input.sourceDimensions);
			const expected = expectedQuality(input);
			const onDisplay = expectedQuality({...input, context: 'display'});
			const source = input.sourceDimensions;
			const budget = proofRungPixels(target.resolution);
			signatures.add(describeProofRuntimeKey(target, levels));

			if (target.mode !== expected.mode || target.resolution !== expected.resolution) {
				fail(
					label,
					`A1 resolved ${target.mode}/${target.resolution}, expected ${expected.mode}/${expected.resolution}`,
				);
			}
			if (target.frameRate !== expected.frameRate) fail(label, `A1 frame rate ${target.frameRate}`);
			if (!input.entitled && (!FREE_RESOLUTIONS.includes(target.resolution) || target.frameRate > 30)) {
				fail(label, 'A1 free user above the free tier');
			}
			if (input.mode === 'custom' && input.storedResolution === 'low_240p') {
				const kept = input.entitled ? 'low_240p' : 'medium';
				if (target.resolution !== kept) fail(label, `A1 low_240p became ${target.resolution}`);
			}

			const fit = resolveEffectiveScreenShareDimensions(target.resolution, source);
			if (fit.width !== target.width || fit.height !== target.height || fit.rung !== target.rung) {
				fail(label, 'A2 target size differs from the fit');
			}
			if (levelPixels(target) > budget) fail(label, `A2 ${target.width}x${target.height} above the budget`);
			const scaled = source !== null && levelPixels(source) > budget;
			if (source !== null && (target.width > source.width || target.height > source.height)) {
				fail(label, 'A2 side above the source');
			}
			if (scaled) {
				const aspect = target.width / target.height / (source.width / source.height);
				if (Math.abs(aspect - 1) > 0.01) fail(label, `A2 aspect off by ${aspect}`);
				if (target.width % 2 !== 0 || target.height % 2 !== 0) fail(label, 'A2 odd side');
			}
			const expectedRung = source === null || scaled ? target.resolution : proofFloorRung(levelPixels(source));
			if (target.rung !== expectedRung) fail(label, `A3 rung ${target.rung}, expected ${expectedRung}`);
			if (target.maxBitrate !== proofTableBitrate(target.rung, target.frameRate))
				fail(label, 'A3 bitrate off the table');
			if (target.maxBitrate > PROOF_MAX_BITRATE_BPS) fail(label, 'A3 bitrate above 6 Mbps');
			if (target.resolution === 'source' && source !== null) {
				for (const example of BITRATE_EXAMPLES) {
					if (
						example.source.width === source.width &&
						example.source.height === source.height &&
						example.frameRate === target.frameRate &&
						target.maxBitrate !== example.bps
					) {
						fail(label, `A3 ${source.width}x${source.height} at ${target.frameRate} bills ${target.maxBitrate}`);
					}
				}
			}

			const hint = expectedHint(expected.mode, input.hintSetting);
			if (target.contentHint !== hint) fail(label, `A4 hint ${target.contentHint}, expected ${hint}`);
			if ((target.keep === 'frameRate') !== (target.contentHint === 'motion')) fail(label, `A5 keep ${target.keep}`);
			const hintClass = target.contentHint === 'motion' ? 'motion' : 'screen';
			if (target.memoryKey !== `${target.resolution}@${target.rung}|${target.frameRate}|${hintClass}`) {
				fail(label, `B5 memory key ${target.memoryKey}`);
			}

			const levelZero = levels[0];
			if (
				levelZero.index !== 0 ||
				levelZero.segment !== 0 ||
				levelZero.width !== target.width ||
				levelZero.height !== target.height ||
				levelZero.frameRate !== target.frameRate ||
				levelZero.rung !== target.rung ||
				levelZero.maxBitrate !== target.maxBitrate
			) {
				fail(label, 'A6 level 0 is not the target');
			}
			if (levels.length > PROOF_MAX_LEVELS) fail(label, `A6 ${levels.length} levels`);
			const floorRung = proofLevelFloorRung(target.keep, levelZero);
			const floorRate = proofLevelFloorRate(target.keep, levelZero);
			for (const [index, level] of levels.entries()) {
				if (level.index !== index) fail(label, 'A6 index out of order');
				if (level.maxBitrate !== proofTableBitrate(level.rung, level.frameRate))
					fail(label, `A8 level ${index} bitrate`);
				if (proofRungIndex(level.rung) < floorRung) fail(label, `A7 level ${index} rung ${level.rung} below the floor`);
				if (level.frameRate < floorRate) fail(label, `A7 level ${index} rate ${level.frameRate} below the floor`);
				if (level.segment === 1 && target.keep === 'frameRate' && levelZero.frameRate <= 30) {
					fail(label, 'A7 kept frame rate has a segment 1 at or below 30');
				}
				if (level.segment === 0) {
					const kept = target.keep === 'resolution';
					if (kept && (level.width !== target.width || level.height !== target.height)) {
						fail(label, `A6 segment 0 level ${index} changed the kept resolution`);
					}
					if (!kept && level.frameRate !== target.frameRate)
						fail(label, `A6 segment 0 level ${index} changed the rate`);
				}
				if (index === 0) continue;
				const previous = levels[index - 1];
				if (level.segment < previous.segment) fail(label, 'A6 segment order');
				if (level.frameRate > previous.frameRate || levelPixels(level) > levelPixels(previous)) {
					fail(label, `A6 level ${index} rises`);
				}
				if (level.frameRate === previous.frameRate && levelPixels(level) === levelPixels(previous)) {
					fail(label, `A6 level ${index} repeats`);
				}
				if (level.segment === 1) {
					const sameRate = level.frameRate === previous.frameRate;
					const sameSize = level.width === previous.width && level.height === previous.height;
					if (target.keep === 'resolution' && !sameRate) fail(label, `A6 segment 1 level ${index} changed the rate`);
					if (target.keep === 'frameRate' && !sameSize) fail(label, `A6 segment 1 level ${index} changed the size`);
				}
			}
			const last = levels[levels.length - 1];
			const lastSegmentZero = levels.filter((level) => level.segment === 0).at(-1) ?? levelZero;
			if (target.keep === 'resolution') {
				if (lastSegmentZero.frameRate !== 15) fail(label, 'A7 kept resolution does not reach 15 FPS');
				if (proofRungIndex(levelZero.rung) > proofRungIndex('medium') && last.rung !== 'medium') {
					fail(label, `A7 kept resolution stops at ${last.rung}`);
				}
			} else {
				if (proofRungIndex(levelZero.rung) > proofRungIndex('low_480p') && lastSegmentZero.rung !== 'low_480p') {
					fail(label, `A7 kept frame rate stops at ${lastSegmentZero.rung}`);
				}
				if (last.frameRate !== Math.min(levelZero.frameRate, 30))
					fail(label, `A7 kept frame rate ends at ${last.frameRate}`);
			}
			if (source?.width === 800 && target.keep === 'resolution' && levels.some((level) => level.segment === 1)) {
				fail(label, 'R10 800x600 source has a resolution step');
			}

			const shapes = OPTION_SHAPES.map((shape) =>
				buildScreenShareOptions({
					...shape,
					resolution: target.resolution,
					frameRate: target.frameRate,
					contentHint: target.contentHint,
					sourceDimensions: source ?? undefined,
				}),
			);
			for (const options of shapes) {
				if (options.publishOptions.degradationPreference !== 'maintain-resolution') fail(label, 'A9 preference');
				const resolution = options.captureOptions.resolution;
				if (
					resolution?.width !== target.width ||
					resolution.height !== target.height ||
					resolution.frameRate !== target.frameRate
				) {
					fail(label, 'A10 capture size differs from the target');
				}
				const encoding = options.publishOptions.screenShareEncoding;
				if (encoding?.maxBitrate !== target.maxBitrate || encoding.maxFramerate !== target.frameRate) {
					fail(label, 'A10 publish encoding differs from the target');
				}
			}
			const reference = JSON.stringify([
				shapes[0].captureOptions.resolution,
				shapes[0].captureOptions.contentHint,
				shapes[0].publishOptions,
			]);
			for (const options of shapes.slice(1)) {
				const shape = JSON.stringify([
					options.captureOptions.resolution,
					options.captureOptions.contentHint,
					options.publishOptions,
				]);
				if (shape !== reference) fail(label, 'A10 shapes disagree');
			}

			if (target.presetOwned !== (expected.mode !== 'custom')) fail(label, 'A11 presetOwned');
			const tierLimited =
				!input.entitled &&
				input.mode === 'custom' &&
				(PREMIUM_RESOLUTIONS.includes(input.storedResolution) || proofQuantiseFrameRate(input.storedFrameRate) > 30);
			if (target.tierLimited !== tierLimited) fail(label, 'A11 tierLimited');
			const deviceMapped =
				expected.mode !== onDisplay.mode ||
				expected.resolution !== onDisplay.resolution ||
				expected.frameRate !== onDisplay.frameRate;
			if (target.deviceMapped !== deviceMapped) fail(label, 'A11 deviceMapped');
			if (input.context !== 'device' && target.deviceMapped) fail(label, 'A11 display share marked device mapped');
			if (input.context === 'device' && (target.mode === 'screenshare' || target.resolution === 'source')) {
				fail(label, 'A11 device share kept screenshare or source');
			}
		});
		for (const example of LADDER_EXAMPLES) {
			const target = resolveScreenShareTarget(example.input);
			const ladder = describeLadder(resolveScreenShareLevels(target, example.input.sourceDimensions));
			if (ladder !== example.ladder) fail(describeInput(example.input), `C ladder ${ladder}`);
		}
		for (const example of FIT_EXAMPLES) {
			const fit = resolveEffectiveScreenShareDimensions(example.resolution, example.source);
			if (JSON.stringify(fit) !== JSON.stringify(example.fit))
				fail(example.resolution, `B2 fit ${JSON.stringify(fit)}`);
		}
		const keys = buildProofRuntimeKeys();
		console.info(`stage A: ${scenarios} scenarios, ${signatures.size} distinct runtime keys`);
		expect(failures).toEqual([]);
		expect(scenarios).toBe(30_240);
		expect(signatures.size).toBe(PROOF_RUNTIME_KEY_COUNT);
		expect(keys).toHaveLength(PROOF_RUNTIME_KEY_COUNT);
		expect(new Set(keys.map((key) => key.signature)).size).toBe(PROOF_RUNTIME_KEY_COUNT);
	}, 30_000);

	it('stage B ranks and negotiates every codec scenario', () => {
		const {failures, fail} = createFailures();
		const pins: ReadonlyArray<ProofPin> = ['auto', 'vp8', 'h264', 'vp9', 'av1', 'h265'];
		const modes: ReadonlyArray<ProofEncoderMode> = ['auto', 'hardware', 'software'];
		const allShort: ProofVerdicts = {
			av1: {short: true, ratio: 0.5},
			h265: {short: true, ratio: 0.7},
			h264: {short: true, ratio: 0.4},
			vp9: {short: true, ratio: 0.6},
			vp8: {short: true, ratio: 0.3},
		};
		const rooms = PROOF_VIEWER_MIXES.map((mix) => ({id: mix.id, room: buildProofRoom(mix.viewers, 0)}));
		for (const {id, room} of rooms) {
			if (room.unknown !== room.expectedUnknown) fail(id, `H counted ${room.unknown} silent viewers`);
		}
		let scenarios = 0;
		let viewerSubstitutions = 0;
		let fallbacks = 0;
		let deviceSubstitutions = 0;
		const narrowFallbacks = new Set<VideoCodec>();
		for (const profile of PROOF_STATIC_PROFILES) {
			const baseOrder =
				profile.browser === 'firefox'
					? GECKO_ORDER
					: profile.browser !== 'chromium' && !profile.desktop
						? WEB_ORDER
						: null;
			for (const optIns of PROOF_OPT_INS) {
				const codecProfile = buildProofCodecProfile(profile, optIns, new Set());
				const admitted = PROOF_ALL_CODECS.filter(
					(codec) => codecProfile.codecs[codec].allowed && codecProfile.codecs[codec].supported,
				);
				const hardware = PROOF_ALL_CODECS.filter(
					(codec) => codecProfile.codecs[codec].supported && codecProfile.codecs[codec].hardware,
				);
				for (const encoderModeSetting of modes) {
					const memoryFree = rankScreenShareCodecs({
						profile: codecProfile,
						encoderModeSetting,
						pin: 'auto',
						verdicts: {},
					});
					if (baseOrder !== null && memoryFree.order.join() !== baseOrder.join()) {
						fail(`${profile.id}/${JSON.stringify(optIns)}/${encoderModeSetting}`, `B10 order ${memoryFree.order}`);
					}
					const memories: ReadonlyArray<{id: string; verdicts: ProofVerdicts}> = [
						{id: 'empty', verdicts: {}},
						{id: 'first-short', verdicts: {[memoryFree.order[0]]: {short: true, ratio: 0.6}}},
						{id: 'all-short', verdicts: allShort},
						{id: 'vp9-ok', verdicts: {vp9: {short: false, ratio: 1}}},
					];
					for (const pin of pins) {
						for (const memory of memories) {
							const label = `${profile.id}/${JSON.stringify(optIns)}/${encoderModeSetting}/${pin}/${memory.id}`;
							const input = {profile: codecProfile, encoderModeSetting, pin, verdicts: memory.verdicts};
							const ranking = rankScreenShareCodecs(input);
							const order = ranking.order;
							const pinAdmitted = pin !== 'auto' && admitted.includes(pin);

							if (order.length === 0) fail(label, 'B1 empty order');
							if (admitted.length === 0 && order.join() !== 'vp8')
								fail(label, 'B1 empty filter does not fall back to vp8');
							if (admitted.length > 0 && order.some((codec) => !admitted.includes(codec))) {
								fail(label, `B1 order ${order} holds a codec outside ${admitted}`);
							}
							if (order.includes('h265') && !hardware.includes('h265') && pin !== 'h265') {
								fail(label, 'B1 software h265 without a pin');
							}
							if (pinAdmitted && order[0] !== pin) fail(label, `B2 pin not first in ${order}`);
							const rest = pinAdmitted ? order.slice(1) : order;
							if (baseOrder !== null && rest.some((codec) => !baseOrder.includes(codec))) {
								fail(label, `B10 order ${order} leaves ${baseOrder}`);
							}
							const isShort = (codec: VideoCodec) => memory.verdicts[codec]?.short === true;
							const firstShort = rest.findIndex(isShort);
							if (firstShort >= 0 && rest.slice(firstShort).some((codec) => !isShort(codec))) {
								fail(label, `B5 short codec before a healthy one in ${order}`);
							}
							const shorts = rest.filter(isShort);
							for (let index = 1; index < shorts.length; index++) {
								const left = memory.verdicts[shorts[index - 1]]?.ratio ?? 0;
								const right = memory.verdicts[shorts[index]]?.ratio ?? 0;
								if (right > left) fail(label, `B5 short codecs out of ratio order ${shorts}`);
							}
							const healthyRest = rest.filter((codec) => !isShort(codec));
							const baseRest = memoryFree.order.filter((codec) => healthyRest.includes(codec));
							if (healthyRest.join() !== baseRest.join())
								fail(label, `B5 healthy order ${healthyRest} left ${baseRest}`);
							const chromiumPath = profile.browser === 'chromium' || profile.desktop;
							if (
								chromiumPath &&
								encoderModeSetting !== 'software' &&
								pin === 'auto' &&
								memory.id !== 'first-short' &&
								memory.id !== 'all-short'
							) {
								const lastHardware = order.map((codec) => hardware.includes(codec)).lastIndexOf(true);
								const firstSoftware = order.findIndex((codec) => !hardware.includes(codec));
								if (firstSoftware >= 0 && lastHardware > firstSoftware)
									fail(label, `B3 software before hardware in ${order}`);
								const base = hardware.length > 0 ? PREFERENCE_ORDER : SOFTWARE_ORDER;
								const tail = order.filter((codec) => !hardware.includes(codec));
								const expectedTail = base.filter((codec) => tail.includes(codec));
								if (tail.join() !== expectedTail.join()) fail(label, `B3 tail ${tail}, expected ${expectedTail}`);
							}
							if (chromiumPath && encoderModeSetting === 'software' && pin === 'auto' && memory.id === 'empty') {
								const expectedOrder = SOFTWARE_ORDER.filter((codec) => admitted.includes(codec));
								if (order.join() !== (expectedOrder.length > 0 ? expectedOrder : ['vp8']).join()) {
									fail(label, `B3 software order ${order}`);
								}
							}
							if (ranking.hardwareUnavailable !== (encoderModeSetting === 'hardware' && hardware.length === 0)) {
								fail(label, 'B6 hardwareUnavailable');
							}

							const local = buildScreenShareCodecAdvertisements(order, LOCAL_DECODE);
							for (const {id, room} of rooms) {
								scenarios += 1;
								const roomLabel = `${label}/${id}`;
								const negotiated = computeNegotiatedVideoCodec(local, room.remotes, room.unknown, order).codec;
								const decodable = (codec: VideoCodec) => room.knownDecode.every((decode) => decode.includes(codec));
								const eligible = (codec: VideoCodec) =>
									decodable(codec) && (room.unknown === 0 || PROOF_COMPATIBLE_CODECS.includes(codec));
								const expectedCodec = order.find(eligible) ?? 'vp8';
								if (negotiated !== expectedCodec)
									fail(roomLabel, `B7 negotiated ${negotiated}, expected ${expectedCodec}`);
								if (room.unknown > 0 && !PROOF_COMPATIBLE_CODECS.includes(negotiated)) {
									fail(roomLabel, 'B7 silent viewer got an incompatible codec');
								}
								if (!order.includes(negotiated) && negotiated !== 'vp8')
									fail(roomLabel, 'B8 negotiated outside the order');
								const demoted: ReadonlyArray<VideoCodec> = order.filter((codec) => codec !== 'vp8');
								const fallback = computeNegotiatedVideoCodec(
									buildScreenShareCodecAdvertisements(demoted, LOCAL_DECODE),
									room.remotes,
									room.unknown,
									demoted,
								).codec;
								const expectedFallback =
									demoted.find(eligible) ??
									PROOF_COMPATIBLE_CODECS.find((codec) => demoted.includes(codec) && decodable(codec)) ??
									'vp8';
								if (fallback !== expectedFallback)
									fail(roomLabel, `B7 fallback ${fallback}, expected ${expectedFallback}`);
								if (fallback !== 'vp8' && !decodable(fallback))
									fail(roomLabel, `B7 fallback ${fallback} a viewer lacks`);
								if (demoted.length > 0 && !demoted.some(decodable)) fallbacks += 1;
								const narrowPreference = order.filter((codec) => !PROOF_COMPATIBLE_CODECS.includes(codec));
								const narrow = computeNegotiatedVideoCodec(local, room.remotes, room.unknown, narrowPreference).codec;
								const narrowChoice = narrowPreference.find(eligible);
								const expectedNarrow =
									narrowChoice ??
									PROOF_COMPATIBLE_CODECS.find((codec) => order.includes(codec) && decodable(codec)) ??
									'vp8';
								if (narrow !== expectedNarrow) {
									fail(roomLabel, `H fallback ${narrow} over ${narrowPreference}, expected ${expectedNarrow}`);
								}
								if (narrowChoice === undefined) narrowFallbacks.add(expectedNarrow);
								const substitution = resolveScreenShareCodecSubstitution(pin, order, negotiated);
								const expectedSubstitution =
									pin === 'auto' || negotiated === pin ? null : pinAdmitted ? 'viewer' : 'device';
								if (substitution !== expectedSubstitution) {
									fail(roomLabel, `B2 B9 substitution ${substitution}, expected ${expectedSubstitution}`);
								}
								if (substitution === 'viewer') viewerSubstitutions += 1;
								if (substitution === 'device') deviceSubstitutions += 1;
							}
						}
					}
				}
			}
		}
		const p1 = PROOF_STATIC_PROFILES.find((profile) => profile.id === 'P1');
		if (p1) {
			const order = rankScreenShareCodecs({
				profile: buildProofCodecProfile(p1, {av1: false, hevc: false}, new Set()),
				encoderModeSetting: 'auto',
				pin: 'auto',
				verdicts: {},
			}).order;
			if (order.join() !== 'vp9,h264,vp8') fail('P1', `B4 software VP9 moved for screen content ${order}`);
		}
		console.info(`stage B: ${scenarios} scenarios`);
		expect(failures).toEqual([]);
		expect(scenarios).toBe(37_440);
		expect([...narrowFallbacks].sort()).toEqual(['h264', 'vp8', 'vp9']);
		expect(viewerSubstitutions).toBeGreaterThan(0);
		expect(fallbacks).toBeGreaterThan(0);
		expect(deviceSubstitutions).toBeGreaterThan(0);
	}, 30_000);

	it('stage B enumerates the publish policy and the repeated stall', () => {
		const {failures, fail} = createFailures();
		const allowed: ReadonlyArray<VideoCodec> = ['vp8', 'h264', 'vp9'];
		const published: ReadonlyArray<VideoCodec | undefined> = [undefined, ...PROOF_ALL_CODECS];
		const negotiated: ReadonlyArray<VideoCodec | undefined> = [undefined, ...PROOF_ALL_CODECS];
		let cases = 0;
		for (const requested of PROOF_ALL_CODECS) {
			for (const publishedCodec of published) {
				for (const negotiatedCodec of negotiated) {
					cases += 1;
					const violation = findVideoPublishCodecPolicyViolation(requested, publishedCodec, negotiatedCodec);
					const clean =
						(publishedCodec === undefined || allowed.includes(publishedCodec)) &&
						(negotiatedCodec === undefined || negotiatedCodec === requested);
					if ((violation === null) !== clean) {
						fail(`${requested}/${publishedCodec}/${negotiatedCodec}`, `policy violation ${JSON.stringify(violation)}`);
					}
				}
			}
		}
		expect(failures).toEqual([]);
		expect(cases).toBe(180);
		expect(resolveScreenShareEncoderVerificationAction({reason: 'stalled', codec: 'vp9'}).kind).toBe('recover-stalled');
		expect(resolveScreenShareEncoderVerificationAction({reason: 'stalled', codec: 'vp9'}).kind).toBe('stop-stalled');
		expect(resolveScreenShareEncoderVerificationAction({reason: 'stalled', codec: 'vp8'}).kind).toBe('recover-stalled');
		expect(resolveScreenShareEncoderVerificationAction({reason: 'stalled', codec: 'vp8'}).kind).toBe('recover-stalled');
	});

	it('stage C builds every sender parameter set', () => {
		const {failures, fail} = createFailures();
		const senderCodecs: ReadonlyArray<VideoCodec | undefined> = [...PROOF_ALL_CODECS, undefined];
		const svcSettings = [
			{id: 'auto', svcSetting: 'auto', multiLayer: false},
			{id: 'single_layer', svcSetting: 'single_layer', multiLayer: true},
			{id: 'temporal', svcSetting: 'temporal', multiLayer: true},
			{id: 'multi layer', svcSetting: 'auto', multiLayer: true},
			{id: 'multi layer temporal', svcSetting: 'temporal', multiLayer: false},
		] as const;
		const encodingShapes: ReadonlyArray<{id: string; encodings: Array<RTCRtpEncodingParameters>}> = [
			{id: 'plain', encodings: [{active: true}]},
			{id: 'carrying L1T3', encodings: [{active: true, maxBitrate: 1_000_000, scalabilityMode: 'L1T3'}]},
			{id: 'pre-scaled', encodings: [{active: true, maxBitrate: 2_000_000, scaleResolutionDownBy: 2}]},
			{
				id: 'f h q weighted',
				encodings: [
					{rid: 'f', maxBitrate: 3_000_000},
					{rid: 'h', maxBitrate: 1_000_000, scaleResolutionDownBy: 2},
					{rid: 'q', maxBitrate: 300_000, scaleResolutionDownBy: 4},
				],
			},
			{
				id: 'f h q unweighted',
				encodings: [{rid: 'f'}, {rid: 'h', scaleResolutionDownBy: 2}, {rid: 'q', scaleResolutionDownBy: 4}],
			},
			{
				id: 'f h q top paused',
				encodings: [
					{rid: 'f', maxBitrate: 3_000_000, active: false},
					{rid: 'h', maxBitrate: 1_000_000, scaleResolutionDownBy: 2},
					{rid: 'q', maxBitrate: 300_000, scaleResolutionDownBy: 4},
				],
			},
			{
				id: 'f h q zero weight',
				encodings: [
					{rid: 'f', maxBitrate: 0, scalabilityMode: 'L1T3', scaleResolutionDownBy: 1.5},
					{rid: 'h', maxBitrate: 1_000_000, scaleResolutionDownBy: 2},
					{rid: 'q', scaleResolutionDownBy: 4},
				],
			},
		];
		const level = (width: number, height: number, frameRate: SupportedScreenShareFrameRate): ScreenShareLevel => {
			const rung = proofFloorRung(width * height);
			return {index: 0, segment: 0, width, height, frameRate, rung, maxBitrate: proofTableBitrate(rung, frameRate)};
		};
		const levels: ReadonlyArray<ScreenShareLevel> = [
			level(3840, 2160, 15),
			level(3840, 2160, 30),
			level(2560, 1440, 60),
			level(1920, 1080, 60),
			level(1280, 720, 60),
			level(854, 480, 60),
			level(854, 480, 30),
			level(1280, 720, 15),
		];
		const captures = [
			'level',
			{width: 3840, height: 2160},
			{width: 5120, height: 1440},
			{width: 800, height: 600},
			null,
		] as const;
		let scenarios = 0;
		for (const overrideCodec of senderCodecs) {
			for (const negotiatedCodec of senderCodecs) {
				for (const publishedCodec of senderCodecs) {
					const codec = resolveScreenShareSenderCodec(overrideCodec, negotiatedCodec, publishedCodec);
					const expectedCodec =
						overrideCodec !== undefined
							? overrideCodec
							: negotiatedCodec !== undefined
								? negotiatedCodec
								: publishedCodec;
					for (const svc of svcSettings) {
						for (const shape of encodingShapes) {
							for (const target of levels) {
								for (const captureOption of captures) {
									scenarios += 1;
									const capture =
										captureOption === 'level' ? {width: target.width, height: target.height} : captureOption;
									const label = `${overrideCodec}/${negotiatedCodec}/${publishedCodec}/${svc.id}/${shape.id}/${target.width}x${target.height}@${target.frameRate}/${capture ? `${capture.width}x${capture.height}` : 'unknown'}`;
									if (codec !== expectedCodec) fail(label, `C2 sender codec ${codec}, expected ${expectedCodec}`);
									const layering = resolveScreenShareLayering({
										codec,
										svcSetting: svc.svcSetting,
										levelPixels: target.width * target.height,
										multiLayer: svc.multiLayer,
									});
									const parameters = buildScreenShareSenderParameters({
										encodings: shape.encodings,
										level: target,
										capture,
										scalabilityMode: layering.scalabilityMode,
									});
									if (parameters.degradationPreference !== 'maintain-resolution') fail(label, 'C1 preference');
									if (parameters.encodings.length !== shape.encodings.length) fail(label, 'C4 encoding count');
									const single = shape.encodings.length === 1;
									const targetPixels = target.width * target.height;
									const levelScale = capture
										? Math.max(1, Math.sqrt((capture.width * capture.height) / targetPixels))
										: 1;
									const baseScale = Math.min(...shape.encodings.map((encoding) => encoding.scaleResolutionDownBy ?? 1));
									const sentPixels = capture ? (capture.width * capture.height) / (levelScale * levelScale) : null;
									const total =
										sentPixels !== null && sentPixels < 0.98 * targetPixels
											? Math.min(target.maxBitrate, proofTableBitrate(proofFloorRung(sentPixels), target.frameRate))
											: target.maxBitrate;
									const weights = shape.encodings.map((encoding) => encoding.maxBitrate ?? 0);
									const weightSum = weights.reduce((sum, weight) => sum + weight, 0);
									const expectedBitrates = single
										? [total]
										: weights.some((weight) => weight <= 0)
											? weights.map(() => Math.floor(total / weights.length))
											: weights.map((weight) => Math.floor((weight / weightSum) * total));
									const expectedMode = layering.scalabilityMode;
									let monitored = 0;
									let monitoredScale = Number.POSITIVE_INFINITY;
									for (const [index, encoding] of shape.encodings.entries()) {
										if (encoding.active === false) continue;
										const scale = encoding.scaleResolutionDownBy ?? 1;
										if (scale >= monitoredScale) continue;
										monitored = index;
										monitoredScale = scale;
									}
									if (parameters.monitoredMaxBitrate !== expectedBitrates[monitored]) {
										fail(label, `C7 monitored bitrate ${parameters.monitoredMaxBitrate}`);
									}
									for (const [index, encoding] of parameters.encodings.entries()) {
										const input = shape.encodings[index];
										if (encoding.rid !== input.rid) fail(label, 'C4 rid lost');
										const expectedScale = ((input.scaleResolutionDownBy ?? 1) / baseScale) * levelScale;
										if (encoding.scaleResolutionDownBy !== expectedScale) {
											fail(
												label,
												`C4 encoding ${index} scale ${encoding.scaleResolutionDownBy}, expected ${expectedScale}`,
											);
										}
										if ((encoding.scaleResolutionDownBy ?? 1) < 1) fail(label, 'R9 scale below 1');
										if (encoding.maxFramerate !== target.frameRate) fail(label, 'C5 frame rate');
										if (encoding.maxBitrate !== expectedBitrates[index]) {
											fail(
												label,
												`C5 encoding ${index} bitrate ${encoding.maxBitrate}, expected ${expectedBitrates[index]}`,
											);
										}
										if (encoding.priority !== 'high' || encoding.networkPriority !== 'high') fail(label, 'C6 priority');
										if (encoding.scalabilityMode !== expectedMode) fail(label, `C3 mode ${encoding.scalabilityMode}`);
										if (expectedMode === undefined && 'scalabilityMode' in encoding)
											fail(label, 'C3 mode left in place');
										const layers = encoding.scalabilityMode?.match(/^L(\d+)T(\d+)/);
										const spatial = layers === undefined || layers === null ? 1 : Number(layers[1]);
										if (spatial > 1 && codec !== 'av1') {
											fail(label, `C3 ${spatial} spatial layers on ${codec}`);
										}
									}
									const bitrateSum = parameters.encodings.reduce(
										(sum, encoding) => sum + (encoding.maxBitrate ?? 0),
										0,
									);
									if (bitrateSum > total || bitrateSum < total - parameters.encodings.length) {
										fail(label, `C5 bitrate sum ${bitrateSum} for ${total}`);
									}
								}
							}
						}
					}
				}
			}
		}
		console.info(`stage C: ${scenarios} scenarios`);
		expect(failures).toEqual([]);
		expect(scenarios).toBe(302_400);
	}, 60_000);

	it('stage C2 admits more than one layer only where it was measured', () => {
		const {failures, fail} = createFailures();
		const svcSettings: ReadonlyArray<'auto' | 'single_layer' | 'temporal' | undefined> = [
			undefined,
			'auto',
			'single_layer',
			'temporal',
		];
		const senderCodecs: ReadonlyArray<VideoCodec | undefined> = [...PROOF_ALL_CODECS, undefined];
		const mediumPixels = proofRungPixels('medium');
		const applied = new Set<string>();
		let scenarios = 0;
		for (const codec of senderCodecs) {
			for (const svcSetting of svcSettings) {
				for (const rung of PROOF_RESOLUTIONS) {
					for (const multiLayer of [false, true]) {
						scenarios += 1;
						const levelPixels = proofRungPixels(rung);
						const label = `${codec}/${svcSetting}/${rung}/${multiLayer}`;
						const layering = resolveScreenShareLayering({codec, svcSetting, levelPixels, multiLayer});
						const auto = svcSetting === undefined || svcSetting === 'auto';
						const spatialAv1 = codec === 'av1' && multiLayer && auto;
						const simulcast =
							codec === 'h264' && multiLayer && svcSetting !== 'single_layer' && levelPixels <= mediumPixels;
						const temporalMode = svcSetting === 'single_layer' ? 'L1T1' : 'L1T3';
						const expectedMode = spatialAv1
							? 'L3T3_KEY'
							: codec === 'av1' || codec === 'vp9'
								? temporalMode
								: undefined;
						if (layering.scalabilityMode !== expectedMode) fail(label, `C8 mode ${layering.scalabilityMode}`);
						if (layering.simulcast !== simulcast) fail(label, `C8 simulcast ${layering.simulcast}`);
						if (layering.multiLayer !== (spatialAv1 || simulcast)) fail(label, `C8 multiLayer ${layering.multiLayer}`);
						if (layering.simulcast && layering.scalabilityMode !== undefined)
							fail(label, 'C8 simulcast carries a mode');
						if (layering.multiLayer && (codec === 'vp9' || codec === 'vp8' || codec === 'h265')) {
							fail(label, `C8 ${codec} admitted a second layer`);
						}
						if (!multiLayer && layering.multiLayer) fail(label, 'C8 multiple layers without a request');
						if (spatialAv1) applied.add('C8 av1 spatial');
						if (simulcast) applied.add('C8 h264 simulcast');
						if (codec === 'h264' && multiLayer && auto && levelPixels > mediumPixels) {
							if (layering.simulcast) fail(label, 'C8 simulcast above the medium rung');
							applied.add('C8 h264 rung cap');
						}
					}
				}
			}
		}
		expect(failures).toEqual([]);
		expect(scenarios).toBe(288);
		expect([...applied].sort()).toEqual(['C8 av1 spatial', 'C8 h264 rung cap', 'C8 h264 simulcast']);
	});

	it('stage D writes only reachable quality picks', () => {
		const {failures, fail} = createFailures();
		const modes: ReadonlyArray<ProofMode> = ['custom', 'gaming', 'screenshare'];
		const contexts: ReadonlyArray<ScreenShareContext> = ['display', 'app', 'device'];
		const picks: ReadonlyArray<ScreenShareQualityPick> = [
			{axis: 'resolution', resolution: 'low_480p'},
			{axis: 'resolution', resolution: 'medium'},
			{axis: 'resolution', resolution: 'high'},
			{axis: 'resolution', resolution: 'ultra'},
			{axis: 'resolution', resolution: 'source'},
			{axis: 'frameRate', frameRate: 15},
			{axis: 'frameRate', frameRate: 30},
			{axis: 'frameRate', frameRate: 60},
		];
		let scenarios = 0;
		let patches = 0;
		for (const mode of modes) {
			for (const storedResolution of PROOF_RESOLUTIONS) {
				for (const storedFrameRate of PROOF_STORED_FRAME_RATES) {
					for (const entitled of [true, false]) {
						for (const context of contexts) {
							const input: ScreenShareQualityInput = {mode, storedResolution, storedFrameRate, entitled, context};
							const targetOf = (quality: ScreenShareQualityInput) =>
								resolveScreenShareTarget({...quality, sourceDimensions: null, hintSetting: 'auto'});
							const effective = targetOf(input);
							const storedAsCustom = targetOf({...input, mode: 'custom'});
							for (const pick of picks) {
								scenarios += 1;
								const label = `${mode}/${storedResolution}@${storedFrameRate}/${entitled ? 'premium' : 'free'}/${context}/${pick.axis === 'resolution' ? pick.resolution : pick.frameRate}`;
								const patch = resolveScreenShareQualityPick(input, pick);
								const premium =
									pick.axis === 'resolution' ? PREMIUM_RESOLUTIONS.includes(pick.resolution) : pick.frameRate > 30;
								const current =
									pick.axis === 'resolution'
										? pick.resolution === effective.resolution
										: pick.frameRate === effective.frameRate;
								const unservable = pick.axis === 'resolution' && context === 'device' && pick.resolution === 'source';
								const reachable = !(premium && !entitled) && !current && !unservable;
								if (!entitled && patch !== null) {
									if (
										patch.screenshareResolution !== undefined &&
										PREMIUM_RESOLUTIONS.includes(patch.screenshareResolution)
									) {
										fail(label, `D1 free patch ${patch.screenshareResolution}`);
									}
									if ((patch.videoFrameRate ?? 0) > 30) fail(label, `D1 free patch ${patch.videoFrameRate} FPS`);
								}
								if (premium && !entitled && patch !== null) fail(label, 'D2 premium pick without entitlement');
								if (current && patch !== null) fail(label, 'D3 already effective pick wrote a patch');
								if (unservable && patch !== null) fail(label, 'D3 device share wrote source');
								if (reachable !== (patch !== null))
									fail(label, `D reachable ${reachable} but patch ${JSON.stringify(patch)}`);
								if (patch === null) continue;
								patches += 1;
								if (patch.streamingMode !== 'custom') fail(label, 'D4 patch is not custom');
								const next = targetOf({
									...input,
									mode: 'custom',
									storedResolution: patch.screenshareResolution ?? storedResolution,
									storedFrameRate: patch.videoFrameRate ?? storedFrameRate,
								});
								if (pick.axis === 'resolution') {
									if (next.resolution !== pick.resolution)
										fail(label, `D4 picked ${pick.resolution}, got ${next.resolution}`);
									if (next.frameRate !== effective.frameRate) fail(label, `D4 rate moved to ${next.frameRate}`);
									const omit = storedAsCustom.frameRate === effective.frameRate;
									if (omit !== (patch.videoFrameRate === undefined)) fail(label, 'D5 rate written or omitted wrongly');
								} else {
									if (next.frameRate !== pick.frameRate)
										fail(label, `D4 picked ${pick.frameRate}, got ${next.frameRate}`);
									if (next.resolution !== effective.resolution)
										fail(label, `D4 resolution moved to ${next.resolution}`);
									const omit = storedAsCustom.resolution === effective.resolution;
									if (omit !== (patch.screenshareResolution === undefined)) {
										fail(label, 'D5 resolution written or omitted wrongly');
									}
								}
							}
						}
					}
				}
			}
		}
		console.info(`stage D: ${scenarios} scenarios, ${patches} patches`);
		expect(failures).toEqual([]);
		expect(scenarios).toBe(6_048);
		expect(patches).toBeGreaterThan(0);
	});

	it('keeps the delivery memory whole through a storage that throws and a stored blob that is junk', () => {
		const source = {width: 1920, height: 1080};
		const target = resolveScreenShareTarget({
			mode: 'screenshare',
			storedResolution: 'medium',
			storedFrameRate: 30,
			entitled: true,
			context: 'display',
			sourceDimensions: source,
			hintSetting: 'auto',
		});
		const level = resolveScreenShareLevels(target, source)[0];
		const header: ScreenShareDeliveryMemoryHeader = {
			chromiumMajor: 140,
			gpuKey: 'proof',
			encoderMode: 'auto',
			hardwareAccelerationDisabled: false,
		};
		const share = {
			header,
			target,
			context: 'display' as const,
			levels: resolveScreenShareLevels(target, source),
			hardware: {h264: true, vp9: false},
			adaptive: true,
		};
		const description: ScreenShareDeliveryDescription = {
			level,
			levelIndex: 0,
			deliveredFrameRate: level.frameRate,
			sentWidth: level.width,
			sentHeight: level.height,
			sourceFrameRate: null,
			notice: null,
			holdingShort: false,
		};
		const settled = (settledIndex: number): ScreenShareDeliveryEvaluation => ({
			kind: 'ok',
			decision: {kind: 'none'},
			notice: null,
			noticeChanged: false,
			memoryWrite: {kind: 'settled', settledIndex},
			shortCause: null,
			levelIndex: settledIndex,
			warmingUp: false,
		});

		proofStorage.items.clear();
		proofStorage.throws = true;
		ScreenShareDelivery.beginShare(share);
		expect(ScreenShareDelivery.verdicts).toEqual({});
		expect(ScreenShareDelivery.record('h264', settled(1), description)).toBe(false);
		ScreenShareDelivery.demoteForShare('h264', 0.6);
		expect(ScreenShareDelivery.setNotice({kind: 'apply-failed'})).toBe(true);
		ScreenShareDelivery.resetToFullQuality();
		expect(ScreenShareDelivery.notice).toBeNull();
		expect(proofStorage.items.size).toBe(0);

		proofStorage.throws = false;
		proofStorage.items.set('ScreenShareDeliveryMemoryV1', JSON.stringify({header, entries: null}));
		ScreenShareDelivery.beginShare(share);
		expect(ScreenShareDelivery.verdicts).toEqual({});
		ScreenShareDelivery.record('h264', settled(2), description);
		ScreenShareDelivery.endShare();

		ScreenShareDelivery.beginShare(share);
		expect(ScreenShareDelivery.verdicts.h264).toMatchObject({short: false, settledIndex: 2});
		ScreenShareDelivery.endShare();

		const stored = proofStorage.items.get('ScreenShareDeliveryMemoryV1');
		ScreenShareDelivery.beginShare({...share, adaptive: false});
		expect(ScreenShareDelivery.adaptive).toBe(false);
		expect(ScreenShareDelivery.verdicts).toEqual({});
		ScreenShareDelivery.record('h264', settled(5), description);
		expect(proofStorage.items.get('ScreenShareDeliveryMemoryV1')).toBe(stored);
		ScreenShareDelivery.resetToFullQuality();
		expect(proofStorage.items.get('ScreenShareDeliveryMemoryV1')).toBe(stored);
		ScreenShareDelivery.endShare();

		ScreenShareDelivery.beginShare(share);
		expect(ScreenShareDelivery.adaptive).toBe(true);
		expect(ScreenShareDelivery.verdicts.h264).toMatchObject({short: false, settledIndex: 2});
		ScreenShareDelivery.endShare();
	});

	it('reads back only the stored entries that still hold the entry shape', () => {
		const source = {width: 1920, height: 1080};
		const target = resolveScreenShareTarget({
			mode: 'screenshare',
			storedResolution: 'medium',
			storedFrameRate: 30,
			entitled: true,
			context: 'display',
			sourceDimensions: source,
			hintSetting: 'auto',
		});
		const header: ScreenShareDeliveryMemoryHeader = {
			chromiumMajor: 140,
			gpuKey: 'proof',
			encoderMode: 'auto',
			hardwareAccelerationDisabled: false,
		};
		const share = {
			header,
			target,
			context: 'display' as const,
			levels: resolveScreenShareLevels(target, source),
			hardware: {h264: true, vp9: false, vp8: false, av1: false},
			adaptive: true,
		};
		const whole = {short: false, ratio: 1, settledIndex: 1, probeBlockedUntil: null, at: Date.now()};

		proofStorage.throws = false;
		proofStorage.items.clear();
		proofStorage.items.set(
			'ScreenShareDeliveryMemoryV1',
			JSON.stringify({
				header,
				entries: {
					[`h264|hardware|${target.memoryKey}`]: whole,
					[`vp9|software|${target.memoryKey}`]: {...whole, at: 'recently'},
					[`vp8|software|${target.memoryKey}`]: {...whole, settledIndex: -3},
					[`av1|software|${target.memoryKey}`]: {...whole, ratio: null},
				},
			}),
		);
		ScreenShareDelivery.beginShare(share);
		expect(ScreenShareDelivery.verdicts.h264).toMatchObject({short: false, settledIndex: 1});
		expect(ScreenShareDelivery.verdicts.vp9).toBeUndefined();
		expect(ScreenShareDelivery.verdicts.vp8).toBeUndefined();
		expect(ScreenShareDelivery.verdicts.av1).toBeUndefined();
		ScreenShareDelivery.endShare();
	});

	it('imports only the functions the app calls', () => {
		const allowed: Record<string, ReadonlyArray<string>> = {
			'@app/features/voice/utils/ScreenShareOptions': [
				'resolveScreenShareTarget',
				'resolveScreenShareLevels',
				'buildScreenShareOptions',
				'buildScreenShareSenderParameters',
				'resolveScreenShareLayering',
				'resolveScreenShareQualityPick',
				'resolveEffectiveScreenShareDimensions',
				'resolveScreenShareSenderCodec',
			],
			'@app/features/voice/utils/ScreenShareCodecSelection': [
				'rankScreenShareCodecs',
				'computeNegotiatedVideoCodec',
				'buildScreenShareCodecAdvertisements',
				'countUnknownScreenShareParticipants',
				'resolveScreenShareCodecSubstitution',
				'resolveScreenShareAlternativeCodec',
			],
			'@app/features/voice/engine/ScreenShareUnderperformance': [
				'readScreenShareDeliverySample',
				'createScreenShareDeliveryState',
				'evaluateScreenShareDelivery',
				'describeScreenShareDelivery',
				'shouldToastScreenShareNotice',
				'readScreenShareDeliveryMemory',
				'updateScreenShareDeliveryMemory',
				'clearScreenShareDeliveryMemoryKey',
			],
			'@app/features/voice/state/ScreenShareDelivery': ['default: ScreenShareDelivery'],
		};
		const detector = '@app/features/voice/utils/CodecCapabilityDetector';
		const harness = '@app/features/voice/engine/ScreenShareDeliveryProofHarness';
		const files = [
			'ScreenShareDeliveryProofHarness.ts',
			'ScreenShareDeliveryProofAdmission.test.ts',
			'ScreenShareDeliveryProofRuntimeA.test.ts',
			'ScreenShareDeliveryProofRuntimeB.test.ts',
			'ScreenShareDeliveryProofRuntimeC.test.ts',
			'ScreenShareDeliveryProofRuntimeD.test.ts',
			'ScreenShareDeliveryProofViewers.test.ts',
		];
		const problems: Array<string> = [];
		const used = new Set<string>();
		for (const file of files) {
			const source = readFileSync(new URL(`./${file}`, import.meta.url), 'utf8');
			const admission = file === 'ScreenShareDeliveryProofAdmission.test.ts';
			if (
				!admission &&
				(/\bvi\.\w+\(/.test(source) || /import\s*\{[^}]*\bvi\b[^}]*\}\s*from\s*'vitest'/.test(source))
			) {
				problems.push(`${file} declares mocks`);
			}
			const imports = [...source.matchAll(/import\s+(type\s+)?\{([^}]*)\}\s+from\s+'([^']+)'/g)].map((match) => ({
				typeOnly: match[1] !== undefined,
				names: match[2],
				module: match[3],
			}));
			for (const match of source.matchAll(/const\s+\{([^}]*)\}\s*=\s*await\s+import\(\s*'([^']+)'\s*\)/g)) {
				imports.push({typeOnly: false, names: match[1], module: match[2]});
			}
			if (/import\s+\w+\s+from\s+'@app\//.test(source) || /import\s+\*\s+as/.test(source)) {
				problems.push(`${file} has a default or namespace app import`);
			}
			for (const entry of imports) {
				if (!entry.module.startsWith('@app/')) {
					const external =
						entry.module === 'vitest' ||
						(entry.module === 'livekit-client' && entry.typeOnly) ||
						(admission && entry.module === 'node:fs');
					if (!external) problems.push(`${file} imports ${entry.module}`);
					continue;
				}
				if (entry.module === harness) {
					if (file === 'ScreenShareDeliveryProofHarness.ts') problems.push('the harness imports itself');
					continue;
				}
				const names = entry.names
					.split(',')
					.map((name) => name.trim())
					.filter((name) => name.length > 0 && !name.startsWith('type '));
				if (entry.typeOnly || names.length === 0) continue;
				const list =
					entry.module === detector && admission
						? ['findVideoPublishCodecPolicyViolation', 'resolveScreenShareEncoderVerificationAction']
						: allowed[entry.module];
				if (!list) {
					problems.push(`${file} imports ${entry.module}`);
					continue;
				}
				for (const name of names) {
					if (!list.includes(name)) problems.push(`${file} imports ${name} from ${entry.module}`);
					used.add(name);
				}
			}
		}
		expect(problems).toEqual([]);
		expect([...used].sort()).toEqual(
			[
				...Object.values(allowed).flat(),
				'findVideoPublishCodecPolicyViolation',
				'resolveScreenShareEncoderVerificationAction',
			].sort(),
		);
		expect(Object.keys(PROOF_RUNG_DIMENSIONS)).toEqual([...PROOF_RESOLUTIONS]);
	});
});
