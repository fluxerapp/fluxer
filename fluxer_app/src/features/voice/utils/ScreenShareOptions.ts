// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScreenshareResolution, StreamingMode} from '@app/features/voice/state/VoiceSettings';
import type {
	ScreenShareContentHint,
	ScreenShareScalabilityModePreference,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import type {ScreenShareCaptureOptions, TrackPublishOptions, VideoCodec, VideoEncoding} from 'livekit-client';

const DIMENSIONS: Record<
	ScreenshareResolution,
	{
		width: number;
		height: number;
	}
> = {
	low_240p: {width: 426, height: 240},
	low_480p: {width: 854, height: 480},
	medium: {width: 1280, height: 720},
	high: {width: 1920, height: 1080},
	ultra: {width: 2560, height: 1440},
	source: {width: 3840, height: 2160},
};
export const SCREEN_SHARE_MAX_VIDEO_BITRATE_BPS = 6_000_000;
export const SUPPORTED_SCREEN_SHARE_FRAME_RATES = [15, 30, 60, 90, 120] as const;

export type SupportedScreenShareFrameRate = (typeof SUPPORTED_SCREEN_SHARE_FRAME_RATES)[number];

const BITRATE_KBPS: Record<ScreenshareResolution, Record<SupportedScreenShareFrameRate, number>> = {
	low_240p: {15: 300, 30: 500, 60: 700, 90: 700, 120: 700},
	low_480p: {15: 1200, 30: 2000, 60: 3000, 90: 3000, 120: 3000},
	medium: {15: 2000, 30: 3000, 60: 4500, 90: 4500, 120: 4500},
	high: {15: 3000, 30: 4500, 60: 6000, 90: 6000, 120: 6000},
	ultra: {15: 4000, 30: 5500, 60: 6000, 90: 6000, 120: 6000},
	source: {15: 4500, 30: 6000, 60: 6000, 90: 6000, 120: 6000},
};

const BITRATE_RUNGS = [
	'low_240p',
	'low_480p',
	'medium',
	'high',
	'ultra',
	'source',
] as const satisfies ReadonlyArray<ScreenshareResolution>;

export function resolveScreenShareFrameRate(frameRate: number): SupportedScreenShareFrameRate {
	if (frameRate >= 120) return 120;
	if (frameRate >= 90) return 90;
	if (frameRate >= 60) return 60;
	if (frameRate >= 30) return 30;
	return 15;
}

function getScreenShareRungPixels(resolution: ScreenshareResolution): number {
	return DIMENSIONS[resolution].width * DIMENSIONS[resolution].height;
}

function resolveBitrateRungForPixels(pixels: number): ScreenshareResolution {
	let rung: ScreenshareResolution = BITRATE_RUNGS[0];
	for (const candidate of BITRATE_RUNGS) {
		if (getScreenShareRungPixels(candidate) > pixels) break;
		rung = candidate;
	}
	return rung;
}

function resolveBitrateRung(
	resolution: ScreenshareResolution,
	sourceDimensions?: {
		width: number;
		height: number;
	} | null,
): ScreenshareResolution {
	return resolveEffectiveScreenShareDimensions(resolution, sourceDimensions).rung;
}

export function getScreenShareBitrateBps(
	resolution: ScreenshareResolution,
	frameRate: SupportedScreenShareFrameRate,
	sourceDimensions?: {
		width: number;
		height: number;
	} | null,
): number {
	return BITRATE_KBPS[resolveBitrateRung(resolution, sourceDimensions)][frameRate] * 1000;
}

export function getScreenShareEncoding(
	resolution: ScreenshareResolution,
	frameRate: number,
	sourceDimensions?: {
		width: number;
		height: number;
	} | null,
): VideoEncoding {
	const resolvedFrameRate = resolveScreenShareFrameRate(frameRate);
	return {
		maxBitrate: getScreenShareBitrateBps(resolution, resolvedFrameRate, sourceDimensions),
		maxFramerate: resolvedFrameRate,
		priority: 'high',
	};
}

export const STREAMING_MODE_PRESETS: Record<
	Exclude<StreamingMode, 'custom'>,
	{
		resolution: ScreenshareResolution;
		frameRate: SupportedScreenShareFrameRate;
	}
> = {
	gaming: {resolution: 'ultra', frameRate: 60},
	screenshare: {resolution: 'source', frameRate: 15},
};
const FREE_STREAMING_MODE_PRESETS: Record<
	Exclude<StreamingMode, 'custom'>,
	{
		resolution: ScreenshareResolution;
		frameRate: SupportedScreenShareFrameRate;
	}
> = {
	gaming: {resolution: 'medium', frameRate: 30},
	screenshare: {resolution: 'medium', frameRate: 30},
};

export interface BuiltScreenShareOptions {
	captureOptions: ScreenShareCaptureOptions;
	publishOptions: TrackPublishOptions;
}

export interface ScreenShareBuildConfig {
	resolution: ScreenshareResolution;
	frameRate: number;
	context: ScreenShareContext;
	includeAudio: boolean;
	contentHint?: ScreenShareCaptureOptions['contentHint'];
	sourceDimensions?: {
		width: number;
		height: number;
	};
	preferredDisplaySurface?: 'window' | 'monitor';
	useBrowserAudioPicker?: boolean;
}

type ScreenShareVideoOptions = NonNullable<Exclude<ScreenShareCaptureOptions['video'], true>> & {
	cursor?: 'always' | 'motion' | 'never';
};

function resolveScreenShareCursorCapture(
	preferredDisplaySurface?: ScreenShareBuildConfig['preferredDisplaySurface'],
): 'always' | 'never' {
	return preferredDisplaySurface === 'window' ? 'never' : 'always';
}

function floorToEven(value: number): number {
	return Math.max(2, Math.floor(value / 2) * 2);
}

export function resolveEffectiveScreenShareDimensions(
	resolution: ScreenshareResolution,
	sourceDimensions?: {
		width: number;
		height: number;
	} | null,
): {
	width: number;
	height: number;
	rung: ScreenshareResolution;
} {
	const preset = DIMENSIONS[resolution];
	if (!sourceDimensions || sourceDimensions.width <= 0 || sourceDimensions.height <= 0) {
		return {width: preset.width, height: preset.height, rung: resolution};
	}
	const budget = getScreenShareRungPixels(resolution);
	const sourcePixels = sourceDimensions.width * sourceDimensions.height;
	if (sourcePixels <= budget) {
		return {
			width: sourceDimensions.width,
			height: sourceDimensions.height,
			rung: resolveBitrateRungForPixels(sourcePixels),
		};
	}
	const scale = Math.sqrt(budget / sourcePixels);
	return {
		width: floorToEven(sourceDimensions.width * scale),
		height: floorToEven(sourceDimensions.height * scale),
		rung: resolution,
	};
}

export function buildScreenShareOptions(config: ScreenShareBuildConfig): BuiltScreenShareOptions {
	const {width, height} = resolveEffectiveScreenShareDimensions(config.resolution, config.sourceDimensions);
	const resolvedFrameRate = resolveScreenShareFrameRate(config.frameRate);
	const video: ScreenShareVideoOptions = {
		cursor: resolveScreenShareCursorCapture(config.preferredDisplaySurface),
		...(config.preferredDisplaySurface ? {displaySurface: config.preferredDisplaySurface} : {}),
	};
	return {
		captureOptions: {
			audio: config.includeAudio,
			...(config.contentHint ? {contentHint: config.contentHint} : {}),
			...(config.includeAudio ? {restrictOwnAudio: true} : {}),
			selfBrowserSurface: 'include',
			monitorTypeSurfaces: config.preferredDisplaySurface === 'window' ? 'exclude' : 'include',
			systemAudio: config.includeAudio && config.useBrowserAudioPicker ? 'include' : 'exclude',
			windowAudio: config.includeAudio ? (config.useBrowserAudioPicker ? 'system' : 'window') : 'exclude',
			resolution: {width, height, frameRate: resolvedFrameRate},
			video,
		},
		publishOptions: {
			degradationPreference: resolveScreenShareDegradationPreference(config),
			screenShareEncoding: getScreenShareEncoding(config.resolution, resolvedFrameRate, config.sourceDimensions),
		},
	};
}

const FREE_TIER_FALLBACK_RESOLUTION: ScreenshareResolution = 'medium';
const FREE_TIER_RESOLUTIONS: ReadonlyArray<ScreenshareResolution> = ['low_480p', 'medium'];
const FREE_TIER_MAX_FRAME_RATE: SupportedScreenShareFrameRate = 30;

function isFreeTierResolution(resolution: ScreenshareResolution): boolean {
	return FREE_TIER_RESOLUTIONS.includes(resolution);
}

function clampToFreeTier(
	resolution: ScreenshareResolution,
	frameRate: SupportedScreenShareFrameRate,
): {
	resolution: ScreenshareResolution;
	frameRate: SupportedScreenShareFrameRate;
} {
	const cappedResolution: ScreenshareResolution = isFreeTierResolution(resolution)
		? resolution
		: FREE_TIER_FALLBACK_RESOLUTION;
	const cappedFrameRate: SupportedScreenShareFrameRate =
		frameRate > FREE_TIER_MAX_FRAME_RATE ? FREE_TIER_MAX_FRAME_RATE : frameRate;
	return {resolution: cappedResolution, frameRate: cappedFrameRate};
}

export function resolveStreamingModeSettings(
	mode: StreamingMode,
	customResolution: ScreenshareResolution,
	customFrameRate: number,
	hasHigherQuality: boolean,
): {
	resolution: ScreenshareResolution;
	frameRate: SupportedScreenShareFrameRate;
} {
	const resolved =
		mode === 'custom'
			? {resolution: customResolution, frameRate: resolveScreenShareFrameRate(customFrameRate)}
			: (hasHigherQuality ? STREAMING_MODE_PRESETS : FREE_STREAMING_MODE_PRESETS)[mode];
	if (hasHigherQuality) {
		return resolved;
	}
	return clampToFreeTier(resolved.resolution, resolved.frameRate);
}

export type ScreenShareContext = 'display' | 'app' | 'device';

export function normaliseStreamingModeForContext(mode: StreamingMode, context: ScreenShareContext): StreamingMode {
	if (context === 'device' && mode === 'screenshare') {
		return 'gaming';
	}
	return mode;
}

export function normaliseResolutionForContext(
	resolution: ScreenshareResolution,
	context: ScreenShareContext,
	hasHigherQuality: boolean,
): ScreenshareResolution {
	if (!hasHigherQuality && !isFreeTierResolution(resolution)) {
		return FREE_TIER_FALLBACK_RESOLUTION;
	}
	if (context === 'device' && resolution === 'source') {
		return hasHigherQuality ? 'ultra' : FREE_TIER_FALLBACK_RESOLUTION;
	}
	return resolution;
}

const PREMIUM_SCREEN_SHARE_RESOLUTIONS: ReadonlyArray<ScreenshareResolution> = ['high', 'ultra', 'source'];
const SENT_PIXEL_FILL_RATIO = 0.98;

export interface ScreenShareQualityInput {
	mode: StreamingMode;
	storedResolution: ScreenshareResolution;
	storedFrameRate: number;
	entitled: boolean;
	context: ScreenShareContext;
}

export interface ScreenShareTargetInput extends ScreenShareQualityInput {
	sourceDimensions: {width: number; height: number} | null;
	hintSetting: ScreenShareContentHint;
}

export interface ScreenShareTarget {
	mode: StreamingMode;
	resolution: ScreenshareResolution;
	frameRate: SupportedScreenShareFrameRate;
	context: ScreenShareContext;
	width: number;
	height: number;
	rung: ScreenshareResolution;
	maxBitrate: number;
	contentHint: ScreenShareCaptureOptions['contentHint'];
	presetOwned: boolean;
	tierLimited: boolean;
	deviceMapped: boolean;
}

export function resolveScreenShareDegradationPreference(target: {
	context: ScreenShareContext;
}): NonNullable<TrackPublishOptions['degradationPreference']> {
	return target.context === 'device' ? 'balanced' : 'maintain-resolution';
}

function resolveEffectiveScreenShareQuality(input: ScreenShareQualityInput): {
	mode: StreamingMode;
	resolution: ScreenshareResolution;
	frameRate: SupportedScreenShareFrameRate;
} {
	const mode = normaliseStreamingModeForContext(input.mode, input.context);
	const resolution = normaliseResolutionForContext(input.storedResolution, input.context, input.entitled);
	const settings = resolveStreamingModeSettings(mode, resolution, input.storedFrameRate, input.entitled);
	return {mode, resolution: settings.resolution, frameRate: settings.frameRate};
}

function resolveScreenShareContentHintForMode(
	mode: StreamingMode,
	hintSetting: ScreenShareContentHint,
): ScreenShareCaptureOptions['contentHint'] {
	if (mode === 'gaming') return 'motion';
	if (mode === 'screenshare') return 'text';
	return hintSetting === 'auto' ? undefined : hintSetting;
}

export function resolveScreenShareTarget(input: ScreenShareTargetInput): ScreenShareTarget {
	const effective = resolveEffectiveScreenShareQuality(input);
	const onDisplay = resolveEffectiveScreenShareQuality({...input, context: 'display'});
	const fit = resolveEffectiveScreenShareDimensions(effective.resolution, input.sourceDimensions);
	return {
		mode: effective.mode,
		resolution: effective.resolution,
		frameRate: effective.frameRate,
		context: input.context,
		width: fit.width,
		height: fit.height,
		rung: fit.rung,
		maxBitrate: BITRATE_KBPS[fit.rung][effective.frameRate] * 1000,
		contentHint: resolveScreenShareContentHintForMode(effective.mode, input.hintSetting),
		presetOwned: effective.mode !== 'custom',
		tierLimited:
			!input.entitled &&
			input.mode === 'custom' &&
			(PREMIUM_SCREEN_SHARE_RESOLUTIONS.includes(input.storedResolution) ||
				resolveScreenShareFrameRate(input.storedFrameRate) > FREE_TIER_MAX_FRAME_RATE),
		deviceMapped:
			effective.mode !== onDisplay.mode ||
			effective.resolution !== onDisplay.resolution ||
			effective.frameRate !== onDisplay.frameRate,
	};
}

export interface ScreenShareLayeringInput {
	codec: VideoCodec | undefined;
	svcSetting: ScreenShareScalabilityModePreference | undefined;
}

export interface ScreenShareLayering {
	simulcast: boolean;
	scalabilityMode: TrackPublishOptions['scalabilityMode'];
}

export function resolveScreenShareLayering(input: ScreenShareLayeringInput): ScreenShareLayering {
	if (input.codec !== 'av1' && input.codec !== 'vp9') {
		return {simulcast: false, scalabilityMode: undefined};
	}
	return {
		simulcast: false,
		scalabilityMode: input.svcSetting === 'single_layer' ? 'L1T1' : 'L1T3',
	};
}

export interface ScreenShareSenderParametersInput {
	encodings: ReadonlyArray<RTCRtpEncodingParameters>;
	target: ScreenShareTarget;
	capture: {width: number; height: number} | null;
	scalabilityMode: TrackPublishOptions['scalabilityMode'];
}

export interface ScreenShareSenderParameters {
	degradationPreference: NonNullable<TrackPublishOptions['degradationPreference']>;
	encodings: Array<RTCRtpEncodingParameters>;
}

function distributeScreenShareBitrate(
	encodings: ReadonlyArray<RTCRtpEncodingParameters>,
	maxBitrate: number,
): Array<number> {
	if (encodings.length === 1) return [maxBitrate];
	const weights = encodings.map((encoding) =>
		typeof encoding.maxBitrate === 'number' && encoding.maxBitrate > 0 ? encoding.maxBitrate : 0,
	);
	if (weights.some((weight) => weight <= 0)) {
		return encodings.map(() => Math.floor(maxBitrate / encodings.length));
	}
	const total = weights.reduce((sum, weight) => sum + weight, 0);
	return weights.map((weight) => Math.floor((weight / total) * maxBitrate));
}

export function resolveScreenShareSenderCodec(
	codecOverride: VideoCodec | undefined,
	negotiatedCodec: VideoCodec | undefined,
	publishedCodec: VideoCodec | undefined,
): VideoCodec | undefined {
	return codecOverride ?? negotiatedCodec ?? publishedCodec;
}

export function buildScreenShareSenderParameters(input: ScreenShareSenderParametersInput): ScreenShareSenderParameters {
	const targetPixels = input.target.width * input.target.height;
	const capturePixels = input.capture ? input.capture.width * input.capture.height : null;
	const baseScale = Math.min(...input.encodings.map((encoding) => encoding.scaleResolutionDownBy ?? 1));
	const targetScale = capturePixels === null ? 1 : Math.max(1, Math.sqrt(capturePixels / targetPixels));
	const sentPixels = capturePixels === null ? null : capturePixels / (targetScale * targetScale);
	const maxBitrate =
		sentPixels !== null && sentPixels < targetPixels * SENT_PIXEL_FILL_RATIO
			? Math.min(
					input.target.maxBitrate,
					BITRATE_KBPS[resolveBitrateRungForPixels(sentPixels)][input.target.frameRate] * 1000,
				)
			: input.target.maxBitrate;
	const bitrates = distributeScreenShareBitrate(input.encodings, maxBitrate);
	return {
		degradationPreference: resolveScreenShareDegradationPreference(input.target),
		encodings: input.encodings.map((encoding, index) => {
			const applied: RTCRtpEncodingParameters = {
				...encoding,
				maxBitrate: bitrates[index],
				maxFramerate: input.target.frameRate,
				priority: 'high',
				networkPriority: 'high',
				scaleResolutionDownBy: ((encoding.scaleResolutionDownBy ?? 1) / baseScale) * targetScale,
			};
			if (input.scalabilityMode) {
				applied.scalabilityMode = input.scalabilityMode;
			} else {
				delete applied.scalabilityMode;
			}
			return applied;
		}),
	};
}

export type ScreenShareQualityPick =
	| {axis: 'resolution'; resolution: ScreenshareResolution}
	| {axis: 'frameRate'; frameRate: SupportedScreenShareFrameRate};

export interface ScreenShareQualityPatch {
	streamingMode: 'custom';
	screenshareResolution?: ScreenshareResolution;
	videoFrameRate?: SupportedScreenShareFrameRate;
}

export function resolveScreenShareQualityPick(
	input: ScreenShareQualityInput,
	pick: ScreenShareQualityPick,
): ScreenShareQualityPatch | null {
	const effective = resolveEffectiveScreenShareQuality(input);
	const stored = resolveEffectiveScreenShareQuality({...input, mode: 'custom'});
	if (pick.axis === 'resolution') {
		if (normaliseResolutionForContext(pick.resolution, input.context, input.entitled) !== pick.resolution) return null;
		if (pick.resolution === effective.resolution) return null;
		return {
			streamingMode: 'custom',
			screenshareResolution: pick.resolution,
			...(stored.frameRate === effective.frameRate ? {} : {videoFrameRate: effective.frameRate}),
		};
	}
	if (!input.entitled && pick.frameRate > FREE_TIER_MAX_FRAME_RATE) return null;
	if (pick.frameRate === effective.frameRate) return null;
	return {
		streamingMode: 'custom',
		...(stored.resolution === effective.resolution ? {} : {screenshareResolution: effective.resolution}),
		videoFrameRate: pick.frameRate,
	};
}
