// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScreenshareResolution, StreamingMode} from '@app/features/voice/state/VoiceSettings';
import type {ScreenShareCaptureOptions, TrackPublishOptions, VideoEncoding} from 'livekit-client';

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
export const SCREEN_SHARE_DEGRADATION_PREFERENCE: NonNullable<TrackPublishOptions['degradationPreference']> =
	'maintain-resolution';
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

function getScreenShareDimensions(resolution: ScreenshareResolution): {
	width: number;
	height: number;
} {
	return DIMENSIONS[resolution];
}

function resolveBitrateRungForDimensions(dimensions: {width: number; height: number}): ScreenshareResolution {
	const pixels = dimensions.width * dimensions.height;
	let rung: ScreenshareResolution = BITRATE_RUNGS[0];
	for (const candidate of BITRATE_RUNGS) {
		if (DIMENSIONS[candidate].width * DIMENSIONS[candidate].height > pixels) break;
		rung = candidate;
	}
	return rung;
}

function resolveBitrateRung(
	resolution: ScreenshareResolution,
	sourceDimensions?: {
		width: number;
		height: number;
	},
): ScreenshareResolution {
	return resolveBitrateRungForDimensions(resolveEffectiveScreenShareDimensions(resolution, sourceDimensions));
}

export function getScreenShareBitrateBps(
	resolution: ScreenshareResolution,
	frameRate: SupportedScreenShareFrameRate,
	sourceDimensions?: {
		width: number;
		height: number;
	},
): number {
	return BITRATE_KBPS[resolveBitrateRung(resolution, sourceDimensions)][frameRate] * 1000;
}

export function capScreenShareEncodingToDimensions(
	encoding: VideoEncoding,
	dimensions: {width?: number; height?: number} | undefined,
): VideoEncoding {
	if (typeof encoding.maxBitrate !== 'number') return encoding;
	const width = dimensions?.width;
	const height = dimensions?.height;
	if (!width || !height || width <= 0 || height <= 0) return encoding;
	const frameRate = resolveScreenShareFrameRate(encoding.maxFramerate ?? 60);
	const ceiling = BITRATE_KBPS[resolveBitrateRungForDimensions({width, height})][frameRate] * 1000;
	if (encoding.maxBitrate <= ceiling) return encoding;
	return {...encoding, maxBitrate: ceiling};
}

export function getScreenShareEncoding(
	resolution: ScreenshareResolution,
	frameRate: number,
	sourceDimensions?: {
		width: number;
		height: number;
	},
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

export function resolveEffectiveScreenShareDimensions(
	resolution: ScreenshareResolution,
	sourceDimensions?: {
		width: number;
		height: number;
	},
): {
	width: number;
	height: number;
} {
	const preset = getScreenShareDimensions(resolution);
	if (resolution !== 'source' || !sourceDimensions) return preset;
	if (sourceDimensions.width <= 0 || sourceDimensions.height <= 0) return preset;
	return {
		width: Math.min(preset.width, sourceDimensions.width),
		height: Math.min(preset.height, sourceDimensions.height),
	};
}

export function buildScreenShareOptions(config: ScreenShareBuildConfig): BuiltScreenShareOptions;
export function buildScreenShareOptions(resolution: ScreenshareResolution, frameRate: number): BuiltScreenShareOptions;
export function buildScreenShareOptions(
	configOrResolution: ScreenShareBuildConfig | ScreenshareResolution,
	maybeFrameRate?: number,
): BuiltScreenShareOptions {
	const config: ScreenShareBuildConfig =
		typeof configOrResolution === 'object'
			? configOrResolution
			: {resolution: configOrResolution, frameRate: maybeFrameRate ?? 30, includeAudio: true};
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
			degradationPreference: SCREEN_SHARE_DEGRADATION_PREFERENCE,
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

export type ScreenShareContext = 'display' | 'device';

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
