// SPDX-License-Identifier: AGPL-3.0-or-later

export interface VideoDimensions {
	width: number;
	height: number;
}

export interface VideoLayoutConstraints {
	maxWidth: number;
	maxHeight: number;
}

export interface VideoLayout {
	sourceDimensions: VideoDimensions;
	renderDimensions: VideoDimensions;
	aspectRatio: string;
	scale: number;
}

const DEFAULT_VIDEO_DIMENSIONS: VideoDimensions = {
	width: 16,
	height: 9,
};
const MIN_RENDER_HEIGHT = 112;
const DEFAULT_INLINE_VIDEO_LAYOUT_CONSTRAINTS: VideoLayoutConstraints = {
	maxWidth: 400,
	maxHeight: 400,
};
const MAX_INLINE_PLAYBACK_LONG_EDGE = 6016;
const MAX_INLINE_PLAYBACK_SHORT_EDGE = 3384;

export function normalizeVideoDimensions(dimensions?: Partial<VideoDimensions> | null): VideoDimensions | null {
	const width = dimensions?.width;
	const height = dimensions?.height;
	if (!Number.isFinite(width) || !Number.isFinite(height) || !width || !height || width <= 0 || height <= 0) {
		return null;
	}
	return {
		width: Math.round(width),
		height: Math.round(height),
	};
}

export function isInlinePlayableVideoSize(dimensions?: Partial<VideoDimensions> | null): boolean {
	const source = normalizeVideoDimensions(dimensions);
	if (source === null) {
		return true;
	}
	const {width, height} = source;
	return (
		(width <= MAX_INLINE_PLAYBACK_LONG_EDGE && height <= MAX_INLINE_PLAYBACK_SHORT_EDGE) ||
		(width <= MAX_INLINE_PLAYBACK_SHORT_EDGE && height <= MAX_INLINE_PLAYBACK_LONG_EDGE)
	);
}

function normalizeConstraint(value: number | undefined, fallback: number): number {
	return Number.isFinite(value) && value !== undefined && value > 0 ? value : fallback;
}

export function getInlineVideoLayoutConstraints(
	constraints?: Partial<VideoLayoutConstraints> | null,
): VideoLayoutConstraints {
	const maxWidth = normalizeConstraint(constraints?.maxWidth, DEFAULT_INLINE_VIDEO_LAYOUT_CONSTRAINTS.maxWidth);
	const maxHeight = normalizeConstraint(constraints?.maxHeight, DEFAULT_INLINE_VIDEO_LAYOUT_CONSTRAINTS.maxHeight);
	return {
		maxWidth: Math.min(maxWidth, DEFAULT_INLINE_VIDEO_LAYOUT_CONSTRAINTS.maxWidth),
		maxHeight,
	};
}

export function getEffectiveVideoLayoutDimensions(
	declaredDimensions?: Partial<VideoDimensions> | null,
	decodedDimensions?: Partial<VideoDimensions> | null,
): VideoDimensions {
	return (
		normalizeVideoDimensions(decodedDimensions) ??
		normalizeVideoDimensions(declaredDimensions) ?? {...DEFAULT_VIDEO_DIMENSIONS}
	);
}

export function resolveVideoLayout(
	dimensions?: Partial<VideoDimensions> | null,
	constraints?: Partial<VideoLayoutConstraints> | null,
): VideoLayout {
	const sourceDimensions = normalizeVideoDimensions(dimensions) ?? {...DEFAULT_VIDEO_DIMENSIONS};
	const {maxWidth, maxHeight} = getInlineVideoLayoutConstraints(constraints);
	const scale = Math.min(1, maxWidth / sourceDimensions.width, maxHeight / sourceDimensions.height);
	const scaledWidth = Math.max(1, Math.round(sourceDimensions.width * scale));
	const scaledHeight = Math.max(1, Math.round(sourceDimensions.height * scale));
	const letterboxed = scaledHeight < MIN_RENDER_HEIGHT;
	const renderDimensions = {
		width: scaledWidth,
		height: letterboxed ? MIN_RENDER_HEIGHT : scaledHeight,
	};
	return {
		sourceDimensions,
		renderDimensions,
		aspectRatio: `${renderDimensions.width} / ${renderDimensions.height}`,
		scale,
	};
}

export function hasDifferentAspectRatio(
	currentDimensions: VideoDimensions,
	nextDimensions: VideoDimensions,
	tolerance = 0.01,
): boolean {
	const currentRatio = currentDimensions.width / currentDimensions.height;
	const nextRatio = nextDimensions.width / nextDimensions.height;
	if (!Number.isFinite(currentRatio) || !Number.isFinite(nextRatio)) {
		return false;
	}
	return Math.abs(currentRatio - nextRatio) / currentRatio > tolerance;
}
