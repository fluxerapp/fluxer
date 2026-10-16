// SPDX-License-Identifier: AGPL-3.0-or-later

export const WEB_CAMERA_EFFECT_MASK_VOID_MAX = 0.3;
export const WEB_CAMERA_EFFECT_MASK_CORE_MIN = 0.7;
export const WEB_CAMERA_EFFECT_MASK_BAND_WIDTH = 0.4;
export const WEB_CAMERA_EFFECT_MASK_SPECKLE_NEIGHBOUR_MAX = 0.28;
export const WEB_CAMERA_EFFECT_MASK_HOLE_NEIGHBOUR_MIN = 0.72;
export const WEB_CAMERA_EFFECT_MASK_CORE_GROW_MIN = 0.45;
export const WEB_CAMERA_EFFECT_MASK_TEMPORAL_KEEP_STILL = 0.55;
export const WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_LOW = 0.1;
export const WEB_CAMERA_EFFECT_MASK_TEMPORAL_MOTION_HIGH = 0.35;
export const WEB_CAMERA_EFFECT_MASK_BAND_EPSILON = 0.004;
export const WEB_CAMERA_EFFECT_MASK_EDGE_SOFTNESS = 0.25;
export const WEB_CAMERA_EFFECT_MASK_GUIDE_SPATIAL_FALLOFF = 1.5;
export const WEB_CAMERA_EFFECT_MASK_GUIDE_RANGE_FALLOFF = 18;

export function shapeWebCameraEffectMaskAlpha(value: number): number {
	const normalized = Math.max(
		0,
		Math.min(1, (value - WEB_CAMERA_EFFECT_MASK_VOID_MAX) / WEB_CAMERA_EFFECT_MASK_BAND_WIDTH),
	);
	return normalized * normalized * (3 - 2 * normalized);
}
