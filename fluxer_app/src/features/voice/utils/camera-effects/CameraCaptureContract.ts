// SPDX-License-Identifier: AGPL-3.0-or-later

export const CameraBackgroundMode = Object.freeze({
	NONE: 'none',
	BLUR: 'blur',
	CUSTOM: 'custom',
} as const);

export type CameraBackgroundMode = (typeof CameraBackgroundMode)[keyof typeof CameraBackgroundMode];

export const MAX_VIDEO_FRAME_RATE = 60;

export function clampVideoFrameRate(frameRate: number): number {
	if (!Number.isFinite(frameRate)) {
		return MAX_VIDEO_FRAME_RATE;
	}
	if (frameRate <= 0) {
		return MAX_VIDEO_FRAME_RATE;
	}
	return Math.min(Math.floor(frameRate), MAX_VIDEO_FRAME_RATE);
}
