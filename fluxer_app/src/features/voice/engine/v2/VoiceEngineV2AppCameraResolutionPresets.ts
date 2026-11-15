// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {CameraResolution} from '@app/features/voice/state/VoiceSettings';

export interface CameraCaptureDimensions {
	width: number;
	height: number;
	frameRate: number;
}

const CAMERA_CAPTURE_DIMENSIONS = {
	high: {width: 1920, height: 1080, frameRate: 30},
	medium: {width: 1280, height: 720, frameRate: 30},
	low: {width: 640, height: 360, frameRate: 20},
} as const satisfies Record<CameraResolution, CameraCaptureDimensions>;

export function getCameraCaptureDimensions(resolution: CameraResolution): CameraCaptureDimensions {
	const dimensions = CAMERA_CAPTURE_DIMENSIONS[resolution] ?? CAMERA_CAPTURE_DIMENSIONS.low;
	assert.ok(dimensions.width > 0, 'camera capture width must be positive');
	assert.ok(dimensions.height > 0, 'camera capture height must be positive');
	return dimensions;
}
