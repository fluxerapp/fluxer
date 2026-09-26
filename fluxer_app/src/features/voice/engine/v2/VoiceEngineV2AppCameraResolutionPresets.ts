// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceMediaGraphVideoQuality} from '@app/features/voice/engine/VoiceMediaGraphSubscriptionTypes';
import type {CameraResolution} from '@app/features/voice/state/VoiceSettings';
import {VideoPresets} from 'livekit-client';

export interface CameraCaptureDimensions {
	width: number;
	height: number;
	frameRate: number;
}

const CAMERA_RESOLUTION_PRESET_TABLE = {
	high: {capture: {width: 1920, height: 1080, frameRate: 30}, videoPreset: VideoPresets.h1080},
	medium: {capture: {width: 1280, height: 720, frameRate: 30}, videoPreset: VideoPresets.h720},
	low: {capture: {width: 640, height: 360, frameRate: 20}, videoPreset: VideoPresets.h360},
} as const;

for (const [resolution, entry] of Object.entries(CAMERA_RESOLUTION_PRESET_TABLE)) {
	assert.equal(entry.videoPreset.width, entry.capture.width, `camera preset width must agree for ${resolution}`);
	assert.equal(entry.videoPreset.height, entry.capture.height, `camera preset height must agree for ${resolution}`);
	assert.equal(
		entry.videoPreset.encoding.maxFramerate,
		entry.capture.frameRate,
		`camera preset frame rate must agree for ${resolution}`,
	);
}

function resolvePresetEntry(resolution: CameraResolution) {
	if (resolution === 'high') return CAMERA_RESOLUTION_PRESET_TABLE.high;
	if (resolution === 'medium') return CAMERA_RESOLUTION_PRESET_TABLE.medium;
	return CAMERA_RESOLUTION_PRESET_TABLE.low;
}

export function getCameraCaptureDimensions(resolution: CameraResolution): CameraCaptureDimensions {
	const dimensions = resolvePresetEntry(resolution).capture;
	assert.ok(dimensions.width > 0, 'camera capture width must be positive');
	assert.ok(dimensions.height > 0, 'camera capture height must be positive');
	return dimensions;
}

const CAMERA_SUBSCRIPTION_QUALITY_LAYERS = [
	{quality: 'low', layer: VideoPresets.h180},
	{quality: 'medium', layer: VideoPresets.h360},
	{quality: 'high', layer: VideoPresets.h720},
] as const satisfies ReadonlyArray<{quality: VoiceMediaGraphVideoQuality; layer: {width: number; height: number}}>;

for (let index = 1; index < CAMERA_SUBSCRIPTION_QUALITY_LAYERS.length; index += 1) {
	const previous = CAMERA_SUBSCRIPTION_QUALITY_LAYERS[index - 1].layer;
	const current = CAMERA_SUBSCRIPTION_QUALITY_LAYERS[index].layer;
	assert.ok(current.width > previous.width, 'camera subscription quality layers must ascend by width');
	assert.ok(current.height > previous.height, 'camera subscription quality layers must ascend by height');
}

export function pickCameraSubscriptionQuality(
	displayWidthPx: number,
	displayHeightPx: number,
): VoiceMediaGraphVideoQuality {
	assert.ok(Number.isFinite(displayWidthPx) && displayWidthPx >= 0, 'display width must be a non-negative number');
	assert.ok(Number.isFinite(displayHeightPx) && displayHeightPx >= 0, 'display height must be a non-negative number');
	let selected: VoiceMediaGraphVideoQuality = CAMERA_SUBSCRIPTION_QUALITY_LAYERS[0].quality;
	for (const {quality, layer} of CAMERA_SUBSCRIPTION_QUALITY_LAYERS) {
		selected = quality;
		if (layer.width >= displayWidthPx && layer.height >= displayHeightPx) {
			return selected;
		}
	}
	return selected;
}
