// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2Error,
} from '@fluxer/voice_engine_v2/src/protocol';

export type VoiceEngineV2CameraEncodingPlanAction = 'noop' | 'updateEncoding' | 'republish' | 'reject';

export type VoiceEngineV2CameraEncodingPlanReason =
	| 'unchanged'
	| 'effectsOrEncodingChanged'
	| 'deviceChanged'
	| 'codecChanged'
	| 'missingPublishedCamera';

export interface VoiceEngineV2CameraEncodingPlanInput {
	published: VoiceEngineV2CameraOptions | null;
	desired: VoiceEngineV2CameraOptions | null;
	update: VoiceEngineV2CameraEncodingOptions;
}

export interface VoiceEngineV2CameraEncodingPlan {
	action: VoiceEngineV2CameraEncodingPlanAction;
	reason: VoiceEngineV2CameraEncodingPlanReason;
	desired: VoiceEngineV2CameraOptions | null;
	update: VoiceEngineV2CameraEncodingOptions | null;
	error: VoiceEngineV2Error | null;
}

function invalidCameraEncodingPlan(
	reason: VoiceEngineV2CameraEncodingPlanReason,
	message: string,
): VoiceEngineV2CameraEncodingPlan {
	return {
		action: 'reject',
		reason,
		desired: null,
		update: null,
		error: {
			code: 'invalidArgument',
			capability: 'camera',
			message,
		},
	};
}

export function applyVoiceEngineV2CameraEncodingOptions(
	current: VoiceEngineV2CameraOptions,
	update: VoiceEngineV2CameraEncodingOptions,
): VoiceEngineV2CameraOptions {
	const desired: VoiceEngineV2CameraOptions = {...current};
	if (update.deviceId !== undefined) desired.deviceId = update.deviceId;
	if (update.width !== undefined) desired.width = update.width;
	if (update.height !== undefined) desired.height = update.height;
	if (update.frameRate !== undefined) desired.frameRate = update.frameRate;
	if (update.codec !== undefined) desired.codec = update.codec;
	if (update.maxBitrateBps !== undefined) desired.maxBitrateBps = update.maxBitrateBps;
	if (update.mirror !== undefined) desired.mirror = update.mirror;
	if (update.backgroundMode !== undefined) desired.backgroundMode = update.backgroundMode;
	if (update.backgroundBlurStrength !== undefined) desired.backgroundBlurStrength = update.backgroundBlurStrength;
	if (update.backgroundCustomMediaPath !== undefined) {
		desired.backgroundCustomMediaPath = update.backgroundCustomMediaPath;
	}
	if (update.backgroundCustomMediaKind !== undefined) {
		desired.backgroundCustomMediaKind = update.backgroundCustomMediaKind;
	}
	return desired;
}

function sameCameraEncodingState(a: VoiceEngineV2CameraOptions, b: VoiceEngineV2CameraOptions): boolean {
	if (a.deviceId !== b.deviceId) return false;
	if (a.width !== b.width) return false;
	if (a.height !== b.height) return false;
	if (a.frameRate !== b.frameRate) return false;
	if (a.codec !== b.codec) return false;
	if (a.maxBitrateBps !== b.maxBitrateBps) return false;
	if (a.mirror !== b.mirror) return false;
	if (a.backgroundMode !== b.backgroundMode) return false;
	if (a.backgroundBlurStrength !== b.backgroundBlurStrength) return false;
	if (a.backgroundCustomMediaPath !== b.backgroundCustomMediaPath) return false;
	if (a.backgroundCustomMediaKind !== b.backgroundCustomMediaKind) return false;
	return true;
}

export function planVoiceEngineV2CameraEncodingChange(
	input: VoiceEngineV2CameraEncodingPlanInput,
): VoiceEngineV2CameraEncodingPlan {
	if (!input.published) {
		return invalidCameraEncodingPlan(
			'missingPublishedCamera',
			'Cannot update camera encoding without a published camera',
		);
	}
	const current = input.desired ?? input.published;
	const desired = applyVoiceEngineV2CameraEncodingOptions(current, input.update);
	if (sameCameraEncodingState(desired, input.published)) {
		return {action: 'noop', reason: 'unchanged', desired, update: null, error: null};
	}
	if (desired.deviceId !== input.published.deviceId) {
		return {action: 'republish', reason: 'deviceChanged', desired, update: null, error: null};
	}
	if (desired.codec !== input.published.codec) {
		return {action: 'republish', reason: 'codecChanged', desired, update: null, error: null};
	}
	return {action: 'updateEncoding', reason: 'effectsOrEncodingChanged', desired, update: input.update, error: null};
}
