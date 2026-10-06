// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	VoiceEngineV2Error,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
} from '@fluxer/voice_engine_v2/src/protocol';

export type VoiceEngineV2ScreenEncodingPlanAction = 'noop' | 'updateEncoding' | 'republish' | 'reject';

export type VoiceEngineV2ScreenEncodingPlanReason =
	| 'unchanged'
	| 'dimensionsOrBitrateChanged'
	| 'codecChanged'
	| 'hardwareEncoderChanged'
	| 'zeroCopyRequirementChanged'
	| 'missingPublishedScreen'
	| 'captureMismatch';

export interface VoiceEngineV2ScreenEncodingPlanInput {
	published: VoiceEngineV2ScreenOptions | null;
	desired: VoiceEngineV2ScreenOptions | null;
	update: VoiceEngineV2ScreenEncodingOptions;
}

export interface VoiceEngineV2ScreenEncodingPlan {
	action: VoiceEngineV2ScreenEncodingPlanAction;
	reason: VoiceEngineV2ScreenEncodingPlanReason;
	desired: VoiceEngineV2ScreenOptions | null;
	update: VoiceEngineV2ScreenEncodingOptions | null;
	error: VoiceEngineV2Error | null;
}

function invalidScreenEncodingPlan(
	reason: VoiceEngineV2ScreenEncodingPlanReason,
	message: string,
): VoiceEngineV2ScreenEncodingPlan {
	return {
		action: 'reject',
		reason,
		desired: null,
		update: null,
		error: {
			code: 'invalidArgument',
			capability: 'screen',
			message,
		},
	};
}

export function applyVoiceEngineV2ScreenEncodingOptions(
	current: VoiceEngineV2ScreenOptions,
	update: VoiceEngineV2ScreenEncodingOptions,
): VoiceEngineV2ScreenOptions {
	const desired = {
		...current,
		width: update.width,
		height: update.height,
		maxFramerate: update.frameRate ?? current.maxFramerate,
		maxBitrateBps: update.maxBitrateBps ?? current.maxBitrateBps,
		codec: update.codec ?? current.codec,
		hardwareEncoding: update.hardwareEncoding ?? current.hardwareEncoding,
		zeroCopyRequired: update.zeroCopyRequired ?? current.zeroCopyRequired,
	};
	if (desired.codec === undefined) delete desired.codec;
	if (desired.hardwareEncoding === undefined) delete desired.hardwareEncoding;
	if (desired.zeroCopyRequired === undefined) delete desired.zeroCopyRequired;
	return desired;
}

export function planVoiceEngineV2ScreenEncodingChange(
	input: VoiceEngineV2ScreenEncodingPlanInput,
): VoiceEngineV2ScreenEncodingPlan {
	if (!input.published) {
		return invalidScreenEncodingPlan(
			'missingPublishedScreen',
			'Cannot update screen encoding without a published screen',
		);
	}
	if (input.update.captureId !== input.published.captureId) {
		return invalidScreenEncodingPlan('captureMismatch', 'Cannot update screen encoding for a different capture id');
	}

	const current = input.desired ?? input.published;
	const desired = applyVoiceEngineV2ScreenEncodingOptions(current, input.update);
	if (
		desired.width === input.published.width &&
		desired.height === input.published.height &&
		desired.maxFramerate === input.published.maxFramerate &&
		desired.maxBitrateBps === input.published.maxBitrateBps &&
		desired.codec === input.published.codec &&
		desired.hardwareEncoding === input.published.hardwareEncoding &&
		desired.zeroCopyRequired === input.published.zeroCopyRequired
	) {
		return {action: 'noop', reason: 'unchanged', desired, update: null, error: null};
	}
	if (desired.codec !== input.published.codec) {
		return {action: 'republish', reason: 'codecChanged', desired, update: null, error: null};
	}
	if (desired.hardwareEncoding !== input.published.hardwareEncoding) {
		return {action: 'republish', reason: 'hardwareEncoderChanged', desired, update: null, error: null};
	}
	if (desired.zeroCopyRequired !== input.published.zeroCopyRequired) {
		return {action: 'republish', reason: 'zeroCopyRequirementChanged', desired, update: null, error: null};
	}
	return {action: 'updateEncoding', reason: 'dimensionsOrBitrateChanged', desired, update: input.update, error: null};
}
