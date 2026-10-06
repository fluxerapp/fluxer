// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	contextAllowsScreenShareResolution,
	OFFERED_SCREEN_SHARE_RESOLUTIONS,
	type OfferedScreenShareResolution,
	offeredScreenShareResolution,
	selectStreamSettingsPresetOverriddenByContext,
} from '@app/features/voice/components/StreamSettingsMenuContentStateMachine';
import type {StreamingMode} from '@app/features/voice/state/VoiceSettings';
import type {ScreenShareContentHint} from '@app/features/voice/utils/CodecCapabilityDetector';
import {
	resolveScreenShareFrameRate,
	resolveScreenShareTarget,
	type ScreenShareContext,
	type ScreenShareQualityInput,
	SUPPORTED_SCREEN_SHARE_FRAME_RATES,
	type SupportedScreenShareFrameRate,
} from '@app/features/voice/utils/ScreenShareOptions';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const SCREEN_SHARE_PRESET_DESCRIPTORS: Record<Exclude<StreamingMode, 'custom'>, MessageDescriptor> = {
	gaming: msg({
		message: 'Gaming',
		comment: 'Video settings label for the motion-focused screen share quality preset.',
	}),
	screenshare: msg({
		message: 'Screen share',
		comment: 'Video settings label for the text-focused screen share quality preset.',
	}),
};

const FREE_SCREEN_SHARE_RESOLUTIONS: ReadonlyArray<OfferedScreenShareResolution> = ['low_480p', 'medium'];
const PREMIUM_SCREEN_SHARE_RESOLUTIONS: ReadonlyArray<OfferedScreenShareResolution> = ['high', 'ultra', 'source'];
const FREE_SCREEN_SHARE_FRAME_RATES: ReadonlyArray<SupportedScreenShareFrameRate> = [15, 30];
const PREMIUM_SCREEN_SHARE_FRAME_RATES: ReadonlyArray<SupportedScreenShareFrameRate> = [15, 30, 60];

export interface UserVideoTabScreenShareInput {
	quality: ScreenShareQualityInput;
	hintSetting: ScreenShareContentHint;
	selfHosted: boolean;
}

export interface UserVideoTabResolutionOption {
	value: OfferedScreenShareResolution;
	isDisabled: boolean;
}

export interface UserVideoTabScreenShareState {
	resolution: OfferedScreenShareResolution;
	frameRate: SupportedScreenShareFrameRate;
	resolutionOptions: ReadonlyArray<UserVideoTabResolutionOption>;
	frameRateOptions: ReadonlyArray<SupportedScreenShareFrameRate>;
	preset: Exclude<StreamingMode, 'custom'> | null;
	presetOverriddenByContext: boolean;
	saved: {resolution: OfferedScreenShareResolution; frameRate: SupportedScreenShareFrameRate} | null;
}

function buildResolutionOptions(
	entitled: boolean,
	selfHosted: boolean,
	context: ScreenShareContext,
	effective: OfferedScreenShareResolution,
): ReadonlyArray<UserVideoTabResolutionOption> {
	const offersPremium = entitled || !selfHosted;
	return OFFERED_SCREEN_SHARE_RESOLUTIONS.filter(
		(value) =>
			value === effective ||
			FREE_SCREEN_SHARE_RESOLUTIONS.includes(value) ||
			(offersPremium &&
				PREMIUM_SCREEN_SHARE_RESOLUTIONS.includes(value) &&
				contextAllowsScreenShareResolution(value, context)),
	).map((value) => ({value, isDisabled: !entitled && PREMIUM_SCREEN_SHARE_RESOLUTIONS.includes(value)}));
}

function buildFrameRateOptions(
	entitled: boolean,
	effective: SupportedScreenShareFrameRate,
): ReadonlyArray<SupportedScreenShareFrameRate> {
	const offered = entitled ? PREMIUM_SCREEN_SHARE_FRAME_RATES : FREE_SCREEN_SHARE_FRAME_RATES;
	return SUPPORTED_SCREEN_SHARE_FRAME_RATES.filter((value) => value === effective || offered.includes(value));
}

export function resolveUserVideoTabScreenShareState(input: UserVideoTabScreenShareInput): UserVideoTabScreenShareState {
	const target = resolveScreenShareTarget({...input.quality, sourceDimensions: null, hintSetting: input.hintSetting});
	const resolution = offeredScreenShareResolution(target.resolution);
	return {
		resolution,
		frameRate: target.frameRate,
		resolutionOptions: buildResolutionOptions(
			input.quality.entitled,
			input.selfHosted,
			input.quality.context,
			resolution,
		),
		frameRateOptions: buildFrameRateOptions(input.quality.entitled, target.frameRate),
		preset: input.quality.mode === 'custom' ? null : input.quality.mode,
		presetOverriddenByContext: selectStreamSettingsPresetOverriddenByContext(input.quality.mode, input.quality.context),
		saved: target.tierLimited
			? {
					resolution: offeredScreenShareResolution(input.quality.storedResolution),
					frameRate: resolveScreenShareFrameRate(input.quality.storedFrameRate),
				}
			: null,
	};
}
