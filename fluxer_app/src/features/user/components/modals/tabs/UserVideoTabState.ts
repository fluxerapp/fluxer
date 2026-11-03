// SPDX-License-Identifier: AGPL-3.0-or-later

import {SCREEN_SHARE_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
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
	type ScreenShareBitrateBudget,
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
	screenshare: SCREEN_SHARE_DESCRIPTOR,
};

const FREE_SCREEN_SHARE_RESOLUTIONS: ReadonlyArray<OfferedScreenShareResolution> = ['low_480p', 'medium'];
const PREMIUM_SCREEN_SHARE_RESOLUTIONS: ReadonlyArray<OfferedScreenShareResolution> = ['high', 'ultra', 'source'];
const FREE_SCREEN_SHARE_FRAME_RATES: ReadonlyArray<SupportedScreenShareFrameRate> = [15, 30];
const PREMIUM_SCREEN_SHARE_FRAME_RATES: ReadonlyArray<SupportedScreenShareFrameRate> = [15, 30, 60];
const SCREEN_SHARE_BITRATE_STEPS_BPS: ReadonlyArray<number> = [
	2_000_000, 4_000_000, 6_000_000, 8_000_000, 10_000_000, 15_000_000, 20_000_000, 25_000_000, 30_000_000, 40_000_000,
	50_000_000,
];

export interface UserVideoTabScreenShareInput {
	quality: ScreenShareQualityInput;
	hintSetting: ScreenShareContentHint;
	bitrate: ScreenShareBitrateBudget;
	selfHosted: boolean;
}

interface UserVideoTabResolutionOption {
	value: OfferedScreenShareResolution;
	isDisabled: boolean;
}

export interface UserVideoTabScreenShareState {
	resolution: OfferedScreenShareResolution;
	frameRate: SupportedScreenShareFrameRate;
	resolutionOptions: ReadonlyArray<UserVideoTabResolutionOption>;
	frameRateOptions: ReadonlyArray<SupportedScreenShareFrameRate>;
	bitrate: number | null;
	bitrateOptions: ReadonlyArray<number>;
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

function buildBitrateOptions(budget: ScreenShareBitrateBudget): ReadonlyArray<number> {
	const offered = new Set(SCREEN_SHARE_BITRATE_STEPS_BPS.filter((value) => value < budget.ceilingBps));
	offered.add(budget.ceilingBps);
	if (budget.chosenBps !== null) offered.add(budget.chosenBps);
	return [...offered].sort((a, b) => a - b);
}

export function resolveUserVideoTabScreenShareState(input: UserVideoTabScreenShareInput): UserVideoTabScreenShareState {
	const target = resolveScreenShareTarget({
		...input.quality,
		sourceDimensions: null,
		hintSetting: input.hintSetting,
		bitrate: input.bitrate,
	});
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
		bitrate: input.bitrate.chosenBps,
		bitrateOptions: buildBitrateOptions(input.bitrate),
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
