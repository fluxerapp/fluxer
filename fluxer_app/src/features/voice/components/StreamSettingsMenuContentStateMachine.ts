// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScreenshareResolution, StreamingMode} from '@app/features/voice/state/VoiceSettings';
import {
	canRestartDisplayShareWithoutPreselectedSource,
	type DisplayShareEnvironment,
	prestartAudioToggleIsPickerOwned,
} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	normaliseResolutionForContext,
	normaliseStreamingModeForContext,
	resolveScreenShareQualityPick,
	type ScreenShareContext,
	type ScreenShareQualityInput,
	type ScreenShareQualityPatch,
	type ScreenShareQualityPick,
	type ScreenShareTarget,
	SUPPORTED_SCREEN_SHARE_FRAME_RATES,
	type SupportedScreenShareFrameRate,
} from '@app/features/voice/utils/ScreenShareOptions';
import {
	canSelectManualAudioSources,
	manualAudioSourcesGovernShare,
	resolveWindowShareAudioScope,
	type ScreenShareAudioSourceMode,
	type StreamSettingsShareContext,
	selectAppShareAudioRoute,
	type WindowShareAudioScope,
} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import type {NativeAudioAvailability} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {initialTransition, setup, transition} from 'xstate';

export const CAPTURE_DEVICES_USE_THE_GAMING_PRESET_DESCRIPTOR = msg({
	message: 'Capture devices use the Gaming preset.',
	comment:
		'Note shown beside the stored Screen share preset in the stream settings menu and the video settings tab of a capture device share, explaining why that preset is not used.',
});

export type StreamSettingsAudioControlStateValue =
	| 'hidden'
	| 'unsupported'
	| 'prestartNativePickerOwned'
	| 'restartRequired'
	| 'toggle';
export type StreamSettingsAudioControlLabelKey =
	| 'captureAppAudio'
	| 'captureDesktopAudio'
	| 'captureDeviceAudio'
	| 'captureSystemAudio';
type StreamSettingsNativeAudioUnsupportedScope = 'process' | 'system';

export interface StreamSettingsNativeAudioSignals {
	shareContext: StreamSettingsShareContext;
	platform?: string | null;
	nativeAudioAvailability: NativeAudioAvailability | null;
}

export interface StreamSettingsAudioControlSignals extends StreamSettingsNativeAudioSignals {
	applyToLiveStream: boolean;
	displayShareEnvironment: DisplayShareEnvironment;
	supportsStreamAudio: boolean;
	captureAudioEnabled: boolean;
	hasLiveScreenShareAudioPublication: boolean;
	audioSourceMode?: ScreenShareAudioSourceMode;
	selectedAudioSourceCount?: number;
	windowAudioScope?: WindowShareAudioScope;
}

export interface StreamSettingsAudioControlViewState {
	value: StreamSettingsAudioControlStateValue;
	checked: boolean;
	labelKey: StreamSettingsAudioControlLabelKey;
}

export interface StreamSettingsAudioMenuViewState {
	control: StreamSettingsAudioControlViewState;
	showManualAudioSources: boolean;
	showDeviceAudioMenu: boolean;
}

type StreamSettingsAudioControlEvent = {
	type: 'audio.evaluate';
	signals: StreamSettingsAudioControlSignals;
};

function selectStreamSettingsNativeAudioUnsupportedScope(
	shareContext: StreamSettingsShareContext,
): StreamSettingsNativeAudioUnsupportedScope | null {
	if (shareContext === 'app') return 'process';
	if (shareContext === 'display') return 'system';
	return null;
}

function selectStreamSettingsNativeAudioUnsupportedOnThisOs(signals: StreamSettingsNativeAudioSignals): boolean {
	const scope = selectStreamSettingsNativeAudioUnsupportedScope(signals.shareContext);
	return (
		scope != null &&
		(signals.platform === 'win32' || signals.platform === 'darwin') &&
		signals.nativeAudioAvailability != null &&
		(signals.nativeAudioAvailability.capabilities?.[scope] === false ||
			(!signals.nativeAudioAvailability.available && signals.nativeAudioAvailability.reason === 'os-version-too-old'))
	);
}

function shouldRestartToEnableDisplayAudio(signals: StreamSettingsAudioControlSignals): boolean {
	return (
		signals.applyToLiveStream &&
		signals.shareContext === 'display' &&
		!signals.captureAudioEnabled &&
		!signals.hasLiveScreenShareAudioPublication &&
		!canRestartDisplayShareWithoutPreselectedSource(signals.displayShareEnvironment) &&
		!canEnableNativeDisplayAudioWithoutRestart(signals)
	);
}

function canEnableNativeDisplayAudioWithoutRestart(signals: StreamSettingsAudioControlSignals): boolean {
	if (signals.displayShareEnvironment !== 'desktop-custom') return false;
	if (signals.platform === 'linux') return true;
	if (signals.platform !== 'darwin' && signals.platform !== 'win32') return false;
	if (!signals.nativeAudioAvailability) return true;
	return signals.nativeAudioAvailability.available && signals.nativeAudioAvailability.capabilities?.system !== false;
}

function shouldDisablePrestartNativeAudioToggle(signals: StreamSettingsAudioControlSignals): boolean {
	return (
		!signals.applyToLiveStream &&
		signals.shareContext === 'display' &&
		prestartAudioToggleIsPickerOwned(signals.displayShareEnvironment)
	);
}

export const streamSettingsAudioControlStateMachine = setup({
	types: {} as {
		events: StreamSettingsAudioControlEvent;
	},
	guards: {
		isHidden: ({event}) => !event.signals.supportsStreamAudio,
		isUnsupported: ({event}) => selectStreamSettingsNativeAudioUnsupportedOnThisOs(event.signals),
		isPrestartNativePickerOwned: ({event}) => shouldDisablePrestartNativeAudioToggle(event.signals),
		isRestartRequired: ({event}) => shouldRestartToEnableDisplayAudio(event.signals),
	},
}).createMachine({
	id: 'streamSettingsAudioControl',
	initial: 'hidden',
	on: {
		'audio.evaluate': [
			{target: '.hidden', guard: 'isHidden'},
			{target: '.unsupported', guard: 'isUnsupported'},
			{target: '.prestartNativePickerOwned', guard: 'isPrestartNativePickerOwned'},
			{target: '.restartRequired', guard: 'isRestartRequired'},
			{target: '.toggle'},
		],
	},
	states: {
		hidden: {},
		unsupported: {},
		prestartNativePickerOwned: {},
		restartRequired: {},
		toggle: {},
	},
});

export function selectStreamSettingsAudioControlState(
	signals: StreamSettingsAudioControlSignals,
): StreamSettingsAudioControlStateValue {
	const [snapshot] = transition(
		streamSettingsAudioControlStateMachine,
		initialTransition(streamSettingsAudioControlStateMachine)[0],
		{
			type: 'audio.evaluate',
			signals,
		},
	);
	return typeof snapshot.value === 'string' ? (snapshot.value as StreamSettingsAudioControlStateValue) : 'hidden';
}

function selectAudioControlLabelKey(signals: StreamSettingsAudioControlSignals): StreamSettingsAudioControlLabelKey {
	if (signals.shareContext === 'device') return 'captureDeviceAudio';
	if (signals.shareContext !== 'app') return 'captureDesktopAudio';
	const route = selectAppShareAudioRoute({
		audioSourceMode: signals.audioSourceMode,
		selectedSourceCount: signals.selectedAudioSourceCount,
		windowAudioScope: resolveWindowShareAudioScope(signals),
	});
	return route === 'system' ? 'captureSystemAudio' : 'captureAppAudio';
}

export function selectStreamSettingsAudioMenuState(
	signals: StreamSettingsAudioControlSignals,
): StreamSettingsAudioMenuViewState {
	const value = selectStreamSettingsAudioControlState(signals);
	return {
		control: {
			value,
			checked: signals.captureAudioEnabled,
			labelKey: selectAudioControlLabelKey(signals),
		},
		showManualAudioSources:
			signals.supportsStreamAudio &&
			signals.captureAudioEnabled &&
			manualAudioSourcesGovernShare(signals) &&
			canSelectManualAudioSources({
				platform: signals.platform,
				nativeAudioAvailability: signals.nativeAudioAvailability,
			}),
		showDeviceAudioMenu: signals.shareContext === 'device' && signals.captureAudioEnabled,
	};
}

export type StreamSettingsQualityWrite =
	| {kind: 'none'}
	| {kind: 'premium'}
	| {kind: 'write'; patch: ScreenShareQualityPatch};

export interface StreamSettingsQualitySignals {
	quality: ScreenShareQualityInput;
	pick: ScreenShareQualityPick;
	premiumOption: boolean;
	showPremiumFeatures: boolean;
}

export function selectStreamSettingsQualityWrite(signals: StreamSettingsQualitySignals): StreamSettingsQualityWrite {
	if (signals.premiumOption && !signals.quality.entitled) {
		return signals.showPremiumFeatures ? {kind: 'premium'} : {kind: 'none'};
	}
	const patch = resolveScreenShareQualityPick(signals.quality, signals.pick);
	return patch === null ? {kind: 'none'} : {kind: 'write', patch};
}

export const OFFERED_SCREEN_SHARE_RESOLUTIONS = [
	'low_480p',
	'medium',
	'high',
	'ultra',
	'source',
] as const satisfies ReadonlyArray<ScreenshareResolution>;

export type OfferedScreenShareResolution = (typeof OFFERED_SCREEN_SHARE_RESOLUTIONS)[number];

export function offeredScreenShareResolution(resolution: ScreenshareResolution): OfferedScreenShareResolution {
	return resolution === 'low_240p' ? 'low_480p' : resolution;
}

const STREAM_SETTINGS_FREE_RESOLUTIONS: ReadonlyArray<OfferedScreenShareResolution> = ['low_480p', 'medium'];
const STREAM_SETTINGS_PREMIUM_RESOLUTIONS: ReadonlyArray<OfferedScreenShareResolution> = ['high', 'ultra', 'source'];
const STREAM_SETTINGS_FREE_FRAME_RATES: ReadonlyArray<SupportedScreenShareFrameRate> = [15, 30];
const STREAM_SETTINGS_PREMIUM_FRAME_RATES: ReadonlyArray<SupportedScreenShareFrameRate> = [60];

export interface StreamSettingsQualityOption<T> {
	value: T;
	premium: boolean;
	selected: boolean;
	write: StreamSettingsQualityWrite;
}

export interface StreamSettingsQualityMenuViewState {
	resolutions: Array<StreamSettingsQualityOption<OfferedScreenShareResolution>>;
	frameRates: Array<StreamSettingsQualityOption<SupportedScreenShareFrameRate>>;
}

export interface StreamSettingsQualityMenuSignals {
	quality: ScreenShareQualityInput;
	target: ScreenShareTarget;
	showPremiumFeatures: boolean;
}

export function contextAllowsScreenShareResolution(
	resolution: OfferedScreenShareResolution,
	context: ScreenShareContext,
): boolean {
	return normaliseResolutionForContext(resolution, context, true) === resolution;
}

export function selectStreamSettingsQualityMenuState(
	signals: StreamSettingsQualityMenuSignals,
): StreamSettingsQualityMenuViewState {
	const offersPremium = signals.quality.entitled || signals.showPremiumFeatures;
	const selectedResolution = offeredScreenShareResolution(signals.target.resolution);
	const buildOption = <T>(
		value: T,
		premium: boolean,
		selected: boolean,
		pick: ScreenShareQualityPick,
	): StreamSettingsQualityOption<T> => ({
		value,
		premium,
		selected,
		write: selectStreamSettingsQualityWrite({
			quality: signals.quality,
			pick,
			premiumOption: premium,
			showPremiumFeatures: signals.showPremiumFeatures,
		}),
	});
	return {
		resolutions: OFFERED_SCREEN_SHARE_RESOLUTIONS.filter(
			(value) =>
				value === selectedResolution ||
				STREAM_SETTINGS_FREE_RESOLUTIONS.includes(value) ||
				(offersPremium &&
					STREAM_SETTINGS_PREMIUM_RESOLUTIONS.includes(value) &&
					contextAllowsScreenShareResolution(value, signals.quality.context)),
		).map((value) =>
			buildOption(value, STREAM_SETTINGS_PREMIUM_RESOLUTIONS.includes(value), value === selectedResolution, {
				axis: 'resolution',
				resolution: value,
			}),
		),
		frameRates: SUPPORTED_SCREEN_SHARE_FRAME_RATES.filter(
			(value) =>
				value === signals.target.frameRate ||
				STREAM_SETTINGS_FREE_FRAME_RATES.includes(value) ||
				(offersPremium && STREAM_SETTINGS_PREMIUM_FRAME_RATES.includes(value)),
		).map((value) =>
			buildOption(value, !STREAM_SETTINGS_FREE_FRAME_RATES.includes(value), value === signals.target.frameRate, {
				axis: 'frameRate',
				frameRate: value,
			}),
		),
	};
}

export function selectStreamSettingsPresetOverriddenByContext(
	mode: StreamingMode,
	context: ScreenShareContext,
): boolean {
	return normaliseStreamingModeForContext(mode, context) !== mode;
}
