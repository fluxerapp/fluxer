// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	canRestartDisplayShareWithoutPreselectedSource,
	type DisplayShareEnvironment,
	prestartAudioToggleIsPickerOwned,
} from '@app/features/voice/utils/ScreenShareEnvironment';
import {
	canSelectManualAudioSources,
	manualAudioSourcesGovernShare,
	type ScreenShareAudioSourceMode,
	type StreamSettingsShareContext,
} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import type {NativeAudioAvailability} from '@app/types/electron.d';
import {getInitialSnapshot, setup, transition} from 'xstate';

export type StreamSettingsAudioControlStateValue =
	| 'hidden'
	| 'unsupported'
	| 'prestartNativePickerOwned'
	| 'restartRequired'
	| 'toggle';
export type StreamSettingsAudioControlLabelKey = 'captureAppAudio' | 'captureDesktopAudio' | 'captureDeviceAudio';
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
	manualAudioSourcesOptIn?: boolean;
	audioSourceMode?: ScreenShareAudioSourceMode;
	selectedAudioSourceCount?: number;
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
		getInitialSnapshot(streamSettingsAudioControlStateMachine),
		{
			type: 'audio.evaluate',
			signals,
		},
	);
	return typeof snapshot.value === 'string' ? (snapshot.value as StreamSettingsAudioControlStateValue) : 'hidden';
}

function selectAudioControlLabelKey(shareContext: StreamSettingsShareContext): StreamSettingsAudioControlLabelKey {
	if (shareContext === 'device') return 'captureDeviceAudio';
	if (shareContext === 'app') return 'captureAppAudio';
	return 'captureDesktopAudio';
}

export function selectStreamSettingsAudioMenuState(
	signals: StreamSettingsAudioControlSignals,
): StreamSettingsAudioMenuViewState {
	const value = selectStreamSettingsAudioControlState(signals);
	return {
		control: {
			value,
			checked: signals.captureAudioEnabled,
			labelKey: selectAudioControlLabelKey(signals.shareContext),
		},
		showManualAudioSources:
			signals.supportsStreamAudio &&
			signals.captureAudioEnabled &&
			manualAudioSourcesGovernShare(signals) &&
			canSelectManualAudioSources({
				platform: signals.platform,
				shareContext: signals.shareContext,
				nativeAudioAvailability: signals.nativeAudioAvailability,
				manualOptIn: signals.manualAudioSourcesOptIn,
				audioSourceMode: signals.audioSourceMode,
				selectedSourceCount: signals.selectedAudioSourceCount,
			}),
		showDeviceAudioMenu: signals.shareContext === 'device' && signals.captureAudioEnabled,
	};
}
