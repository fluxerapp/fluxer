// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import type {NativeAudioAvailability} from '@app/types/electron.d';

export type StreamSettingsShareContext = 'app' | 'device' | 'display';
export type ScreenShareAudioSourceMode = 'none' | 'system' | 'specific';

export interface StreamSettingsUpdatePolicyInput {
	platform?: string | null;
	shareContext: StreamSettingsShareContext;
	audioSettingsChanged?: boolean;
}

export interface ManualAudioSourceSelectionInput {
	platform?: string | null;
	shareContext: StreamSettingsShareContext;
	nativeAudioAvailability?: NativeAudioAvailability | null;
	manualOptIn?: boolean;
	audioSourceMode?: ScreenShareAudioSourceMode;
	selectedSourceCount?: number;
}

export function manualAudioSourcesGovernShare(input: {
	shareContext: StreamSettingsShareContext;
	displayShareEnvironment?: DisplayShareEnvironment;
}): boolean {
	return !(input.shareContext === 'app' && input.displayShareEnvironment === 'desktop-custom');
}

export function isLinuxDesktopAudioShare(
	input: Pick<StreamSettingsUpdatePolicyInput, 'platform' | 'shareContext'>,
): boolean {
	return input.platform === 'linux' && input.shareContext !== 'device';
}

export function supportsManualScreenShareAudioSourceSelection(
	input: Pick<ManualAudioSourceSelectionInput, 'platform' | 'nativeAudioAvailability'>,
): boolean {
	if (input.platform !== 'linux') return false;
	const availability = input.nativeAudioAvailability;
	if (availability == null) return false;
	return availability.available === true && availability.capabilities?.process !== false;
}

export function maySupportManualScreenShareAudioSourceSelection(
	input: Pick<ManualAudioSourceSelectionInput, 'platform' | 'nativeAudioAvailability'>,
): boolean {
	if (input.platform !== 'linux') return false;
	if (input.nativeAudioAvailability == null) return true;
	return supportsManualScreenShareAudioSourceSelection(input);
}

export function canSelectManualAudioSources(input: ManualAudioSourceSelectionInput): boolean {
	return input.manualOptIn === true && supportsManualScreenShareAudioSourceSelection(input);
}

export function routesManualAudioSources(input: ManualAudioSourceSelectionInput): boolean {
	return (
		canSelectManualAudioSources(input) && input.audioSourceMode === 'specific' && (input.selectedSourceCount ?? 0) > 0
	);
}

export function shouldReconfigureAudioForActiveStreamSettings(input: StreamSettingsUpdatePolicyInput): boolean {
	if (input.audioSettingsChanged !== true) return false;
	if (isLinuxDesktopAudioShare(input)) return true;
	return input.shareContext === 'device';
}
