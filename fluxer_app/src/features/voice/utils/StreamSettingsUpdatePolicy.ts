// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import type {NativeAudioAvailability} from '@app/types/electron.d';

export type StreamSettingsShareContext = 'app' | 'device' | 'display';
export type ScreenShareAudioSourceMode = 'none' | 'system' | 'specific';
export type WindowShareAudioScope = 'window' | 'system';
export type AppShareAudioRoute = 'none' | 'window' | 'apps' | 'system';

export interface StreamSettingsUpdatePolicyInput {
	platform?: string | null;
	shareContext: StreamSettingsShareContext;
	audioSettingsChanged?: boolean;
}

export interface ManualAudioSourceSelectionInput {
	platform?: string | null;
	shareContext: StreamSettingsShareContext;
	nativeAudioAvailability?: NativeAudioAvailability | null;
	audioSourceMode?: ScreenShareAudioSourceMode;
	selectedSourceCount?: number;
}

export interface WindowShareAudioScopeInput {
	shareContext: StreamSettingsShareContext;
	displayShareEnvironment?: DisplayShareEnvironment;
	windowAudioScope?: WindowShareAudioScope;
}

export interface AppShareAudioRouteInput {
	audioSourceMode?: ScreenShareAudioSourceMode;
	selectedSourceCount?: number;
	windowAudioScope: WindowShareAudioScope;
}

export function manualAudioSourcesGovernShare(input: {
	platform?: string | null;
	displayShareEnvironment?: DisplayShareEnvironment;
}): boolean {
	return input.platform === 'linux' && input.displayShareEnvironment !== 'web';
}

export function supportsWindowShareAudioScope(input: WindowShareAudioScopeInput): boolean {
	if (input.shareContext !== 'app') return false;
	return input.displayShareEnvironment === 'desktop-custom';
}

export function resolveWindowShareAudioScope(input: WindowShareAudioScopeInput): WindowShareAudioScope {
	if (!supportsWindowShareAudioScope(input)) return 'system';
	return input.windowAudioScope ?? 'window';
}

export function selectAppShareAudioRoute(input: AppShareAudioRouteInput): AppShareAudioRoute {
	if (input.windowAudioScope !== 'system') return 'window';
	if (input.audioSourceMode === 'none') return 'none';
	if (input.audioSourceMode === 'specific' && (input.selectedSourceCount ?? 0) > 0) return 'apps';
	return 'system';
}

export function isLinuxDesktopAudioShare(
	input: Pick<StreamSettingsUpdatePolicyInput, 'platform' | 'shareContext'>,
): boolean {
	return input.platform === 'linux' && input.shareContext !== 'device';
}

export function canSelectManualAudioSources(
	input: Pick<ManualAudioSourceSelectionInput, 'platform' | 'nativeAudioAvailability'>,
): boolean {
	if (input.platform !== 'linux') return false;
	const availability = input.nativeAudioAvailability;
	if (availability == null) return false;
	return availability.available === true && availability.capabilities?.process !== false;
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
