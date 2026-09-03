// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NativeAudioAvailability} from '@app/types/electron.d';
import {describe, expect, it} from 'vitest';
import {
	canSelectManualAudioSources,
	isLinuxDesktopAudioShare,
	maySupportManualScreenShareAudioSourceSelection,
	routesManualAudioSources,
	shouldReconfigureAudioForActiveStreamSettings,
	supportsManualScreenShareAudioSourceSelection,
} from './StreamSettingsUpdatePolicy';

const LINUX_PIPEWIRE: NativeAudioAvailability = {
	available: true,
	backend: 'linux-pipewire',
	capabilities: {process: true, system: true, systemExcludesSelf: true},
};
const WINDOWS_WASAPI: NativeAudioAvailability = {
	available: true,
	backend: 'windows-wasapi-loopback',
	capabilities: {
		process: true,
		system: true,
		systemExcludesSelf: true,
		processInclude: true,
		processExclude: true,
		sessionMixer: true,
		systemLoopbackMode: 'process-exclude',
	},
};
const MACOS_SCK: NativeAudioAvailability = {
	available: true,
	backend: 'macos-sck',
	capabilities: {process: true, system: true, systemExcludesSelf: true},
};

describe('StreamSettingsUpdatePolicy', () => {
	it('reconfigures Linux audio only for live desktop audio shares with changed audio settings', () => {
		expect(
			shouldReconfigureAudioForActiveStreamSettings({
				platform: 'linux',
				shareContext: 'display',
				audioSettingsChanged: true,
			}),
		).toBe(true);
		expect(
			shouldReconfigureAudioForActiveStreamSettings({
				platform: 'linux',
				shareContext: 'app',
				audioSettingsChanged: true,
			}),
		).toBe(true);
		expect(
			shouldReconfigureAudioForActiveStreamSettings({
				platform: 'linux',
				shareContext: 'display',
				audioSettingsChanged: false,
			}),
		).toBe(false);
		expect(
			shouldReconfigureAudioForActiveStreamSettings({
				platform: 'win32',
				shareContext: 'display',
				audioSettingsChanged: true,
			}),
		).toBe(false);
		expect(isLinuxDesktopAudioShare({platform: 'linux', shareContext: 'device'})).toBe(false);
	});

	it('reconfigures a device share on any platform whenever its audio settings change', () => {
		for (const platform of ['linux', 'win32', 'darwin']) {
			expect(
				shouldReconfigureAudioForActiveStreamSettings({
					platform,
					shareContext: 'device',
					audioSettingsChanged: true,
				}),
			).toBe(true);
			expect(
				shouldReconfigureAudioForActiveStreamSettings({
					platform,
					shareContext: 'device',
					audioSettingsChanged: false,
				}),
			).toBe(false);
		}
	});

	it('offers manual audio source selection only where the capture layer can express it', () => {
		expect(
			supportsManualScreenShareAudioSourceSelection({platform: 'linux', nativeAudioAvailability: LINUX_PIPEWIRE}),
		).toBe(true);
		expect(
			supportsManualScreenShareAudioSourceSelection({platform: 'win32', nativeAudioAvailability: WINDOWS_WASAPI}),
		).toBe(false);
		expect(
			supportsManualScreenShareAudioSourceSelection({platform: 'darwin', nativeAudioAvailability: MACOS_SCK}),
		).toBe(false);
		expect(supportsManualScreenShareAudioSourceSelection({platform: 'linux', nativeAudioAvailability: null})).toBe(
			false,
		);
		expect(
			supportsManualScreenShareAudioSourceSelection({
				platform: 'linux',
				nativeAudioAvailability: {available: false, backend: 'linux-pipewire', reason: 'no-pipewire'},
			}),
		).toBe(false);
		expect(
			supportsManualScreenShareAudioSourceSelection({
				platform: 'linux',
				nativeAudioAvailability: {available: true, capabilities: {process: false, system: true}},
			}),
		).toBe(false);
	});

	it('shows the advanced toggle on Linux while the capture layer is still being probed', () => {
		expect(maySupportManualScreenShareAudioSourceSelection({platform: 'linux', nativeAudioAvailability: null})).toBe(
			true,
		);
		expect(
			maySupportManualScreenShareAudioSourceSelection({platform: 'linux', nativeAudioAvailability: LINUX_PIPEWIRE}),
		).toBe(true);
		expect(
			maySupportManualScreenShareAudioSourceSelection({
				platform: 'linux',
				nativeAudioAvailability: {available: false, backend: 'linux-pipewire', reason: 'no-pipewire'},
			}),
		).toBe(false);
		expect(maySupportManualScreenShareAudioSourceSelection({platform: 'win32', nativeAudioAvailability: null})).toBe(
			false,
		);
		expect(
			maySupportManualScreenShareAudioSourceSelection({platform: 'darwin', nativeAudioAvailability: MACOS_SCK}),
		).toBe(false);
		expect(maySupportManualScreenShareAudioSourceSelection({platform: null, nativeAudioAvailability: null})).toBe(
			false,
		);
	});

	it('keeps manual audio source selection behind the advanced opt-in in every share context', () => {
		for (const shareContext of ['app', 'device', 'display'] as const) {
			expect(
				canSelectManualAudioSources({
					platform: 'linux',
					shareContext,
					nativeAudioAvailability: LINUX_PIPEWIRE,
					manualOptIn: true,
				}),
			).toBe(true);
			expect(
				canSelectManualAudioSources({
					platform: 'linux',
					shareContext,
					nativeAudioAvailability: LINUX_PIPEWIRE,
				}),
			).toBe(false);
			expect(
				canSelectManualAudioSources({
					platform: 'win32',
					shareContext,
					nativeAudioAvailability: WINDOWS_WASAPI,
					manualOptIn: true,
				}),
			).toBe(false);
		}
	});

	it('routes manual audio sources only once specific sources are selected', () => {
		const base = {
			platform: 'linux',
			shareContext: 'device',
			nativeAudioAvailability: LINUX_PIPEWIRE,
			manualOptIn: true,
		} as const;
		expect(routesManualAudioSources({...base, audioSourceMode: 'specific', selectedSourceCount: 1})).toBe(true);
		expect(routesManualAudioSources({...base, audioSourceMode: 'specific', selectedSourceCount: 0})).toBe(false);
		expect(routesManualAudioSources({...base, audioSourceMode: 'system', selectedSourceCount: 2})).toBe(false);
		expect(routesManualAudioSources({...base, audioSourceMode: 'none', selectedSourceCount: 2})).toBe(false);
		expect(
			routesManualAudioSources({
				...base,
				manualOptIn: false,
				audioSourceMode: 'specific',
				selectedSourceCount: 2,
			}),
		).toBe(false);
	});
});
