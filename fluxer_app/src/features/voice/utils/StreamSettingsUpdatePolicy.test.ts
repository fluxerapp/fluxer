// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NativeAudioAvailability} from '@app/types/electron.d';
import {describe, expect, it} from 'vitest';
import {
	canSelectManualAudioSources,
	isLinuxDesktopAudioShare,
	manualAudioSourcesGovernShare,
	resolveWindowShareAudioScope,
	routesManualAudioSources,
	selectAppShareAudioRoute,
	shouldReconfigureAudioForActiveStreamSettings,
	supportsWindowShareAudioScope,
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
		expect(canSelectManualAudioSources({platform: 'linux', nativeAudioAvailability: LINUX_PIPEWIRE})).toBe(true);
		expect(canSelectManualAudioSources({platform: 'win32', nativeAudioAvailability: WINDOWS_WASAPI})).toBe(false);
		expect(canSelectManualAudioSources({platform: 'darwin', nativeAudioAvailability: MACOS_SCK})).toBe(false);
		expect(canSelectManualAudioSources({platform: 'linux', nativeAudioAvailability: null})).toBe(false);
		expect(
			canSelectManualAudioSources({
				platform: 'linux',
				nativeAudioAvailability: {available: false, backend: 'linux-pipewire', reason: 'no-pipewire'},
			}),
		).toBe(false);
		expect(
			canSelectManualAudioSources({
				platform: 'linux',
				nativeAudioAvailability: {available: true, capabilities: {process: false, system: true}},
			}),
		).toBe(false);
	});

	it('offers manual audio source selection on every desktop share type, with no opt-in left to satisfy', () => {
		expect(
			canSelectManualAudioSources({
				platform: 'linux',
				nativeAudioAvailability: LINUX_PIPEWIRE,
			}),
		).toBe(true);
		for (const displayShareEnvironment of ['desktop-custom', 'desktop-wayland'] as const) {
			expect(manualAudioSourcesGovernShare({platform: 'linux', displayShareEnvironment})).toBe(true);
		}
	});

	it('leaves the audio source decision to the platform wherever the stored mode cannot reach the capture', () => {
		expect(manualAudioSourcesGovernShare({platform: 'linux', displayShareEnvironment: 'web'})).toBe(false);
		for (const platform of ['win32', 'darwin', null, undefined]) {
			expect(manualAudioSourcesGovernShare({platform, displayShareEnvironment: 'desktop-custom'})).toBe(false);
		}
	});

	it('offers the window audio scope only to a window share whose window the capture layer can name', () => {
		expect(supportsWindowShareAudioScope({shareContext: 'app', displayShareEnvironment: 'desktop-custom'})).toBe(true);
		expect(supportsWindowShareAudioScope({shareContext: 'app', displayShareEnvironment: 'desktop-wayland'})).toBe(
			false,
		);
		expect(supportsWindowShareAudioScope({shareContext: 'app', displayShareEnvironment: 'web'})).toBe(false);
		expect(supportsWindowShareAudioScope({shareContext: 'app'})).toBe(false);
		for (const shareContext of ['device', 'display'] as const) {
			expect(supportsWindowShareAudioScope({shareContext, displayShareEnvironment: 'desktop-custom'})).toBe(false);
		}
	});

	it('defaults a window share to its own window and widens only where the user asked for it', () => {
		expect(resolveWindowShareAudioScope({shareContext: 'app', displayShareEnvironment: 'desktop-custom'})).toBe(
			'window',
		);
		expect(
			resolveWindowShareAudioScope({
				shareContext: 'app',
				displayShareEnvironment: 'desktop-custom',
				windowAudioScope: 'window',
			}),
		).toBe('window');
		expect(
			resolveWindowShareAudioScope({
				shareContext: 'app',
				displayShareEnvironment: 'desktop-custom',
				windowAudioScope: 'system',
			}),
		).toBe('system');
		expect(
			resolveWindowShareAudioScope({
				shareContext: 'app',
				displayShareEnvironment: 'desktop-wayland',
				windowAudioScope: 'window',
			}),
		).toBe('system');
		for (const shareContext of ['device', 'display'] as const) {
			expect(
				resolveWindowShareAudioScope({
					shareContext,
					displayShareEnvironment: 'desktop-custom',
					windowAudioScope: 'window',
				}),
			).toBe('system');
		}
	});

	it('keeps a window share on its own window until the user widens the scope', () => {
		for (const audioSourceMode of ['none', 'system', 'specific'] as const) {
			for (const selectedSourceCount of [0, 2]) {
				expect(selectAppShareAudioRoute({audioSourceMode, selectedSourceCount, windowAudioScope: 'window'})).toBe(
					'window',
				);
			}
		}
		expect(selectAppShareAudioRoute({windowAudioScope: 'window'})).toBe('window');
	});

	it('lets the stored selection govern a window share only once it is widened to the system', () => {
		expect(selectAppShareAudioRoute({audioSourceMode: 'system', windowAudioScope: 'system'})).toBe('system');
		expect(selectAppShareAudioRoute({windowAudioScope: 'system'})).toBe('system');
		expect(selectAppShareAudioRoute({audioSourceMode: 'none', windowAudioScope: 'system'})).toBe('none');
		expect(
			selectAppShareAudioRoute({audioSourceMode: 'specific', selectedSourceCount: 2, windowAudioScope: 'system'}),
		).toBe('apps');
		expect(
			selectAppShareAudioRoute({audioSourceMode: 'specific', selectedSourceCount: 0, windowAudioScope: 'system'}),
		).toBe('system');
	});

	it('routes manual audio sources only once specific sources are selected', () => {
		const base = {
			platform: 'linux',
			shareContext: 'device',
			nativeAudioAvailability: LINUX_PIPEWIRE,
		} as const;
		expect(routesManualAudioSources({...base, audioSourceMode: 'specific', selectedSourceCount: 1})).toBe(true);
		expect(routesManualAudioSources({...base, audioSourceMode: 'specific', selectedSourceCount: 0})).toBe(false);
		expect(routesManualAudioSources({...base, audioSourceMode: 'system', selectedSourceCount: 2})).toBe(false);
		expect(routesManualAudioSources({...base, audioSourceMode: 'none', selectedSourceCount: 2})).toBe(false);
		expect(
			routesManualAudioSources({
				...base,
				platform: 'win32',
				nativeAudioAvailability: WINDOWS_WASAPI,
				audioSourceMode: 'specific',
				selectedSourceCount: 2,
			}),
		).toBe(false);
	});
});
