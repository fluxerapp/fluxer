// SPDX-License-Identifier: AGPL-3.0-or-later

import {canSelectWindowsDesktopAudioSource} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import type {NativeAudioAvailability} from '@app/types/electron.d';
import {describe, expect, it} from 'vitest';

const withEndpointSelect: NativeAudioAvailability = {
	available: true,
	backend: 'windows-wasapi-loopback',
	capabilities: {process: true, system: true, systemExcludesSelf: true, sessionMixer: true, endpointSelect: true},
};
const withoutEndpointSelect: NativeAudioAvailability = {
	available: true,
	backend: 'windows-wasapi-loopback',
	capabilities: {process: true, system: true, systemExcludesSelf: true},
};

describe('canSelectWindowsDesktopAudioSource', () => {
	it('offers device selection for Windows entire-screen shares', () => {
		expect(
			canSelectWindowsDesktopAudioSource({
				platform: 'win32',
				shareContext: 'display',
				nativeAudioAvailability: withEndpointSelect,
			}),
		).toBe(true);
	});

	it('hides device selection elsewhere', () => {
		expect(
			canSelectWindowsDesktopAudioSource({
				platform: 'darwin',
				shareContext: 'display',
				nativeAudioAvailability: withEndpointSelect,
			}),
		).toBe(false);
		expect(
			canSelectWindowsDesktopAudioSource({
				platform: 'win32',
				shareContext: 'app',
				nativeAudioAvailability: withEndpointSelect,
			}),
		).toBe(false);
		expect(
			canSelectWindowsDesktopAudioSource({
				platform: 'win32',
				shareContext: 'display',
				nativeAudioAvailability: withoutEndpointSelect,
			}),
		).toBe(false);
		expect(
			canSelectWindowsDesktopAudioSource({platform: 'win32', shareContext: 'display', nativeAudioAvailability: null}),
		).toBe(false);
	});
});
