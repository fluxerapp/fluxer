// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	selectWindowsDesktopAudioStart,
	windowsEndpointsEqual,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';
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

describe('selectWindowsDesktopAudioStart', () => {
	it('keeps entire system audio when the shell cannot pick devices', () => {
		expect(selectWindowsDesktopAudioStart({all: false, devices: ['default']}, withoutEndpointSelect)).toEqual({
			winCaptureScope: 'system',
		});
		expect(selectWindowsDesktopAudioStart({all: false, devices: ['default']}, null)).toEqual({
			winCaptureScope: 'system',
		});
	});

	it('keeps entire system audio when the user asked for it', () => {
		expect(selectWindowsDesktopAudioStart({all: true, devices: ['default']}, withEndpointSelect)).toEqual({
			winCaptureScope: 'system',
		});
	});

	it('limits capture to the chosen playback devices', () => {
		expect(
			selectWindowsDesktopAudioStart({all: false, devices: ['default', '{speakers}']}, withEndpointSelect),
		).toEqual({
			winCaptureScope: 'session-mixer',
			winEndpoints: ['default', '{speakers}'],
		});
	});
});

describe('windowsEndpointsEqual', () => {
	it('ignores order', () => {
		expect(windowsEndpointsEqual(['default', '{a}'], ['{a}', 'default'])).toBe(true);
	});

	it('compares contents and length', () => {
		expect(windowsEndpointsEqual(['default'], ['default', '{a}'])).toBe(false);
		expect(windowsEndpointsEqual(['default', '{a}'], ['default', '{b}'])).toBe(false);
	});
});
