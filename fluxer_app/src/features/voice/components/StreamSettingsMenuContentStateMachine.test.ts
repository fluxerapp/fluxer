// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type StreamSettingsAudioControlSignals,
	selectStreamSettingsAudioControlState,
	selectStreamSettingsAudioMenuState,
} from '@app/features/voice/components/StreamSettingsMenuContentStateMachine';
import type {NativeAudioAvailability} from '@app/types/electron.d';
import {describe, expect, it} from 'vitest';

function availableNativeAudio(overrides: Partial<NativeAudioAvailability> = {}): NativeAudioAvailability {
	return {
		available: true,
		capabilities: {
			process: true,
			system: true,
		},
		...overrides,
	};
}

function signals(overrides: Partial<StreamSettingsAudioControlSignals> = {}): StreamSettingsAudioControlSignals {
	return {
		applyToLiveStream: true,
		shareContext: 'display',
		displayShareEnvironment: 'desktop-custom',
		supportsStreamAudio: true,
		captureAudioEnabled: false,
		hasLiveScreenShareAudioPublication: false,
		nativeAudioAvailability: null,
		platform: null,
		...overrides,
	};
}

describe('StreamSettingsMenuContentStateMachine', () => {
	it('hides the audio control when stream audio capture is unavailable', () => {
		const state = selectStreamSettingsAudioMenuState(
			signals({
				supportsStreamAudio: false,
			}),
		);

		expect(state.control).toMatchObject({
			value: 'hidden',
			labelKey: 'captureDesktopAudio',
		});
		expect(state.showManualAudioSources).toBe(false);
	});

	it('models native audio support blocks before other display audio states', () => {
		const appState = selectStreamSettingsAudioMenuState(
			signals({
				shareContext: 'app',
				platform: 'win32',
				nativeAudioAvailability: availableNativeAudio({
					capabilities: {
						process: false,
						system: true,
					},
				}),
			}),
		);
		expect(appState.control).toMatchObject({
			value: 'unsupported',
			labelKey: 'captureAppAudio',
		});

		const displayState = selectStreamSettingsAudioMenuState(
			signals({
				displayShareEnvironment: 'web',
				platform: 'darwin',
				nativeAudioAvailability: availableNativeAudio({
					available: false,
					reason: 'os-version-too-old',
				}),
			}),
		);
		expect(displayState.control.value).toBe('unsupported');
	});

	it('disables prestart web desktop audio because the browser picker owns selection', () => {
		const state = selectStreamSettingsAudioMenuState(
			signals({
				applyToLiveStream: false,
				displayShareEnvironment: 'web',
			}),
		);

		expect(state.control).toMatchObject({
			value: 'prestartNativePickerOwned',
			labelKey: 'captureDesktopAudio',
		});
	});

	it('requires restart only when a live custom display share cannot add audio separately', () => {
		expect(selectStreamSettingsAudioControlState(signals({displayShareEnvironment: 'desktop-custom'}))).toBe(
			'restartRequired',
		);
		expect(
			selectStreamSettingsAudioControlState(
				signals({
					displayShareEnvironment: 'desktop-custom',
					platform: 'darwin',
					nativeAudioAvailability: availableNativeAudio(),
				}),
			),
		).toBe('toggle');
		expect(
			selectStreamSettingsAudioControlState(
				signals({
					displayShareEnvironment: 'desktop-custom',
					platform: 'linux',
				}),
			),
		).toBe('toggle');
		expect(
			selectStreamSettingsAudioControlState(
				signals({
					displayShareEnvironment: 'desktop-custom',
					hasLiveScreenShareAudioPublication: true,
				}),
			),
		).toBe('toggle');
		expect(
			selectStreamSettingsAudioControlState(
				signals({
					displayShareEnvironment: 'desktop-custom',
					captureAudioEnabled: true,
				}),
			),
		).toBe('toggle');
		expect(selectStreamSettingsAudioControlState(signals({displayShareEnvironment: 'web'}))).toBe('toggle');
	});

	it('hides manual audio sources for every share type until the advanced opt-in is on', () => {
		for (const shareContext of ['app', 'device', 'display'] as const) {
			const displayShareEnvironment = shareContext === 'app' ? 'desktop-wayland' : 'desktop-custom';
			expect(
				selectStreamSettingsAudioMenuState(
					signals({
						shareContext,
						displayShareEnvironment,
						platform: 'linux',
						captureAudioEnabled: true,
						nativeAudioAvailability: availableNativeAudio(),
					}),
				).showManualAudioSources,
			).toBe(false);
			expect(
				selectStreamSettingsAudioMenuState(
					signals({
						shareContext,
						displayShareEnvironment,
						platform: 'linux',
						captureAudioEnabled: true,
						nativeAudioAvailability: availableNativeAudio(),
						manualAudioSourcesOptIn: true,
					}),
				).showManualAudioSources,
			).toBe(true);
		}
	});

	it('keeps manual audio sources off the platforms that cannot express a selection', () => {
		for (const platform of ['win32', 'darwin', 'freebsd']) {
			expect(
				selectStreamSettingsAudioMenuState(
					signals({
						platform,
						captureAudioEnabled: true,
						nativeAudioAvailability: availableNativeAudio(),
						manualAudioSourcesOptIn: true,
					}),
				).showManualAudioSources,
			).toBe(false);
		}
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					platform: null,
					captureAudioEnabled: true,
					nativeAudioAvailability: availableNativeAudio(),
					manualAudioSourcesOptIn: true,
				}),
			).showManualAudioSources,
		).toBe(false);
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					platform: 'linux',
					captureAudioEnabled: true,
					nativeAudioAvailability: availableNativeAudio({available: false, reason: 'no-pipewire'}),
					manualAudioSourcesOptIn: true,
				}),
			).showManualAudioSources,
		).toBe(false);
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					platform: 'linux',
					captureAudioEnabled: true,
					nativeAudioAvailability: null,
					manualAudioSourcesOptIn: true,
				}),
			).showManualAudioSources,
		).toBe(false);
	});

	it('ties manual audio sources to the capture audio toggle and to stream audio support', () => {
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					platform: 'linux',
					captureAudioEnabled: false,
					nativeAudioAvailability: availableNativeAudio(),
					manualAudioSourcesOptIn: true,
				}),
			).showManualAudioSources,
		).toBe(false);
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					platform: 'linux',
					supportsStreamAudio: false,
					captureAudioEnabled: true,
					nativeAudioAvailability: availableNativeAudio(),
					manualAudioSourcesOptIn: true,
				}),
			).showManualAudioSources,
		).toBe(false);
	});

	it('keeps the audio device menu on a device share whether or not sources are routed', () => {
		const routed = selectStreamSettingsAudioMenuState(
			signals({
				shareContext: 'device',
				platform: 'linux',
				captureAudioEnabled: true,
				nativeAudioAvailability: availableNativeAudio(),
				manualAudioSourcesOptIn: true,
				audioSourceMode: 'specific',
				selectedAudioSourceCount: 2,
			}),
		);
		expect(routed.showManualAudioSources).toBe(true);
		expect(routed.showDeviceAudioMenu).toBe(true);
	});

	it('pins the audio menu shape for every share type on every platform, opt-in off and on', () => {
		const platforms = [
			{platform: 'linux', nativeAudioAvailability: availableNativeAudio(), manualCapable: true},
			{platform: 'win32', nativeAudioAvailability: availableNativeAudio(), manualCapable: false},
			{platform: 'darwin', nativeAudioAvailability: availableNativeAudio(), manualCapable: false},
			{platform: null, nativeAudioAvailability: null, manualCapable: false},
		] as const;
		const expectedLabelKey = {
			app: 'captureAppAudio',
			device: 'captureDeviceAudio',
			display: 'captureDesktopAudio',
		} as const;

		for (const {platform, nativeAudioAvailability, manualCapable} of platforms) {
			for (const shareContext of ['app', 'device', 'display'] as const) {
				for (const manualAudioSourcesOptIn of [false, true]) {
					const state = selectStreamSettingsAudioMenuState(
						signals({
							shareContext,
							platform,
							nativeAudioAvailability,
							captureAudioEnabled: true,
							manualAudioSourcesOptIn,
						}),
					);

					expect(state.control).toMatchObject({
						value: 'toggle',
						checked: true,
						labelKey: expectedLabelKey[shareContext],
					});
					expect(state.showManualAudioSources).toBe(manualCapable && manualAudioSourcesOptIn && shareContext !== 'app');
					expect(state.showDeviceAudioMenu).toBe(shareContext === 'device');
				}
			}
		}
	});

	it('offers manual sources on a window share only where the capture rule decides its audio', () => {
		const common = {
			shareContext: 'app',
			platform: 'linux',
			nativeAudioAvailability: availableNativeAudio(),
			captureAudioEnabled: true,
			manualAudioSourcesOptIn: true,
		} as const;

		expect(
			selectStreamSettingsAudioMenuState(signals({...common, displayShareEnvironment: 'desktop-custom'}))
				.showManualAudioSources,
		).toBe(false);
		expect(
			selectStreamSettingsAudioMenuState(signals({...common, displayShareEnvironment: 'desktop-wayland'}))
				.showManualAudioSources,
		).toBe(true);
	});

	it('resolves the same audio group whether the menu is pre-start or attached to a live stream', () => {
		for (const shareContext of ['app', 'device', 'display'] as const) {
			for (const manualAudioSourcesOptIn of [false, true]) {
				const common = {
					shareContext,
					platform: 'linux',
					nativeAudioAvailability: availableNativeAudio(),
					captureAudioEnabled: true,
					manualAudioSourcesOptIn,
				};
				const prestart = selectStreamSettingsAudioMenuState(signals({...common, applyToLiveStream: false}));
				const live = selectStreamSettingsAudioMenuState(signals({...common, applyToLiveStream: true}));

				expect(live.showManualAudioSources).toBe(prestart.showManualAudioSources);
				expect(live.showDeviceAudioMenu).toBe(prestart.showDeviceAudioMenu);
				expect(live.control.labelKey).toBe(prestart.control.labelKey);
			}
		}
	});

	it('keeps device audio menu visibility tied to the device share audio setting', () => {
		const disabledCapture = selectStreamSettingsAudioMenuState(
			signals({
				shareContext: 'device',
				captureAudioEnabled: false,
			}),
		);
		expect(disabledCapture.control).toMatchObject({
			value: 'toggle',
			labelKey: 'captureDeviceAudio',
			checked: false,
		});
		expect(disabledCapture.showDeviceAudioMenu).toBe(false);

		const enabledCapture = selectStreamSettingsAudioMenuState(
			signals({
				shareContext: 'device',
				captureAudioEnabled: true,
			}),
		);
		expect(enabledCapture.control).toMatchObject({
			value: 'toggle',
			labelKey: 'captureDeviceAudio',
			checked: true,
		});
		expect(enabledCapture.showDeviceAudioMenu).toBe(true);

		const unsupportedDeviceCapture = selectStreamSettingsAudioMenuState(
			signals({
				shareContext: 'device',
				supportsStreamAudio: false,
				captureAudioEnabled: true,
			}),
		);
		expect(unsupportedDeviceCapture.control.value).toBe('hidden');
		expect(unsupportedDeviceCapture.showDeviceAudioMenu).toBe(true);
	});
});
