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

	it('offers manual audio sources on every desktop share type, window shares included', () => {
		for (const shareContext of ['app', 'device', 'display'] as const) {
			for (const displayShareEnvironment of ['desktop-custom', 'desktop-wayland'] as const) {
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
				).toBe(true);
			}
		}
	});

	it('leaves audio source selection to the browser picker on the web', () => {
		for (const shareContext of ['app', 'device', 'display'] as const) {
			expect(
				selectStreamSettingsAudioMenuState(
					signals({
						shareContext,
						displayShareEnvironment: 'web',
						platform: 'linux',
						captureAudioEnabled: true,
						nativeAudioAvailability: availableNativeAudio(),
					}),
				).showManualAudioSources,
			).toBe(false);
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
				}),
			).showManualAudioSources,
		).toBe(false);
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					platform: 'linux',
					captureAudioEnabled: true,
					nativeAudioAvailability: availableNativeAudio({available: false, reason: 'no-pipewire'}),
				}),
			).showManualAudioSources,
		).toBe(false);
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					platform: 'linux',
					captureAudioEnabled: true,
					nativeAudioAvailability: null,
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
				}),
			).showManualAudioSources,
		).toBe(false);
	});

	it('names the real scope on a window share instead of always saying app audio', () => {
		const common = {
			shareContext: 'app',
			displayShareEnvironment: 'desktop-custom',
			platform: 'linux',
			captureAudioEnabled: true,
			nativeAudioAvailability: availableNativeAudio(),
		} as const;

		expect(selectStreamSettingsAudioMenuState(signals(common)).control.labelKey).toBe('captureAppAudio');
		expect(selectStreamSettingsAudioMenuState(signals({...common, windowAudioScope: 'window'})).control.labelKey).toBe(
			'captureAppAudio',
		);
		expect(selectStreamSettingsAudioMenuState(signals({...common, windowAudioScope: 'system'})).control.labelKey).toBe(
			'captureSystemAudio',
		);
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					...common,
					windowAudioScope: 'system',
					audioSourceMode: 'specific',
					selectedAudioSourceCount: 2,
				}),
			).control.labelKey,
		).toBe('captureAppAudio');
		expect(
			selectStreamSettingsAudioMenuState(signals({...common, windowAudioScope: 'system', audioSourceMode: 'none'}))
				.control.labelKey,
		).toBe('captureAppAudio');
	});

	it('keeps saying app audio on a window share whose stored display selection it does not follow', () => {
		const common = {
			shareContext: 'app',
			displayShareEnvironment: 'desktop-custom',
			platform: 'linux',
			captureAudioEnabled: true,
			nativeAudioAvailability: availableNativeAudio(),
			windowAudioScope: 'window',
		} as const;

		for (const audioSourceMode of ['none', 'system', 'specific'] as const) {
			expect(
				selectStreamSettingsAudioMenuState(signals({...common, audioSourceMode, selectedAudioSourceCount: 2})).control
					.labelKey,
			).toBe('captureAppAudio');
		}
	});

	it('never widens a Wayland window share label, because its window cannot be isolated', () => {
		expect(
			selectStreamSettingsAudioMenuState(
				signals({
					shareContext: 'app',
					displayShareEnvironment: 'desktop-wayland',
					platform: 'linux',
					captureAudioEnabled: true,
					nativeAudioAvailability: availableNativeAudio(),
					windowAudioScope: 'window',
				}),
			).control.labelKey,
		).toBe('captureSystemAudio');
	});

	it('keeps the audio device menu on a device share whether or not sources are routed', () => {
		const routed = selectStreamSettingsAudioMenuState(
			signals({
				shareContext: 'device',
				platform: 'linux',
				captureAudioEnabled: true,
				nativeAudioAvailability: availableNativeAudio(),
				audioSourceMode: 'specific',
				selectedAudioSourceCount: 2,
			}),
		);
		expect(routed.showManualAudioSources).toBe(true);
		expect(routed.showDeviceAudioMenu).toBe(true);
	});

	it('pins the audio menu shape for every share type on every platform', () => {
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
				const state = selectStreamSettingsAudioMenuState(
					signals({
						shareContext,
						platform,
						nativeAudioAvailability,
						captureAudioEnabled: true,
					}),
				);

				expect(state.control).toMatchObject({
					value: 'toggle',
					checked: true,
					labelKey: expectedLabelKey[shareContext],
				});
				expect(state.showManualAudioSources).toBe(manualCapable);
				expect(state.showDeviceAudioMenu).toBe(shareContext === 'device');
			}
		}
	});

	it('resolves the same audio group whether the menu is pre-start or attached to a live stream', () => {
		for (const shareContext of ['app', 'device', 'display'] as const) {
			for (const windowAudioScope of ['window', 'system'] as const) {
				const common = {
					shareContext,
					platform: 'linux',
					nativeAudioAvailability: availableNativeAudio(),
					captureAudioEnabled: true,
					windowAudioScope,
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
