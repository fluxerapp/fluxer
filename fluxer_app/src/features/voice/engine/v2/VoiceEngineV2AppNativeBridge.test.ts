// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

const nativeVoiceEngineSelected = vi.fn<() => boolean>();
const electronApi = vi.fn<() => unknown>();

vi.mock('@app/features/voice/engine/native_voice_engine/NativeVoiceEngineSelection', () => ({
	isNativeVoiceEngineSelected: () => nativeVoiceEngineSelected(),
}));

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	getElectronAPI: () => electronApi(),
}));

const {
	isVoiceEngineV2AppNativeAudioDeviceBridgeAvailable,
	isVoiceEngineV2AppNativeScreenShareAudioBridgeAvailable,
	isVoiceEngineV2AppNativeScreenShareBridgeAvailable,
	isVoiceEngineV2AppNativeScreenShareEncodingUpdateAvailable,
} = await import('./VoiceEngineV2AppNativeBridge');

const FULLY_CAPABLE_BRIDGE = {
	getCapabilities: async () => ({
		screenShare: true,
		screenShareAudio: true,
		screenShareEncodingUpdate: true,
	}),
	listAudioInputDevices: async () => [],
	listAudioOutputDevices: async () => [],
};

describe('VoiceEngineV2AppNativeBridge availability', () => {
	beforeEach(() => {
		electronApi.mockReturnValue({voiceEngine: FULLY_CAPABLE_BRIDGE});
	});

	it('reports native capabilities when the native voice engine is selected', async () => {
		nativeVoiceEngineSelected.mockReturnValue(true);
		expect(await isVoiceEngineV2AppNativeScreenShareBridgeAvailable()).toBe(true);
		expect(await isVoiceEngineV2AppNativeScreenShareEncodingUpdateAvailable()).toBe(true);
		expect(await isVoiceEngineV2AppNativeScreenShareAudioBridgeAvailable()).toBe(true);
		expect(isVoiceEngineV2AppNativeAudioDeviceBridgeAvailable()).toBe(true);
	});

	it('reports nothing available when the native voice engine is not selected, even on a fully capable bridge', async () => {
		nativeVoiceEngineSelected.mockReturnValue(false);
		expect(await isVoiceEngineV2AppNativeScreenShareBridgeAvailable()).toBe(false);
		expect(await isVoiceEngineV2AppNativeScreenShareEncodingUpdateAvailable()).toBe(false);
		expect(await isVoiceEngineV2AppNativeScreenShareAudioBridgeAvailable()).toBe(false);
		expect(isVoiceEngineV2AppNativeAudioDeviceBridgeAvailable()).toBe(false);
	});

	it('does not query the bridge at all when the native voice engine is not selected', async () => {
		nativeVoiceEngineSelected.mockReturnValue(false);
		const getCapabilities = vi.fn(async () => ({screenShare: true}));
		electronApi.mockReturnValue({voiceEngine: {...FULLY_CAPABLE_BRIDGE, getCapabilities}});
		await isVoiceEngineV2AppNativeScreenShareBridgeAvailable();
		expect(getCapabilities).not.toHaveBeenCalled();
	});
});
