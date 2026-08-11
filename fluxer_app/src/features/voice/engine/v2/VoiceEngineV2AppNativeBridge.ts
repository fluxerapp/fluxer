// SPDX-License-Identifier: AGPL-3.0-or-later

import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {isNativeVoiceEngineSelected} from '@app/features/voice/engine/native_voice_engine/NativeVoiceEngineSelection';
import type {VoiceEngineV2BridgeApi, VoiceEngineV2BridgeCapabilities} from '@fluxer/voice_engine_v2/bridge';

function buildMissingBridgeError(context: string): Error {
	const error = new Error(`${context}: voice engine v2 native bridge is unavailable`);
	error.name = 'VoiceEngineV2AppNativeBridgeUnavailableError';
	return error;
}

export function getVoiceEngineV2AppNativeBridge(): VoiceEngineV2BridgeApi | null {
	return getElectronAPI()?.voiceEngine ?? null;
}

export function requireVoiceEngineV2AppNativeBridge(context: string): VoiceEngineV2BridgeApi {
	const bridge = getVoiceEngineV2AppNativeBridge();
	if (!bridge) throw buildMissingBridgeError(context);
	return bridge;
}

export async function getVoiceEngineV2AppNativeBridgeCapabilities(): Promise<VoiceEngineV2BridgeCapabilities | null> {
	const bridge = getVoiceEngineV2AppNativeBridge();
	if (!bridge) return null;
	return bridge.getCapabilities();
}

export async function isVoiceEngineV2AppNativeScreenShareBridgeAvailable(): Promise<boolean> {
	if (!isNativeVoiceEngineSelected()) return false;
	const capabilities = await getVoiceEngineV2AppNativeBridgeCapabilities();
	return capabilities?.screenShare === true;
}

export async function isVoiceEngineV2AppNativeScreenShareEncodingUpdateAvailable(): Promise<boolean> {
	if (!isNativeVoiceEngineSelected()) return false;
	const capabilities = await getVoiceEngineV2AppNativeBridgeCapabilities();
	return capabilities?.screenShareEncodingUpdate === true;
}

export async function isVoiceEngineV2AppNativeScreenShareAudioBridgeAvailable(): Promise<boolean> {
	if (!isNativeVoiceEngineSelected()) return false;
	const capabilities = await getVoiceEngineV2AppNativeBridgeCapabilities();
	return capabilities?.screenShareAudio === true;
}

export function isVoiceEngineV2AppNativeAudioDeviceBridgeAvailable(): boolean {
	if (!isNativeVoiceEngineSelected()) return false;
	const bridge = getVoiceEngineV2AppNativeBridge();
	if (!bridge) return false;
	return typeof bridge.listAudioInputDevices === 'function' && typeof bridge.listAudioOutputDevices === 'function';
}
