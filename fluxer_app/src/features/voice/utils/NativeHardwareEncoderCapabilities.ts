// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {
	hasVoiceEngineV2NativeHardwareEncoder,
	hasVoiceEngineV2NativeNvencEncoder,
	normalizeVoiceEngineV2HardwareEncoderCapabilities,
	unavailableVoiceEngineV2HardwareEncoderCapabilities,
	type VoiceEngineV2HardwareEncoderCapabilities,
} from '@fluxer/voice_engine_v2';
import type {VideoCodec} from 'livekit-client';

const logger = new Logger('NativeHardwareEncoderCapabilities');

let cachedCapabilities: VoiceEngineV2HardwareEncoderCapabilities | null = null;
let pendingPromise: Promise<VoiceEngineV2HardwareEncoderCapabilities | null> | null = null;

function fetchCapabilities(): Promise<VoiceEngineV2HardwareEncoderCapabilities | null> {
	if (!isDesktop()) return Promise.resolve(null);
	const voiceEngine = getElectronAPI()?.voiceEngine;
	if (!voiceEngine?.getHardwareEncoderCapabilities) return Promise.resolve(null);
	return voiceEngine
		.getHardwareEncoderCapabilities()
		.then((capabilities) => {
			const normalized = normalizeVoiceEngineV2HardwareEncoderCapabilities(capabilities);
			cachedCapabilities = normalized;
			return normalized;
		})
		.catch((error) => {
			logger.warn('Failed to query native hardware encoder capabilities', {error});
			cachedCapabilities = unavailableVoiceEngineV2HardwareEncoderCapabilities('query-failed');
			return cachedCapabilities;
		});
}

export function loadNativeHardwareEncoderCapabilities(): Promise<VoiceEngineV2HardwareEncoderCapabilities | null> {
	if (cachedCapabilities) return Promise.resolve(cachedCapabilities);
	if (pendingPromise) return pendingPromise;
	pendingPromise = fetchCapabilities().finally(() => {
		pendingPromise = null;
	});
	return pendingPromise;
}

export function getNativeHardwareEncoderCapabilitiesSync(): VoiceEngineV2HardwareEncoderCapabilities | null {
	return cachedCapabilities;
}

export function hasNativeNvencEncoder(codec: VideoCodec): boolean {
	return hasVoiceEngineV2NativeNvencEncoder(getNativeHardwareEncoderCapabilitiesSync(), codec);
}

export function hasNativeHardwareEncoder(codec: VideoCodec): boolean {
	return hasVoiceEngineV2NativeHardwareEncoder(getNativeHardwareEncoderCapabilitiesSync(), codec);
}

export function resetNativeHardwareEncoderCapabilities(): void {
	cachedCapabilities = null;
	pendingPromise = null;
}
