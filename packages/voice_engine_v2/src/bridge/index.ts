// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2HardwareEncoderCapabilities} from '../protocol';
import {assertAudioFrameInvariants, assertVideoFrameInvariants} from './ffi_assertions';

export const VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL = 'voice-engine-v2:get-hardware-encoder-capabilities';

export type VoiceEngineV2BridgeHardwareEncoderCapabilities = VoiceEngineV2HardwareEncoderCapabilities;

export interface VoiceEngineV2BridgeHardwareEncoderApi {
	getHardwareEncoderCapabilities(): Promise<VoiceEngineV2BridgeHardwareEncoderCapabilities>;
}

export function assertVoiceEngineV2BridgeAudioOptionsInvariants(options: {
	sampleRate: number;
	numChannels: number;
}): void {
	assertAudioFrameInvariants({
		sampleRateHz: options.sampleRate,
		numChannels: options.numChannels,
		frameBytes: 1,
		timestampNs: 1,
	});
}

export function assertVoiceEngineV2BridgeVideoOptionsInvariants(options: {width: number; height: number}): void {
	assertVideoFrameInvariants({
		widthPx: options.width,
		heightPx: options.height,
		frameBytes: 1,
		timestampNs: 1,
	});
}

export function unavailableVoiceEngineV2BridgeHardwareEncoderCapabilities(
	reason: NonNullable<VoiceEngineV2BridgeHardwareEncoderCapabilities['reason']>,
	detail?: string,
): VoiceEngineV2BridgeHardwareEncoderCapabilities {
	return {
		available: false,
		backend: 'none',
		compiled: false,
		runtime: false,
		codecs: [],
		zeroCopy: false,
		nativeInputs: [],
		reason,
		...(detail ? {detail} : {}),
	};
}

export function normalizeVoiceEngineV2BridgeHardwareEncoderCapabilities(
	value: unknown,
): VoiceEngineV2BridgeHardwareEncoderCapabilities {
	if (typeof value !== 'object' || value === null) {
		return unavailableVoiceEngineV2BridgeHardwareEncoderCapabilities(
			'query-failed',
			'Native addon returned an invalid result',
		);
	}
	const candidate = value as Partial<VoiceEngineV2BridgeHardwareEncoderCapabilities>;
	const backend = typeof candidate.backend === 'string' && candidate.backend.length > 0 ? candidate.backend : 'none';
	return {
		available: candidate.available === true,
		backend,
		compiled: candidate.compiled === true,
		runtime: candidate.runtime === true,
		codecs: Array.isArray(candidate.codecs) ? candidate.codecs.filter((codec) => typeof codec === 'string') : [],
		zeroCopy: candidate.zeroCopy === true,
		nativeInputs: Array.isArray(candidate.nativeInputs)
			? candidate.nativeInputs.filter((input) => typeof input === 'string')
			: [],
		...(typeof candidate.reason === 'string' ? {reason: candidate.reason} : {}),
		...(typeof candidate.detail === 'string' ? {detail: candidate.detail} : {}),
	};
}
