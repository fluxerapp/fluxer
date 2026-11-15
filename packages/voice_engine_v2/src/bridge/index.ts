// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	assertAudioFrameInvariants,
	assertVideoFrameInvariants,
} from '@fluxer/voice_engine_v2/src/bridge/ffi_assertions';
import type {VoiceEngineV2HardwareEncoderCapabilities} from '@fluxer/voice_engine_v2/src/protocol/types';

export const VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL = 'voice-engine-v2:get-hardware-encoder-capabilities';

export interface VoiceEngineV2BridgeHardwareEncoderApi {
	getHardwareEncoderCapabilities(): Promise<VoiceEngineV2HardwareEncoderCapabilities>;
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
