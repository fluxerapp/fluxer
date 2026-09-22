// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	VoiceEngineV2HardwareEncoderCapabilities,
	VoiceEngineV2VideoCodec,
} from '@fluxer/voice_engine_v2/src/protocol';

const ZERO_COPY_NATIVE_INPUTS = new Set(['dmabuf', 'd3d11Texture', 'd3d11-texture', 'cvPixelBuffer', 'sharedTexture']);

export function unavailableVoiceEngineV2HardwareEncoderCapabilities(
	reason: NonNullable<VoiceEngineV2HardwareEncoderCapabilities['reason']>,
	detail?: string,
): VoiceEngineV2HardwareEncoderCapabilities {
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

export function normalizeVoiceEngineV2HardwareEncoderCapabilities(
	value: unknown,
): VoiceEngineV2HardwareEncoderCapabilities {
	if (typeof value !== 'object' || value === null) {
		return unavailableVoiceEngineV2HardwareEncoderCapabilities(
			'query-failed',
			'Native addon returned an invalid result',
		);
	}
	const candidate = value as Partial<VoiceEngineV2HardwareEncoderCapabilities>;
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

export function hasVoiceEngineV2ZeroCopyNativeInput(
	capabilities: VoiceEngineV2HardwareEncoderCapabilities | null | undefined,
): boolean {
	return (
		capabilities?.zeroCopy === true && capabilities.nativeInputs.some((input) => ZERO_COPY_NATIVE_INPUTS.has(input))
	);
}

export function hasVoiceEngineV2NativeNvencEncoder(
	capabilities: VoiceEngineV2HardwareEncoderCapabilities | null | undefined,
	codec: VoiceEngineV2VideoCodec | string,
): boolean {
	if (!capabilities?.available || capabilities.backend !== 'nvenc') return false;
	if (!hasVoiceEngineV2ZeroCopyNativeInput(capabilities)) return false;
	if (codec !== 'h264' && codec !== 'h265') return false;
	const codecs = new Set(capabilities.codecs.map((entry) => entry.toLowerCase()));
	return codecs.has(codec) || (codec === 'h265' && codecs.has('hevc'));
}

export function hasVoiceEngineV2NativeHardwareEncoder(
	capabilities: VoiceEngineV2HardwareEncoderCapabilities | null | undefined,
	codec: VoiceEngineV2VideoCodec | string,
): boolean {
	if (!capabilities?.available || capabilities.backend === 'none') return false;
	if (codec !== 'h264' && codec !== 'h265') return false;
	const codecs = new Set(capabilities.codecs.map((entry) => entry.toLowerCase()));
	const hasCodec = codecs.has(codec) || (codec === 'h265' && codecs.has('hevc'));
	if (!hasCodec) return false;
	if (capabilities.backend === 'nvenc') return hasVoiceEngineV2ZeroCopyNativeInput(capabilities);
	return true;
}
