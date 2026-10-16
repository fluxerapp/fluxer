// SPDX-License-Identifier: AGPL-3.0-or-later

export const VOICE_ENGINE_V2_AUDIO_FRAME_BYTES_MAX = 1 << 20;
export const VOICE_ENGINE_V2_VIDEO_FRAME_BYTES_MAX = 64 << 20;
export const VOICE_ENGINE_V2_VIDEO_DIMENSION_MAX = 16384;
const VOICE_ENGINE_V2_AUDIO_SAMPLE_RATES_HZ = [16000, 32000, 48000] as const;
const VOICE_ENGINE_V2_AUDIO_CHANNELS_VALID = [1, 2] as const;

interface VoiceEngineV2AudioFrameInvariants {
	sampleRateHz: number;
	numChannels: number;
	frameBytes: number;
	timestampNs: number;
}

interface VoiceEngineV2VideoFrameInvariants {
	widthPx: number;
	heightPx: number;
	frameBytes: number;
	timestampNs: number;
}

export class VoiceEngineV2FfiAssertError extends Error {
	readonly code:
		| 'audioFrameBytesOutOfRange'
		| 'audioSampleRateInvalid'
		| 'audioChannelsInvalid'
		| 'audioTimestampRegressed'
		| 'videoFrameBytesOutOfRange'
		| 'videoDimensionOutOfRange'
		| 'videoTimestampRegressed';

	constructor(code: VoiceEngineV2FfiAssertError['code'], message: string) {
		super(message);
		this.code = code;
		this.name = 'VoiceEngineV2FfiAssertError';
	}
}

function isFiniteInteger(value: number): boolean {
	return Number.isFinite(value) && Number.isInteger(value);
}

function assertAudioFrameBytes(bytes: number): void {
	if (!isFiniteInteger(bytes)) {
		throw new VoiceEngineV2FfiAssertError(
			'audioFrameBytesOutOfRange',
			`audio frame bytes must be a finite integer (received ${bytes})`,
		);
	}
	if (bytes <= 0) {
		throw new VoiceEngineV2FfiAssertError(
			'audioFrameBytesOutOfRange',
			`audio frame bytes must be positive (received ${bytes})`,
		);
	}
	if (bytes > VOICE_ENGINE_V2_AUDIO_FRAME_BYTES_MAX) {
		throw new VoiceEngineV2FfiAssertError(
			'audioFrameBytesOutOfRange',
			`audio frame bytes ${bytes} exceeds cap ${VOICE_ENGINE_V2_AUDIO_FRAME_BYTES_MAX}`,
		);
	}
}

function assertAudioSampleRate(hz: number): void {
	if (!(VOICE_ENGINE_V2_AUDIO_SAMPLE_RATES_HZ as ReadonlyArray<number>).includes(hz)) {
		throw new VoiceEngineV2FfiAssertError(
			'audioSampleRateInvalid',
			`audio sample rate ${hz} not in [16000, 32000, 48000]`,
		);
	}
}

function assertAudioChannels(channels: number): void {
	if (!(VOICE_ENGINE_V2_AUDIO_CHANNELS_VALID as ReadonlyArray<number>).includes(channels)) {
		throw new VoiceEngineV2FfiAssertError('audioChannelsInvalid', `audio channel count ${channels} not in [1, 2]`);
	}
}

function assertAudioTimestampMonotonic(timestampNs: number, previousTimestampNs: number | null): void {
	if (previousTimestampNs === null) return;
	if (timestampNs <= previousTimestampNs) {
		throw new VoiceEngineV2FfiAssertError(
			'audioTimestampRegressed',
			`audio timestamp ${timestampNs} did not exceed previous ${previousTimestampNs}`,
		);
	}
}

export function assertAudioFrameInvariants(
	frame: VoiceEngineV2AudioFrameInvariants,
	previousTimestampNs: number | null = null,
): void {
	assertAudioFrameBytes(frame.frameBytes);
	assertAudioSampleRate(frame.sampleRateHz);
	assertAudioChannels(frame.numChannels);
	assertAudioTimestampMonotonic(frame.timestampNs, previousTimestampNs);
}

function assertVideoFrameBytes(bytes: number): void {
	if (!isFiniteInteger(bytes)) {
		throw new VoiceEngineV2FfiAssertError(
			'videoFrameBytesOutOfRange',
			`video frame bytes must be a finite integer (received ${bytes})`,
		);
	}
	if (bytes <= 0) {
		throw new VoiceEngineV2FfiAssertError(
			'videoFrameBytesOutOfRange',
			`video frame bytes must be positive (received ${bytes})`,
		);
	}
	if (bytes > VOICE_ENGINE_V2_VIDEO_FRAME_BYTES_MAX) {
		throw new VoiceEngineV2FfiAssertError(
			'videoFrameBytesOutOfRange',
			`video frame bytes ${bytes} exceeds cap ${VOICE_ENGINE_V2_VIDEO_FRAME_BYTES_MAX}`,
		);
	}
}

function assertVideoDimension(pixels: number, label: string): void {
	if (!isFiniteInteger(pixels)) {
		throw new VoiceEngineV2FfiAssertError(
			'videoDimensionOutOfRange',
			`video ${label} must be a finite integer (received ${pixels})`,
		);
	}
	if (pixels <= 0) {
		throw new VoiceEngineV2FfiAssertError(
			'videoDimensionOutOfRange',
			`video ${label} must be positive (received ${pixels})`,
		);
	}
	if (pixels > VOICE_ENGINE_V2_VIDEO_DIMENSION_MAX) {
		throw new VoiceEngineV2FfiAssertError(
			'videoDimensionOutOfRange',
			`video ${label} ${pixels} exceeds cap ${VOICE_ENGINE_V2_VIDEO_DIMENSION_MAX}`,
		);
	}
}

function assertVideoTimestampMonotonic(timestampNs: number, previousTimestampNs: number | null): void {
	if (previousTimestampNs === null) return;
	if (timestampNs <= previousTimestampNs) {
		throw new VoiceEngineV2FfiAssertError(
			'videoTimestampRegressed',
			`video timestamp ${timestampNs} did not exceed previous ${previousTimestampNs}`,
		);
	}
}

export function assertVideoFrameInvariants(
	frame: VoiceEngineV2VideoFrameInvariants,
	previousTimestampNs: number | null = null,
): void {
	assertVideoFrameBytes(frame.frameBytes);
	assertVideoDimension(frame.widthPx, 'width');
	assertVideoDimension(frame.heightPx, 'height');
	assertVideoTimestampMonotonic(frame.timestampNs, previousTimestampNs);
}
