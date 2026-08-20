// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	assertAudioFrameInvariants,
	assertVideoFrameInvariants,
	VOICE_ENGINE_V2_AUDIO_FRAME_BYTES_MAX,
	VOICE_ENGINE_V2_VIDEO_DIMENSION_MAX,
	VOICE_ENGINE_V2_VIDEO_FRAME_BYTES_MAX,
	VoiceEngineV2FfiAssertError,
} from './ffi_assertions';
import {
	assertVoiceEngineV2BridgeAudioOptionsInvariants,
	assertVoiceEngineV2BridgeVideoOptionsInvariants,
	normalizeVoiceEngineV2BridgeHardwareEncoderCapabilities,
	unavailableVoiceEngineV2BridgeHardwareEncoderCapabilities,
	VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL,
} from './index';

describe('voice engine v2 bridge contract', () => {
	it('pins the hardware encoder ipc channel shared by preload and the main process', () => {
		expect(VOICE_ENGINE_V2_HARDWARE_ENCODER_IPC_CHANNEL).toBe('voice-engine-v2:get-hardware-encoder-capabilities');
	});
});

describe('voice engine v2 audio frame invariants', () => {
	const canonicalAudioFrame = {
		sampleRateHz: 48000,
		numChannels: 2,
		frameBytes: 1920,
		timestampNs: 1000,
	};

	it('accepts a canonical audio frame with no previous timestamp', () => {
		expect(() => assertAudioFrameInvariants(canonicalAudioFrame)).not.toThrow();
	});

	it('accepts a canonical audio frame with an older previous timestamp', () => {
		expect(() => assertAudioFrameInvariants(canonicalAudioFrame, 500)).not.toThrow();
	});

	it('rejects an empty audio frame', () => {
		try {
			assertAudioFrameInvariants({...canonicalAudioFrame, frameBytes: 0});
			expect.fail('expected frame bytes assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('audioFrameBytesOutOfRange');
		}
	});

	it('rejects an audio frame past the byte cap', () => {
		try {
			assertAudioFrameInvariants({...canonicalAudioFrame, frameBytes: VOICE_ENGINE_V2_AUDIO_FRAME_BYTES_MAX + 1});
			expect.fail('expected frame bytes assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('audioFrameBytesOutOfRange');
		}
	});

	it('rejects an unsupported sample rate', () => {
		try {
			assertAudioFrameInvariants({...canonicalAudioFrame, sampleRateHz: 44100});
			expect.fail('expected sample rate assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('audioSampleRateInvalid');
		}
	});

	it('rejects an unsupported channel count', () => {
		try {
			assertAudioFrameInvariants({...canonicalAudioFrame, numChannels: 3});
			expect.fail('expected channels assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('audioChannelsInvalid');
		}
	});

	it('rejects a repeated audio timestamp', () => {
		try {
			assertAudioFrameInvariants(canonicalAudioFrame, canonicalAudioFrame.timestampNs);
			expect.fail('expected timestamp assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('audioTimestampRegressed');
		}
	});

	it('rejects a regressed audio timestamp', () => {
		try {
			assertAudioFrameInvariants(canonicalAudioFrame, canonicalAudioFrame.timestampNs + 1);
			expect.fail('expected timestamp assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('audioTimestampRegressed');
		}
	});
});

describe('voice engine v2 video frame invariants', () => {
	const canonicalVideoFrame = {
		widthPx: 1920,
		heightPx: 1080,
		frameBytes: 1920 * 1080 * 4,
		timestampNs: 1000,
	};

	it('accepts a canonical video frame', () => {
		expect(() => assertVideoFrameInvariants(canonicalVideoFrame)).not.toThrow();
	});

	it('rejects an empty video frame', () => {
		try {
			assertVideoFrameInvariants({...canonicalVideoFrame, frameBytes: 0});
			expect.fail('expected frame bytes assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('videoFrameBytesOutOfRange');
		}
	});

	it('rejects a video frame past the byte cap', () => {
		try {
			assertVideoFrameInvariants({...canonicalVideoFrame, frameBytes: VOICE_ENGINE_V2_VIDEO_FRAME_BYTES_MAX + 1});
			expect.fail('expected frame bytes assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('videoFrameBytesOutOfRange');
		}
	});

	it('rejects a zero video dimension', () => {
		try {
			assertVideoFrameInvariants({...canonicalVideoFrame, widthPx: 0});
			expect.fail('expected dimension assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('videoDimensionOutOfRange');
		}
	});

	it('rejects a video dimension past the cap', () => {
		try {
			assertVideoFrameInvariants({...canonicalVideoFrame, heightPx: VOICE_ENGINE_V2_VIDEO_DIMENSION_MAX + 1});
			expect.fail('expected dimension assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('videoDimensionOutOfRange');
		}
	});

	it('rejects a repeated video timestamp', () => {
		try {
			assertVideoFrameInvariants(canonicalVideoFrame, canonicalVideoFrame.timestampNs);
			expect.fail('expected timestamp assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('videoTimestampRegressed');
		}
	});
});

describe('voice engine v2 bridge option airlock helpers', () => {
	it('accepts canonical audio options', () => {
		expect(() => assertVoiceEngineV2BridgeAudioOptionsInvariants({sampleRate: 48_000, numChannels: 2})).not.toThrow();
	});

	it('rejects audio options with an unsupported sample rate', () => {
		try {
			assertVoiceEngineV2BridgeAudioOptionsInvariants({sampleRate: 44_100, numChannels: 2});
			expect.fail('expected sample rate assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('audioSampleRateInvalid');
		}
	});

	it('rejects audio options with an unsupported channel count', () => {
		try {
			assertVoiceEngineV2BridgeAudioOptionsInvariants({sampleRate: 48_000, numChannels: 5});
			expect.fail('expected channels assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('audioChannelsInvalid');
		}
	});

	it('accepts canonical video options', () => {
		expect(() => assertVoiceEngineV2BridgeVideoOptionsInvariants({width: 1280, height: 720})).not.toThrow();
	});

	it('rejects video options with a zero width', () => {
		try {
			assertVoiceEngineV2BridgeVideoOptionsInvariants({width: 0, height: 720});
			expect.fail('expected dimension assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('videoDimensionOutOfRange');
		}
	});

	it('rejects video options with an oversized height', () => {
		try {
			assertVoiceEngineV2BridgeVideoOptionsInvariants({width: 1280, height: 1 << 20});
			expect.fail('expected dimension assertion to throw');
		} catch (error) {
			expect(error).toBeInstanceOf(VoiceEngineV2FfiAssertError);
			expect((error as VoiceEngineV2FfiAssertError).code).toBe('videoDimensionOutOfRange');
		}
	});
});

describe('voice engine v2 bridge hardware encoder capabilities', () => {
	it('reports an unavailable encoder with the supplied reason and detail', () => {
		expect(unavailableVoiceEngineV2BridgeHardwareEncoderCapabilities('load-failed', 'addon missing')).toEqual({
			available: false,
			backend: 'none',
			compiled: false,
			runtime: false,
			codecs: [],
			zeroCopy: false,
			nativeInputs: [],
			reason: 'load-failed',
			detail: 'addon missing',
		});
	});

	it('omits the detail field when the caller supplies none', () => {
		expect(unavailableVoiceEngineV2BridgeHardwareEncoderCapabilities('unsupported-addon-version')).not.toHaveProperty(
			'detail',
		);
	});

	it('normalizes a well-formed addon payload', () => {
		expect(
			normalizeVoiceEngineV2BridgeHardwareEncoderCapabilities({
				available: true,
				backend: 'nvenc',
				compiled: true,
				runtime: true,
				codecs: ['h264', 42, 'av1'],
				zeroCopy: true,
				nativeInputs: ['d3d11Texture', null],
			}),
		).toEqual({
			available: true,
			backend: 'nvenc',
			compiled: true,
			runtime: true,
			codecs: ['h264', 'av1'],
			zeroCopy: true,
			nativeInputs: ['d3d11Texture'],
		});
	});

	it('falls back to an unavailable encoder when the addon returns a non-object', () => {
		expect(normalizeVoiceEngineV2BridgeHardwareEncoderCapabilities(null)).toEqual({
			available: false,
			backend: 'none',
			compiled: false,
			runtime: false,
			codecs: [],
			zeroCopy: false,
			nativeInputs: [],
			reason: 'query-failed',
			detail: 'Native addon returned an invalid result',
		});
	});

	it('falls back to the none backend when the addon omits one', () => {
		expect(normalizeVoiceEngineV2BridgeHardwareEncoderCapabilities({available: true}).backend).toBe('none');
	});
});
