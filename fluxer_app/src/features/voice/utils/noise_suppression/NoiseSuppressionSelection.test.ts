// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NoiseSuppressionRuntimeCapabilities} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {
	applyNoiseSuppressionOverride,
	INERT_EFFECTIVE_NOISE_SUPPRESSION,
	resolveEffectiveNoiseSuppression,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';
import type {ResolvedVoiceProcessing} from '@app/features/voice/utils/VoiceProcessingProfile';
import type {
	VoiceNoiseSuppressionAssignmentResponse,
	VoiceNoiseSuppressionBackend,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {describe, expect, it} from 'vitest';

const CAPABLE: NoiseSuppressionRuntimeCapabilities = {sampleRate: 48000, wasmSimd: true, audioWorklet: true};

function assignment(
	overrides: Partial<VoiceNoiseSuppressionAssignmentResponse> = {},
): VoiceNoiseSuppressionAssignmentResponse {
	return {
		...INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
		enabled: true,
		user_targeted: true,
		backend: 'rnnoise',
		source: 'canary',
		enabled_backends: ['none', 'standard', 'gate', 'speex', 'rnnoise', 'gtcrn', 'deep_filter'],
		allow_user_override: true,
		stereo_enabled: true,
		suppression_strength: 65,
		config_version: 7,
		...overrides,
	};
}

function voiceProfile(overrides: Partial<ResolvedVoiceProcessing> = {}): ResolvedVoiceProcessing {
	return {
		mode: 'voice',
		echoCancellation: true,
		browserNoiseSuppression: true,
		autoGainControl: true,
		deepFilter: false,
		deepFilterNoiseReductionLevel: 0,
		contentHint: 'speech',
		noiseSuppressionBackend: 'standard',
		stereoCapture: false,
		...overrides,
	};
}

describe('resolveEffectiveNoiseSuppression', () => {
	it('is inert for the default assignment', () => {
		const effective = resolveEffectiveNoiseSuppression(INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT, null, null, CAPABLE);
		expect(effective).toEqual(INERT_EFFECTIVE_NOISE_SUPPRESSION);
		expect(effective.rolloutApplied).toBe(false);
	});

	it('is inert for an untargeted user even while the rollout is enabled', () => {
		const effective = resolveEffectiveNoiseSuppression(
			assignment({user_targeted: false, backend: null, source: null}),
			null,
			null,
			CAPABLE,
		);
		expect(effective.rolloutApplied).toBe(false);
	});

	it('carries the targeted backend and knobs through', () => {
		const effective = resolveEffectiveNoiseSuppression(assignment(), null, null, CAPABLE);
		expect(effective.rolloutApplied).toBe(true);
		expect(effective.backend).toBe('rnnoise');
		expect(effective.requestedBackend).toBe('rnnoise');
		expect(effective.source).toBe('canary');
		expect(effective.suppressionStrength).toBe(65);
		expect(effective.configVersion).toBe(7);
	});

	it('honours a guild override', () => {
		const effective = resolveEffectiveNoiseSuppression(
			assignment({
				user_targeted: false,
				backend: null,
				source: null,
				guild_overrides: [{guild_id: '42', backend: 'speex'}],
			}),
			'42',
			null,
			CAPABLE,
		);
		expect(effective.backend).toBe('speex');
		expect(effective.source).toBe('guild_rule');
	});

	it('ignores a guild override for a different call', () => {
		const effective = resolveEffectiveNoiseSuppression(
			assignment({
				user_targeted: false,
				backend: null,
				source: null,
				guild_overrides: [{guild_id: '42', backend: 'speex'}],
			}),
			'43',
			null,
			CAPABLE,
		);
		expect(effective.rolloutApplied).toBe(false);
	});

	it('honours an allowed user preference', () => {
		const effective = resolveEffectiveNoiseSuppression(assignment(), null, 'gtcrn', CAPABLE);
		expect(effective.backend).toBe('gtcrn');
		expect(effective.source).toBe('user_override');
	});

	it('ignores a preference that is not a known backend', () => {
		const effective = resolveEffectiveNoiseSuppression(assignment(), null, 'krisp', CAPABLE);
		expect(effective.backend).toBe('rnnoise');
	});

	it('ignores a preference the config does not enable', () => {
		const effective = resolveEffectiveNoiseSuppression(
			assignment({enabled_backends: ['standard', 'rnnoise']}),
			null,
			'gtcrn',
			CAPABLE,
		);
		expect(effective.backend).toBe('rnnoise');
	});

	it('ignores a preference when overrides are disallowed', () => {
		const effective = resolveEffectiveNoiseSuppression(
			assignment({allow_user_override: false}),
			null,
			'gtcrn',
			CAPABLE,
		);
		expect(effective.backend).toBe('rnnoise');
	});

	it('falls back to a usable backend when the target cannot run here', () => {
		const at44k: NoiseSuppressionRuntimeCapabilities = {...CAPABLE, sampleRate: 44100};
		const effective = resolveEffectiveNoiseSuppression(assignment(), null, null, at44k);
		expect(effective.requestedBackend).toBe('rnnoise');
		expect(effective.backend).toBe('standard');
	});

	it('falls back when gtcrn is targeted without wasm simd', () => {
		const effective = resolveEffectiveNoiseSuppression(assignment({backend: 'gtcrn'}), null, null, {
			...CAPABLE,
			wasmSimd: false,
		});
		expect(effective.backend).toBe('standard');
	});

	it('only reports stereo for a backend that keeps the channel layout', () => {
		expect(resolveEffectiveNoiseSuppression(assignment({backend: 'standard'}), null, null, CAPABLE).stereoEnabled).toBe(
			true,
		);
		expect(resolveEffectiveNoiseSuppression(assignment({backend: 'none'}), null, null, CAPABLE).stereoEnabled).toBe(
			true,
		);
		for (const backend of ['gate', 'speex', 'rnnoise', 'gtcrn', 'deep_filter'] as const) {
			expect(resolveEffectiveNoiseSuppression(assignment({backend}), null, null, CAPABLE).stereoEnabled).toBe(false);
		}
	});

	it('lets the user turn stereo off without turning it on when the rollout has not', () => {
		expect(
			resolveEffectiveNoiseSuppression(assignment({backend: 'standard'}), null, null, CAPABLE, false).stereoEnabled,
		).toBe(false);
		expect(
			resolveEffectiveNoiseSuppression(
				assignment({backend: 'standard', stereo_enabled: false}),
				null,
				null,
				CAPABLE,
				true,
			).stereoEnabled,
		).toBe(false);
	});
});

describe('applyNoiseSuppressionOverride', () => {
	it('leaves the profile untouched when the rollout is inert', () => {
		const profile = voiceProfile();
		expect(applyNoiseSuppressionOverride(profile, INERT_EFFECTIVE_NOISE_SUPPRESSION)).toBe(profile);
	});

	it('never overrides studio mode', () => {
		const profile = voiceProfile({mode: 'studio', browserNoiseSuppression: false, contentHint: 'music'});
		const effective = resolveEffectiveNoiseSuppression(assignment(), null, null, CAPABLE);
		expect(applyNoiseSuppressionOverride(profile, effective)).toBe(profile);
	});

	it('turns the browser filter off for every worklet backend', () => {
		for (const backend of ['gate', 'speex', 'rnnoise', 'gtcrn', 'deep_filter', 'none'] as const) {
			const effective = resolveEffectiveNoiseSuppression(assignment({backend}), null, null, CAPABLE);
			const applied = applyNoiseSuppressionOverride(voiceProfile(), effective);
			expect(applied.browserNoiseSuppression).toBe(false);
			expect(applied.noiseSuppressionBackend).toBe(backend);
		}
	});

	it('turns the browser filter on for standard', () => {
		const effective = resolveEffectiveNoiseSuppression(assignment({backend: 'standard'}), null, null, CAPABLE);
		const applied = applyNoiseSuppressionOverride(voiceProfile({browserNoiseSuppression: false}), effective);
		expect(applied.browserNoiseSuppression).toBe(true);
		expect(applied.deepFilter).toBe(false);
	});

	it('routes the strength into the deep filter level only for deep_filter', () => {
		const deep = resolveEffectiveNoiseSuppression(assignment({backend: 'deep_filter'}), null, null, CAPABLE);
		const appliedDeep = applyNoiseSuppressionOverride(voiceProfile(), deep);
		expect(appliedDeep.deepFilter).toBe(true);
		expect(appliedDeep.deepFilterNoiseReductionLevel).toBe(65);

		const rnnoise = resolveEffectiveNoiseSuppression(assignment({backend: 'rnnoise'}), null, null, CAPABLE);
		const appliedRnnoise = applyNoiseSuppressionOverride(voiceProfile(), rnnoise);
		expect(appliedRnnoise.deepFilter).toBe(false);
		expect(appliedRnnoise.deepFilterNoiseReductionLevel).toBe(0);
	});

	it('preserves every field it does not own', () => {
		const profile = voiceProfile({echoCancellation: false, autoGainControl: false, contentHint: 'speech'});
		const effective = resolveEffectiveNoiseSuppression(assignment(), null, null, CAPABLE);
		const applied = applyNoiseSuppressionOverride(profile, effective);
		expect(applied.mode).toBe(profile.mode);
		expect(applied.echoCancellation).toBe(false);
		expect(applied.autoGainControl).toBe(false);
		expect(applied.contentHint).toBe('speech');
	});

	it('sets stereo capture only when the effective resolution says so', () => {
		const stereo = resolveEffectiveNoiseSuppression(assignment({backend: 'standard'}), null, null, CAPABLE);
		expect(applyNoiseSuppressionOverride(voiceProfile(), stereo).stereoCapture).toBe(true);
		const mono = resolveEffectiveNoiseSuppression(assignment({backend: 'rnnoise'}), null, null, CAPABLE);
		expect(applyNoiseSuppressionOverride(voiceProfile(), mono).stereoCapture).toBe(false);
	});
});

describe('backend coverage', () => {
	it('resolves every contract backend to something runnable', () => {
		const backends: ReadonlyArray<VoiceNoiseSuppressionBackend> = [
			'none',
			'standard',
			'gate',
			'speex',
			'rnnoise',
			'gtcrn',
			'deep_filter',
		];
		for (const backend of backends) {
			const effective = resolveEffectiveNoiseSuppression(assignment({backend}), null, null, CAPABLE);
			expect(effective.backend).toBe(backend);
		}
	});
});
