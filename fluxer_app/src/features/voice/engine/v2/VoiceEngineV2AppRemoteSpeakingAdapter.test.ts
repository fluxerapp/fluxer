// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {computeTimeDomainRms} from './VoiceEngineV2AppRemoteSpeakingAdapter';

vi.mock('@app/features/voice/state/ParticipantVolume', () => ({default: {}}));
vi.mock('@app/features/voice/state/VoiceSettings', () => ({default: {getVadThreshold: () => 50}}));

describe('computeTimeDomainRms', () => {
	it('returns the amplitude of an alternating full-scale-relative signal', () => {
		const samples = new Float32Array(512);
		for (let i = 0; i < samples.length; i++) {
			samples[i] = i % 2 === 0 ? 0.09 : -0.09;
		}
		expect(computeTimeDomainRms(samples)).toBe(Math.fround(0.09));
	});

	it('returns zero for digital silence', () => {
		expect(computeTimeDomainRms(new Float32Array(512))).toBe(0);
	});

	it('returns zero for an empty buffer', () => {
		expect(computeTimeDomainRms(new Float32Array(0))).toBe(0);
	});

	it('spreads a single full-scale sample across the window', () => {
		const samples = new Float32Array(512);
		samples[17] = 1;
		expect(computeTimeDomainRms(samples)).toBeCloseTo(1 / Math.sqrt(512), 12);
	});

	it('resolves levels far below one 8-bit quantisation step', () => {
		expect(computeTimeDomainRms(Float32Array.of(0.0005, -0.0005))).toBe(Math.fround(0.0005));
	});
});
