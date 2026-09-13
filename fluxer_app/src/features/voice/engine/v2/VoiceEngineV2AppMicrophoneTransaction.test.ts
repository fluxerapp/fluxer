// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	chooseMicrophoneRefreshStrategy,
	computeSpeakingDetectorRms,
	createInitialMicrophoneEnableState,
	createInitialMicrophoneRefreshState,
	readSpeakingDetectorThresholdRms,
	shouldReleaseSpeakingHold,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppMicrophoneTransaction';
import {describe, expect, it} from 'vitest';

describe('VoiceEngineV2AppMicrophoneTransaction', () => {
	describe('createInitialMicrophoneEnableState', () => {
		it('returns a fresh state with no committed mutations', () => {
			const state = createInitialMicrophoneEnableState();
			expect(state.microphoneWasPublished).toBe(false);
			expect(state.audioTrack).toBeNull();
			expect(state.primaryMicPublication).toBeNull();
			expect(state.shouldUnmuteAfter).toBe(false);
		});

		it('returns independent state objects on each call so rollback cannot reach across transactions', () => {
			const a = createInitialMicrophoneEnableState();
			const b = createInitialMicrophoneEnableState();
			a.microphoneWasPublished = true;
			expect(b.microphoneWasPublished).toBe(false);
		});
	});

	describe('createInitialMicrophoneRefreshState', () => {
		it('returns a fresh state with restartSucceeded false so the parent can detect uncommitted restarts', () => {
			const state = createInitialMicrophoneRefreshState();
			expect(state.restartSucceeded).toBe(false);
			expect(state.primaryMicPublication).toBeNull();
			expect(state.shouldUnmuteAfter).toBe(false);
		});
	});

	describe('chooseMicrophoneRefreshStrategy', () => {
		it('returns restart-in-place when forceRepublish is not set so the refresh reuses the active track', () => {
			expect(chooseMicrophoneRefreshStrategy({})).toBe('restart-in-place');
		});

		it('returns restart-in-place when forceRepublish is explicitly false', () => {
			expect(chooseMicrophoneRefreshStrategy({forceRepublish: false})).toBe('restart-in-place');
		});

		it('returns force-republish when forceRepublish is true so the refresh tears down and re-publishes', () => {
			expect(chooseMicrophoneRefreshStrategy({forceRepublish: true})).toBe('force-republish');
		});

		it('asserts on null input to surface programmer error', () => {
			expect(() => chooseMicrophoneRefreshStrategy(null as unknown as {forceRepublish?: boolean})).toThrow();
		});
	});

	describe('computeSpeakingDetectorRms', () => {
		it('returns zero for a silent (all 128) buffer', () => {
			const samples = new Uint8Array(256).fill(128);
			expect(computeSpeakingDetectorRms(samples)).toBe(0);
		});

		it('returns 1 for a maximally loud buffer (alternating 0/255)', () => {
			const samples = new Uint8Array(8);
			for (let i = 0; i < samples.length; i++) {
				samples[i] = i % 2 === 0 ? 0 : 255;
			}
			const rms = computeSpeakingDetectorRms(samples);
			expect(rms).toBeGreaterThan(0.99);
			expect(rms).toBeLessThanOrEqual(1.001);
		});

		it('returns a value strictly between 0 and 1 for a mid-amplitude buffer', () => {
			const samples = new Uint8Array(4);
			samples[0] = 192;
			samples[1] = 64;
			samples[2] = 192;
			samples[3] = 64;
			const rms = computeSpeakingDetectorRms(samples);
			expect(rms).toBeGreaterThan(0);
			expect(rms).toBeLessThan(1);
		});

		it('asserts on empty buffer to prevent divide-by-zero downstream', () => {
			expect(() => computeSpeakingDetectorRms(new Uint8Array(0))).toThrow();
		});

		it('reads threshold from the live provider so VAD changes apply without detector restart', () => {
			let threshold = 0.01;
			expect(readSpeakingDetectorThresholdRms(() => threshold)).toBe(0.01);
			threshold = 0.04;
			expect(readSpeakingDetectorThresholdRms(() => threshold)).toBe(0.04);
		});

		it('asserts invalid live threshold values before a speaking tick can use them', () => {
			expect(() => readSpeakingDetectorThresholdRms(() => Number.NaN)).toThrow();
			expect(() => readSpeakingDetectorThresholdRms(() => -1)).toThrow();
		});
	});

	describe('shouldReleaseSpeakingHold', () => {
		it('does not release while still speaking from silence the same tick', () => {
			expect(
				shouldReleaseSpeakingHold({
					silenceStartedAt: 1000,
					now: 1000,
					releaseDelayMs: 200,
					currentlySpeaking: true,
				}),
			).toBe(false);
		});

		it('releases once the silence window has elapsed', () => {
			expect(
				shouldReleaseSpeakingHold({
					silenceStartedAt: 1000,
					now: 1300,
					releaseDelayMs: 200,
					currentlySpeaking: true,
				}),
			).toBe(true);
		});

		it('never releases when not currently speaking so the detector teardown is idempotent', () => {
			expect(
				shouldReleaseSpeakingHold({
					silenceStartedAt: 1000,
					now: 5000,
					releaseDelayMs: 200,
					currentlySpeaking: false,
				}),
			).toBe(false);
		});

		it('does not release when silence has not yet started', () => {
			expect(
				shouldReleaseSpeakingHold({
					silenceStartedAt: null,
					now: 5000,
					releaseDelayMs: 200,
					currentlySpeaking: true,
				}),
			).toBe(false);
		});

		it('asserts on negative releaseDelayMs to surface programmer error', () => {
			expect(() =>
				shouldReleaseSpeakingHold({
					silenceStartedAt: 0,
					now: 0,
					releaseDelayMs: -1,
					currentlySpeaking: true,
				}),
			).toThrow();
		});
	});
});
