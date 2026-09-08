// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getLocalSpeakingThresholdRms,
	LOCAL_MAX_RMS,
	LOCAL_MIN_RMS,
	SPEAKING_LOCAL_RELEASE_MS,
} from '@app/features/voice/engine/VoiceSpeakingThreshold';
import {describe, expect, it} from 'vitest';
import {
	type LocalSpeakingOverrideInput,
	resolveLocalSpeakingOverrideState,
	resolveVoiceActivityGateState,
	resolveVoiceActivityThresholdRms,
	updateVoiceActivityNoiseFloorRms,
	type VoiceActivityGateInput,
	type VoiceActivityGateState,
} from './VoiceLocalSpeakingGate';

const UNMUTED_OPEN_MIC: LocalSpeakingOverrideInput = {
	pushToTalkActive: false,
	pushToMuteActive: false,
	pushToMuteHeld: false,
	selfDeaf: false,
	effectiveSelfMute: false,
	hasMicrophonePublication: true,
	microphonePublicationMuted: false,
};

describe('resolveLocalSpeakingOverrideState', () => {
	it('lets VAD own normal open-mic speaking state', () => {
		expect(resolveLocalSpeakingOverrideState(UNMUTED_OPEN_MIC)).toBeNull();
	});
	it('forces local speaking off when open mic is effectively muted', () => {
		expect(
			resolveLocalSpeakingOverrideState({
				...UNMUTED_OPEN_MIC,
				effectiveSelfMute: true,
				microphonePublicationMuted: true,
			}),
		).toBe(false);
	});
	it('uses the push-to-talk transmit gate instead of VAD', () => {
		expect(
			resolveLocalSpeakingOverrideState({
				...UNMUTED_OPEN_MIC,
				pushToTalkActive: true,
			}),
		).toBe(true);
		expect(
			resolveLocalSpeakingOverrideState({
				...UNMUTED_OPEN_MIC,
				pushToTalkActive: true,
				effectiveSelfMute: true,
				microphonePublicationMuted: true,
			}),
		).toBe(false);
	});
	it('does not mark push-to-talk speech active without a live mic publication', () => {
		expect(
			resolveLocalSpeakingOverrideState({
				...UNMUTED_OPEN_MIC,
				pushToTalkActive: true,
				hasMicrophonePublication: false,
				microphonePublicationMuted: true,
			}),
		).toBe(false);
	});
	it('forces local speaking off while push-to-mute is held', () => {
		expect(
			resolveLocalSpeakingOverrideState({
				...UNMUTED_OPEN_MIC,
				pushToMuteActive: true,
				pushToMuteHeld: true,
			}),
		).toBe(false);
	});
	it('forces local speaking off while deafened', () => {
		expect(
			resolveLocalSpeakingOverrideState({
				...UNMUTED_OPEN_MIC,
				selfDeaf: true,
			}),
		).toBe(false);
	});
});

const GATE_TICK_MS = 50;
const KEYBOARD_NOISE_RMS = 0.012;
const SPEECH_RMS = 0.06;

function runGate(samples: ReadonlyArray<number>, thresholdRms: number): Array<boolean> {
	let open = false;
	let silenceStartedAtMs: number | null = null;
	return samples.map((rms, index) => {
		const input: VoiceActivityGateInput = {
			rms,
			thresholdRms,
			nowMs: index * GATE_TICK_MS,
			silenceStartedAtMs,
			gateOpen: open,
			releaseDelayMs: SPEAKING_LOCAL_RELEASE_MS,
		};
		const next = resolveVoiceActivityGateState(input);
		open = next.open;
		silenceStartedAtMs = next.silenceStartedAtMs;
		return open;
	});
}

describe('resolveVoiceActivityGateState', () => {
	it('keeps keyboard noise off the wire when the activity threshold is raised', () => {
		const opened = runGate(new Array(20).fill(KEYBOARD_NOISE_RMS), getLocalSpeakingThresholdRms(100));
		expect(opened.some((open) => open)).toBe(false);
	});

	it('still transmits the same noise when the activity threshold is lowered', () => {
		const opened = runGate(new Array(20).fill(KEYBOARD_NOISE_RMS), getLocalSpeakingThresholdRms(0));
		expect(opened.every((open) => open)).toBe(true);
	});

	it('opens on the first sample at or above the threshold and clears the silence clock', () => {
		const expected: VoiceActivityGateState = {open: true, silenceStartedAtMs: null};
		expect(
			resolveVoiceActivityGateState({
				rms: SPEECH_RMS,
				thresholdRms: getLocalSpeakingThresholdRms(100),
				nowMs: 1_000,
				silenceStartedAtMs: 800,
				gateOpen: false,
				releaseDelayMs: SPEAKING_LOCAL_RELEASE_MS,
			}),
		).toEqual(expected);
	});

	it('holds the gate open for the release window before closing', () => {
		const threshold = getLocalSpeakingThresholdRms(100);
		expect(
			resolveVoiceActivityGateState({
				rms: KEYBOARD_NOISE_RMS,
				thresholdRms: threshold,
				nowMs: 1_000,
				silenceStartedAtMs: null,
				gateOpen: true,
				releaseDelayMs: SPEAKING_LOCAL_RELEASE_MS,
			}),
		).toEqual({open: true, silenceStartedAtMs: 1_000});
		expect(
			resolveVoiceActivityGateState({
				rms: KEYBOARD_NOISE_RMS,
				thresholdRms: threshold,
				nowMs: 1_000 + SPEAKING_LOCAL_RELEASE_MS - 1,
				silenceStartedAtMs: 1_000,
				gateOpen: true,
				releaseDelayMs: SPEAKING_LOCAL_RELEASE_MS,
			}).open,
		).toBe(true);
		expect(
			resolveVoiceActivityGateState({
				rms: KEYBOARD_NOISE_RMS,
				thresholdRms: threshold,
				nowMs: 1_000 + SPEAKING_LOCAL_RELEASE_MS,
				silenceStartedAtMs: 1_000,
				gateOpen: true,
				releaseDelayMs: SPEAKING_LOCAL_RELEASE_MS,
			}).open,
		).toBe(false);
	});

	it('closes after speech stops and reopens when speech returns', () => {
		const threshold = getLocalSpeakingThresholdRms(100);
		const opened = runGate(
			[SPEECH_RMS, SPEECH_RMS, ...new Array(6).fill(KEYBOARD_NOISE_RMS), SPEECH_RMS, KEYBOARD_NOISE_RMS],
			threshold,
		);
		expect(opened).toEqual([true, true, true, true, true, true, false, false, true, true]);
	});
});

describe('resolveVoiceActivityThresholdRms', () => {
	it('follows the activity threshold slider when auto sensitivity is off', () => {
		expect(resolveVoiceActivityThresholdRms({autoSensitivity: false, vadThreshold: 100, noiseFloorRms: 0})).toBe(
			getLocalSpeakingThresholdRms(100),
		);
		expect(resolveVoiceActivityThresholdRms({autoSensitivity: false, vadThreshold: 0, noiseFloorRms: 0})).toBe(
			getLocalSpeakingThresholdRms(0),
		);
	});

	it('ignores the slider and tracks the measured noise floor when auto sensitivity is on', () => {
		expect(
			resolveVoiceActivityThresholdRms({autoSensitivity: true, vadThreshold: 0, noiseFloorRms: 0.005}),
		).toBeCloseTo(0.015, 6);
	});

	it('clamps the auto threshold to the local speaking range', () => {
		expect(resolveVoiceActivityThresholdRms({autoSensitivity: true, vadThreshold: 50, noiseFloorRms: 0})).toBe(
			LOCAL_MIN_RMS,
		);
		expect(resolveVoiceActivityThresholdRms({autoSensitivity: true, vadThreshold: 50, noiseFloorRms: 1})).toBe(
			LOCAL_MAX_RMS,
		);
	});
});

describe('updateVoiceActivityNoiseFloorRms', () => {
	it('converges on the level measured while the gate is closed', () => {
		let noiseFloorRms = 0;
		for (let i = 0; i < 200; i++) {
			noiseFloorRms = updateVoiceActivityNoiseFloorRms(noiseFloorRms, KEYBOARD_NOISE_RMS);
		}
		expect(noiseFloorRms).toBeCloseTo(KEYBOARD_NOISE_RMS, 4);
	});
});
