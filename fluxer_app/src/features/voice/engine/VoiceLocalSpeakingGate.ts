// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getLocalSpeakingThresholdRms,
	LOCAL_MAX_RMS,
	LOCAL_MIN_RMS,
} from '@app/features/voice/engine/VoiceSpeakingThreshold';

const VOICE_ACTIVITY_NOISE_FLOOR_HEADROOM = 3;
const VOICE_ACTIVITY_NOISE_FLOOR_SMOOTHING = 0.05;

export interface LocalSpeakingOverrideInput {
	pushToTalkActive: boolean;
	pushToMuteActive: boolean;
	pushToMuteHeld: boolean;
	selfDeaf: boolean;
	effectiveSelfMute: boolean;
	hasMicrophonePublication: boolean;
	microphonePublicationMuted: boolean;
}

export function resolveLocalSpeakingOverrideState(input: LocalSpeakingOverrideInput): boolean | null {
	const effectivelyMuted =
		input.selfDeaf || input.effectiveSelfMute || !input.hasMicrophonePublication || input.microphonePublicationMuted;
	if (input.pushToTalkActive) return !effectivelyMuted;
	if (input.pushToMuteActive && input.pushToMuteHeld) return false;
	if (effectivelyMuted) return false;
	return null;
}

export interface VoiceActivityGateInput {
	rms: number;
	thresholdRms: number;
	nowMs: number;
	silenceStartedAtMs: number | null;
	gateOpen: boolean;
	releaseDelayMs: number;
}

export interface VoiceActivityGateState {
	open: boolean;
	silenceStartedAtMs: number | null;
}

export function resolveVoiceActivityGateState(input: VoiceActivityGateInput): VoiceActivityGateState {
	if (input.rms >= input.thresholdRms) {
		return {open: true, silenceStartedAtMs: null};
	}
	const silenceStartedAtMs = input.silenceStartedAtMs ?? input.nowMs;
	return {
		open: input.gateOpen && input.nowMs - silenceStartedAtMs < input.releaseDelayMs,
		silenceStartedAtMs,
	};
}

export function resolveVoiceActivityThresholdRms(input: {
	autoSensitivity: boolean;
	vadThreshold: number;
	noiseFloorRms: number;
}): number {
	if (!input.autoSensitivity) {
		return getLocalSpeakingThresholdRms(input.vadThreshold);
	}
	const tracked = input.noiseFloorRms * VOICE_ACTIVITY_NOISE_FLOOR_HEADROOM;
	if (!Number.isFinite(tracked)) return LOCAL_MIN_RMS;
	return Math.min(LOCAL_MAX_RMS, Math.max(LOCAL_MIN_RMS, tracked));
}

export function updateVoiceActivityNoiseFloorRms(noiseFloorRms: number, rms: number): number {
	return noiseFloorRms * (1 - VOICE_ACTIVITY_NOISE_FLOOR_SMOOTHING) + rms * VOICE_ACTIVITY_NOISE_FLOOR_SMOOTHING;
}
