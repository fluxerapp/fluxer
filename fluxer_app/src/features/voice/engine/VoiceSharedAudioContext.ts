// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {VOICE_TRACK_MAX_TOTAL_GAIN} from '@app/features/voice/utils/VoiceVolumeUtils';

const logger = new Logger('VoiceSharedAudioContext');

type AudioContextConstructor = typeof AudioContext;

const MASTER_SOFT_CLIP_KNEE = 0.8;
const MASTER_SOFT_CLIP_CURVE_POINTS = 8192;

function softClipSample(x: number): number {
	const magnitude = Math.abs(x);
	if (magnitude <= MASTER_SOFT_CLIP_KNEE) return x;
	const over = (magnitude - MASTER_SOFT_CLIP_KNEE) / (1 - MASTER_SOFT_CLIP_KNEE);
	const shaped = MASTER_SOFT_CLIP_KNEE + (1 - MASTER_SOFT_CLIP_KNEE) * Math.tanh(over);
	return x < 0 ? -shaped : shaped;
}

export function createVoiceSoftClipCurve(): Float32Array<ArrayBuffer> {
	const curve = new Float32Array(new ArrayBuffer(MASTER_SOFT_CLIP_CURVE_POINTS * Float32Array.BYTES_PER_ELEMENT));
	for (let i = 0; i < MASTER_SOFT_CLIP_CURVE_POINTS; i++) {
		const normalized = (i / (MASTER_SOFT_CLIP_CURVE_POINTS - 1)) * 2 - 1;
		curve[i] = softClipSample(normalized * VOICE_TRACK_MAX_TOTAL_GAIN);
	}
	return curve;
}

export function createVoiceSoftClipNode(context: BaseAudioContext): {input: GainNode; output: WaveShaperNode} | null {
	if (typeof context.createGain !== 'function' || typeof context.createWaveShaper !== 'function') return null;
	const input = context.createGain();
	input.gain.value = 1 / VOICE_TRACK_MAX_TOTAL_GAIN;
	const output = context.createWaveShaper();
	output.curve = createVoiceSoftClipCurve();
	output.oversample = 'none';
	input.connect(output);
	return {input, output};
}

function installMasterSoftClip(context: AudioContext): void {
	const hardwareDestination = context.destination;
	if (!hardwareDestination) return;
	const softClip = createVoiceSoftClipNode(context);
	if (!softClip) return;
	try {
		softClip.output.connect(hardwareDestination);
		Object.defineProperty(context, 'destination', {
			configurable: true,
			get: () => softClip.input,
		});
	} catch (error) {
		logger.warn('Failed to install master soft clip on shared voice AudioContext', {error});
	}
}

let sharedAudioContext: AudioContext | null = null;

function resolveAudioContextConstructor(): AudioContextConstructor | null {
	if (typeof window === 'undefined') return null;
	const ctor =
		window.AudioContext ||
		(window as typeof window & {webkitAudioContext?: AudioContextConstructor}).webkitAudioContext;
	return ctor ?? null;
}

export function createVoiceAudioContext(options: AudioContextOptions): AudioContext | null {
	const ctor = resolveAudioContextConstructor();
	if (!ctor) return null;
	try {
		return new ctor(options);
	} catch (error) {
		logger.warn('Failed to construct voice AudioContext', {options, error});
		return null;
	}
}

function installResumeOnUserGesture(context: AudioContext): void {
	if (typeof window === 'undefined') return;
	if (!window.document?.body) return;
	const body = window.document.body;
	const handleResume = (): void => {
		if (context.state === 'suspended') {
			void context.resume().catch((error) => {
				logger.debug('Failed to auto-resume shared voice AudioContext on user gesture', {error});
			});
		}
		body.removeEventListener('click', handleResume);
	};
	context.addEventListener('statechange', () => {
		if (context.state === 'closed') {
			body.removeEventListener('click', handleResume);
		}
	});
	body.addEventListener('click', handleResume);
}

export function getSharedVoiceAudioContext(): AudioContext | null {
	if (sharedAudioContext && sharedAudioContext.state !== 'closed') {
		return sharedAudioContext;
	}
	sharedAudioContext = createVoiceAudioContext({latencyHint: 'interactive'});
	if (!sharedAudioContext) return null;
	assert.notEqual(sharedAudioContext.state, 'closed', 'shared voice AudioContext must not start closed');
	installMasterSoftClip(sharedAudioContext);
	if (sharedAudioContext.state === 'suspended') {
		void sharedAudioContext.resume().catch((error) => {
			logger.debug('Initial resume of shared voice AudioContext rejected', {error});
		});
		installResumeOnUserGesture(sharedAudioContext);
	}
	return sharedAudioContext;
}
