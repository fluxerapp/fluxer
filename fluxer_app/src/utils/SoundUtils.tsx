/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import cameraOffSound from '~/sounds/camera-off.mp3';
import cameraOnSound from '~/sounds/camera-on.mp3';
import deafSound from '~/sounds/deaf.mp3';
import incomingRingSound from '~/sounds/incoming-ring.mp3';
import messageSound from '~/sounds/message.mp3';
import muteSound from '~/sounds/mute.mp3';
import streamSound from '~/sounds/stream-start.mp3';
import streamStopSound from '~/sounds/stream-stop.mp3';
import undeafSound from '~/sounds/undeaf.mp3';
import unmuteSound from '~/sounds/unmute.mp3';
import userJoinSound from '~/sounds/user-join.mp3';
import userLeaveSound from '~/sounds/user-leave.mp3';
import userMoveSound from '~/sounds/user-move.mp3';
import viewerJoinSound from '~/sounds/viewer-join.mp3';
import viewerLeaveSound from '~/sounds/viewer-leave.mp3';
import voiceDisconnectSound from '~/sounds/voice-disconnect.mp3';
import * as CustomSoundDB from '~/utils/CustomSoundDB';

const MAX_EFFECTIVE_VOLUME = 0.4;
const MASTER_HEADROOM = 0.8;

const MIN_GAIN = 0.0001;
const DEFAULT_FADE_DURATION = 0.08;

export const SoundType = {
	Deaf: 'deaf',
	Undeaf: 'undeaf',
	Mute: 'mute',
	Unmute: 'unmute',
	Message: 'message',
	IncomingRing: 'incoming-ring',
	UserJoin: 'user-join',
	UserLeave: 'user-leave',
	UserMove: 'user-move',
	ViewerJoin: 'viewer-join',
	ViewerLeave: 'viewer-leave',
	VoiceDisconnect: 'voice-disconnect',
	CameraOn: 'camera-on',
	CameraOff: 'camera-off',
	ScreenShareStart: 'screen-share-start',
	ScreenShareStop: 'screen-share-stop',
} as const;

export type SoundType = (typeof SoundType)[keyof typeof SoundType];

const SOUND_FILES: Record<SoundType, string> = {
	[SoundType.Deaf]: deafSound,
	[SoundType.Undeaf]: undeafSound,
	[SoundType.Mute]: muteSound,
	[SoundType.Unmute]: unmuteSound,
	[SoundType.Message]: messageSound,
	[SoundType.IncomingRing]: incomingRingSound,
	[SoundType.UserJoin]: userJoinSound,
	[SoundType.UserLeave]: userLeaveSound,
	[SoundType.UserMove]: userMoveSound,
	[SoundType.ViewerJoin]: viewerJoinSound,
	[SoundType.ViewerLeave]: viewerLeaveSound,
	[SoundType.VoiceDisconnect]: voiceDisconnectSound,
	[SoundType.CameraOn]: cameraOnSound,
	[SoundType.CameraOff]: cameraOffSound,
	[SoundType.ScreenShareStart]: streamSound,
	[SoundType.ScreenShareStop]: streamStopSound,
};

interface AudioInstance {
	audio: HTMLAudioElement;
	gainNode: GainNode;
	sourceNode: MediaElementAudioSourceNode;
}

const activeSounds: Map<SoundType, AudioInstance> = new Map();
const activePreviewSounds: Set<AudioInstance> = new Set();
const customSoundCache: Map<SoundType, string> = new Map();

let audioContext: AudioContext | null = null;
let masterGainNode: GainNode | null = null;

const clamp = (value: number, min = 0, max = 1): number => Math.min(Math.max(value, min), max);
const disconnectNodes = (...nodes: Array<AudioNode | null | undefined>): void => {
	nodes.forEach((node) => {
		if (!node) return;
		try {
			node.disconnect();
		} catch {}
	});
};

const getAudioContext = (): AudioContext => {
	if (!audioContext) {
		audioContext = new AudioContext();
	}
	return audioContext;
};

const getMasterGainNode = (): GainNode => {
	const ctx = getAudioContext();

	if (!masterGainNode || masterGainNode.context.state === 'closed') {
		masterGainNode = ctx.createGain();
		masterGainNode.gain.value = MASTER_HEADROOM;
		masterGainNode.connect(ctx.destination);
	}

	return masterGainNode;
};

const resumeAudioContextIfNeeded = async (): Promise<AudioContext> => {
	const ctx = getAudioContext();
	if (ctx.state === 'suspended') {
		try {
			await ctx.resume();
		} catch {}
	}
	return ctx;
};

const fadeIn = (gainNode: GainNode, targetVolume: number, duration = DEFAULT_FADE_DURATION): void => {
	const ctx = getAudioContext();
	const now = ctx.currentTime;

	targetVolume = clamp(targetVolume, 0, MAX_EFFECTIVE_VOLUME);

	gainNode.gain.cancelScheduledValues(now);
	gainNode.gain.setValueAtTime(MIN_GAIN, now);
	gainNode.gain.linearRampToValueAtTime(targetVolume, now + duration);
};

const fadeOut = (gainNode: GainNode, duration = DEFAULT_FADE_DURATION): Promise<void> => {
	return new Promise((resolve) => {
		const ctx = getAudioContext();
		const now = ctx.currentTime;
		const currentVolume = gainNode.gain.value;

		if (currentVolume <= MIN_GAIN) {
			gainNode.gain.setValueAtTime(MIN_GAIN, now);
			resolve();
			return;
		}

		gainNode.gain.cancelScheduledValues(now);
		gainNode.gain.setValueAtTime(currentVolume, now);
		gainNode.gain.linearRampToValueAtTime(MIN_GAIN, now + duration);

		setTimeout(resolve, duration * 1000);
	});
};

const getSoundUrl = async (type: SoundType): Promise<string> => {
	const cachedUrl = customSoundCache.get(type);
	if (cachedUrl) {
		return cachedUrl;
	}

	const customSound = await CustomSoundDB.getCustomSound(type);
	if (!customSound) {
		return SOUND_FILES[type];
	}

	const url = URL.createObjectURL(customSound.blob);
	customSoundCache.set(type, url);
	return url;
};

const createAudioElement = (src: string): HTMLAudioElement => {
	const audio = new Audio();
	audio.crossOrigin = 'anonymous';
	audio.src = src;
	audio.preload = 'auto';
	return audio;
};

export const playSound = async (type: SoundType, loop = false, volume = 0.4): Promise<HTMLAudioElement | null> => {
	const activeSound = activeSounds.get(type);
	if (loop && activeSound && !activeSound.audio.paused) {
		return null;
	}

	try {
		const ctx = await resumeAudioContextIfNeeded();

		const soundUrl = await getSoundUrl(type);
		const audio = createAudioElement(soundUrl);

		audio.currentTime = 0;
		audio.loop = loop;
		const sourceNode = ctx.createMediaElementSource(audio);
		const gainNode = ctx.createGain();
		const masterGain = getMasterGainNode();

		sourceNode.connect(gainNode);
		gainNode.connect(masterGain);

		const effectiveVolume = clamp(volume, 0, MAX_EFFECTIVE_VOLUME);
		fadeIn(gainNode, effectiveVolume);

		const playPromise = audio.play();
		if (playPromise) {
			playPromise.catch((error) => {
				console.warn(`Failed to play sound ${type}:`, error);
			});
		}

		const instance: AudioInstance = {
			audio,
			gainNode,
			sourceNode,
		};

		if (loop) {
			activeSounds.set(type, instance);
		} else {
			activePreviewSounds.add(instance);

			audio.addEventListener(
				'ended',
				async () => {
					try {
						await fadeOut(gainNode, 0.05);
					} finally {
						activePreviewSounds.delete(instance);
						disconnectNodes(sourceNode, gainNode);
					}
				},
				{once: true},
			);
		}

		return audio;
	} catch (error) {
		console.warn(`Failed to initialize or play sound ${type}:`, error);
		return null;
	}
};

export const clearCustomSoundCache = (type?: SoundType): void => {
	if (type) {
		const cachedUrl = customSoundCache.get(type);
		if (cachedUrl) {
			URL.revokeObjectURL(cachedUrl);
			customSoundCache.delete(type);
		}
		return;
	}

	customSoundCache.forEach((url) => {
		URL.revokeObjectURL(url);
	});
	customSoundCache.clear();
};

export const stopSound = async (type: SoundType): Promise<void> => {
	const activeSound = activeSounds.get(type);
	if (!activeSound) return;

	const {audio, gainNode, sourceNode} = activeSound;

	try {
		await fadeOut(gainNode, 0.08);
	} catch {}

	audio.pause();
	audio.currentTime = 0;
	audio.loop = false;

	disconnectNodes(sourceNode, gainNode);

	activeSounds.delete(type);
};

export const stopAllSounds = async (): Promise<void> => {
	const stopPromises: Array<Promise<void>> = [];

	activeSounds.forEach((_, type) => {
		stopPromises.push(stopSound(type));
	});

	activePreviewSounds.forEach((instance) => {
		const {audio, gainNode, sourceNode} = instance;

		const fadePromise = fadeOut(gainNode, 0.08)
			.catch(() => {})
			.finally(() => {
				audio.pause();
				audio.currentTime = 0;

				disconnectNodes(sourceNode, gainNode);
			});

		stopPromises.push(fadePromise);
	});

	activePreviewSounds.clear();

	await Promise.all(stopPromises);
};
