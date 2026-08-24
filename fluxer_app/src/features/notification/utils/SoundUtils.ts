// SPDX-License-Identifier: AGPL-3.0-or-later

import * as CustomSoundDB from '@app/features/notification/utils/CustomSoundDB';
import {Logger} from '@app/features/platform/utils/AppLogger';
import cameraOffSound from '@app/media/sounds/camera-off.mp3';
import cameraOnSound from '@app/media/sounds/camera-on.mp3';
import deafSound from '@app/media/sounds/deaf.mp3';
import sameChannelMessageSound from '@app/media/sounds/in-channel-notification.ogg';
import incomingRingSound from '@app/media/sounds/incoming-ring.mp3';
import messageSound from '@app/media/sounds/message.mp3';
import muteSound from '@app/media/sounds/mute.mp3';
import streamSound from '@app/media/sounds/stream-start.mp3';
import streamStopSound from '@app/media/sounds/stream-stop.mp3';
import undeafSound from '@app/media/sounds/undeaf.mp3';
import unmuteSound from '@app/media/sounds/unmute.mp3';
import userJoinSound from '@app/media/sounds/user-join.mp3';
import userLeaveSound from '@app/media/sounds/user-leave.mp3';
import userMoveSound from '@app/media/sounds/user-move.mp3';
import viewerJoinSound from '@app/media/sounds/viewer-join.mp3';
import viewerLeaveSound from '@app/media/sounds/viewer-leave.mp3';
import voiceDisconnectSound from '@app/media/sounds/voice-disconnect.mp3';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

const logger = new Logger('SoundUtils');
const MAX_EFFECTIVE_VOLUME = 0.8;
const MASTER_HEADROOM = 0.8;
const MIN_GAIN = 0.0001;
const DEFAULT_FADE_DURATION = 0.08;
const MAX_ACTIVE_ONE_SHOT_SOUNDS = 4;
export const SoundType = {
	Deaf: 'deaf',
	Undeaf: 'undeaf',
	Mute: 'mute',
	Unmute: 'unmute',
	Message: 'message',
	DirectMessage: 'direct-message',
	SameChannelMessage: 'same-channel-message',
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

export type SoundType = ValueOf<typeof SoundType>;

const SOUND_FILES: Record<SoundType, string> = {
	[SoundType.Deaf]: deafSound,
	[SoundType.Undeaf]: undeafSound,
	[SoundType.Mute]: muteSound,
	[SoundType.Unmute]: unmuteSound,
	[SoundType.Message]: messageSound,
	[SoundType.DirectMessage]: messageSound,
	[SoundType.SameChannelMessage]: sameChannelMessageSound,
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
const CALL_RELATED_SOUND_TYPES = new Set<SoundType>([
	SoundType.Deaf,
	SoundType.Undeaf,
	SoundType.Mute,
	SoundType.Unmute,
	SoundType.IncomingRing,
	SoundType.UserJoin,
	SoundType.UserLeave,
	SoundType.UserMove,
	SoundType.ViewerJoin,
	SoundType.ViewerLeave,
	SoundType.VoiceDisconnect,
	SoundType.CameraOn,
	SoundType.CameraOff,
	SoundType.ScreenShareStart,
	SoundType.ScreenShareStop,
]);
const shouldRouteSoundToCaptureBus = (type: SoundType): boolean => !CALL_RELATED_SOUND_TYPES.has(type);

interface AudioInstance {
	audio: HTMLAudioElement;
	gainNode: GainNode;
	sourceNode: MediaElementAudioSourceNode;
}

interface OneShotAudioInstance {
	audio: HTMLAudioElement;
	gainNode?: GainNode;
	sourceNode?: MediaElementAudioSourceNode;
	cleanup: () => void;
}

const activeSounds: Map<SoundType, AudioInstance> = new Map();
const activePreviewSounds: Set<AudioInstance> = new Set();
const activeOneShotSounds: Set<OneShotAudioInstance> = new Set();
const customSoundCache: Map<SoundType, string> = new Map();

let audioContext: AudioContext | null = null;
let masterGainNode: GainNode | null = null;
let soundOutputDeviceIdResolver: (() => string | null) | null = null;
let lastAppliedContextSinkId: string | null = null;
let rejectedContextSinkId: string | null = null;

type SinkableAudioContext = AudioContext & {
	setSinkId?: (sinkId: string) => Promise<void>;
};

type SinkableAudioElement = HTMLAudioElement & {
	setSinkId?: (sinkId: string) => Promise<void>;
};

export function setSoundOutputDeviceIdResolver(resolver: () => string | null): void {
	soundOutputDeviceIdResolver = resolver;
}

const resolveSoundOutputSinkId = (): string | null => {
	const deviceId = soundOutputDeviceIdResolver?.();
	if (!deviceId) return null;
	if (rejectedContextSinkId && deviceId !== rejectedContextSinkId) {
		rejectedContextSinkId = null;
	}
	if (deviceId === rejectedContextSinkId) return '';
	return deviceId === 'default' ? '' : deviceId;
};

function isOutputDeviceNotFoundError(error: unknown): boolean {
	if (!error || typeof error !== 'object') return false;
	return Reflect.get(error, 'name') === 'NotFoundError';
}

const applyOutputDeviceToSoundContext = (ctx: AudioContext): void => {
	const sinkId = resolveSoundOutputSinkId();
	if (sinkId === null) return;
	if (sinkId === lastAppliedContextSinkId) return;
	if (sinkId === '' && lastAppliedContextSinkId === null) return;
	const sinkableContext = ctx as SinkableAudioContext;
	if (typeof sinkableContext.setSinkId !== 'function') return;
	const previousSinkId = lastAppliedContextSinkId;
	lastAppliedContextSinkId = sinkId;
	void sinkableContext.setSinkId(sinkId).catch((error) => {
		if (sinkId !== '' && isOutputDeviceNotFoundError(error)) {
			rejectedContextSinkId = sinkId;
			lastAppliedContextSinkId = null;
			logger.debug('Sound output device was not found; falling back to default audio context output', {sinkId, error});
			void sinkableContext
				.setSinkId?.('')
				.then(() => {
					lastAppliedContextSinkId = '';
				})
				.catch((fallbackError) => {
					lastAppliedContextSinkId = previousSinkId;
					logger.debug('Failed to fall back to default sound output device for audio context', {
						sinkId,
						error: fallbackError,
					});
				});
			return;
		}
		lastAppliedContextSinkId = previousSinkId;
		logger.debug('Failed to apply sound output device to audio context', {sinkId, error});
	});
};

const applyOutputDeviceToAudioElement = (audio: HTMLAudioElement): void => {
	const sinkId = resolveSoundOutputSinkId();
	if (sinkId === null || sinkId === '') return;
	const sinkableElement = audio as SinkableAudioElement;
	if (typeof sinkableElement.setSinkId !== 'function') return;
	void sinkableElement.setSinkId(sinkId).catch((error) => {
		logger.debug('Failed to apply sound output device to audio element', {sinkId, error});
	});
};

const captureTaps: Set<AudioNode> = new Set();
const clamp = (value: number, min = 0, max = 1): number => Math.min(Math.max(value, min), max);
const disconnectNodes = (...nodes: Array<AudioNode | null | undefined>): void => {
	nodes.forEach((node) => {
		if (!node) return;
		try {
			node.disconnect();
		} catch {}
	});
};
const stopOneShotInstance = (instance: OneShotAudioInstance): void => {
	instance.audio.pause();
	instance.audio.currentTime = 0;
	instance.cleanup();
};
const trimActiveOneShotSounds = (): void => {
	while (activeOneShotSounds.size >= MAX_ACTIVE_ONE_SHOT_SOUNDS) {
		const oldest = activeOneShotSounds.values().next().value;
		if (!oldest) return;
		stopOneShotInstance(oldest);
	}
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
		for (const node of captureTaps) {
			try {
				masterGainNode.connect(node);
			} catch (error) {
				logger.warn('Failed to reconnect sound capture tap after master gain reset', error);
			}
		}
	}
	return masterGainNode;
};

export function getSoundCaptureAudioContext(): AudioContext {
	return getAudioContext();
}

export function getSoundCaptureMasterGainNode(): GainNode {
	return getMasterGainNode();
}

const soundCaptureActivationListeners: Set<() => void> = new Set();

export function isSoundCaptureActive(): boolean {
	return captureTaps.size > 0;
}

export function onSoundCaptureActivated(listener: () => void): () => void {
	soundCaptureActivationListeners.add(listener);
	return () => {
		soundCaptureActivationListeners.delete(listener);
	};
}

export function addSoundCaptureDestination(node: AudioNode): void {
	if (captureTaps.has(node)) return;
	const master = getMasterGainNode();
	const wasInactive = captureTaps.size === 0;
	captureTaps.add(node);
	if (wasInactive) {
		for (const listener of soundCaptureActivationListeners) {
			try {
				listener();
			} catch (error) {
				logger.warn('Sound capture activation listener failed', error);
			}
		}
	}
	try {
		master.connect(node);
	} catch (error) {
		logger.warn('Failed to attach sound capture destination', error);
	}
}

export function removeSoundCaptureDestination(node: AudioNode): void {
	if (!captureTaps.delete(node)) return;
	if (!masterGainNode) return;
	try {
		masterGainNode.disconnect(node);
	} catch {}
}

const resumeAudioContextIfNeeded = async (): Promise<AudioContext> => {
	const ctx = getAudioContext();
	if (ctx.state === 'suspended') {
		try {
			await ctx.resume();
		} catch {}
	}
	applyOutputDeviceToSoundContext(ctx);
	return ctx;
};

export async function acquireOutputAudioContext(): Promise<AudioContext> {
	return resumeAudioContextIfNeeded();
}

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
const isAutoplayBlockedError = (error: unknown): boolean => {
	if (!error || typeof error !== 'object') return false;
	const name = (
		error as {
			name?: string;
		}
	).name;
	return name === 'NotAllowedError' || name === 'AbortError';
};
const playOneShotSound = async (
	type: SoundType,
	volume: number,
	onAutoplayBlocked?: () => void,
): Promise<HTMLAudioElement | null> => {
	const soundUrl = await getSoundUrl(type);
	const audio = createAudioElement(soundUrl);
	audio.currentTime = 0;
	audio.loop = false;
	const effectiveVolume = clamp(volume, 0, MAX_EFFECTIVE_VOLUME);
	let sourceNode: MediaElementAudioSourceNode | undefined;
	let gainNode: GainNode | undefined;
	const routeToCaptureBus = shouldRouteSoundToCaptureBus(type);
	try {
		const ctx = await resumeAudioContextIfNeeded();
		if (ctx.state !== 'suspended') {
			sourceNode = ctx.createMediaElementSource(audio);
			gainNode = ctx.createGain();
			gainNode.gain.value = effectiveVolume;
			sourceNode.connect(gainNode);
			gainNode.connect(routeToCaptureBus ? getMasterGainNode() : ctx.destination);
			audio.volume = 1;
		} else {
			audio.volume = effectiveVolume;
			applyOutputDeviceToAudioElement(audio);
		}
	} catch (error) {
		logger.debug('Falling back to direct element playback for one-shot sound', {type, error});
		audio.volume = effectiveVolume;
		applyOutputDeviceToAudioElement(audio);
	}
	const instance: OneShotAudioInstance = {
		audio,
		gainNode,
		sourceNode,
		cleanup: () => {
			activeOneShotSounds.delete(instance);
			audio.removeEventListener('ended', instance.cleanup);
			audio.removeEventListener('error', instance.cleanup);
			disconnectNodes(sourceNode, gainNode);
		},
	};
	audio.addEventListener('ended', instance.cleanup);
	audio.addEventListener('error', instance.cleanup);
	trimActiveOneShotSounds();
	activeOneShotSounds.add(instance);
	try {
		await audio.play();
		return audio;
	} catch (error) {
		instance.cleanup();
		if (isAutoplayBlockedError(error)) {
			logger.debug('Autoplay blocked; dropping sound', {type});
			onAutoplayBlocked?.();
		} else {
			logger.warn(`Failed to play sound ${type}:`, error);
		}
		return null;
	}
};

export async function playSound(
	type: SoundType,
	loop = false,
	volume = 0.4,
	onAutoplayBlocked?: () => void,
): Promise<HTMLAudioElement | null> {
	const activeSound = activeSounds.get(type);
	if (loop && activeSound && !activeSound.audio.paused) {
		return null;
	}
	try {
		if (!loop) {
			return await playOneShotSound(type, volume, onAutoplayBlocked);
		}
		const ctx = await resumeAudioContextIfNeeded();
		if (ctx.state === 'suspended') {
			logger.debug('Audio context still suspended; skipping sound', {type});
			onAutoplayBlocked?.();
			return null;
		}
		const soundUrl = await getSoundUrl(type);
		const audio = createAudioElement(soundUrl);
		audio.currentTime = 0;
		audio.loop = loop;
		const sourceNode = ctx.createMediaElementSource(audio);
		const gainNode = ctx.createGain();
		sourceNode.connect(gainNode);
		gainNode.connect(shouldRouteSoundToCaptureBus(type) ? getMasterGainNode() : ctx.destination);
		const effectiveVolume = clamp(volume, 0, MAX_EFFECTIVE_VOLUME);
		fadeIn(gainNode, effectiveVolume);
		const instance: AudioInstance = {
			audio,
			gainNode,
			sourceNode,
		};
		const playPromise = audio.play();
		if (playPromise) {
			try {
				await playPromise;
			} catch (error) {
				if (isAutoplayBlockedError(error)) {
					logger.debug('Autoplay blocked; dropping sound', {type});
					onAutoplayBlocked?.();
				} else {
					logger.warn(`Failed to play sound ${type}:`, error);
				}
				disconnectNodes(sourceNode, gainNode);
				return null;
			}
		}
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
		logger.warn(`Failed to initialize or play sound ${type}:`, error);
		return null;
	}
}

export function clearCustomSoundCache(type?: SoundType): void {
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
}

export async function stopSound(type: SoundType): Promise<void> {
	const activeSound = activeSounds.get(type);
	if (!activeSound) return;
	activeSounds.delete(type);
	const {audio, gainNode, sourceNode} = activeSound;
	try {
		await fadeOut(gainNode, 0.08);
	} catch {}
	audio.pause();
	audio.currentTime = 0;
	audio.loop = false;
	disconnectNodes(sourceNode, gainNode);
}

export async function stopAllSounds(): Promise<void> {
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
	activeOneShotSounds.forEach(({audio, cleanup}) => {
		audio.pause();
		audio.currentTime = 0;
		cleanup();
	});
	activeOneShotSounds.clear();
	await Promise.all(stopPromises);
}
