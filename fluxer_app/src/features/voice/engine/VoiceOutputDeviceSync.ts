// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {VoiceTrackKind} from '@app/features/voice/engine/VoiceTrackSource';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {voiceDeviceManager} from '@app/features/voice/utils/VoiceDeviceManager';
import type {RemoteAudioTrack, Room} from 'livekit-client';

const logger = new Logger('VoiceOutputDeviceSync');

interface SinkableAudioContext extends AudioContext {
	setSinkId?: (
		sinkId:
			| string
			| {
					type: 'none';
			  },
	) => Promise<void>;
}

type SinkableMediaElement = HTMLMediaElement & {
	setSinkId?: (sinkId: string) => Promise<void>;
};

function normalizeSinkIdForBrowserApi(deviceId: string): string {
	return deviceId === 'default' ? '' : deviceId;
}

function isDeviceMissingError(error: unknown): boolean {
	if (!(error instanceof Error)) return false;
	if (error.name === 'NotFoundError') return true;
	return /not\s+found/i.test(error.message);
}

function isSinkableAudioContext(value: unknown): value is SinkableAudioContext {
	if (typeof value !== 'object' || value === null) return false;
	const candidate = value as {setSinkId?: unknown};
	return candidate.setSinkId === undefined || typeof candidate.setSinkId === 'function';
}

function getRoomAudioContext(room: Room): SinkableAudioContext | null {
	const audioContext: unknown = Reflect.get(room, 'audioContext');
	return isSinkableAudioContext(audioContext) ? audioContext : null;
}

function isRoomWebAudioMixEnabled(room: Room): boolean {
	const options: unknown = Reflect.get(room, 'options');
	if (typeof options !== 'object' || options === null) return false;
	return Boolean((options as {webAudioMix?: unknown}).webAudioMix);
}

function supportsMediaElementSetSinkId(): boolean {
	if (typeof HTMLMediaElement === 'undefined') return false;
	return 'setSinkId' in HTMLMediaElement.prototype;
}

function canRoomSwitchAudioOutput(room: Room): boolean {
	if (!isRoomWebAudioMixEnabled(room)) return supportsMediaElementSetSinkId();
	const audioContext = getRoomAudioContext(room);
	if (!audioContext) return true;
	return typeof audioContext.setSinkId === 'function';
}

function resolveLiveKitSwitchDeviceId(room: Room, deviceId: string): string {
	if (isRoomWebAudioMixEnabled(room)) return deviceId;
	return normalizeSinkIdForBrowserApi(deviceId);
}

function isSinkableMediaElement(value: unknown): value is SinkableMediaElement {
	if (typeof HTMLMediaElement === 'undefined') return false;
	return value instanceof HTMLMediaElement;
}

function getAttachedElements(track: RemoteAudioTrack | undefined): ReadonlyArray<SinkableMediaElement> {
	if (!track) return [];
	const attachedElements: unknown = Reflect.get(track, 'attachedElements');
	if (!Array.isArray(attachedElements)) return [];
	return attachedElements.filter(isSinkableMediaElement);
}

async function resolveAvailableOutputDeviceId(deviceId: string): Promise<string> {
	if (deviceId === 'default') {
		return deviceId;
	}
	const state = await voiceDeviceManager.ensureDevices({requestPermissions: false});
	if (state.outputDevices.length === 0) {
		return deviceId;
	}
	if (state.outputDevices.some((device) => device.deviceId === deviceId)) {
		return deviceId;
	}
	logger.warn('Selected audio output device no longer available; falling back to default', {deviceId});
	VoiceSettings.updateSettings({outputDeviceId: 'default'});
	return 'default';
}

async function applyOutputDeviceToWebAudioMixer(room: Room, deviceId: string): Promise<void> {
	const audioContext = getRoomAudioContext(room);
	if (!audioContext?.setSinkId) {
		return;
	}
	const sinkId = normalizeSinkIdForBrowserApi(deviceId);
	try {
		await audioContext.setSinkId(sinkId);
	} catch (error) {
		if (isDeviceMissingError(error)) {
			logger.warn('Web Audio mixer sink no longer available', {deviceId});
			if (deviceId !== 'default') {
				VoiceSettings.updateSettings({outputDeviceId: 'default'});
				try {
					await audioContext.setSinkId('');
				} catch (fallbackError) {
					logger.warn('Failed to fall back Web Audio mixer sink to default', {deviceId, error: fallbackError});
				}
			}
			return;
		}
		throw error;
	}
}

async function applyOutputDeviceToAttachedElements(room: Room, deviceId: string): Promise<void> {
	const sinkId = normalizeSinkIdForBrowserApi(deviceId);
	const operations: Array<Promise<void>> = [];
	room.remoteParticipants.forEach((participant) => {
		participant.audioTrackPublications.forEach((publication) => {
			if (publication.kind !== VoiceTrackKind.Audio) return;
			const track = publication.track as RemoteAudioTrack | undefined;
			if (track?.kind === VoiceTrackKind.Audio && typeof track.setSinkId === 'function') {
				operations.push(track.setSinkId(sinkId));
				return;
			}
			for (const element of getAttachedElements(track)) {
				if (typeof element.setSinkId === 'function') {
					operations.push(element.setSinkId(sinkId));
				}
			}
		});
	});
	const results = await Promise.allSettled(operations);
	for (const result of results) {
		if (result.status !== 'rejected') continue;
		if (isDeviceMissingError(result.reason)) {
			logger.warn('Audio element sink no longer available', {deviceId});
			continue;
		}
		logger.warn('Failed to apply audio output device to attached audio element', {deviceId, error: result.reason});
	}
}

export async function applyOutputDeviceToRoom(room: Room, deviceId: string): Promise<void> {
	const resolvedDeviceId = await resolveAvailableOutputDeviceId(deviceId);
	if (canRoomSwitchAudioOutput(room)) {
		try {
			await room.switchActiveDevice('audiooutput', resolveLiveKitSwitchDeviceId(room, resolvedDeviceId));
		} catch (error) {
			logger.warn('LiveKit failed to apply audio output device', {deviceId: resolvedDeviceId, error});
		}
	} else {
		logger.warn('Audio output selection is unavailable: this browser cannot move the voice playback sink', {
			deviceId: resolvedDeviceId,
			webAudioMix: isRoomWebAudioMixEnabled(room),
		});
	}
	try {
		await applyOutputDeviceToWebAudioMixer(room, resolvedDeviceId);
	} catch (error) {
		logger.warn('Failed to apply audio output device to Web Audio mixer', {deviceId: resolvedDeviceId, error});
	}
	try {
		await applyOutputDeviceToAttachedElements(room, resolvedDeviceId);
	} catch (error) {
		logger.warn('Failed to apply audio output device to attached audio elements', {deviceId: resolvedDeviceId, error});
	}
}

export function bindOutputDeviceSync(room: Room): () => void {
	const initial = VoiceSettings.getOutputDeviceId();
	if (initial) {
		void applyOutputDeviceToRoom(room, initial).catch((error) => {
			logger.warn('Initial audio output sync failed', {deviceId: initial, error});
		});
	}
	let previousDeviceId = initial;
	return VoiceSettings.subscribe(() => {
		const deviceId = VoiceSettings.getOutputDeviceId();
		if (!deviceId || deviceId === previousDeviceId) return;
		previousDeviceId = deviceId;
		void applyOutputDeviceToRoom(room, deviceId).catch((error) => {
			logger.warn('Audio output sync failed', {deviceId, error});
		});
	});
}
