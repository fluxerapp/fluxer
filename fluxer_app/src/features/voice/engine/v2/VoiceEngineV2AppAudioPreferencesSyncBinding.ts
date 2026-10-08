// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import Keybind from '@app/features/input/state/InputKeybind';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import {
	createVoiceEngineV2AppAudioSettingsSnapshot,
	hasVoiceEngineV2InputProcessorSettingsChanged,
	hasVoiceEngineV2MicrophoneCaptureSettingsChanged,
	hasVoiceEngineV2MicrophonePublishSettingsChanged,
	type VoiceEngineV2AppAudioSettingsSnapshot,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppAudioSettingsSync';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import NoiseSuppressionAvailability from '@app/features/voice/utils/noise_suppression/NoiseSuppressionAvailability';
import type {Room} from 'livekit-client';
import {reaction} from 'mobx';

interface VoiceEngineV2AppAudioPreferencesSnapshot {
	readonly audioSettings: VoiceEngineV2AppAudioSettingsSnapshot;
	readonly participantVolumes: Readonly<Record<string, number>>;
	readonly participantMutes: Readonly<Record<string, boolean>>;
	readonly connectionVolumes: Readonly<Record<string, Readonly<Record<string, number>>>>;
	readonly streamAudioRevision: number;
	readonly transmitMode: typeof Keybind.transmitMode;
	readonly pushToMuteActive: boolean;
}

export interface VoiceEngineV2AppAudioPreferencesMediaAdapter {
	requestMicrophoneRefresh(room: Room, request: {republish: boolean}): Promise<void>;
	configureVoiceInput(room: Room): void;
	handleInputKeybindChange(room: Room, options: {preserveSelfMute: boolean}): void;
	applyAllLocalAudioPreferences(room: Room): void;
}

export interface VoiceEngineV2AppAudioPreferencesLogger {
	warn(message: string, details?: unknown): void;
}

export interface VoiceEngineV2AppAudioPreferencesSyncSources {
	readonly stores: ReadonlyArray<{subscribe(listener: () => void): () => void}>;
	getSnapshot(): VoiceEngineV2AppAudioPreferencesSnapshot;
}

function createVoiceEngineV2AppAudioPreferencesSnapshot(): VoiceEngineV2AppAudioPreferencesSnapshot {
	return {
		audioSettings: createVoiceEngineV2AppAudioSettingsSnapshot(),
		participantVolumes: ParticipantVolume.volumes,
		participantMutes: ParticipantVolume.localMutes,
		connectionVolumes: ParticipantVolume.connectionVolumesByLocalConnectionId,
		streamAudioRevision: StreamAudioPrefs.audioPrefsRevision,
		transmitMode: Keybind.transmitMode,
		pushToMuteActive: Keybind.isPushToMuteEffective(),
	};
}

function createVoiceEngineV2AppAudioPreferencesSyncSources(): VoiceEngineV2AppAudioPreferencesSyncSources {
	return {
		stores: [
			VoiceSettings,
			VoiceDevicePermissionState,
			ParticipantVolume,
			StreamAudioPrefs,
			NoiseSuppressionAvailability,
			{
				subscribe: (listener) => reaction(() => [Keybind.transmitMode, Keybind.isPushToMuteEffective()], listener),
			},
		],
		getSnapshot: createVoiceEngineV2AppAudioPreferencesSnapshot,
	};
}

function refreshMicrophone(
	room: Room,
	adapter: VoiceEngineV2AppAudioPreferencesMediaAdapter,
	logger: VoiceEngineV2AppAudioPreferencesLogger,
	republish: boolean,
): void {
	void adapter.requestMicrophoneRefresh(room, {republish}).catch((error) => {
		logger.warn('Failed to refresh microphone after audio settings change', {error});
	});
}

function syncVoiceEngineV2AppAudioPreferences(
	room: Room,
	adapter: VoiceEngineV2AppAudioPreferencesMediaAdapter,
	logger: VoiceEngineV2AppAudioPreferencesLogger,
	previous: VoiceEngineV2AppAudioPreferencesSnapshot,
	current: VoiceEngineV2AppAudioPreferencesSnapshot,
): void {
	assert.ok(room !== null && typeof room === 'object', 'audio preferences sync room must be an object');
	assert.equal(
		typeof adapter.requestMicrophoneRefresh,
		'function',
		'audio preferences sync adapter missing requestMicrophoneRefresh',
	);
	assert.equal(typeof logger.warn, 'function', 'audio preferences sync logger missing warn');
	if (previous.pushToMuteActive !== current.pushToMuteActive) {
		adapter.handleInputKeybindChange(room, {preserveSelfMute: previous.transmitMode === current.transmitMode});
	}
	const republish = hasVoiceEngineV2MicrophonePublishSettingsChanged(previous.audioSettings, current.audioSettings);
	if (republish || hasVoiceEngineV2MicrophoneCaptureSettingsChanged(previous.audioSettings, current.audioSettings)) {
		refreshMicrophone(room, adapter, logger, republish);
	} else if (hasVoiceEngineV2InputProcessorSettingsChanged(previous.audioSettings, current.audioSettings)) {
		adapter.configureVoiceInput(room);
	}
	if (
		current.audioSettings.outputVolume !== previous.audioSettings.outputVolume ||
		current.participantVolumes !== previous.participantVolumes ||
		current.participantMutes !== previous.participantMutes ||
		current.connectionVolumes !== previous.connectionVolumes ||
		current.streamAudioRevision !== previous.streamAudioRevision
	) {
		adapter.applyAllLocalAudioPreferences(room);
	}
}

export function bindVoiceEngineV2AppAudioPreferencesSync(
	room: Room,
	adapter: VoiceEngineV2AppAudioPreferencesMediaAdapter,
	logger: VoiceEngineV2AppAudioPreferencesLogger,
	sources: VoiceEngineV2AppAudioPreferencesSyncSources = createVoiceEngineV2AppAudioPreferencesSyncSources(),
): () => void {
	assert.ok(room !== null && typeof room === 'object', 'bind audio preferences sync room must be an object');
	assert.ok(Array.isArray(sources.stores), 'bind audio preferences sync sources.stores must be an array');
	let previous = sources.getSnapshot();
	const sync = (): void => {
		const current = sources.getSnapshot();
		syncVoiceEngineV2AppAudioPreferences(room, adapter, logger, previous, current);
		previous = current;
	};
	const disposers = sources.stores.map((store) => store.subscribe(sync));
	return () => {
		for (const dispose of disposers) {
			dispose();
		}
	};
}
