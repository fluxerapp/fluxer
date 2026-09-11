// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import type {Room} from 'livekit-client';
import {
	createVoiceEngineV2AppAudioSettingsSnapshot,
	hasVoiceEngineV2InputProcessorSettingsChanged,
	hasVoiceEngineV2MicrophoneCaptureSettingsChanged,
	type VoiceEngineV2AppAudioSettingsSnapshot,
} from './VoiceEngineV2AppAudioSettingsSync';

export interface VoiceEngineV2AppAudioPreferencesSnapshot {
	readonly audioSettings: VoiceEngineV2AppAudioSettingsSnapshot;
	readonly participantVolumes: Readonly<Record<string, number>>;
	readonly participantMutes: Readonly<Record<string, boolean>>;
	readonly connectionVolumes: Readonly<Record<string, Readonly<Record<string, number>>>>;
	readonly streamAudioRevision: number;
}

export interface VoiceEngineV2AppAudioPreferencesMediaAdapter {
	refreshMicrophone(room: Room): Promise<void>;
	refreshLocalVoiceInputProcessor(room: Room): Promise<void>;
	applyLocalInputVolume(room: Room): void;
	applyAllLocalAudioPreferences(room: Room): void;
}

export interface VoiceEngineV2AppAudioPreferencesLogger {
	warn(message: string, details?: unknown): void;
}

export interface VoiceEngineV2AppAudioPreferencesSyncSources {
	readonly stores: ReadonlyArray<{subscribe(listener: () => void): () => void}>;
	getSnapshot(): VoiceEngineV2AppAudioPreferencesSnapshot;
}

export function createVoiceEngineV2AppAudioPreferencesSnapshot(): VoiceEngineV2AppAudioPreferencesSnapshot {
	return {
		audioSettings: createVoiceEngineV2AppAudioSettingsSnapshot(),
		participantVolumes: ParticipantVolume.volumes,
		participantMutes: ParticipantVolume.localMutes,
		connectionVolumes: ParticipantVolume.connectionVolumesByLocalConnectionId,
		streamAudioRevision: StreamAudioPrefs.audioPrefsRevision,
	};
}

export function createVoiceEngineV2AppAudioPreferencesSyncSources(): VoiceEngineV2AppAudioPreferencesSyncSources {
	return {
		stores: [VoiceSettings, VoiceDevicePermissionState, ParticipantVolume, StreamAudioPrefs, ExperimentAssignments],
		getSnapshot: createVoiceEngineV2AppAudioPreferencesSnapshot,
	};
}

function refreshMicrophoneCapture(
	room: Room,
	adapter: VoiceEngineV2AppAudioPreferencesMediaAdapter,
	logger: VoiceEngineV2AppAudioPreferencesLogger,
): void {
	void adapter.refreshMicrophone(room).catch((error) => {
		logger.warn('Failed to refresh microphone after audio settings change', {error});
	});
}

function refreshVoiceInputProcessor(
	room: Room,
	adapter: VoiceEngineV2AppAudioPreferencesMediaAdapter,
	logger: VoiceEngineV2AppAudioPreferencesLogger,
): void {
	void adapter.refreshLocalVoiceInputProcessor(room).catch((error) => {
		logger.warn('Failed to refresh voice input processor after audio settings change', {error});
	});
}

export function syncVoiceEngineV2AppAudioPreferences(
	room: Room,
	adapter: VoiceEngineV2AppAudioPreferencesMediaAdapter,
	logger: VoiceEngineV2AppAudioPreferencesLogger,
	previous: VoiceEngineV2AppAudioPreferencesSnapshot,
	current: VoiceEngineV2AppAudioPreferencesSnapshot,
): void {
	assert.ok(room !== null && typeof room === 'object', 'audio preferences sync room must be an object');
	assert.equal(
		typeof adapter.refreshMicrophone,
		'function',
		'audio preferences sync adapter missing refreshMicrophone',
	);
	assert.equal(typeof logger.warn, 'function', 'audio preferences sync logger missing warn');
	if (hasVoiceEngineV2MicrophoneCaptureSettingsChanged(previous.audioSettings, current.audioSettings)) {
		refreshMicrophoneCapture(room, adapter, logger);
	} else if (hasVoiceEngineV2InputProcessorSettingsChanged(previous.audioSettings, current.audioSettings)) {
		refreshVoiceInputProcessor(room, adapter, logger);
	} else if (current.audioSettings.inputVolume !== previous.audioSettings.inputVolume) {
		adapter.applyLocalInputVolume(room);
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
