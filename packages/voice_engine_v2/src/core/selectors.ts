// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	hasVoiceEngineV2NativeNvencEncoder,
	hasVoiceEngineV2ZeroCopyNativeInput,
	summarizeVoiceEngineV2Stats,
	type VoiceEngineV2StatsNetworkSummary,
	type VoiceEngineV2StatsSummary,
} from '../policies';
import type {
	VoiceEngineV2AudioInputDevice,
	VoiceEngineV2AudioOutputDevice,
	VoiceEngineV2CameraDevice,
	VoiceEngineV2Capabilities,
	VoiceEngineV2ConnectionModel,
	VoiceEngineV2DeviceInventory,
	VoiceEngineV2DiagnosticEntry,
	VoiceEngineV2E2eeState,
	VoiceEngineV2Error,
	VoiceEngineV2HardwareEncoderCapabilities,
	VoiceEngineV2InboundVideoTrack,
	VoiceEngineV2MediaModel,
	VoiceEngineV2Model,
	VoiceEngineV2Participant,
	VoiceEngineV2PermissionResult,
	VoiceEngineV2Stats,
	VoiceEngineV2Track,
	VoiceEngineV2WatchedStream,
} from '../protocol/types';
import type {SourceLifecycleState} from '../source_isolation/SourceLifecycleState';
import type {VoiceEngineV2Snapshot} from './state';

export interface VoiceEngineV2ParticipantProjection {
	participants: Array<VoiceEngineV2Participant>;
	tracks: Array<VoiceEngineV2Track>;
	inboundVideoTracks: Array<VoiceEngineV2InboundVideoTrack>;
}

export interface VoiceEngineV2DeviceProjection {
	devices: VoiceEngineV2DeviceInventory;
	permissions: Record<string, VoiceEngineV2PermissionResult>;
	selectedAudioInput: VoiceEngineV2AudioInputDevice | null;
	selectedAudioOutput: VoiceEngineV2AudioOutputDevice | null;
	selectedCamera: VoiceEngineV2CameraDevice | null;
}

export interface VoiceEngineV2StatsProjection {
	stats: VoiceEngineV2Stats | null;
	summary: VoiceEngineV2StatsSummary | null;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2StatsPresentationNetworkSummary extends VoiceEngineV2StatsNetworkSummary {
	droppedVideoFrameCallbacks?: number;
}

export interface VoiceEngineV2StatsPresentationProjection extends VoiceEngineV2StatsSummary {
	network: VoiceEngineV2StatsPresentationNetworkSummary;
}

export interface VoiceEngineV2CapabilitiesProjection {
	capabilities: VoiceEngineV2Capabilities;
	hardwareEncoderCapabilities: VoiceEngineV2HardwareEncoderCapabilities | null;
	hardwareEncoderFailure: VoiceEngineV2Error | null;
	hasZeroCopyNativeInput: boolean;
	hasNativeNvencH264: boolean;
	hasNativeNvencH265: boolean;
}

export interface VoiceEngineV2DiagnosticsProjection {
	entries: Array<VoiceEngineV2DiagnosticEntry>;
	lastFailure: VoiceEngineV2Error | null;
}

export function selectVoiceEngineV2E2eeProjection(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2E2eeState {
	return {
		status: snapshot.e2ee.status,
		keyId: snapshot.e2ee.keyId,
		failure: snapshot.e2ee.failure,
	};
}

export function selectVoiceEngineV2ConnectionProjection(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2ConnectionModel {
	const status = snapshot.connection.status;
	return {
		status,
		connected: status === 'connected',
		connecting: status === 'connecting',
		reconnecting: status === 'reconnecting',
		failed: status === 'failed',
		gateway: {
			selfVoiceState: snapshot.gateway.selfVoiceState,
			voiceServer: snapshot.gateway.voiceServer,
		},
		liveKit: {
			connectionState: snapshot.liveKit.connectionState,
			roomSid: snapshot.liveKit.roomSid,
			roomName: snapshot.liveKit.roomName,
			serverRegion: snapshot.liveKit.serverRegion,
		},
	};
}

export function selectVoiceEngineV2MediaProjection(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2MediaModel {
	return {
		microphone: snapshot.microphone.status,
		camera: snapshot.camera.status,
		screen: snapshot.screen.status,
		screenAudio: snapshot.screenAudio.status,
		audio: snapshot.audioControls,
		effectiveMicrophoneEnabled: snapshot.microphone.enabled,
		localSpeakingOverride: snapshot.microphone.localSpeakingOverride,
		e2ee: selectVoiceEngineV2E2eeProjection(snapshot),
		screenCaptureId: snapshot.screen.published?.captureId ?? snapshot.screen.desired?.captureId ?? null,
	};
}

export function selectVoiceEngineV2ParticipantProjection(
	snapshot: VoiceEngineV2Snapshot,
): VoiceEngineV2ParticipantProjection {
	return {
		participants: Object.values(snapshot.room.participants),
		tracks: Object.values(snapshot.room.tracks),
		inboundVideoTracks: Object.values(snapshot.inboundVideo.tracks),
	};
}

export function selectVoiceEngineV2WatchedStreams(snapshot: VoiceEngineV2Snapshot): Array<VoiceEngineV2WatchedStream> {
	return Object.values(snapshot.watchedStreams);
}

export function selectVoiceEngineV2DeviceProjection(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2DeviceProjection {
	const {inventory} = snapshot.devices;
	return {
		devices: inventory,
		permissions: snapshot.permissions.results,
		selectedAudioInput:
			inventory.audioInputs.find((device) => device.deviceId === inventory.selectedAudioInputId) ?? null,
		selectedAudioOutput:
			inventory.audioOutputs.find((device) => device.deviceId === inventory.selectedAudioOutputId) ?? null,
		selectedCamera: inventory.cameras.find((device) => device.deviceId === inventory.selectedCameraId) ?? null,
	};
}

export function selectVoiceEngineV2StatsSummary(stats: VoiceEngineV2Stats | null): VoiceEngineV2StatsSummary | null {
	return stats ? summarizeVoiceEngineV2Stats(stats) : null;
}

export function selectVoiceEngineV2StatsPresentationProjection(
	stats: VoiceEngineV2Stats | null,
): VoiceEngineV2StatsPresentationProjection | null {
	const summary = selectVoiceEngineV2StatsSummary(stats);
	if (!stats || !summary) return null;
	const droppedVideoFrameCallbacks = stats.droppedVideoFrameCallbacks ?? stats.droppedNativeVideoFrames;
	if (droppedVideoFrameCallbacks == null) return summary;
	return {
		...summary,
		network: {
			...summary.network,
			droppedVideoFrameCallbacks,
		},
	};
}

export function selectVoiceEngineV2StatsProjection(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2StatsProjection {
	return {
		stats: snapshot.stats,
		summary: selectVoiceEngineV2StatsSummary(snapshot.stats),
		failure: snapshot.statsFailure,
	};
}

export function selectVoiceEngineV2CapabilitiesProjection(
	snapshot: VoiceEngineV2Snapshot,
): VoiceEngineV2CapabilitiesProjection {
	const hardwareEncoderCapabilities = snapshot.hardwareEncoder.capabilities;
	return {
		capabilities: snapshot.capabilities,
		hardwareEncoderCapabilities,
		hardwareEncoderFailure: snapshot.hardwareEncoder.failure,
		hasZeroCopyNativeInput: hasVoiceEngineV2ZeroCopyNativeInput(hardwareEncoderCapabilities),
		hasNativeNvencH264: hasVoiceEngineV2NativeNvencEncoder(hardwareEncoderCapabilities, 'h264'),
		hasNativeNvencH265: hasVoiceEngineV2NativeNvencEncoder(hardwareEncoderCapabilities, 'h265'),
	};
}

export function selectVoiceEngineV2DiagnosticsProjection(
	snapshot: VoiceEngineV2Snapshot,
): VoiceEngineV2DiagnosticsProjection {
	return {
		entries: snapshot.diagnostics,
		lastFailure: snapshot.lastFailure,
	};
}

export function selectVoiceEngineV2SourceLifecycle(
	snapshot: VoiceEngineV2Snapshot,
	sourceId: string,
): SourceLifecycleState | null {
	if (typeof sourceId !== 'string') throw new TypeError('sourceId must be a string');
	if (sourceId.length === 0) throw new RangeError('sourceId must not be empty');
	return snapshot.sourceLifecycles[sourceId] ?? null;
}

export function selectVoiceEngineV2FailedSourceIds(snapshot: VoiceEngineV2Snapshot): ReadonlyArray<string> {
	if (snapshot === null || typeof snapshot !== 'object') throw new TypeError('snapshot must be an object');
	const lifecycles = snapshot.sourceLifecycles;
	if (lifecycles === null || typeof lifecycles !== 'object') throw new TypeError('sourceLifecycles must be a record');
	const result: Array<string> = [];
	for (const sourceId of Object.keys(lifecycles).sort()) {
		const state = lifecycles[sourceId];
		if (state && state.kind === 'failed') result.push(sourceId);
	}
	return result;
}

export function selectVoiceEngineV2Model(snapshot: VoiceEngineV2Snapshot): VoiceEngineV2Model {
	const status = snapshot.connection.status;
	const participants = selectVoiceEngineV2ParticipantProjection(snapshot);
	const devices = selectVoiceEngineV2DeviceProjection(snapshot);
	return {
		connection: selectVoiceEngineV2ConnectionProjection(snapshot),
		media: selectVoiceEngineV2MediaProjection(snapshot),
		canPublishMedia: status === 'connected',
		hasActiveLocalMedia:
			snapshot.microphone.status === 'published' ||
			snapshot.camera.status === 'published' ||
			snapshot.screen.status === 'published' ||
			snapshot.screenAudio.status === 'published',
		participants: participants.participants,
		tracks: participants.tracks,
		watchedStreams: selectVoiceEngineV2WatchedStreams(snapshot),
		inboundVideoTracks: participants.inboundVideoTracks,
		devices: devices.devices,
		permissions: devices.permissions,
		stats: snapshot.stats,
		diagnostics: snapshot.diagnostics,
		tearingDown: snapshot.lifecycle.tearingDown,
	};
}
