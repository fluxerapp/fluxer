// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2Command} from '../protocol/commands';
import type {SourceLifecycleState} from '../source_isolation/SourceLifecycleState';

export type {SourceLifecycleState} from '../source_isolation/SourceLifecycleState';

import type {
	VoiceEngineV2AudioControls,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2Capabilities,
	VoiceEngineV2CodecNegotiationState,
	VoiceEngineV2ConnectionStatus,
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2DeviceInventory,
	VoiceEngineV2DiagnosticEntry,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2E2eeState,
	VoiceEngineV2Error,
	VoiceEngineV2GatewayDesiredVoiceState,
	VoiceEngineV2GatewayVoiceServer,
	VoiceEngineV2GatewayVoiceState,
	VoiceEngineV2GatewayVoiceStateWrite,
	VoiceEngineV2HardwareEncoderCapabilities,
	VoiceEngineV2InboundVideoTrack,
	VoiceEngineV2LifecycleReason,
	VoiceEngineV2LiveKitRoomState,
	VoiceEngineV2MediaStatus,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2NativeAudioTapOptions,
	VoiceEngineV2NativeCaptureOptions,
	VoiceEngineV2NativeFrameSinkOptions,
	VoiceEngineV2OperationId,
	VoiceEngineV2Participant,
	VoiceEngineV2PermissionName,
	VoiceEngineV2PermissionResult,
	VoiceEngineV2PermissionStatus,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2ResourceKey,
	VoiceEngineV2ScreenAudioOptions,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2Stats,
	VoiceEngineV2Track,
	VoiceEngineV2WatchedStream,
} from '../protocol/types';

export interface VoiceEngineV2ConnectionState {
	status: VoiceEngineV2ConnectionStatus;
	active: VoiceEngineV2ConnectOptions | null;
	desired: VoiceEngineV2ConnectOptions | null;
	operationId: VoiceEngineV2OperationId | null;
	disconnectReason: VoiceEngineV2DisconnectReason | null;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2LocalMediaState<Options> {
	status: VoiceEngineV2MediaStatus;
	desired: Options | null;
	published: Options | null;
	operationId: VoiceEngineV2OperationId | null;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2MicrophoneState extends VoiceEngineV2LocalMediaState<VoiceEngineV2MicrophoneOptions> {
	enabled: boolean;
	localSpeakingOverride: boolean | null;
	setEnabledOperationId: VoiceEngineV2OperationId | null;
}

export interface VoiceEngineV2OutputDeviceState {
	desiredDeviceId: string | null;
	activeDeviceId: string | null;
	operationId: VoiceEngineV2OperationId | null;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2RoomState {
	participants: Record<string, VoiceEngineV2Participant>;
	tracks: Record<string, VoiceEngineV2Track>;
}

export interface VoiceEngineV2InboundVideoState {
	tracks: Record<string, VoiceEngineV2InboundVideoTrack>;
	droppedFrameCount: number;
}

export interface VoiceEngineV2GatewayState {
	desiredVoiceState: VoiceEngineV2GatewayDesiredVoiceState | null;
	desiredVoiceStateWrite: VoiceEngineV2GatewayVoiceStateWrite | null;
	selfVoiceState: VoiceEngineV2GatewayVoiceState | null;
	voiceServer: VoiceEngineV2GatewayVoiceServer | null;
	operationId: VoiceEngineV2OperationId | null;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2LiveKitState extends VoiceEngineV2LiveKitRoomState {
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2PermissionState {
	results: Record<string, VoiceEngineV2PermissionResult>;
	operationIds: Record<string, VoiceEngineV2OperationId>;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2DeviceState {
	inventory: VoiceEngineV2DeviceInventory;
	operationId: VoiceEngineV2OperationId | null;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2HardwareEncoderState {
	capabilities: VoiceEngineV2HardwareEncoderCapabilities | null;
	operationId: VoiceEngineV2OperationId | null;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2E2eeLifecycleState extends VoiceEngineV2E2eeState {
	operationId: VoiceEngineV2OperationId | null;
}

export interface VoiceEngineV2NativeCaptureState {
	captures: Record<string, VoiceEngineV2NativeCaptureOptions>;
	operationIds: Record<string, VoiceEngineV2OperationId>;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2NativeAudioTapState {
	taps: Record<string, VoiceEngineV2NativeAudioTapOptions>;
	operationIds: Record<string, VoiceEngineV2OperationId>;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2NativeFrameSinkState {
	sinks: Record<string, VoiceEngineV2NativeFrameSinkOptions>;
	operationIds: Record<string, VoiceEngineV2OperationId>;
	failure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2LifecycleState {
	tearingDown: boolean;
	reason: VoiceEngineV2LifecycleReason | null;
	operationId: VoiceEngineV2OperationId | null;
	failure: VoiceEngineV2Error | null;
}

export type VoiceEngineV2OperationStatus = 'queued' | 'running' | 'succeeded' | 'failed' | 'cancelled' | 'stale';

export interface VoiceEngineV2OperationState {
	operationId: VoiceEngineV2OperationId;
	commandType: string;
	resourceKey: VoiceEngineV2ResourceKey;
	status: VoiceEngineV2OperationStatus;
	error: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2Snapshot {
	nextOperationId: VoiceEngineV2OperationId;
	capabilities: VoiceEngineV2Capabilities;
	operations: Record<string, VoiceEngineV2OperationState>;
	connection: VoiceEngineV2ConnectionState;
	gateway: VoiceEngineV2GatewayState;
	liveKit: VoiceEngineV2LiveKitState;
	microphone: VoiceEngineV2MicrophoneState;
	camera: VoiceEngineV2LocalMediaState<VoiceEngineV2CameraOptions>;
	screen: VoiceEngineV2LocalMediaState<VoiceEngineV2ScreenOptions>;
	screenAudio: VoiceEngineV2LocalMediaState<VoiceEngineV2ScreenAudioOptions>;
	audioControls: VoiceEngineV2AudioControls;
	outputDevice: VoiceEngineV2OutputDeviceState;
	participantVolumes: Record<string, number>;
	remoteTrackSubscriptions: Record<string, VoiceEngineV2RemoteTrackSubscriptionOptions>;
	watchedStreams: Record<string, VoiceEngineV2WatchedStream>;
	codecNegotiation: VoiceEngineV2CodecNegotiationState;
	stats: VoiceEngineV2Stats | null;
	statsOperationId: VoiceEngineV2OperationId | null;
	statsFailure: VoiceEngineV2Error | null;
	hardwareEncoder: VoiceEngineV2HardwareEncoderState;
	permissions: VoiceEngineV2PermissionState;
	devices: VoiceEngineV2DeviceState;
	e2ee: VoiceEngineV2E2eeLifecycleState;
	nativeCapture: VoiceEngineV2NativeCaptureState;
	nativeAudioTap: VoiceEngineV2NativeAudioTapState;
	nativeFrameSink: VoiceEngineV2NativeFrameSinkState;
	diagnostics: Array<VoiceEngineV2DiagnosticEntry>;
	lifecycle: VoiceEngineV2LifecycleState;
	room: VoiceEngineV2RoomState;
	inboundVideo: VoiceEngineV2InboundVideoState;
	sourceLifecycles: Record<string, SourceLifecycleState>;
	lastFailure: VoiceEngineV2Error | null;
}

export interface VoiceEngineV2Transition {
	snapshot: VoiceEngineV2Snapshot;
	commands: Array<VoiceEngineV2Command>;
}

export function unavailableVoiceEngineV2Capabilities(): VoiceEngineV2Capabilities {
	return {
		connect: false,
		microphone: false,
		camera: false,
		screen: false,
		screenAudio: false,
		outputDevice: false,
		participantVolume: false,
		remoteTrackSubscription: false,
		dataChannel: false,
		stats: false,
		nativeVideoFrames: false,
		hardwareEncoding: false,
		zeroCopyScreenTransport: false,
		nativeAudioTaps: false,
	};
}

export function availableVoiceEngineV2Capabilities(): VoiceEngineV2Capabilities {
	return {
		connect: true,
		microphone: true,
		camera: true,
		screen: true,
		screenAudio: true,
		outputDevice: true,
		participantVolume: true,
		remoteTrackSubscription: true,
		dataChannel: true,
		stats: true,
		nativeVideoFrames: true,
		hardwareEncoding: true,
		zeroCopyScreenTransport: true,
		nativeAudioTaps: true,
	};
}

export function emptyVoiceEngineV2DeviceInventory(): VoiceEngineV2DeviceInventory {
	return {
		audioInputs: [],
		audioOutputs: [],
		cameras: [],
		selectedAudioInputId: null,
		selectedAudioOutputId: null,
		selectedCameraId: null,
	};
}

export function createVoiceEngineV2PermissionResult(
	name: VoiceEngineV2PermissionName,
	status: VoiceEngineV2PermissionStatus = 'unknown',
): VoiceEngineV2PermissionResult {
	return {
		name,
		status,
		canPrompt: status === 'prompt' || status === 'unknown',
	};
}

export function createVoiceEngineV2InitialSnapshot(
	capabilities: VoiceEngineV2Capabilities = unavailableVoiceEngineV2Capabilities(),
): VoiceEngineV2Snapshot {
	return {
		nextOperationId: 1,
		capabilities,
		operations: {},
		connection: {
			status: 'idle',
			active: null,
			desired: null,
			operationId: null,
			disconnectReason: null,
			failure: null,
		},
		gateway: {
			desiredVoiceState: null,
			desiredVoiceStateWrite: null,
			selfVoiceState: null,
			voiceServer: null,
			operationId: null,
			failure: null,
		},
		liveKit: {
			connectionState: 'disconnected',
			roomSid: null,
			roomName: null,
			serverRegion: null,
			failure: null,
		},
		microphone: {
			status: 'idle',
			desired: null,
			published: null,
			operationId: null,
			failure: null,
			enabled: true,
			localSpeakingOverride: false,
			setEnabledOperationId: null,
		},
		camera: {
			status: 'idle',
			desired: null,
			published: null,
			operationId: null,
			failure: null,
		},
		screen: {
			status: 'idle',
			desired: null,
			published: null,
			operationId: null,
			failure: null,
		},
		screenAudio: {
			status: 'idle',
			desired: null,
			published: null,
			operationId: null,
			failure: null,
		},
		audioControls: {
			mode: 'voiceActivity',
			locallyMuted: false,
			preferredLocallyMuted: false,
			locallyDeafened: false,
			mutedByPermission: false,
			hasUserSetMute: false,
			hasUserSetDeaf: false,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		},
		outputDevice: {
			desiredDeviceId: null,
			activeDeviceId: null,
			operationId: null,
			failure: null,
		},
		participantVolumes: {},
		remoteTrackSubscriptions: {},
		watchedStreams: {},
		codecNegotiation: {
			overrides: {},
			localSupportedVideoCodecs: [],
			remoteSupportedVideoCodecs: {},
			streams: {},
		},
		stats: null,
		statsOperationId: null,
		statsFailure: null,
		hardwareEncoder: {
			capabilities: null,
			operationId: null,
			failure: null,
		},
		permissions: {
			results: {},
			operationIds: {},
			failure: null,
		},
		devices: {
			inventory: emptyVoiceEngineV2DeviceInventory(),
			operationId: null,
			failure: null,
		},
		e2ee: {
			status: 'disabled',
			keyId: null,
			failure: null,
			operationId: null,
		},
		nativeCapture: {
			captures: {},
			operationIds: {},
			failure: null,
		},
		nativeAudioTap: {
			taps: {},
			operationIds: {},
			failure: null,
		},
		nativeFrameSink: {
			sinks: {},
			operationIds: {},
			failure: null,
		},
		diagnostics: [],
		lifecycle: {
			tearingDown: false,
			reason: null,
			operationId: null,
			failure: null,
		},
		room: {
			participants: {},
			tracks: {},
		},
		inboundVideo: {
			tracks: {},
			droppedFrameCount: 0,
		},
		sourceLifecycles: {},
		lastFailure: null,
	};
}
