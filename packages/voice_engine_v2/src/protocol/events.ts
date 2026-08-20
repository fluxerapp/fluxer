// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SourceFault, SourceLifecycleState} from '../source_isolation/SourceLifecycleState';
import type {VoiceEngineV2CommandType} from './commands';

export type VoiceEngineV2SourceLifecycleTransitionKind = SourceLifecycleState['kind'];

export interface VoiceEngineV2SourceLifecycleTransitionedEvent {
	type: 'sourceLifecycle.transitioned';
	sourceId: string;
	kind: VoiceEngineV2SourceLifecycleTransitionKind;
	since: bigint;
	attempts: number;
	fault: SourceFault | null;
	atMs: number;
}

import type {
	VoiceEngineV2AudioControlsPatch,
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2Capabilities,
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2DataOptions,
	VoiceEngineV2DeviceChangeReason,
	VoiceEngineV2DeviceInventory,
	VoiceEngineV2DiagnosticEntry,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2Error,
	VoiceEngineV2GatewayDesiredVoiceState,
	VoiceEngineV2GatewayVoiceServer,
	VoiceEngineV2GatewayVoiceState,
	VoiceEngineV2GatewayVoiceStateWrite,
	VoiceEngineV2HardwareEncoderCapabilities,
	VoiceEngineV2InboundVideoFrame,
	VoiceEngineV2InboundVideoFrameStats,
	VoiceEngineV2InboundVideoTrackSubscription,
	VoiceEngineV2LifecycleReason,
	VoiceEngineV2LiveKitRoomState,
	VoiceEngineV2LocalStreamSource,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2NativeAudioTapOptions,
	VoiceEngineV2NativeCaptureFrame,
	VoiceEngineV2NativeCaptureOptions,
	VoiceEngineV2NativeFrameSinkOptions,
	VoiceEngineV2OperationId,
	VoiceEngineV2OutputDeviceOptions,
	VoiceEngineV2Participant,
	VoiceEngineV2ParticipantVolumeOptions,
	VoiceEngineV2PermissionName,
	VoiceEngineV2PermissionResult,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2ResourceKey,
	VoiceEngineV2ScreenAudioOptions,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2Stats,
	VoiceEngineV2TimerOptions,
	VoiceEngineV2Track,
	VoiceEngineV2VideoCodec,
	VoiceEngineV2WatchedStream,
	VoiceEngineV2WatchedStreamKey,
} from './types';

export type VoiceEngineV2Event =
	| {type: 'implementation.prewarmRequested'}
	| {type: 'implementation.prewarmSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'implementation.prewarmFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'command.succeeded'; operationId: VoiceEngineV2OperationId; commandType: VoiceEngineV2CommandType}
	| {
			type: 'command.failed';
			operationId: VoiceEngineV2OperationId;
			commandType: VoiceEngineV2CommandType;
			error: VoiceEngineV2Error;
	  }
	| {
			type: 'command.staleCompletionRejected';
			operationId: VoiceEngineV2OperationId;
			commandType: VoiceEngineV2CommandType;
			resourceKey: VoiceEngineV2ResourceKey;
	  }
	| {
			type: 'operation.cancelRequested';
			operationId: VoiceEngineV2OperationId;
			resourceKey: VoiceEngineV2ResourceKey;
			reason: string;
	  }
	| {
			type: 'operation.cancelled';
			operationId: VoiceEngineV2OperationId;
			targetOperationId: VoiceEngineV2OperationId;
			resourceKey: VoiceEngineV2ResourceKey;
	  }
	| {type: 'gateway.desiredVoiceStateChanged'; desired: VoiceEngineV2GatewayDesiredVoiceState}
	| {type: 'gateway.voiceStateReconcileRequested'}
	| {type: 'gateway.voiceStateWriteRequested'; options: VoiceEngineV2GatewayVoiceStateWrite}
	| {type: 'gateway.voiceStateWriteSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'gateway.voiceStateWriteFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'gateway.voiceStateClearRequested'; guildId: string | null}
	| {type: 'gateway.voiceStateClearSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'gateway.voiceStateClearFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'gateway.voiceStateUpdated'; voiceState: VoiceEngineV2GatewayVoiceState | null}
	| {type: 'gateway.voiceServerUpdated'; voiceServer: VoiceEngineV2GatewayVoiceServer | null}
	| {type: 'livekit.roomStateChanged'; room: VoiceEngineV2LiveKitRoomState}
	| {type: 'connection.connectRequested'; options: VoiceEngineV2ConnectOptions}
	| {type: 'connection.connectSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'connection.connectFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'connection.disconnectRequested'; reason: VoiceEngineV2DisconnectReason}
	| {type: 'connection.disconnectSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'connection.disconnectFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'connection.remoteDisconnected'; reason: VoiceEngineV2DisconnectReason; error?: VoiceEngineV2Error}
	| {type: 'connection.reconnectRequested'}
	| {type: 'connection.externallyEstablished'; options: VoiceEngineV2ConnectOptions}
	| {type: 'microphone.publishRequested'; options: VoiceEngineV2MicrophoneOptions}
	| {type: 'microphone.publishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'microphone.publishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'microphone.unpublishRequested'}
	| {type: 'microphone.unpublishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'microphone.unpublishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'microphone.setEnabledRequested'; enabled: boolean}
	| {type: 'microphone.setEnabledSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'microphone.setEnabledFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'localAudio.muteRequested'; muted: boolean}
	| {type: 'localAudio.deafenRequested'; deafened: boolean}
	| {type: 'camera.publishRequested'; options: VoiceEngineV2CameraOptions}
	| {type: 'camera.publishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'camera.publishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'camera.updateEncodingRequested'; options: VoiceEngineV2CameraEncodingOptions}
	| {type: 'camera.updateEncodingSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'camera.updateEncodingFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'camera.unpublishRequested'; options?: VoiceEngineV2CameraOptions}
	| {type: 'camera.unpublishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'camera.unpublishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'screen.publishRequested'; options: VoiceEngineV2ScreenOptions}
	| {type: 'screen.publishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'screen.publishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'screen.updateEncodingRequested'; options: VoiceEngineV2ScreenEncodingOptions}
	| {type: 'screen.updateEncodingSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'screen.updateEncodingFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'screen.unpublishRequested'}
	| {type: 'screen.unpublishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'screen.unpublishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'screenAudio.publishRequested'; options: VoiceEngineV2ScreenAudioOptions}
	| {type: 'screenAudio.publishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'screenAudio.publishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'screenAudio.unpublishRequested'}
	| {type: 'screenAudio.unpublishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'screenAudio.unpublishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'outputDevice.setRequested'; options: VoiceEngineV2OutputDeviceOptions}
	| {type: 'outputDevice.setSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'outputDevice.setFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'participantVolume.setRequested'; options: VoiceEngineV2ParticipantVolumeOptions}
	| {type: 'participantVolume.setSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'participantVolume.setFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'remoteTrackSubscription.setRequested'; options: VoiceEngineV2RemoteTrackSubscriptionOptions}
	| {type: 'remoteTrackSubscription.setSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'remoteTrackSubscription.setFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'data.publishRequested'; options: VoiceEngineV2DataOptions}
	| {type: 'data.publishSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'data.publishFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'stats.collectRequested'}
	| {type: 'stats.collected'; operationId: VoiceEngineV2OperationId; stats: VoiceEngineV2Stats}
	| {type: 'stats.collectFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'capabilities.changed'; capabilities: VoiceEngineV2Capabilities}
	| {type: 'capabilities.hardwareEncoderQueryRequested'}
	| {
			type: 'capabilities.hardwareEncoderChanged';
			operationId: VoiceEngineV2OperationId | null;
			capabilities: VoiceEngineV2HardwareEncoderCapabilities;
	  }
	| {
			type: 'capabilities.hardwareEncoderQueryFailed';
			operationId: VoiceEngineV2OperationId;
			error: VoiceEngineV2Error;
	  }
	| {type: 'permissions.checkRequested'; name: VoiceEngineV2PermissionName}
	| {type: 'permissions.requestRequested'; name: VoiceEngineV2PermissionName}
	| {
			type: 'permissions.result';
			operationId: VoiceEngineV2OperationId | null;
			result: VoiceEngineV2PermissionResult;
	  }
	| {
			type: 'permissions.failed';
			operationId: VoiceEngineV2OperationId;
			name: VoiceEngineV2PermissionName;
			error: VoiceEngineV2Error;
	  }
	| {type: 'devices.enumerateRequested'}
	| {
			type: 'devices.changed';
			operationId: VoiceEngineV2OperationId | null;
			reason: VoiceEngineV2DeviceChangeReason;
			devices: VoiceEngineV2DeviceInventory;
	  }
	| {type: 'devices.enumerateFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'devices.selectAudioInputRequested'; deviceId: string | null}
	| {type: 'devices.selectAudioOutputRequested'; deviceId: string | null}
	| {type: 'devices.selectCameraRequested'; deviceId: string | null}
	| {type: 'audioControls.changed'; controls: VoiceEngineV2AudioControlsPatch}
	| {type: 'nativeCapture.startRequested'; options: VoiceEngineV2NativeCaptureOptions}
	| {type: 'nativeCapture.updateRequested'; options: VoiceEngineV2NativeCaptureOptions}
	| {type: 'nativeCapture.stopRequested'; captureId: string}
	| {type: 'nativeCapture.started'; operationId: VoiceEngineV2OperationId; captureId: string}
	| {type: 'nativeCapture.stopped'; operationId: VoiceEngineV2OperationId | null; captureId: string}
	| {
			type: 'nativeCapture.failed';
			operationId: VoiceEngineV2OperationId | null;
			captureId: string;
			error: VoiceEngineV2Error;
	  }
	| {type: 'nativeCapture.frame'; frame: VoiceEngineV2NativeCaptureFrame}
	| {type: 'nativeAudioTap.startRequested'; options: VoiceEngineV2NativeAudioTapOptions}
	| {type: 'nativeAudioTap.stopRequested'; tapId: string}
	| {type: 'nativeFrameSink.attachRequested'; options: VoiceEngineV2NativeFrameSinkOptions}
	| {type: 'nativeFrameSink.detachRequested'; sinkId: string}
	| {type: 'e2ee.setEnabledRequested'; enabled: boolean; keyId?: string | null}
	| {type: 'e2ee.enabled'; operationId: VoiceEngineV2OperationId | null; keyId: string | null}
	| {type: 'e2ee.disabled'; operationId: VoiceEngineV2OperationId | null}
	| {type: 'e2ee.failed'; operationId: VoiceEngineV2OperationId | null; error: VoiceEngineV2Error}
	| {type: 'timer.scheduleRequested'; options: VoiceEngineV2TimerOptions}
	| {type: 'timer.cancelRequested'; timerId: string}
	| {type: 'timer.fired'; timerId: string; operationId: VoiceEngineV2OperationId | null}
	| {type: 'diagnostics.logRequested'; entry: VoiceEngineV2DiagnosticEntry}
	| {type: 'diagnostics.logged'; operationId: VoiceEngineV2OperationId | null; entry: VoiceEngineV2DiagnosticEntry}
	| {type: 'lifecycle.teardownRequested'; reason: VoiceEngineV2LifecycleReason}
	| {type: 'lifecycle.teardownSucceeded'; operationId: VoiceEngineV2OperationId}
	| {type: 'lifecycle.teardownFailed'; operationId: VoiceEngineV2OperationId; error: VoiceEngineV2Error}
	| {type: 'room.participantJoined'; participant: VoiceEngineV2Participant}
	| {type: 'room.participantLeft'; participantIdentity?: string; participantSid?: string}
	| {type: 'room.trackPublished'; track: VoiceEngineV2Track}
	| {type: 'room.trackUnpublished'; trackSid: string}
	| {type: 'room.trackMuted'; trackSid: string}
	| {type: 'room.trackUnmuted'; trackSid: string}
	| {
			type: 'codecNegotiation.overrideSetRequested';
			source: VoiceEngineV2LocalStreamSource;
			codec: VoiceEngineV2VideoCodec | null;
	  }
	| {type: 'codecNegotiation.localCapabilityChanged'; supportedVideoCodecs: Array<VoiceEngineV2VideoCodec>}
	| {
			type: 'codecNegotiation.streamRegistered';
			source: VoiceEngineV2LocalStreamSource;
			streamIdentity: string;
			preferredCodec: VoiceEngineV2VideoCodec;
	  }
	| {type: 'codecNegotiation.streamUnregistered'; source: VoiceEngineV2LocalStreamSource}
	| {
			type: 'codecNegotiation.viewerChanged';
			source: VoiceEngineV2LocalStreamSource;
			viewerIdentity: string;
			watching: boolean;
			supportedVideoCodecs: Array<VoiceEngineV2VideoCodec>;
	  }
	| {
			type: 'codecNegotiation.remoteCapabilityChanged';
			identity: string;
			supportedVideoCodecs: Array<VoiceEngineV2VideoCodec>;
	  }
	| {type: 'watchedStream.watchRequested'; stream: VoiceEngineV2WatchedStream}
	| {type: 'watchedStream.unwatchRequested'; stream: VoiceEngineV2WatchedStreamKey}
	| {type: 'watchedStreams.replaced'; streams: Array<VoiceEngineV2WatchedStream>}
	| {type: 'inboundVideo.trackSubscribed'; track: VoiceEngineV2InboundVideoTrackSubscription}
	| {type: 'inboundVideo.trackUnsubscribed'; trackSid: string}
	| {type: 'inboundVideo.frameReceived'; frame: VoiceEngineV2InboundVideoFrame}
	| {type: 'inboundVideo.frameStats'; stats: VoiceEngineV2InboundVideoFrameStats}
	| VoiceEngineV2SourceLifecycleTransitionedEvent;
