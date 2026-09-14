// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2DataOptions,
	VoiceEngineV2DiagnosticEntry,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2GatewayVoiceStateWrite,
	VoiceEngineV2LifecycleReason,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2NativeAudioTapOptions,
	VoiceEngineV2NativeCaptureOptions,
	VoiceEngineV2NativeFrameSinkOptions,
	VoiceEngineV2OperationId,
	VoiceEngineV2OutputDeviceOptions,
	VoiceEngineV2ParticipantVolumeOptions,
	VoiceEngineV2PermissionName,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2ResourceKey,
	VoiceEngineV2ScreenAudioOptions,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2TimerOptions,
} from '@fluxer/voice_engine_v2/src/protocol/types';

export type VoiceEngineV2Command =
	| {type: 'implementation.prewarm'; operationId: VoiceEngineV2OperationId}
	| {
			type: 'gateway.voiceState.write';
			operationId: VoiceEngineV2OperationId;
			options: VoiceEngineV2GatewayVoiceStateWrite;
	  }
	| {type: 'gateway.voiceState.clear'; operationId: VoiceEngineV2OperationId; guildId: string | null}
	| {type: 'connection.connect'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2ConnectOptions}
	| {
			type: 'connection.disconnect';
			operationId: VoiceEngineV2OperationId;
			reason: VoiceEngineV2DisconnectReason;
	  }
	| {
			type: 'microphone.publish';
			operationId: VoiceEngineV2OperationId;
			options: VoiceEngineV2MicrophoneOptions;
	  }
	| {type: 'microphone.unpublish'; operationId: VoiceEngineV2OperationId}
	| {type: 'microphone.setEnabled'; operationId: VoiceEngineV2OperationId; enabled: boolean}
	| {type: 'camera.publish'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2CameraOptions}
	| {type: 'camera.updateEncoding'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2CameraEncodingOptions}
	| {type: 'camera.unpublish'; operationId: VoiceEngineV2OperationId; options?: VoiceEngineV2CameraOptions}
	| {type: 'screen.publish'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2ScreenOptions}
	| {type: 'screen.updateEncoding'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2ScreenEncodingOptions}
	| {type: 'screen.unpublish'; operationId: VoiceEngineV2OperationId}
	| {type: 'screenAudio.publish'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2ScreenAudioOptions}
	| {type: 'screenAudio.unpublish'; operationId: VoiceEngineV2OperationId}
	| {type: 'outputDevice.set'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2OutputDeviceOptions}
	| {
			type: 'participantVolume.set';
			operationId: VoiceEngineV2OperationId;
			options: VoiceEngineV2ParticipantVolumeOptions;
	  }
	| {
			type: 'remoteTrackSubscription.set';
			operationId: VoiceEngineV2OperationId;
			options: VoiceEngineV2RemoteTrackSubscriptionOptions;
	  }
	| {type: 'data.publish'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2DataOptions}
	| {type: 'stats.collect'; operationId: VoiceEngineV2OperationId}
	| {type: 'capabilities.queryHardwareEncoder'; operationId: VoiceEngineV2OperationId}
	| {type: 'permissions.check'; operationId: VoiceEngineV2OperationId; name: VoiceEngineV2PermissionName}
	| {type: 'permissions.request'; operationId: VoiceEngineV2OperationId; name: VoiceEngineV2PermissionName}
	| {type: 'devices.enumerate'; operationId: VoiceEngineV2OperationId}
	| {type: 'devices.selectAudioInput'; operationId: VoiceEngineV2OperationId; deviceId: string | null}
	| {type: 'devices.selectAudioOutput'; operationId: VoiceEngineV2OperationId; deviceId: string | null}
	| {type: 'devices.selectCamera'; operationId: VoiceEngineV2OperationId; deviceId: string | null}
	| {type: 'nativeCapture.start'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2NativeCaptureOptions}
	| {type: 'nativeCapture.update'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2NativeCaptureOptions}
	| {type: 'nativeCapture.stop'; operationId: VoiceEngineV2OperationId; captureId: string}
	| {type: 'nativeAudioTap.start'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2NativeAudioTapOptions}
	| {type: 'nativeAudioTap.stop'; operationId: VoiceEngineV2OperationId; tapId: string}
	| {
			type: 'nativeFrameSink.attach';
			operationId: VoiceEngineV2OperationId;
			options: VoiceEngineV2NativeFrameSinkOptions;
	  }
	| {type: 'nativeFrameSink.detach'; operationId: VoiceEngineV2OperationId; sinkId: string}
	| {type: 'e2ee.setEnabled'; operationId: VoiceEngineV2OperationId; enabled: boolean; keyId?: string | null}
	| {type: 'timer.schedule'; operationId: VoiceEngineV2OperationId; options: VoiceEngineV2TimerOptions}
	| {type: 'timer.cancel'; operationId: VoiceEngineV2OperationId; timerId: string}
	| {type: 'diagnostics.log'; operationId: VoiceEngineV2OperationId; entry: VoiceEngineV2DiagnosticEntry}
	| {
			type: 'operation.cancel';
			operationId: VoiceEngineV2OperationId;
			targetOperationId: VoiceEngineV2OperationId;
			resourceKey: VoiceEngineV2ResourceKey;
			reason: string;
	  }
	| {type: 'lifecycle.teardown'; operationId: VoiceEngineV2OperationId; reason: VoiceEngineV2LifecycleReason};

export type VoiceEngineV2CommandType = VoiceEngineV2Command['type'];

export function getVoiceEngineV2CommandResourceKey(command: VoiceEngineV2Command): VoiceEngineV2ResourceKey {
	if (command.type === 'operation.cancel') return command.resourceKey;
	const resourceKey = getVoiceEngineV2CommandTypeResourceKey(command.type);
	if (resourceKey == null) {
		throw new Error(`Voice engine v2 command type does not have a static resource key: ${command.type}`);
	}
	return resourceKey;
}

export function getVoiceEngineV2CommandTypeResourceKey(
	commandType: VoiceEngineV2CommandType,
): VoiceEngineV2ResourceKey | null {
	switch (commandType) {
		case 'implementation.prewarm':
			return 'implementation';
		case 'gateway.voiceState.write':
		case 'gateway.voiceState.clear':
			return 'gateway';
		case 'connection.connect':
		case 'connection.disconnect':
			return 'connection';
		case 'microphone.publish':
		case 'microphone.unpublish':
		case 'microphone.setEnabled':
			return 'microphone';
		case 'camera.publish':
		case 'camera.updateEncoding':
		case 'camera.unpublish':
			return 'camera';
		case 'screen.publish':
		case 'screen.updateEncoding':
		case 'screen.unpublish':
			return 'screen';
		case 'screenAudio.publish':
		case 'screenAudio.unpublish':
			return 'screenAudio';
		case 'outputDevice.set':
			return 'outputDevice';
		case 'participantVolume.set':
			return 'participantVolume';
		case 'remoteTrackSubscription.set':
			return 'remoteTrackSubscription';
		case 'data.publish':
			return 'dataChannel';
		case 'stats.collect':
			return 'stats';
		case 'capabilities.queryHardwareEncoder':
			return 'capabilities';
		case 'permissions.check':
		case 'permissions.request':
			return 'permissions';
		case 'devices.enumerate':
		case 'devices.selectAudioInput':
		case 'devices.selectAudioOutput':
		case 'devices.selectCamera':
			return 'devices';
		case 'nativeCapture.start':
		case 'nativeCapture.update':
		case 'nativeCapture.stop':
			return 'nativeCapture';
		case 'nativeAudioTap.start':
		case 'nativeAudioTap.stop':
			return 'nativeAudioTap';
		case 'nativeFrameSink.attach':
		case 'nativeFrameSink.detach':
			return 'nativeFrameSink';
		case 'e2ee.setEnabled':
			return 'e2ee';
		case 'timer.schedule':
		case 'timer.cancel':
			return 'timer';
		case 'diagnostics.log':
			return 'diagnostics';
		case 'operation.cancel':
			return null;
		case 'lifecycle.teardown':
			return 'lifecycle';
	}
}
