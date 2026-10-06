// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2CommandResult} from '@fluxer/voice_engine_v2/src/implementations';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {VoiceEngineV2Error} from '@fluxer/voice_engine_v2/src/protocol/types';

type VoiceEngineV2CommandOfType<Type extends VoiceEngineV2Command['type']> = Extract<
	VoiceEngineV2Command,
	{type: Type}
>;

type VoiceEngineV2SessionCommand = VoiceEngineV2CommandOfType<
	| 'implementation.prewarm'
	| 'gateway.voiceState.write'
	| 'gateway.voiceState.clear'
	| 'connection.connect'
	| 'connection.disconnect'
	| 'lifecycle.teardown'
>;

type VoiceEngineV2LocalMediaCommand = VoiceEngineV2CommandOfType<
	| 'microphone.publish'
	| 'microphone.unpublish'
	| 'microphone.setEnabled'
	| 'camera.publish'
	| 'camera.updateEncoding'
	| 'camera.unpublish'
	| 'screen.publish'
	| 'screen.updateEncoding'
	| 'screen.unpublish'
	| 'screenAudio.publish'
	| 'screenAudio.unpublish'
>;

type VoiceEngineV2RoutingCommand = VoiceEngineV2CommandOfType<
	'outputDevice.set' | 'participantVolume.set' | 'remoteTrackSubscription.set' | 'data.publish'
>;

type VoiceEngineV2QueryCommand = VoiceEngineV2CommandOfType<
	'stats.collect' | 'capabilities.queryHardwareEncoder' | 'permissions.check' | 'permissions.request'
>;

type VoiceEngineV2DevicesCommand = VoiceEngineV2CommandOfType<
	'devices.enumerate' | 'devices.selectAudioInput' | 'devices.selectAudioOutput' | 'devices.selectCamera'
>;

type VoiceEngineV2NativeCommand = VoiceEngineV2CommandOfType<
	| 'nativeCapture.start'
	| 'nativeCapture.update'
	| 'nativeCapture.stop'
	| 'nativeAudioTap.start'
	| 'nativeAudioTap.stop'
	| 'nativeFrameSink.attach'
	| 'nativeFrameSink.detach'
>;

type VoiceEngineV2UtilityCommand = VoiceEngineV2CommandOfType<
	'e2ee.setEnabled' | 'timer.schedule' | 'timer.cancel' | 'diagnostics.log' | 'operation.cancel'
>;

type VoiceEngineV2CommandSuccess = Extract<VoiceEngineV2CommandResult, {ok: true}>;

export function commandResultToEvent(
	command: VoiceEngineV2Command,
	result: VoiceEngineV2CommandResult,
): VoiceEngineV2Event {
	if (!result.ok) return commandFailureToEvent(command, result.error);
	switch (command.type) {
		case 'implementation.prewarm':
		case 'gateway.voiceState.write':
		case 'gateway.voiceState.clear':
		case 'connection.connect':
		case 'connection.disconnect':
		case 'lifecycle.teardown':
			return sessionSuccessToEvent(command);
		case 'microphone.publish':
		case 'microphone.unpublish':
		case 'microphone.setEnabled':
		case 'camera.publish':
		case 'camera.updateEncoding':
		case 'camera.unpublish':
		case 'screen.publish':
		case 'screen.updateEncoding':
		case 'screen.unpublish':
		case 'screenAudio.publish':
		case 'screenAudio.unpublish':
			return localMediaSuccessToEvent(command);
		case 'outputDevice.set':
		case 'participantVolume.set':
		case 'remoteTrackSubscription.set':
		case 'data.publish':
			return routingSuccessToEvent(command);
		case 'stats.collect':
		case 'capabilities.queryHardwareEncoder':
		case 'permissions.check':
		case 'permissions.request':
			return querySuccessToEvent(command, result);
		case 'devices.enumerate':
		case 'devices.selectAudioInput':
		case 'devices.selectAudioOutput':
		case 'devices.selectCamera':
			return devicesSuccessToEvent(command, result);
		case 'nativeCapture.start':
		case 'nativeCapture.update':
		case 'nativeCapture.stop':
		case 'nativeAudioTap.start':
		case 'nativeAudioTap.stop':
		case 'nativeFrameSink.attach':
		case 'nativeFrameSink.detach':
			return nativeSuccessToEvent(command);
		case 'e2ee.setEnabled':
		case 'timer.schedule':
		case 'timer.cancel':
		case 'diagnostics.log':
		case 'operation.cancel':
			return utilitySuccessToEvent(command);
	}
}

function sessionSuccessToEvent(command: VoiceEngineV2SessionCommand): VoiceEngineV2Event {
	switch (command.type) {
		case 'implementation.prewarm':
			return {type: 'implementation.prewarmSucceeded', operationId: command.operationId};
		case 'gateway.voiceState.write':
			return {type: 'gateway.voiceStateWriteSucceeded', operationId: command.operationId};
		case 'gateway.voiceState.clear':
			return {type: 'gateway.voiceStateClearSucceeded', operationId: command.operationId};
		case 'connection.connect':
			return {type: 'connection.connectSucceeded', operationId: command.operationId};
		case 'connection.disconnect':
			return {type: 'connection.disconnectSucceeded', operationId: command.operationId};
		case 'lifecycle.teardown':
			return {type: 'lifecycle.teardownSucceeded', operationId: command.operationId};
	}
}

function localMediaSuccessToEvent(command: VoiceEngineV2LocalMediaCommand): VoiceEngineV2Event {
	switch (command.type) {
		case 'microphone.publish':
			return {type: 'microphone.publishSucceeded', operationId: command.operationId};
		case 'microphone.unpublish':
			return {type: 'microphone.unpublishSucceeded', operationId: command.operationId};
		case 'microphone.setEnabled':
			return {type: 'microphone.setEnabledSucceeded', operationId: command.operationId};
		case 'camera.publish':
			return {type: 'camera.publishSucceeded', operationId: command.operationId};
		case 'camera.updateEncoding':
			return {type: 'camera.updateEncodingSucceeded', operationId: command.operationId};
		case 'camera.unpublish':
			return {type: 'camera.unpublishSucceeded', operationId: command.operationId};
		case 'screen.publish':
			return {type: 'screen.publishSucceeded', operationId: command.operationId};
		case 'screen.updateEncoding':
			return {type: 'screen.updateEncodingSucceeded', operationId: command.operationId};
		case 'screen.unpublish':
			return {type: 'screen.unpublishSucceeded', operationId: command.operationId};
		case 'screenAudio.publish':
			return {type: 'screenAudio.publishSucceeded', operationId: command.operationId};
		case 'screenAudio.unpublish':
			return {type: 'screenAudio.unpublishSucceeded', operationId: command.operationId};
	}
}

function routingSuccessToEvent(command: VoiceEngineV2RoutingCommand): VoiceEngineV2Event {
	switch (command.type) {
		case 'outputDevice.set':
			return {type: 'outputDevice.setSucceeded', operationId: command.operationId};
		case 'participantVolume.set':
			return {type: 'participantVolume.setSucceeded', operationId: command.operationId};
		case 'remoteTrackSubscription.set':
			return {type: 'remoteTrackSubscription.setSucceeded', operationId: command.operationId};
		case 'data.publish':
			return {type: 'data.publishSucceeded', operationId: command.operationId};
	}
}

function querySuccessToEvent(
	command: VoiceEngineV2QueryCommand,
	result: VoiceEngineV2CommandSuccess,
): VoiceEngineV2Event {
	switch (command.type) {
		case 'stats.collect':
			if (result.stats) return {type: 'stats.collected', operationId: command.operationId, stats: result.stats};
			return {
				type: 'stats.collectFailed',
				operationId: command.operationId,
				error: {
					code: 'implementationError',
					message: 'Voice engine v2 stats command succeeded without stats',
					capability: 'stats',
				},
			};
		case 'capabilities.queryHardwareEncoder':
			if (result.hardwareEncoderCapabilities) {
				return {
					type: 'capabilities.hardwareEncoderChanged',
					operationId: command.operationId,
					capabilities: result.hardwareEncoderCapabilities,
				};
			}
			return {
				type: 'capabilities.hardwareEncoderQueryFailed',
				operationId: command.operationId,
				error: {
					code: 'implementationError',
					message: 'Voice engine v2 hardware encoder capability command succeeded without capabilities',
					capability: 'hardwareEncoding',
				},
			};
		case 'permissions.check':
		case 'permissions.request':
			if (result.permissionResult) {
				return {type: 'permissions.result', operationId: command.operationId, result: result.permissionResult};
			}
			return {
				type: 'permissions.failed',
				operationId: command.operationId,
				name: command.name,
				error: {
					code: 'implementationError',
					message: 'Voice engine v2 permission command succeeded without a permission result',
					capability: 'permissions',
				},
			};
	}
}

function devicesSuccessToEvent(
	command: VoiceEngineV2DevicesCommand,
	result: VoiceEngineV2CommandSuccess,
): VoiceEngineV2Event {
	switch (command.type) {
		case 'devices.enumerate':
			if (result.deviceInventory) {
				return {
					type: 'devices.changed',
					operationId: command.operationId,
					reason: 'initial',
					devices: result.deviceInventory,
				};
			}
			return {
				type: 'devices.enumerateFailed',
				operationId: command.operationId,
				error: {
					code: 'implementationError',
					message: 'Voice engine v2 device enumeration succeeded without a device inventory',
					capability: 'devices',
				},
			};
		case 'devices.selectAudioInput':
		case 'devices.selectAudioOutput':
		case 'devices.selectCamera':
			return {type: 'command.succeeded', operationId: command.operationId, commandType: command.type};
	}
}

function nativeSuccessToEvent(command: VoiceEngineV2NativeCommand): VoiceEngineV2Event {
	switch (command.type) {
		case 'nativeCapture.start':
			return {type: 'nativeCapture.started', operationId: command.operationId, captureId: command.options.captureId};
		case 'nativeCapture.update':
			return {type: 'command.succeeded', operationId: command.operationId, commandType: command.type};
		case 'nativeCapture.stop':
			return {type: 'nativeCapture.stopped', operationId: command.operationId, captureId: command.captureId};
		case 'nativeAudioTap.start':
		case 'nativeAudioTap.stop':
		case 'nativeFrameSink.attach':
		case 'nativeFrameSink.detach':
			return {type: 'command.succeeded', operationId: command.operationId, commandType: command.type};
	}
}

function utilitySuccessToEvent(command: VoiceEngineV2UtilityCommand): VoiceEngineV2Event {
	switch (command.type) {
		case 'e2ee.setEnabled':
			return command.enabled
				? {type: 'e2ee.enabled', operationId: command.operationId, keyId: command.keyId ?? null}
				: {type: 'e2ee.disabled', operationId: command.operationId};
		case 'timer.schedule':
		case 'timer.cancel':
		case 'diagnostics.log':
			return {type: 'command.succeeded', operationId: command.operationId, commandType: command.type};
		case 'operation.cancel':
			return {
				type: 'operation.cancelled',
				operationId: command.operationId,
				targetOperationId: command.targetOperationId,
				resourceKey: command.resourceKey,
			};
	}
}

function commandFailureToEvent(command: VoiceEngineV2Command, error: VoiceEngineV2Error): VoiceEngineV2Event {
	switch (command.type) {
		case 'implementation.prewarm':
		case 'gateway.voiceState.write':
		case 'gateway.voiceState.clear':
		case 'connection.connect':
		case 'connection.disconnect':
		case 'lifecycle.teardown':
			return sessionFailureToEvent(command, error);
		case 'microphone.publish':
		case 'microphone.unpublish':
		case 'microphone.setEnabled':
		case 'camera.publish':
		case 'camera.updateEncoding':
		case 'camera.unpublish':
		case 'screen.publish':
		case 'screen.updateEncoding':
		case 'screen.unpublish':
		case 'screenAudio.publish':
		case 'screenAudio.unpublish':
			return localMediaFailureToEvent(command, error);
		case 'outputDevice.set':
			return {type: 'outputDevice.setFailed', operationId: command.operationId, error};
		case 'participantVolume.set':
			return {type: 'participantVolume.setFailed', operationId: command.operationId, error};
		case 'remoteTrackSubscription.set':
			return {type: 'remoteTrackSubscription.setFailed', operationId: command.operationId, error};
		case 'data.publish':
			return {type: 'data.publishFailed', operationId: command.operationId, error};
		case 'stats.collect':
			return {type: 'stats.collectFailed', operationId: command.operationId, error};
		case 'capabilities.queryHardwareEncoder':
			return {type: 'capabilities.hardwareEncoderQueryFailed', operationId: command.operationId, error};
		case 'permissions.check':
		case 'permissions.request':
			return {type: 'permissions.failed', operationId: command.operationId, name: command.name, error};
		case 'devices.enumerate':
			return {type: 'devices.enumerateFailed', operationId: command.operationId, error};
		case 'nativeCapture.start':
		case 'nativeCapture.update':
			return {
				type: 'nativeCapture.failed',
				operationId: command.operationId,
				captureId: command.options.captureId,
				error,
			};
		case 'nativeCapture.stop':
			return {type: 'nativeCapture.failed', operationId: command.operationId, captureId: command.captureId, error};
		case 'e2ee.setEnabled':
			return {type: 'e2ee.failed', operationId: command.operationId, error};
		case 'devices.selectAudioInput':
		case 'devices.selectAudioOutput':
		case 'devices.selectCamera':
		case 'nativeAudioTap.start':
		case 'nativeAudioTap.stop':
		case 'nativeFrameSink.attach':
		case 'nativeFrameSink.detach':
		case 'timer.schedule':
		case 'timer.cancel':
		case 'diagnostics.log':
		case 'operation.cancel':
			return {type: 'command.failed', operationId: command.operationId, commandType: command.type, error};
	}
}

function sessionFailureToEvent(command: VoiceEngineV2SessionCommand, error: VoiceEngineV2Error): VoiceEngineV2Event {
	switch (command.type) {
		case 'implementation.prewarm':
			return {type: 'implementation.prewarmFailed', operationId: command.operationId, error};
		case 'gateway.voiceState.write':
			return {type: 'gateway.voiceStateWriteFailed', operationId: command.operationId, error};
		case 'gateway.voiceState.clear':
			return {type: 'gateway.voiceStateClearFailed', operationId: command.operationId, error};
		case 'connection.connect':
			return {type: 'connection.connectFailed', operationId: command.operationId, error};
		case 'connection.disconnect':
			return {type: 'connection.disconnectFailed', operationId: command.operationId, error};
		case 'lifecycle.teardown':
			return {type: 'lifecycle.teardownFailed', operationId: command.operationId, error};
	}
}

function localMediaFailureToEvent(
	command: VoiceEngineV2LocalMediaCommand,
	error: VoiceEngineV2Error,
): VoiceEngineV2Event {
	switch (command.type) {
		case 'microphone.publish':
			return {type: 'microphone.publishFailed', operationId: command.operationId, error};
		case 'microphone.unpublish':
			return {type: 'microphone.unpublishFailed', operationId: command.operationId, error};
		case 'microphone.setEnabled':
			return {type: 'microphone.setEnabledFailed', operationId: command.operationId, error};
		case 'camera.publish':
			return {type: 'camera.publishFailed', operationId: command.operationId, error};
		case 'camera.updateEncoding':
			return {type: 'camera.updateEncodingFailed', operationId: command.operationId, error};
		case 'camera.unpublish':
			return {type: 'camera.unpublishFailed', operationId: command.operationId, error};
		case 'screen.publish':
			return {type: 'screen.publishFailed', operationId: command.operationId, error};
		case 'screen.updateEncoding':
			return {type: 'screen.updateEncodingFailed', operationId: command.operationId, error};
		case 'screen.unpublish':
			return {type: 'screen.unpublishFailed', operationId: command.operationId, error};
		case 'screenAudio.publish':
			return {type: 'screenAudio.publishFailed', operationId: command.operationId, error};
		case 'screenAudio.unpublish':
			return {type: 'screenAudio.unpublishFailed', operationId: command.operationId, error};
	}
}
