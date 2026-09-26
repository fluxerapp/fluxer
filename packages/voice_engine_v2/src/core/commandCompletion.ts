// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2Snapshot} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';

export function isVoiceEngineV2CommandCompletionStale(
	snapshot: VoiceEngineV2Snapshot,
	command: VoiceEngineV2Command,
): boolean {
	assert.ok(snapshot != null, 'isVoiceEngineV2CommandCompletionStale snapshot must not be null');
	assert.ok(command != null, 'isVoiceEngineV2CommandCompletionStale command must not be null');
	assert.equal(typeof command.type, 'string', 'command.type must be a string');
	const local = staleLocalMediaCommand(snapshot, command);
	if (local !== null) return local;
	const session = staleSessionCommand(snapshot, command);
	if (session !== null) return session;
	const platform = stalePlatformCommand(snapshot, command);
	if (platform !== null) return platform;
	return staleAlwaysFreshCommand(command);
}

function staleLocalMediaCommand(snapshot: VoiceEngineV2Snapshot, command: VoiceEngineV2Command): boolean | null {
	assert.ok(snapshot != null, 'staleLocalMediaCommand snapshot must not be null');
	assert.ok(command != null, 'staleLocalMediaCommand command must not be null');
	switch (command.type) {
		case 'connection.connect':
		case 'connection.disconnect':
			return snapshot.connection.operationId !== command.operationId;
		case 'microphone.publish':
		case 'microphone.unpublish':
			return snapshot.microphone.operationId !== command.operationId;
		case 'microphone.setEnabled':
			return snapshot.microphone.setEnabledOperationId !== command.operationId;
		case 'camera.publish':
		case 'camera.updateEncoding':
		case 'camera.unpublish':
			return snapshot.camera.operationId !== command.operationId;
		case 'screen.publish':
		case 'screen.updateEncoding':
		case 'screen.unpublish':
			return snapshot.screen.operationId !== command.operationId;
		case 'screenAudio.publish':
		case 'screenAudio.unpublish':
			return snapshot.screenAudio.operationId !== command.operationId;
		default:
			return null;
	}
}

function staleSessionCommand(snapshot: VoiceEngineV2Snapshot, command: VoiceEngineV2Command): boolean | null {
	assert.ok(snapshot != null, 'staleSessionCommand snapshot must not be null');
	assert.ok(command != null, 'staleSessionCommand command must not be null');
	switch (command.type) {
		case 'outputDevice.set':
			return snapshot.outputDevice.operationId !== command.operationId;
		case 'stats.collect':
			return snapshot.statsOperationId !== command.operationId;
		case 'capabilities.queryHardwareEncoder':
			return snapshot.hardwareEncoder.operationId !== command.operationId;
		case 'gateway.voiceState.write':
		case 'gateway.voiceState.clear':
			return snapshot.gateway.operationId !== command.operationId;
		case 'lifecycle.teardown':
			return snapshot.lifecycle.operationId !== command.operationId;
		case 'e2ee.setEnabled':
			return snapshot.e2ee.operationId !== command.operationId;
		default:
			return null;
	}
}

function stalePlatformCommand(snapshot: VoiceEngineV2Snapshot, command: VoiceEngineV2Command): boolean | null {
	assert.ok(snapshot != null, 'stalePlatformCommand snapshot must not be null');
	assert.ok(command != null, 'stalePlatformCommand command must not be null');
	switch (command.type) {
		case 'permissions.check':
		case 'permissions.request':
			return snapshot.permissions.operationIds[String(command.name)] !== command.operationId;
		case 'devices.enumerate':
		case 'devices.selectAudioInput':
		case 'devices.selectAudioOutput':
		case 'devices.selectCamera':
			return snapshot.devices.operationId !== command.operationId;
		case 'nativeCapture.start':
		case 'nativeCapture.update':
			return snapshot.nativeCapture.operationIds[command.options.captureId] !== command.operationId;
		case 'nativeCapture.stop':
			return snapshot.nativeCapture.operationIds[command.captureId] !== command.operationId;
		case 'nativeAudioTap.start':
			return snapshot.nativeAudioTap.operationIds[command.options.tapId] !== command.operationId;
		case 'nativeAudioTap.stop':
			return snapshot.nativeAudioTap.operationIds[command.tapId] !== command.operationId;
		case 'nativeFrameSink.attach':
			return snapshot.nativeFrameSink.operationIds[command.options.sinkId] !== command.operationId;
		case 'nativeFrameSink.detach':
			return snapshot.nativeFrameSink.operationIds[command.sinkId] !== command.operationId;
		default:
			return null;
	}
}

function staleAlwaysFreshCommand(command: VoiceEngineV2Command): boolean {
	assert.ok(command != null, 'staleAlwaysFreshCommand command must not be null');
	assert.equal(typeof command.type, 'string', 'command.type must be a string');
	switch (command.type) {
		case 'implementation.prewarm':
		case 'participantVolume.set':
		case 'remoteTrackSubscription.set':
		case 'data.publish':
		case 'timer.schedule':
		case 'timer.cancel':
		case 'diagnostics.log':
		case 'operation.cancel':
			return false;
		default:
			return false;
	}
}
