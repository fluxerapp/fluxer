// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2Event} from '../../protocol/events';
import type {VoiceEngineV2Snapshot, VoiceEngineV2Transition} from '../state';
import {planDesiredState} from './_plan';
import {transitionCamera} from './camera';
import {transitionCapabilities} from './capabilities';
import {transitionCodecNegotiation} from './codecNegotiation';
import {transitionCommand} from './command';
import {transitionConnection} from './connection';
import {transitionData} from './data';
import {transitionDevices} from './devices';
import {transitionE2ee} from './e2ee';
import {transitionGateway} from './gateway';
import {transitionImplementation} from './implementation';
import {transitionInboundVideo} from './inboundVideo';
import {transitionLifecycle} from './lifecycle';
import {transitionMicrophone} from './microphone';
import {transitionNativeAudioTap} from './nativeAudioTap';
import {transitionNativeCapture} from './nativeCapture';
import {transitionNativeFrameSink} from './nativeFrameSink';
import {transitionOutputDevice} from './outputDevice';
import {transitionParticipantVolume} from './participantVolume';
import {transitionPermissions} from './permissions';
import {transitionRemoteTrackSubscription} from './remoteTrackSubscription';
import {transitionRoom} from './room';
import {transitionScreen} from './screen';
import {transitionScreenAudio} from './screenAudio';
import {transitionSourceLifecycles} from './sourceLifecycles';
import {transitionStats} from './stats';
import {transitionUtilityPorts} from './utilityPorts';

export function dispatchLocalMediaEvent(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2Event,
): VoiceEngineV2Transition | null {
	assert.ok(snapshot != null, 'dispatchLocalMediaEvent snapshot must not be null');
	assert.ok(event != null, 'dispatchLocalMediaEvent event must not be null');
	assert.equal(typeof event.type, 'string', 'dispatchLocalMediaEvent event.type must be a string');
	switch (event.type) {
		case 'implementation.prewarmRequested':
		case 'implementation.prewarmSucceeded':
		case 'implementation.prewarmFailed':
			return transitionImplementation(snapshot, event);
		case 'connection.connectRequested':
		case 'connection.connectSucceeded':
		case 'connection.connectFailed':
		case 'connection.disconnectRequested':
		case 'connection.disconnectSucceeded':
		case 'connection.disconnectFailed':
		case 'connection.remoteDisconnected':
		case 'connection.reconnectRequested':
		case 'connection.externallyEstablished':
			return transitionConnection(snapshot, event, planDesiredState);
		case 'microphone.publishRequested':
		case 'microphone.publishSucceeded':
		case 'microphone.publishFailed':
		case 'microphone.unpublishRequested':
		case 'microphone.unpublishSucceeded':
		case 'microphone.unpublishFailed':
		case 'microphone.setEnabledRequested':
		case 'microphone.setEnabledSucceeded':
		case 'microphone.setEnabledFailed':
		case 'localAudio.muteRequested':
		case 'localAudio.deafenRequested':
		case 'audioControls.changed':
			return transitionMicrophone(snapshot, event);
		case 'camera.publishRequested':
		case 'camera.publishSucceeded':
		case 'camera.publishFailed':
		case 'camera.updateEncodingRequested':
		case 'camera.updateEncodingSucceeded':
		case 'camera.updateEncodingFailed':
		case 'camera.unpublishRequested':
		case 'camera.unpublishSucceeded':
		case 'camera.unpublishFailed':
			return transitionCamera(snapshot, event);
		default:
			return dispatchScreenEvent(snapshot, event);
	}
}

function dispatchScreenEvent(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2Event,
): VoiceEngineV2Transition | null {
	assert.ok(snapshot != null, 'dispatchScreenEvent snapshot must not be null');
	assert.ok(event != null, 'dispatchScreenEvent event must not be null');
	switch (event.type) {
		case 'screen.publishRequested':
		case 'screen.publishSucceeded':
		case 'screen.publishFailed':
		case 'screen.updateEncodingRequested':
		case 'screen.updateEncodingSucceeded':
		case 'screen.updateEncodingFailed':
		case 'screen.unpublishRequested':
		case 'screen.unpublishSucceeded':
		case 'screen.unpublishFailed':
			return transitionScreen(snapshot, event);
		case 'screenAudio.publishRequested':
		case 'screenAudio.publishSucceeded':
		case 'screenAudio.publishFailed':
		case 'screenAudio.unpublishRequested':
		case 'screenAudio.unpublishSucceeded':
		case 'screenAudio.unpublishFailed':
			return transitionScreenAudio(snapshot, event);
		default:
			return null;
	}
}

export function dispatchSessionEvent(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2Event,
): VoiceEngineV2Transition | null {
	assert.ok(snapshot != null, 'dispatchSessionEvent snapshot must not be null');
	assert.ok(event != null, 'dispatchSessionEvent event must not be null');
	assert.equal(typeof event.type, 'string', 'dispatchSessionEvent event.type must be a string');
	switch (event.type) {
		case 'outputDevice.setRequested':
		case 'outputDevice.setSucceeded':
		case 'outputDevice.setFailed':
			return transitionOutputDevice(snapshot, event);
		case 'participantVolume.setRequested':
		case 'participantVolume.setSucceeded':
		case 'participantVolume.setFailed':
			return transitionParticipantVolume(snapshot, event);
		case 'remoteTrackSubscription.setRequested':
		case 'remoteTrackSubscription.setSucceeded':
		case 'remoteTrackSubscription.setFailed':
		case 'watchedStream.watchRequested':
		case 'watchedStream.unwatchRequested':
		case 'watchedStreams.replaced':
			return transitionRemoteTrackSubscription(snapshot, event);
		case 'data.publishRequested':
		case 'data.publishSucceeded':
		case 'data.publishFailed':
			return transitionData(snapshot, event);
		case 'codecNegotiation.overrideSetRequested':
		case 'codecNegotiation.localCapabilityChanged':
		case 'codecNegotiation.remoteCapabilityChanged':
		case 'codecNegotiation.streamRegistered':
		case 'codecNegotiation.streamUnregistered':
		case 'codecNegotiation.viewerChanged':
			return transitionCodecNegotiation(snapshot, event);
		case 'stats.collectRequested':
		case 'stats.collected':
		case 'stats.collectFailed':
			return transitionStats(snapshot, event);
		case 'capabilities.changed':
		case 'capabilities.hardwareEncoderQueryRequested':
		case 'capabilities.hardwareEncoderChanged':
		case 'capabilities.hardwareEncoderQueryFailed':
			return transitionCapabilities(snapshot, event);
		default:
			return null;
	}
}

export function dispatchPlatformEvent(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2Event,
): VoiceEngineV2Transition | null {
	assert.ok(snapshot != null, 'dispatchPlatformEvent snapshot must not be null');
	assert.ok(event != null, 'dispatchPlatformEvent event must not be null');
	assert.equal(typeof event.type, 'string', 'dispatchPlatformEvent event.type must be a string');
	switch (event.type) {
		case 'permissions.checkRequested':
		case 'permissions.requestRequested':
		case 'permissions.result':
		case 'permissions.failed':
			return transitionPermissions(snapshot, event);
		case 'devices.enumerateRequested':
		case 'devices.changed':
		case 'devices.enumerateFailed':
		case 'devices.selectAudioInputRequested':
		case 'devices.selectAudioOutputRequested':
		case 'devices.selectCameraRequested':
			return transitionDevices(snapshot, event);
		case 'nativeCapture.startRequested':
		case 'nativeCapture.updateRequested':
		case 'nativeCapture.stopRequested':
		case 'nativeCapture.started':
		case 'nativeCapture.stopped':
		case 'nativeCapture.failed':
		case 'nativeCapture.frame':
			return transitionNativeCapture(snapshot, event);
		case 'nativeAudioTap.startRequested':
		case 'nativeAudioTap.stopRequested':
			return transitionNativeAudioTap(snapshot, event);
		case 'nativeFrameSink.attachRequested':
		case 'nativeFrameSink.detachRequested':
			return transitionNativeFrameSink(snapshot, event);
		case 'e2ee.setEnabledRequested':
		case 'e2ee.enabled':
		case 'e2ee.disabled':
		case 'e2ee.failed':
			return transitionE2ee(snapshot, event);
		default:
			return null;
	}
}

export function dispatchRuntimeEvent(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2Event,
): VoiceEngineV2Transition | null {
	assert.ok(snapshot != null, 'dispatchRuntimeEvent snapshot must not be null');
	assert.ok(event != null, 'dispatchRuntimeEvent event must not be null');
	assert.equal(typeof event.type, 'string', 'dispatchRuntimeEvent event.type must be a string');
	switch (event.type) {
		case 'timer.scheduleRequested':
		case 'timer.cancelRequested':
		case 'timer.fired':
		case 'diagnostics.logRequested':
		case 'diagnostics.logged':
			return transitionUtilityPorts(snapshot, event);
		case 'lifecycle.teardownRequested':
		case 'lifecycle.teardownSucceeded':
		case 'lifecycle.teardownFailed':
			return transitionLifecycle(snapshot, event);
		case 'room.participantJoined':
		case 'room.participantLeft':
		case 'room.trackPublished':
		case 'room.trackUnpublished':
		case 'room.trackMuted':
		case 'room.trackUnmuted':
			return transitionRoom(snapshot, event);
		case 'inboundVideo.trackSubscribed':
		case 'inboundVideo.trackUnsubscribed':
		case 'inboundVideo.frameReceived':
		case 'inboundVideo.frameStats':
			return transitionInboundVideo(snapshot, event);
		default:
			return dispatchControlEvent(snapshot, event);
	}
}

function dispatchControlEvent(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2Event,
): VoiceEngineV2Transition | null {
	assert.ok(snapshot != null, 'dispatchControlEvent snapshot must not be null');
	assert.ok(event != null, 'dispatchControlEvent event must not be null');
	switch (event.type) {
		case 'command.succeeded':
		case 'command.failed':
		case 'command.staleCompletionRejected':
		case 'operation.cancelRequested':
		case 'operation.cancelled':
			return transitionCommand(snapshot, event);
		case 'gateway.desiredVoiceStateChanged':
		case 'gateway.voiceStateReconcileRequested':
		case 'gateway.voiceStateWriteRequested':
		case 'gateway.voiceStateWriteSucceeded':
		case 'gateway.voiceStateWriteFailed':
		case 'gateway.voiceStateClearRequested':
		case 'gateway.voiceStateClearSucceeded':
		case 'gateway.voiceStateClearFailed':
		case 'gateway.voiceStateUpdated':
		case 'gateway.voiceServerUpdated':
		case 'livekit.roomStateChanged':
			return transitionGateway(snapshot, event);
		default:
			return null;
	}
}

export function dispatchObservabilityEvent(
	snapshot: VoiceEngineV2Snapshot,
	event: VoiceEngineV2Event,
): VoiceEngineV2Transition | null {
	assert.ok(snapshot != null, 'dispatchObservabilityEvent snapshot must not be null');
	assert.ok(event != null, 'dispatchObservabilityEvent event must not be null');
	assert.equal(typeof event.type, 'string', 'dispatchObservabilityEvent event.type must be a string');
	switch (event.type) {
		case 'sourceLifecycle.transitioned':
			return transitionSourceLifecycles(snapshot, event);
		default:
			return null;
	}
}
