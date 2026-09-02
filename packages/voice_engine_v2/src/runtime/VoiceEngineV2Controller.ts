// SPDX-License-Identifier: AGPL-3.0-or-later

import {selectVoiceEngineV2Model, type VoiceEngineV2Snapshot} from '../core';
import type {
	VoiceEngineV2AudioControlsPatch,
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2DataOptions,
	VoiceEngineV2DiagnosticEntry,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2GatewayDesiredVoiceState,
	VoiceEngineV2GatewayVoiceStateWrite,
	VoiceEngineV2LifecycleReason,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2Model,
	VoiceEngineV2NativeAudioTapOptions,
	VoiceEngineV2NativeCaptureOptions,
	VoiceEngineV2NativeFrameSinkOptions,
	VoiceEngineV2OutputDeviceOptions,
	VoiceEngineV2ParticipantVolumeOptions,
	VoiceEngineV2PermissionName,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2ResourceKey,
	VoiceEngineV2ScreenAudioOptions,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2TimerOptions,
	VoiceEngineV2WatchedStream,
	VoiceEngineV2WatchedStreamKey,
} from '../protocol';
import type {VoiceEngineV2Event} from '../protocol/events';
import type {VoiceEngineV2Runtime, VoiceEngineV2RuntimeListener} from './VoiceEngineV2Runtime';

export class VoiceEngineV2Controller {
	constructor(private readonly runtime: VoiceEngineV2Runtime) {}

	get snapshot(): VoiceEngineV2Snapshot {
		return this.runtime.snapshot;
	}

	get model(): VoiceEngineV2Model {
		return selectVoiceEngineV2Model(this.runtime.snapshot);
	}

	subscribe(listener: VoiceEngineV2RuntimeListener): () => void {
		return this.runtime.subscribe(listener);
	}

	dispatch(event: VoiceEngineV2Event): void {
		this.runtime.dispatch(event);
	}

	prewarm(): void {
		this.runtime.dispatch({type: 'implementation.prewarmRequested'});
	}

	connect(options: VoiceEngineV2ConnectOptions): void {
		this.runtime.dispatch({type: 'connection.connectRequested', options});
	}

	writeGatewayVoiceState(options: VoiceEngineV2GatewayVoiceStateWrite): void {
		this.runtime.dispatch({type: 'gateway.voiceStateWriteRequested', options});
	}

	setDesiredGatewayVoiceState(desired: VoiceEngineV2GatewayDesiredVoiceState): void {
		this.runtime.dispatch({type: 'gateway.desiredVoiceStateChanged', desired});
	}

	reconcileGatewayVoiceState(): void {
		this.runtime.dispatch({type: 'gateway.voiceStateReconcileRequested'});
	}

	clearGatewayVoiceState(guildId: string | null): void {
		this.runtime.dispatch({type: 'gateway.voiceStateClearRequested', guildId});
	}

	reconnect(): void {
		this.runtime.dispatch({type: 'connection.reconnectRequested'});
	}

	disconnect(reason: VoiceEngineV2DisconnectReason = 'user'): void {
		this.runtime.dispatch({type: 'connection.disconnectRequested', reason});
	}

	publishMicrophone(options: VoiceEngineV2MicrophoneOptions): void {
		this.runtime.dispatch({type: 'microphone.publishRequested', options});
	}

	unpublishMicrophone(): void {
		this.runtime.dispatch({type: 'microphone.unpublishRequested'});
	}

	setMicrophoneEnabled(enabled: boolean): void {
		this.runtime.dispatch({type: 'microphone.setEnabledRequested', enabled});
	}

	setLocalMute(muted: boolean): void {
		this.runtime.dispatch({type: 'localAudio.muteRequested', muted});
	}

	setLocalDeafen(deafened: boolean): void {
		this.runtime.dispatch({type: 'localAudio.deafenRequested', deafened});
	}

	publishCamera(options: VoiceEngineV2CameraOptions): void {
		this.runtime.dispatch({type: 'camera.publishRequested', options});
	}

	updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): void {
		this.runtime.dispatch({type: 'camera.updateEncodingRequested', options});
	}

	unpublishCamera(options?: VoiceEngineV2CameraOptions): void {
		this.runtime.dispatch({type: 'camera.unpublishRequested', options});
	}

	publishScreen(options: VoiceEngineV2ScreenOptions): void {
		this.runtime.dispatch({type: 'screen.publishRequested', options});
	}

	updateScreenEncoding(options: VoiceEngineV2ScreenEncodingOptions): void {
		this.runtime.dispatch({type: 'screen.updateEncodingRequested', options});
	}

	unpublishScreen(): void {
		this.runtime.dispatch({type: 'screen.unpublishRequested'});
	}

	publishScreenAudio(options: VoiceEngineV2ScreenAudioOptions): void {
		this.runtime.dispatch({type: 'screenAudio.publishRequested', options});
	}

	unpublishScreenAudio(): void {
		this.runtime.dispatch({type: 'screenAudio.unpublishRequested'});
	}

	setOutputDevice(options: VoiceEngineV2OutputDeviceOptions): void {
		this.runtime.dispatch({type: 'outputDevice.setRequested', options});
	}

	setParticipantVolume(options: VoiceEngineV2ParticipantVolumeOptions): void {
		this.runtime.dispatch({type: 'participantVolume.setRequested', options});
	}

	setRemoteTrackSubscription(options: VoiceEngineV2RemoteTrackSubscriptionOptions): void {
		this.runtime.dispatch({type: 'remoteTrackSubscription.setRequested', options});
	}

	watchStream(stream: VoiceEngineV2WatchedStream): void {
		this.runtime.dispatch({type: 'watchedStream.watchRequested', stream});
	}

	unwatchStream(stream: VoiceEngineV2WatchedStreamKey): void {
		this.runtime.dispatch({type: 'watchedStream.unwatchRequested', stream});
	}

	replaceWatchedStreams(streams: Array<VoiceEngineV2WatchedStream>): void {
		this.runtime.dispatch({type: 'watchedStreams.replaced', streams});
	}

	publishData(options: VoiceEngineV2DataOptions): void {
		this.runtime.dispatch({type: 'data.publishRequested', options});
	}

	collectStats(): void {
		this.runtime.dispatch({type: 'stats.collectRequested'});
	}

	queryHardwareEncoderCapabilities(): void {
		this.runtime.dispatch({type: 'capabilities.hardwareEncoderQueryRequested'});
	}

	checkPermission(name: VoiceEngineV2PermissionName): void {
		this.runtime.dispatch({type: 'permissions.checkRequested', name});
	}

	requestPermission(name: VoiceEngineV2PermissionName): void {
		this.runtime.dispatch({type: 'permissions.requestRequested', name});
	}

	enumerateDevices(): void {
		this.runtime.dispatch({type: 'devices.enumerateRequested'});
	}

	selectAudioInput(deviceId: string | null): void {
		this.runtime.dispatch({type: 'devices.selectAudioInputRequested', deviceId});
	}

	selectAudioOutput(deviceId: string | null): void {
		this.runtime.dispatch({type: 'devices.selectAudioOutputRequested', deviceId});
	}

	selectCamera(deviceId: string | null): void {
		this.runtime.dispatch({type: 'devices.selectCameraRequested', deviceId});
	}

	setAudioControls(controls: VoiceEngineV2AudioControlsPatch): void {
		this.runtime.dispatch({type: 'audioControls.changed', controls});
	}

	startNativeCapture(options: VoiceEngineV2NativeCaptureOptions): void {
		this.runtime.dispatch({type: 'nativeCapture.startRequested', options});
	}

	updateNativeCapture(options: VoiceEngineV2NativeCaptureOptions): void {
		this.runtime.dispatch({type: 'nativeCapture.updateRequested', options});
	}

	stopNativeCapture(captureId: string): void {
		this.runtime.dispatch({type: 'nativeCapture.stopRequested', captureId});
	}

	startNativeAudioTap(options: VoiceEngineV2NativeAudioTapOptions): void {
		this.runtime.dispatch({type: 'nativeAudioTap.startRequested', options});
	}

	stopNativeAudioTap(tapId: string): void {
		this.runtime.dispatch({type: 'nativeAudioTap.stopRequested', tapId});
	}

	attachNativeFrameSink(options: VoiceEngineV2NativeFrameSinkOptions): void {
		this.runtime.dispatch({type: 'nativeFrameSink.attachRequested', options});
	}

	detachNativeFrameSink(sinkId: string): void {
		this.runtime.dispatch({type: 'nativeFrameSink.detachRequested', sinkId});
	}

	setE2eeEnabled(enabled: boolean, keyId?: string | null): void {
		this.runtime.dispatch({type: 'e2ee.setEnabledRequested', enabled, keyId});
	}

	scheduleTimer(options: VoiceEngineV2TimerOptions): void {
		this.runtime.dispatch({type: 'timer.scheduleRequested', options});
	}

	cancelTimer(timerId: string): void {
		this.runtime.dispatch({type: 'timer.cancelRequested', timerId});
	}

	logDiagnostic(entry: VoiceEngineV2DiagnosticEntry): void {
		this.runtime.dispatch({type: 'diagnostics.logRequested', entry});
	}

	cancelOperation(operationId: number, resourceKey: VoiceEngineV2ResourceKey, reason: string): void {
		this.runtime.dispatch({type: 'operation.cancelRequested', operationId, resourceKey, reason});
	}

	teardown(reason: VoiceEngineV2LifecycleReason): void {
		this.runtime.dispatch({type: 'lifecycle.teardownRequested', reason});
	}

	dispose(): void {
		this.runtime.dispose();
	}
}
