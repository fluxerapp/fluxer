// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {
	VoiceEngineV2Driver,
	VoiceEngineV2ExternalEventListener,
} from '@fluxer/voice_engine_v2/src/implementations/VoiceEngineV2ImplementationBase';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2DataOptions,
	VoiceEngineV2DeviceInventory,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2Error,
	VoiceEngineV2GatewayVoiceStateWrite,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2NativeAudioTapOptions,
	VoiceEngineV2NativeCaptureOptions,
	VoiceEngineV2NativeFrameSinkOptions,
	VoiceEngineV2OutputDeviceOptions,
	VoiceEngineV2ParticipantVolumeOptions,
	VoiceEngineV2PermissionName,
	VoiceEngineV2PermissionResult,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2ScreenAudioOptions,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2Stats,
} from '@fluxer/voice_engine_v2/src/protocol/types';

export interface SimulatorDriverFaultPolicy {
	shouldDropConnect(): boolean;
	shouldDropDisconnect(): boolean;
	shouldFailMicrophonePublish(): boolean;
	shouldFailCameraPublish(): boolean;
	shouldFailScreenPublish(): boolean;
	shouldFailNativeCaptureStart(captureId: string): boolean;
	shouldEmitDeviceLoss(): boolean;
}

interface SimulatorDriverDeviceInventory {
	audioInputs: Array<string>;
	audioOutputs: Array<string>;
	cameras: Array<string>;
}

interface SimulatorDriverOptions {
	policy: SimulatorDriverFaultPolicy;
	inventory: SimulatorDriverDeviceInventory;
}

type SimulatorDriverCall =
	| {type: 'connect'; options: VoiceEngineV2ConnectOptions}
	| {type: 'disconnect'; reason: VoiceEngineV2DisconnectReason}
	| {type: 'publishMicrophone'; options: VoiceEngineV2MicrophoneOptions}
	| {type: 'unpublishMicrophone'}
	| {type: 'publishCamera'; options: VoiceEngineV2CameraOptions}
	| {type: 'updateCameraEncoding'; options: VoiceEngineV2CameraEncodingOptions}
	| {type: 'unpublishCamera'}
	| {type: 'publishScreen'; options: VoiceEngineV2ScreenOptions}
	| {type: 'unpublishScreen'}
	| {type: 'startNativeCapture'; options: VoiceEngineV2NativeCaptureOptions}
	| {type: 'stopNativeCapture'; captureId: string}
	| {type: 'attachNativeFrameSink'; options: VoiceEngineV2NativeFrameSinkOptions}
	| {type: 'detachNativeFrameSink'; sinkId: string}
	| {type: 'setMicrophoneEnabled'; enabled: boolean}
	| {type: 'updateScreenEncoding'; options: VoiceEngineV2ScreenEncodingOptions}
	| {type: 'publishScreenAudio'; options: VoiceEngineV2ScreenAudioOptions}
	| {type: 'unpublishScreenAudio'}
	| {type: 'collectStats'};

export class VoiceEngineV2SimulatorDriver implements VoiceEngineV2Driver {
	readonly calls: Array<SimulatorDriverCall> = [];
	private readonly listeners = new Set<VoiceEngineV2ExternalEventListener>();
	private readonly policy: SimulatorDriverFaultPolicy;
	private readonly inventory: SimulatorDriverDeviceInventory;
	private readonly activeCaptures = new Set<string>();
	private readonly activeFrameSinks = new Set<string>();

	constructor(options: SimulatorDriverOptions) {
		assert.ok(options.policy, 'simulator driver requires a fault policy');
		assert.ok(options.inventory, 'simulator driver requires a device inventory');
		this.policy = options.policy;
		this.inventory = options.inventory;
	}

	subscribe(listener: VoiceEngineV2ExternalEventListener): () => void {
		assert.ok(typeof listener === 'function', 'subscribe expects a listener function');
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	emit(event: VoiceEngineV2Event): void {
		assert.ok(event, 'cannot emit an undefined event');
		assert.ok(typeof event.type === 'string', 'emitted event must have a string type');
		for (const listener of this.listeners) listener(event);
	}

	async prewarm(): Promise<void> {}

	async writeGatewayVoiceState(_options: VoiceEngineV2GatewayVoiceStateWrite): Promise<void> {}

	async clearGatewayVoiceState(_guildId: string | null): Promise<void> {}

	async connect(options: VoiceEngineV2ConnectOptions): Promise<void> {
		assert.ok(options, 'connect requires options');
		this.calls.push({type: 'connect', options});
		if (this.policy.shouldDropConnect()) throw networkError('simulated connect failure');
	}

	async disconnect(reason: VoiceEngineV2DisconnectReason): Promise<void> {
		assert.ok(reason, 'disconnect requires a reason');
		this.calls.push({type: 'disconnect', reason});
		if (this.policy.shouldDropDisconnect()) throw networkError('simulated disconnect failure');
	}

	async publishMicrophone(options: VoiceEngineV2MicrophoneOptions): Promise<void> {
		assert.ok(options, 'microphone publish requires options');
		this.calls.push({type: 'publishMicrophone', options});
		if (this.policy.shouldFailMicrophonePublish()) throw deviceError('simulated microphone publish failure');
	}

	async unpublishMicrophone(): Promise<void> {
		this.calls.push({type: 'unpublishMicrophone'});
	}

	async setMicrophoneEnabled(enabled: boolean): Promise<void> {
		this.calls.push({type: 'setMicrophoneEnabled', enabled});
	}

	async publishCamera(options: VoiceEngineV2CameraOptions): Promise<void> {
		assert.ok(options, 'camera publish requires options');
		this.calls.push({type: 'publishCamera', options});
		if (this.policy.shouldFailCameraPublish()) throw deviceError('simulated camera publish failure');
	}

	async updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): Promise<void> {
		assert.ok(options, 'camera updateEncoding requires options');
		this.calls.push({type: 'updateCameraEncoding', options});
	}

	async unpublishCamera(): Promise<void> {
		this.calls.push({type: 'unpublishCamera'});
	}

	async publishScreen(options: VoiceEngineV2ScreenOptions): Promise<void> {
		assert.ok(options, 'screen publish requires options');
		this.calls.push({type: 'publishScreen', options});
		if (this.policy.shouldFailScreenPublish()) throw captureError('simulated screen publish failure');
	}

	async updateScreenEncoding(options: VoiceEngineV2ScreenEncodingOptions): Promise<void> {
		this.calls.push({type: 'updateScreenEncoding', options});
	}

	async unpublishScreen(): Promise<void> {
		this.calls.push({type: 'unpublishScreen'});
	}

	async publishScreenAudio(options: VoiceEngineV2ScreenAudioOptions): Promise<void> {
		this.calls.push({type: 'publishScreenAudio', options});
	}

	async unpublishScreenAudio(): Promise<void> {
		this.calls.push({type: 'unpublishScreenAudio'});
	}

	async setOutputDevice(_options: VoiceEngineV2OutputDeviceOptions): Promise<void> {}

	async setParticipantVolume(_options: VoiceEngineV2ParticipantVolumeOptions): Promise<void> {}

	async setRemoteTrackSubscription(_options: VoiceEngineV2RemoteTrackSubscriptionOptions): Promise<void> {}

	async publishData(_options: VoiceEngineV2DataOptions): Promise<void> {}

	async collectStats(): Promise<VoiceEngineV2Stats> {
		this.calls.push({type: 'collectStats'});
		return {rttMs: 0, outbound: [], inbound: []};
	}

	async checkPermission(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult> {
		return {name, status: 'granted', canPrompt: false};
	}

	async requestPermission(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult> {
		return {name, status: 'granted', canPrompt: false};
	}

	async enumerateDevices(): Promise<VoiceEngineV2DeviceInventory> {
		return {
			audioInputs: this.inventory.audioInputs.map(toInputDevice),
			audioOutputs: this.inventory.audioOutputs.map(toOutputDevice),
			cameras: this.inventory.cameras.map(toCameraDevice),
			selectedAudioInputId: this.inventory.audioInputs[0] ?? null,
			selectedAudioOutputId: this.inventory.audioOutputs[0] ?? null,
			selectedCameraId: this.inventory.cameras[0] ?? null,
		};
	}

	async selectAudioInput(_deviceId: string | null): Promise<void> {}

	async selectAudioOutput(_deviceId: string | null): Promise<void> {}

	async selectCamera(_deviceId: string | null): Promise<void> {}

	async startNativeCapture(options: VoiceEngineV2NativeCaptureOptions): Promise<void> {
		assert.ok(options.captureId, 'native capture requires a captureId');
		this.calls.push({type: 'startNativeCapture', options});
		if (this.policy.shouldFailNativeCaptureStart(options.captureId)) {
			throw captureError(`simulated native capture failure for ${options.captureId}`);
		}
		this.activeCaptures.add(options.captureId);
	}

	async updateNativeCapture(_options: VoiceEngineV2NativeCaptureOptions): Promise<void> {}

	async stopNativeCapture(captureId: string): Promise<void> {
		assert.ok(captureId, 'stopNativeCapture requires a captureId');
		this.calls.push({type: 'stopNativeCapture', captureId});
		this.activeCaptures.delete(captureId);
	}

	async startNativeAudioTap(_options: VoiceEngineV2NativeAudioTapOptions): Promise<void> {}

	async stopNativeAudioTap(_tapId: string): Promise<void> {}

	async attachNativeFrameSink(options: VoiceEngineV2NativeFrameSinkOptions): Promise<void> {
		assert.ok(options.sinkId, 'attachNativeFrameSink requires a sinkId');
		this.calls.push({type: 'attachNativeFrameSink', options});
		this.activeFrameSinks.add(options.sinkId);
	}

	async detachNativeFrameSink(sinkId: string): Promise<void> {
		assert.ok(sinkId, 'detachNativeFrameSink requires a sinkId');
		this.calls.push({type: 'detachNativeFrameSink', sinkId});
		this.activeFrameSinks.delete(sinkId);
	}

	async setE2eeEnabled(_enabled: boolean, _keyId?: string | null): Promise<void> {}

	async scheduleTimer(_timerId: string, _delayMs: number, _repeat: boolean): Promise<void> {}

	async cancelTimer(_timerId: string): Promise<void> {}

	async logDiagnostic(_level: string, _code: string, _message: string, _detail?: unknown): Promise<void> {}

	async cancelOperation(_operationId: number, _reason: string): Promise<void> {}

	async teardown(): Promise<void> {}
}

function networkError(message: string): VoiceEngineV2Error {
	return {code: 'liveKitError', message, capability: 'connect'};
}

function deviceError(message: string): VoiceEngineV2Error {
	return {code: 'deviceUnavailable', message, capability: 'microphone'};
}

function captureError(message: string): VoiceEngineV2Error {
	return {code: 'nativeCaptureError', message, capability: 'nativeCapture'};
}

function toInputDevice(deviceId: string): {deviceId: string; label: string; isDefault: boolean} {
	return {deviceId, label: `input-${deviceId}`, isDefault: false};
}

function toOutputDevice(deviceId: string): {deviceId: string; label: string; isDefault: boolean} {
	return {deviceId, label: `output-${deviceId}`, isDefault: false};
}

function toCameraDevice(deviceId: string): {deviceId: string; label: string} {
	return {deviceId, label: `camera-${deviceId}`};
}
