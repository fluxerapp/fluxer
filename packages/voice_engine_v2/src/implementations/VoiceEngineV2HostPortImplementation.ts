// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2ExternalEventListener} from '@fluxer/voice_engine_v2/src/implementations/VoiceEngineV2ImplementationBase';
import {
	type VoiceEngineV2Driver,
	VoiceEngineV2ImplementationBase,
} from '@fluxer/voice_engine_v2/src/implementations/VoiceEngineV2ImplementationBase';
import type {
	CapabilitiesPort,
	DevicePort,
	DiagnosticsPort,
	GatewayPort,
	LiveKitMediaPort,
	LiveKitPort,
	NativeMediaPort,
	ParticipantProjectionIngestionPort,
	PermissionPort,
	StatsPort,
	SubscriptionPort,
	TimerPort,
	VoiceEngineV2EventSourcePort,
	VoiceEngineV2HostPorts,
	VoiceStateIngestionPort,
} from '@fluxer/voice_engine_v2/src/ports';
import {unsupportedPortError} from '@fluxer/voice_engine_v2/src/ports';
import type {
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2DataOptions,
	VoiceEngineV2DeviceInventory,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2GatewayVoiceStateWrite,
	VoiceEngineV2HardwareEncoderCapabilities,
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
} from '@fluxer/voice_engine_v2/src/protocol';

export class VoiceEngineV2HostPortImplementation extends VoiceEngineV2ImplementationBase {
	readonly kind = 'native' as const;

	constructor(ports: VoiceEngineV2HostPorts) {
		super(new VoiceEngineV2HostPortDriver(ports));
	}
}

export class VoiceEngineV2HostPortDriver implements VoiceEngineV2Driver {
	constructor(private readonly ports: VoiceEngineV2HostPorts) {}

	subscribe(listener: VoiceEngineV2ExternalEventListener): () => void {
		const unsubscribers: Array<() => void> = [];
		this.subscribeEventSource(this.ports.voiceState, listener, unsubscribers);
		this.subscribeEventSource(this.ports.participantProjection, listener, unsubscribers);
		return () => {
			for (const unsubscribe of unsubscribers.splice(0).reverse()) {
				unsubscribe();
			}
		};
	}

	async prewarm(): Promise<void> {
		const port = this.ports.media ?? this.ports.liveKit;
		if (port) await port.prewarm();
	}

	async writeGatewayVoiceState(options: VoiceEngineV2GatewayVoiceStateWrite): Promise<void> {
		return this.gatewayPort().writeVoiceState(options);
	}

	async clearGatewayVoiceState(guildId: string | null): Promise<void> {
		return this.gatewayPort().clearVoiceState(guildId);
	}

	async connect(options: VoiceEngineV2ConnectOptions): Promise<void> {
		return this.mediaPort().connect(options);
	}

	async disconnect(reason: VoiceEngineV2DisconnectReason): Promise<void> {
		return this.mediaPort().disconnect(reason);
	}

	async publishMicrophone(options: VoiceEngineV2MicrophoneOptions): Promise<void> {
		return this.mediaPort().publishMicrophone(options);
	}

	async unpublishMicrophone(): Promise<void> {
		return this.mediaPort().unpublishMicrophone();
	}

	async setMicrophoneEnabled(enabled: boolean): Promise<void> {
		return this.mediaPort().setMicrophoneEnabled(enabled);
	}

	async publishCamera(options: VoiceEngineV2CameraOptions): Promise<void> {
		return this.mediaPort().publishCamera(options);
	}

	async updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): Promise<void> {
		return this.mediaPort().updateCameraEncoding(options);
	}

	async unpublishCamera(options?: VoiceEngineV2CameraOptions): Promise<void> {
		return this.mediaPort().unpublishCamera(options);
	}

	async publishScreen(options: VoiceEngineV2ScreenOptions): Promise<void> {
		return this.mediaPort().publishScreen(options);
	}

	async updateScreenEncoding(options: VoiceEngineV2ScreenEncodingOptions): Promise<void> {
		return this.mediaPort().updateScreenEncoding(options);
	}

	async unpublishScreen(): Promise<void> {
		return this.mediaPort().unpublishScreen();
	}

	async publishScreenAudio(options: VoiceEngineV2ScreenAudioOptions): Promise<void> {
		return this.mediaPort().publishScreenAudio(options);
	}

	async unpublishScreenAudio(): Promise<void> {
		return this.mediaPort().unpublishScreenAudio();
	}

	async setOutputDevice(options: VoiceEngineV2OutputDeviceOptions): Promise<void> {
		return this.mediaPort().setOutputDevice(options);
	}

	async setParticipantVolume(options: VoiceEngineV2ParticipantVolumeOptions): Promise<void> {
		return this.subscriptionPort().setParticipantVolume(options);
	}

	async setRemoteTrackSubscription(options: VoiceEngineV2RemoteTrackSubscriptionOptions): Promise<void> {
		return this.subscriptionPort().setRemoteTrackSubscription(options);
	}

	async publishData(options: VoiceEngineV2DataOptions): Promise<void> {
		return this.mediaPort().publishData(options);
	}

	async collectStats(): Promise<VoiceEngineV2Stats> {
		return this.statsPort().collectStats();
	}

	async getHardwareEncoderCapabilities(): Promise<VoiceEngineV2HardwareEncoderCapabilities> {
		return this.capabilitiesPort().getHardwareEncoderCapabilities();
	}

	async checkPermission(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult> {
		return this.permissionPort().checkPermission(name);
	}

	async requestPermission(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult> {
		return this.permissionPort().requestPermission(name);
	}

	async enumerateDevices(): Promise<VoiceEngineV2DeviceInventory> {
		return this.devicePort().enumerateDevices();
	}

	async selectAudioInput(deviceId: string | null): Promise<void> {
		return this.devicePort().selectAudioInput(deviceId);
	}

	async selectAudioOutput(deviceId: string | null): Promise<void> {
		return this.devicePort().selectAudioOutput(deviceId);
	}

	async selectCamera(deviceId: string | null): Promise<void> {
		return this.devicePort().selectCamera(deviceId);
	}

	async startNativeCapture(options: VoiceEngineV2NativeCaptureOptions): Promise<void> {
		return this.nativeMediaPort().startCapture(options);
	}

	async updateNativeCapture(options: VoiceEngineV2NativeCaptureOptions): Promise<void> {
		return this.nativeMediaPort().updateCapture(options);
	}

	async stopNativeCapture(captureId: string): Promise<void> {
		return this.nativeMediaPort().stopCapture(captureId);
	}

	async startNativeAudioTap(options: VoiceEngineV2NativeAudioTapOptions): Promise<void> {
		return this.nativeMediaPort().startAudioTap(options);
	}

	async stopNativeAudioTap(tapId: string): Promise<void> {
		return this.nativeMediaPort().stopAudioTap(tapId);
	}

	async attachNativeFrameSink(options: VoiceEngineV2NativeFrameSinkOptions): Promise<void> {
		return this.nativeMediaPort().attachFrameSink(options);
	}

	async detachNativeFrameSink(sinkId: string): Promise<void> {
		return this.nativeMediaPort().detachFrameSink(sinkId);
	}

	async scheduleTimer(timerId: string, delayMs: number, repeat: boolean): Promise<void> {
		return this.timerPort().schedule({timerId, delayMs, repeat});
	}

	async cancelTimer(timerId: string): Promise<void> {
		return this.timerPort().cancel(timerId);
	}

	async logDiagnostic(level: string, code: string, message: string, detail?: unknown): Promise<void> {
		return this.diagnosticsPort().log(level, code, message, detail);
	}

	async cancelOperation(operationId: number, reason: string): Promise<void> {
		const cancelOperation = this.ports.cancelOperation;
		if (!cancelOperation) throw unsupportedPortError('operation.cancel');
		return cancelOperation(operationId, reason);
	}

	async teardown(): Promise<void> {
		const teardown = this.ports.teardown;
		if (!teardown) throw unsupportedPortError('lifecycle');
		return teardown();
	}

	private gatewayPort(): GatewayPort {
		const port = this.ports.gateway;
		if (!port) throw unsupportedPortError('gateway');
		return port;
	}

	private mediaPort(): LiveKitMediaPort | LiveKitPort {
		const port = this.ports.media ?? this.ports.liveKit;
		if (!port) throw unsupportedPortError('media');
		return port;
	}

	private subscriptionPort(): SubscriptionPort | LiveKitPort {
		const port = this.ports.subscriptions ?? this.ports.liveKit;
		if (!port) throw unsupportedPortError('subscriptions');
		return port;
	}

	private statsPort(): StatsPort | LiveKitPort {
		const port = this.ports.stats ?? this.ports.liveKit;
		if (!port) throw unsupportedPortError('stats');
		return port;
	}

	private nativeMediaPort(): NativeMediaPort {
		const port = this.ports.nativeMedia;
		if (!port) throw unsupportedPortError('nativeMedia');
		return port;
	}

	private capabilitiesPort(): CapabilitiesPort {
		const port = this.ports.capabilities;
		if (!port) throw unsupportedPortError('capabilities');
		return port;
	}

	private devicePort(): DevicePort {
		const port = this.ports.devices;
		if (!port) throw unsupportedPortError('devices');
		return port;
	}

	private permissionPort(): PermissionPort {
		const port = this.ports.permissions;
		if (!port) throw unsupportedPortError('permissions');
		return port;
	}

	private timerPort(): TimerPort {
		const port = this.ports.timers;
		if (!port) throw unsupportedPortError('timer');
		return port;
	}

	private diagnosticsPort(): DiagnosticsPort {
		const port = this.ports.diagnostics;
		if (!port) throw unsupportedPortError('diagnostics');
		return port;
	}

	private subscribeEventSource(
		port: VoiceStateIngestionPort | ParticipantProjectionIngestionPort | VoiceEngineV2EventSourcePort | undefined,
		listener: VoiceEngineV2ExternalEventListener,
		unsubscribers: Array<() => void>,
	): void {
		if (port) unsubscribers.push(port.subscribe(listener));
	}
}
