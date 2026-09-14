// SPDX-License-Identifier: AGPL-3.0-or-later

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
	VoiceEngineV2TimerOptions,
} from '@fluxer/voice_engine_v2/src/protocol/types';

export type VoiceEngineV2HostEventListener = (event: VoiceEngineV2Event) => void;

export interface VoiceEngineV2EventSourcePort {
	subscribe(listener: VoiceEngineV2HostEventListener): () => void;
}

export interface GatewayPort {
	writeVoiceState(options: VoiceEngineV2GatewayVoiceStateWrite): Promise<void>;
	clearVoiceState(guildId: string | null): Promise<void>;
}

export interface LiveKitPort {
	prewarm(): Promise<void>;
	connect(options: VoiceEngineV2ConnectOptions): Promise<void>;
	disconnect(reason: VoiceEngineV2DisconnectReason): Promise<void>;
	publishMicrophone(options: VoiceEngineV2MicrophoneOptions): Promise<void>;
	unpublishMicrophone(): Promise<void>;
	setMicrophoneEnabled(enabled: boolean): Promise<void>;
	publishCamera(options: VoiceEngineV2CameraOptions): Promise<void>;
	updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): Promise<void>;
	unpublishCamera(options?: VoiceEngineV2CameraOptions): Promise<void>;
	publishScreen(options: VoiceEngineV2ScreenOptions): Promise<void>;
	updateScreenEncoding(options: VoiceEngineV2ScreenEncodingOptions): Promise<void>;
	unpublishScreen(): Promise<void>;
	publishScreenAudio(options: VoiceEngineV2ScreenAudioOptions): Promise<void>;
	unpublishScreenAudio(): Promise<void>;
	setOutputDevice(options: VoiceEngineV2OutputDeviceOptions): Promise<void>;
	setParticipantVolume(options: VoiceEngineV2ParticipantVolumeOptions): Promise<void>;
	setRemoteTrackSubscription(options: VoiceEngineV2RemoteTrackSubscriptionOptions): Promise<void>;
	publishData(options: VoiceEngineV2DataOptions): Promise<void>;
	collectStats(): Promise<VoiceEngineV2Stats>;
}

export interface LiveKitMediaPort {
	prewarm(): Promise<void>;
	connect(options: VoiceEngineV2ConnectOptions): Promise<void>;
	disconnect(reason: VoiceEngineV2DisconnectReason): Promise<void>;
	publishMicrophone(options: VoiceEngineV2MicrophoneOptions): Promise<void>;
	unpublishMicrophone(): Promise<void>;
	setMicrophoneEnabled(enabled: boolean): Promise<void>;
	publishCamera(options: VoiceEngineV2CameraOptions): Promise<void>;
	updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): Promise<void>;
	unpublishCamera(options?: VoiceEngineV2CameraOptions): Promise<void>;
	publishScreen(options: VoiceEngineV2ScreenOptions): Promise<void>;
	updateScreenEncoding(options: VoiceEngineV2ScreenEncodingOptions): Promise<void>;
	unpublishScreen(): Promise<void>;
	publishScreenAudio(options: VoiceEngineV2ScreenAudioOptions): Promise<void>;
	unpublishScreenAudio(): Promise<void>;
	setOutputDevice(options: VoiceEngineV2OutputDeviceOptions): Promise<void>;
	publishData(options: VoiceEngineV2DataOptions): Promise<void>;
}

export interface SubscriptionPort {
	setParticipantVolume(options: VoiceEngineV2ParticipantVolumeOptions): Promise<void>;
	setRemoteTrackSubscription(options: VoiceEngineV2RemoteTrackSubscriptionOptions): Promise<void>;
}

export interface StatsPort {
	collectStats(): Promise<VoiceEngineV2Stats>;
}

export interface VoiceStateIngestionPort extends VoiceEngineV2EventSourcePort {}

export interface ParticipantProjectionIngestionPort extends VoiceEngineV2EventSourcePort {}

export interface NativeMediaPort {
	startCapture(options: VoiceEngineV2NativeCaptureOptions): Promise<void>;
	updateCapture(options: VoiceEngineV2NativeCaptureOptions): Promise<void>;
	stopCapture(captureId: string): Promise<void>;
	startAudioTap(options: VoiceEngineV2NativeAudioTapOptions): Promise<void>;
	stopAudioTap(tapId: string): Promise<void>;
	attachFrameSink(options: VoiceEngineV2NativeFrameSinkOptions): Promise<void>;
	detachFrameSink(sinkId: string): Promise<void>;
}

export interface DevicePort {
	enumerateDevices(): Promise<VoiceEngineV2DeviceInventory>;
	selectAudioInput(deviceId: string | null): Promise<void>;
	selectAudioOutput(deviceId: string | null): Promise<void>;
	selectCamera(deviceId: string | null): Promise<void>;
}

export interface CapabilitiesPort {
	getHardwareEncoderCapabilities(): Promise<VoiceEngineV2HardwareEncoderCapabilities>;
}

export interface PermissionPort {
	checkPermission(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult>;
	requestPermission(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult>;
}

export interface TimerPort {
	schedule(options: VoiceEngineV2TimerOptions): Promise<void>;
	cancel(timerId: string): Promise<void>;
}

export interface DiagnosticsPort {
	log(level: string, code: string, message: string, detail?: unknown): Promise<void>;
}

export interface VoiceEngineV2HostPorts {
	gateway?: GatewayPort;
	liveKit?: LiveKitPort;
	media?: LiveKitMediaPort;
	subscriptions?: SubscriptionPort;
	stats?: StatsPort;
	voiceState?: VoiceStateIngestionPort;
	participantProjection?: ParticipantProjectionIngestionPort;
	nativeMedia?: NativeMediaPort;
	capabilities?: CapabilitiesPort;
	devices?: DevicePort;
	permissions?: PermissionPort;
	timers?: TimerPort;
	diagnostics?: DiagnosticsPort;
	cancelOperation?: (operationId: number, reason: string) => Promise<void>;
	teardown?: () => Promise<void>;
}

export function unsupportedPortError(portName: string): VoiceEngineV2Error {
	return {
		code: 'unsupportedCapability',
		message: `Voice engine v2 host port is not available: ${portName}`,
		capability: portName,
	};
}
