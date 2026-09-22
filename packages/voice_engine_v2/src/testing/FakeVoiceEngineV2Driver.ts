// SPDX-License-Identifier: AGPL-3.0-or-later

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
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2Error,
	VoiceEngineV2GatewayVoiceStateWrite,
	VoiceEngineV2HardwareEncoderCapabilities,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2OutputDeviceOptions,
	VoiceEngineV2ParticipantVolumeOptions,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2ScreenAudioOptions,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2Stats,
} from '@fluxer/voice_engine_v2/src/protocol/types';

export type FakeVoiceEngineV2DriverCall =
	| {type: 'prewarm'}
	| {type: 'writeGatewayVoiceState'; options: VoiceEngineV2GatewayVoiceStateWrite}
	| {type: 'clearGatewayVoiceState'; guildId: string | null}
	| {type: 'connect'; options: VoiceEngineV2ConnectOptions}
	| {type: 'disconnect'; reason: VoiceEngineV2DisconnectReason}
	| {type: 'publishMicrophone'; options: VoiceEngineV2MicrophoneOptions}
	| {type: 'unpublishMicrophone'}
	| {type: 'setMicrophoneEnabled'; enabled: boolean}
	| {type: 'publishCamera'; options: VoiceEngineV2CameraOptions}
	| {type: 'updateCameraEncoding'; options: VoiceEngineV2CameraEncodingOptions}
	| {type: 'unpublishCamera'}
	| {type: 'publishScreen'; options: VoiceEngineV2ScreenOptions}
	| {type: 'updateScreenEncoding'; options: VoiceEngineV2ScreenEncodingOptions}
	| {type: 'unpublishScreen'}
	| {type: 'publishScreenAudio'; options: VoiceEngineV2ScreenAudioOptions}
	| {type: 'unpublishScreenAudio'}
	| {type: 'setOutputDevice'; options: VoiceEngineV2OutputDeviceOptions}
	| {type: 'setParticipantVolume'; options: VoiceEngineV2ParticipantVolumeOptions}
	| {type: 'setRemoteTrackSubscription'; options: VoiceEngineV2RemoteTrackSubscriptionOptions}
	| {type: 'publishData'; options: VoiceEngineV2DataOptions}
	| {type: 'collectStats'}
	| {type: 'getHardwareEncoderCapabilities'};

export type FakeVoiceEngineV2DriverCallType = FakeVoiceEngineV2DriverCall['type'];

export interface FakeVoiceEngineV2FailureMap {
	prewarm?: VoiceEngineV2Error;
	writeGatewayVoiceState?: VoiceEngineV2Error;
	clearGatewayVoiceState?: VoiceEngineV2Error;
	connect?: VoiceEngineV2Error;
	disconnect?: VoiceEngineV2Error;
	publishMicrophone?: VoiceEngineV2Error;
	unpublishMicrophone?: VoiceEngineV2Error;
	setMicrophoneEnabled?: VoiceEngineV2Error;
	publishCamera?: VoiceEngineV2Error;
	updateCameraEncoding?: VoiceEngineV2Error;
	unpublishCamera?: VoiceEngineV2Error;
	publishScreen?: VoiceEngineV2Error;
	updateScreenEncoding?: VoiceEngineV2Error;
	unpublishScreen?: VoiceEngineV2Error;
	publishScreenAudio?: VoiceEngineV2Error;
	unpublishScreenAudio?: VoiceEngineV2Error;
	setOutputDevice?: VoiceEngineV2Error;
	setParticipantVolume?: VoiceEngineV2Error;
	setRemoteTrackSubscription?: VoiceEngineV2Error;
	publishData?: VoiceEngineV2Error;
	collectStats?: VoiceEngineV2Error;
	getHardwareEncoderCapabilities?: VoiceEngineV2Error;
}

export interface FakeVoiceEngineV2DriverOptions {
	failures?: FakeVoiceEngineV2FailureMap;
	stats?: VoiceEngineV2Stats;
	hardwareEncoderCapabilities?: VoiceEngineV2HardwareEncoderCapabilities;
}

export class FakeVoiceEngineV2Driver implements VoiceEngineV2Driver {
	readonly calls: Array<FakeVoiceEngineV2DriverCall> = [];
	private readonly listeners = new Set<VoiceEngineV2ExternalEventListener>();
	private readonly failures: FakeVoiceEngineV2FailureMap;
	private readonly stats: VoiceEngineV2Stats;
	private readonly hardwareEncoderCapabilities: VoiceEngineV2HardwareEncoderCapabilities;

	constructor(options: FakeVoiceEngineV2DriverOptions = {}) {
		this.failures = options.failures ?? {};
		this.stats = options.stats ?? {rttMs: null, outbound: [], inbound: []};
		this.hardwareEncoderCapabilities = options.hardwareEncoderCapabilities ?? {
			available: false,
			backend: 'none',
			compiled: false,
			runtime: false,
			codecs: [],
			zeroCopy: false,
			nativeInputs: [],
			reason: 'not-queried',
		};
	}

	async prewarm(): Promise<void> {
		this.record({type: 'prewarm'});
	}

	async writeGatewayVoiceState(options: VoiceEngineV2GatewayVoiceStateWrite): Promise<void> {
		this.record({type: 'writeGatewayVoiceState', options});
	}

	async clearGatewayVoiceState(guildId: string | null): Promise<void> {
		this.record({type: 'clearGatewayVoiceState', guildId});
	}

	async connect(options: VoiceEngineV2ConnectOptions): Promise<void> {
		this.record({type: 'connect', options});
	}

	async disconnect(reason: VoiceEngineV2DisconnectReason): Promise<void> {
		this.record({type: 'disconnect', reason});
	}

	async publishMicrophone(options: VoiceEngineV2MicrophoneOptions): Promise<void> {
		this.record({type: 'publishMicrophone', options});
	}

	async unpublishMicrophone(): Promise<void> {
		this.record({type: 'unpublishMicrophone'});
	}

	async setMicrophoneEnabled(enabled: boolean): Promise<void> {
		this.record({type: 'setMicrophoneEnabled', enabled});
	}

	async publishCamera(options: VoiceEngineV2CameraOptions): Promise<void> {
		this.record({type: 'publishCamera', options});
	}

	async updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): Promise<void> {
		this.record({type: 'updateCameraEncoding', options});
	}

	async unpublishCamera(): Promise<void> {
		this.record({type: 'unpublishCamera'});
	}

	async publishScreen(options: VoiceEngineV2ScreenOptions): Promise<void> {
		this.record({type: 'publishScreen', options});
	}

	async updateScreenEncoding(options: VoiceEngineV2ScreenEncodingOptions): Promise<void> {
		this.record({type: 'updateScreenEncoding', options});
	}

	async unpublishScreen(): Promise<void> {
		this.record({type: 'unpublishScreen'});
	}

	async publishScreenAudio(options: VoiceEngineV2ScreenAudioOptions): Promise<void> {
		this.record({type: 'publishScreenAudio', options});
	}

	async unpublishScreenAudio(): Promise<void> {
		this.record({type: 'unpublishScreenAudio'});
	}

	async setOutputDevice(options: VoiceEngineV2OutputDeviceOptions): Promise<void> {
		this.record({type: 'setOutputDevice', options});
	}

	async setParticipantVolume(options: VoiceEngineV2ParticipantVolumeOptions): Promise<void> {
		this.record({type: 'setParticipantVolume', options});
	}

	async setRemoteTrackSubscription(options: VoiceEngineV2RemoteTrackSubscriptionOptions): Promise<void> {
		this.record({type: 'setRemoteTrackSubscription', options});
	}

	async publishData(options: VoiceEngineV2DataOptions): Promise<void> {
		this.record({type: 'publishData', options});
	}

	async collectStats(): Promise<VoiceEngineV2Stats> {
		this.record({type: 'collectStats'});
		return this.stats;
	}

	async getHardwareEncoderCapabilities(): Promise<VoiceEngineV2HardwareEncoderCapabilities> {
		this.record({type: 'getHardwareEncoderCapabilities'});
		return this.hardwareEncoderCapabilities;
	}

	subscribe(listener: VoiceEngineV2ExternalEventListener): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}

	emitExternalEvent(event: VoiceEngineV2Event): void {
		for (const listener of this.listeners) {
			listener(event);
		}
	}

	private record(call: FakeVoiceEngineV2DriverCall): void {
		this.calls.push(call);
		const failure = this.failures[call.type];
		if (failure) throw failure;
	}
}
