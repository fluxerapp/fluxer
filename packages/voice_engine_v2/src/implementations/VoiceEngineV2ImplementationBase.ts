// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
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
	VoiceEngineV2ImplementationKind,
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

export interface VoiceEngineV2CommandSuccess {
	ok: true;
	stats?: VoiceEngineV2Stats;
	hardwareEncoderCapabilities?: VoiceEngineV2HardwareEncoderCapabilities;
	permissionResult?: VoiceEngineV2PermissionResult;
	deviceInventory?: VoiceEngineV2DeviceInventory;
}

export interface VoiceEngineV2CommandFailure {
	ok: false;
	error: VoiceEngineV2Error;
}

export type VoiceEngineV2CommandResult = VoiceEngineV2CommandSuccess | VoiceEngineV2CommandFailure;

export type VoiceEngineV2ExternalEventListener = (event: VoiceEngineV2Event) => void;

export interface VoiceEngineV2Implementation {
	readonly kind: VoiceEngineV2ImplementationKind;
	execute(command: VoiceEngineV2Command): Promise<VoiceEngineV2CommandResult>;
	subscribe?(listener: VoiceEngineV2ExternalEventListener): () => void;
}

export interface VoiceEngineV2Driver {
	prewarm(): Promise<void>;
	writeGatewayVoiceState?(options: VoiceEngineV2GatewayVoiceStateWrite): Promise<void>;
	clearGatewayVoiceState?(guildId: string | null): Promise<void>;
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
	getHardwareEncoderCapabilities?(): Promise<VoiceEngineV2HardwareEncoderCapabilities>;
	checkPermission?(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult>;
	requestPermission?(name: VoiceEngineV2PermissionName): Promise<VoiceEngineV2PermissionResult>;
	enumerateDevices?(): Promise<VoiceEngineV2DeviceInventory>;
	selectAudioInput?(deviceId: string | null): Promise<void>;
	selectAudioOutput?(deviceId: string | null): Promise<void>;
	selectCamera?(deviceId: string | null): Promise<void>;
	startNativeCapture?(options: VoiceEngineV2NativeCaptureOptions): Promise<void>;
	updateNativeCapture?(options: VoiceEngineV2NativeCaptureOptions): Promise<void>;
	stopNativeCapture?(captureId: string): Promise<void>;
	startNativeAudioTap?(options: VoiceEngineV2NativeAudioTapOptions): Promise<void>;
	stopNativeAudioTap?(tapId: string): Promise<void>;
	attachNativeFrameSink?(options: VoiceEngineV2NativeFrameSinkOptions): Promise<void>;
	detachNativeFrameSink?(sinkId: string): Promise<void>;
	setE2eeEnabled?(enabled: boolean, keyId?: string | null): Promise<void>;
	scheduleTimer?(timerId: string, delayMs: number, repeat: boolean): Promise<void>;
	cancelTimer?(timerId: string): Promise<void>;
	logDiagnostic?(level: string, code: string, message: string, detail?: unknown): Promise<void>;
	cancelOperation?(operationId: number, reason: string): Promise<void>;
	teardown?(): Promise<void>;
	subscribe?(listener: VoiceEngineV2ExternalEventListener): () => void;
}

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

export abstract class VoiceEngineV2ImplementationBase implements VoiceEngineV2Implementation {
	abstract readonly kind: VoiceEngineV2ImplementationKind;

	constructor(protected readonly driver: VoiceEngineV2Driver) {}

	subscribe(listener: VoiceEngineV2ExternalEventListener): () => void {
		return this.driver.subscribe?.(listener) ?? (() => {});
	}

	async execute(command: VoiceEngineV2Command): Promise<VoiceEngineV2CommandResult> {
		try {
			return await this.dispatchCommand(command);
		} catch (error) {
			return {ok: false, error: errorToVoiceEngineV2Error(error)};
		}
	}

	private dispatchCommand(command: VoiceEngineV2Command): Promise<VoiceEngineV2CommandResult> {
		switch (command.type) {
			case 'implementation.prewarm':
			case 'gateway.voiceState.write':
			case 'gateway.voiceState.clear':
			case 'connection.connect':
			case 'connection.disconnect':
			case 'lifecycle.teardown':
				return this.executeSession(command);
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
				return this.executeLocalMedia(command);
			case 'outputDevice.set':
			case 'participantVolume.set':
			case 'remoteTrackSubscription.set':
			case 'data.publish':
				return this.executeRouting(command);
			case 'stats.collect':
			case 'capabilities.queryHardwareEncoder':
			case 'permissions.check':
			case 'permissions.request':
				return this.executeQuery(command);
			case 'devices.enumerate':
			case 'devices.selectAudioInput':
			case 'devices.selectAudioOutput':
			case 'devices.selectCamera':
				return this.executeDevices(command);
			case 'nativeCapture.start':
			case 'nativeCapture.update':
			case 'nativeCapture.stop':
			case 'nativeAudioTap.start':
			case 'nativeAudioTap.stop':
			case 'nativeFrameSink.attach':
			case 'nativeFrameSink.detach':
				return this.executeNative(command);
			case 'e2ee.setEnabled':
			case 'timer.schedule':
			case 'timer.cancel':
			case 'diagnostics.log':
			case 'operation.cancel':
				return this.executeUtility(command);
		}
	}

	private async executeSession(command: VoiceEngineV2SessionCommand): Promise<VoiceEngineV2CommandResult> {
		switch (command.type) {
			case 'implementation.prewarm':
				await this.driver.prewarm();
				return {ok: true};
			case 'gateway.voiceState.write':
				await this.callOptional('gateway', this.driver.writeGatewayVoiceState, command.options);
				return {ok: true};
			case 'gateway.voiceState.clear':
				await this.callOptional('gateway', this.driver.clearGatewayVoiceState, command.guildId);
				return {ok: true};
			case 'connection.connect':
				await this.driver.connect(command.options);
				return {ok: true};
			case 'connection.disconnect':
				await this.driver.disconnect(command.reason);
				return {ok: true};
			case 'lifecycle.teardown':
				await this.callOptional('lifecycle', this.driver.teardown);
				return {ok: true};
		}
	}

	private async executeLocalMedia(command: VoiceEngineV2LocalMediaCommand): Promise<VoiceEngineV2CommandResult> {
		switch (command.type) {
			case 'microphone.publish':
				await this.driver.publishMicrophone(command.options);
				return {ok: true};
			case 'microphone.unpublish':
				await this.driver.unpublishMicrophone();
				return {ok: true};
			case 'microphone.setEnabled':
				await this.driver.setMicrophoneEnabled(command.enabled);
				return {ok: true};
			case 'camera.publish':
				await this.driver.publishCamera(command.options);
				return {ok: true};
			case 'camera.updateEncoding':
				await this.driver.updateCameraEncoding(command.options);
				return {ok: true};
			case 'camera.unpublish':
				await this.driver.unpublishCamera(command.options);
				return {ok: true};
			case 'screen.publish':
				await this.driver.publishScreen(command.options);
				return {ok: true};
			case 'screen.updateEncoding':
				await this.driver.updateScreenEncoding(command.options);
				return {ok: true};
			case 'screen.unpublish':
				await this.driver.unpublishScreen();
				return {ok: true};
			case 'screenAudio.publish':
				await this.driver.publishScreenAudio(command.options);
				return {ok: true};
			case 'screenAudio.unpublish':
				await this.driver.unpublishScreenAudio();
				return {ok: true};
		}
	}

	private async executeRouting(command: VoiceEngineV2RoutingCommand): Promise<VoiceEngineV2CommandResult> {
		switch (command.type) {
			case 'outputDevice.set':
				await this.driver.setOutputDevice(command.options);
				return {ok: true};
			case 'participantVolume.set':
				await this.driver.setParticipantVolume(command.options);
				return {ok: true};
			case 'remoteTrackSubscription.set':
				await this.driver.setRemoteTrackSubscription(command.options);
				return {ok: true};
			case 'data.publish':
				await this.driver.publishData(command.options);
				return {ok: true};
		}
	}

	private async executeQuery(command: VoiceEngineV2QueryCommand): Promise<VoiceEngineV2CommandResult> {
		switch (command.type) {
			case 'stats.collect':
				return {ok: true, stats: await this.driver.collectStats()};
			case 'capabilities.queryHardwareEncoder': {
				const getHardwareEncoderCapabilities = this.driver.getHardwareEncoderCapabilities;
				if (!getHardwareEncoderCapabilities) throw unsupportedDriverMethod('capabilities');
				return {
					ok: true,
					hardwareEncoderCapabilities: await getHardwareEncoderCapabilities.call(this.driver),
				};
			}
			case 'permissions.check':
				return {
					ok: true,
					permissionResult: await this.callOptional('permissions', this.driver.checkPermission, command.name),
				};
			case 'permissions.request':
				return {
					ok: true,
					permissionResult: await this.callOptional('permissions', this.driver.requestPermission, command.name),
				};
		}
	}

	private async executeDevices(command: VoiceEngineV2DevicesCommand): Promise<VoiceEngineV2CommandResult> {
		switch (command.type) {
			case 'devices.enumerate':
				return {ok: true, deviceInventory: await this.callOptional('devices', this.driver.enumerateDevices)};
			case 'devices.selectAudioInput':
				await this.callOptional('devices', this.driver.selectAudioInput, command.deviceId);
				return {ok: true};
			case 'devices.selectAudioOutput':
				await this.callOptional('devices', this.driver.selectAudioOutput, command.deviceId);
				return {ok: true};
			case 'devices.selectCamera':
				await this.callOptional('devices', this.driver.selectCamera, command.deviceId);
				return {ok: true};
		}
	}

	private async executeNative(command: VoiceEngineV2NativeCommand): Promise<VoiceEngineV2CommandResult> {
		switch (command.type) {
			case 'nativeCapture.start':
				await this.callOptional('nativeCapture', this.driver.startNativeCapture, command.options);
				return {ok: true};
			case 'nativeCapture.update':
				await this.callOptional('nativeCapture', this.driver.updateNativeCapture, command.options);
				return {ok: true};
			case 'nativeCapture.stop':
				await this.callOptional('nativeCapture', this.driver.stopNativeCapture, command.captureId);
				return {ok: true};
			case 'nativeAudioTap.start':
				await this.callOptional('nativeAudioTap', this.driver.startNativeAudioTap, command.options);
				return {ok: true};
			case 'nativeAudioTap.stop':
				await this.callOptional('nativeAudioTap', this.driver.stopNativeAudioTap, command.tapId);
				return {ok: true};
			case 'nativeFrameSink.attach':
				await this.callOptional('nativeFrameSink', this.driver.attachNativeFrameSink, command.options);
				return {ok: true};
			case 'nativeFrameSink.detach':
				await this.callOptional('nativeFrameSink', this.driver.detachNativeFrameSink, command.sinkId);
				return {ok: true};
		}
	}

	private async executeUtility(command: VoiceEngineV2UtilityCommand): Promise<VoiceEngineV2CommandResult> {
		switch (command.type) {
			case 'e2ee.setEnabled':
				await this.callOptional('e2ee', this.driver.setE2eeEnabled, command.enabled, command.keyId);
				return {ok: true};
			case 'timer.schedule':
				await this.callOptional(
					'timer',
					this.driver.scheduleTimer,
					command.options.timerId,
					command.options.delayMs,
					command.options.repeat ?? false,
				);
				return {ok: true};
			case 'timer.cancel':
				await this.callOptional('timer', this.driver.cancelTimer, command.timerId);
				return {ok: true};
			case 'diagnostics.log':
				await this.callOptional(
					'diagnostics',
					this.driver.logDiagnostic,
					command.entry.level,
					command.entry.code,
					command.entry.message,
					command.entry.detail,
				);
				return {ok: true};
			case 'operation.cancel':
				await this.callOptional(
					'operation.cancel',
					this.driver.cancelOperation,
					command.targetOperationId,
					command.reason,
				);
				return {ok: true};
		}
	}

	private async callOptional<Arguments extends Array<unknown>, Result>(
		capability: string,
		method: ((...args: Arguments) => Promise<Result>) | undefined,
		...args: Arguments
	): Promise<Result> {
		if (!method) throw unsupportedDriverMethod(capability);
		return method.call(this.driver, ...args);
	}
}

function unsupportedDriverMethod(capability: string): VoiceEngineV2Error {
	return {
		code: 'unsupportedCapability',
		message: `Voice engine v2 driver does not implement ${capability}`,
		capability,
	};
}

export function errorToVoiceEngineV2Error(error: unknown): VoiceEngineV2Error {
	if (isVoiceEngineV2Error(error)) return error;
	return {
		code: 'implementationError',
		message: error instanceof Error ? error.message : String(error),
	};
}

function isVoiceEngineV2Error(error: unknown): error is VoiceEngineV2Error {
	if (typeof error !== 'object' || error == null) return false;
	const candidate = error as {code?: unknown; message?: unknown};
	return typeof candidate.code === 'string' && typeof candidate.message === 'string';
}
