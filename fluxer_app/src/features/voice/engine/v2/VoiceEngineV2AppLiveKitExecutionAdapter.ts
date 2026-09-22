// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {buildVoiceEngineV2AppCameraPermissionDeniedError} from '@app/features/voice/engine/v2/VoiceEngineV2AppCameraPermissionDeniedError';
import type {VoiceEngineV2AppCameraTransitionOutcome} from '@app/features/voice/engine/v2/VoiceEngineV2AppCameraTransition';
import type {
	LiveKitPort,
	StatsPort,
	SubscriptionPort,
	VoiceEngineV2CameraEncodingOptions,
	VoiceEngineV2CameraOptions,
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2DataOptions,
	VoiceEngineV2DisconnectReason,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2OutputDeviceOptions,
	VoiceEngineV2ParticipantVolumeOptions,
	VoiceEngineV2RemoteTrackSubscriptionOptions,
	VoiceEngineV2ScreenAudioOptions,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2Stats,
} from '@fluxer/voice_engine_v2';
import {
	assertVoiceEngineV2BridgeAudioOptionsInvariants,
	assertVoiceEngineV2BridgeVideoOptionsInvariants,
} from '@fluxer/voice_engine_v2/bridge';
import type {Room, ScreenShareCaptureOptions, TrackPublishOptions, VideoCodec} from 'livekit-client';

export interface VoiceEngineV2AppLiveKitMediaDelegate {
	enableMicrophone(room: Room, channelId: string | null, options?: VoiceEngineV2MicrophoneOptions): Promise<void>;
	disableMicrophone(room: Room): Promise<void>;
	setMicrophoneEnabled(enabled: boolean, room: Room, channelId: string | null): Promise<void>;
	setCameraEnabled(
		enabled: boolean,
		options?: {deviceId?: string; sendUpdate?: boolean},
	): Promise<VoiceEngineV2AppCameraTransitionOutcome>;
	updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): Promise<void>;
}

export interface VoiceEngineV2AppLiveKitConnectionDelegate {
	startConnection?(guildId: string | null, channelId: string): boolean;
	connectToVoiceServer?(options: VoiceEngineV2ConnectOptions): Promise<void>;
	disconnectFromVoiceChannel?(reason: 'user' | 'error' | 'server'): void;
}

export interface VoiceEngineV2AppLiveKitScreenShareDelegate {
	publishControllerScreenViaLiveKitFlows(room: Room | null, options: VoiceEngineV2ScreenOptions): Promise<void>;
	unpublishControllerScreenViaLiveKitFlows(room: Room | null): Promise<void>;
	updateActiveScreenShareSettings(
		room: Room | null,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<boolean>;
	setScreenShareAudioMuted(room: Room | null, muted: boolean): void;
}

export interface VoiceEngineV2AppLiveKitAudioOutputStore {
	setOutputDevice(deviceId: string): Promise<void>;
}

export interface VoiceEngineV2AppLiveKitLogger {
	debug(...args: Array<unknown>): void;
	info(...args: Array<unknown>): void;
	warn(...args: Array<unknown>): void;
}

export type VoiceEngineV2AppLiveKitRoomAccessor = () => Room | null;

export interface VoiceEngineV2AppLiveKitExecutionAdapterOptions {
	media: VoiceEngineV2AppLiveKitMediaDelegate;
	connection: VoiceEngineV2AppLiveKitConnectionDelegate;
	screenShare: VoiceEngineV2AppLiveKitScreenShareDelegate;
	getRoom: VoiceEngineV2AppLiveKitRoomAccessor;
	audioOutputStore?: VoiceEngineV2AppLiveKitAudioOutputStore | null;
	subscriptions?: SubscriptionPort | null;
	stats?: StatsPort | null;
	logger?: VoiceEngineV2AppLiveKitLogger;
	getActiveChannelId?: () => string | null;
	getActiveGuildId?: () => string | null;
}

const ADAPTER_NAME = 'VoiceEngineV2AppLiveKitExecutionAdapter';

function buildOperatingError(method: string, reason: string): Error {
	const error = new Error(`${ADAPTER_NAME}.${method}: ${reason}`);
	error.name = 'VoiceEngineV2AppLiveKitOperatingError';
	return error;
}

function toUint8Array(payload: ArrayBuffer | ArrayBufferView): Uint8Array<ArrayBuffer> {
	if (!ArrayBuffer.isView(payload)) return new Uint8Array(payload);
	const {buffer, byteOffset, byteLength} = payload;
	if (buffer instanceof ArrayBuffer) return new Uint8Array(buffer, byteOffset, byteLength);
	return new Uint8Array(buffer, byteOffset, byteLength).slice();
}

function isDataPayload(value: unknown): value is ArrayBuffer | ArrayBufferView {
	if (value instanceof ArrayBuffer) return true;
	if (ArrayBuffer.isView(value)) return true;
	return false;
}

function isVideoCodecValue(value: unknown): value is VideoCodec {
	return value === 'av1' || value === 'h265' || value === 'h264' || value === 'vp9' || value === 'vp8';
}

function buildScreenCaptureOptions(options: VoiceEngineV2ScreenEncodingOptions): ScreenShareCaptureOptions {
	const resolution =
		options.frameRate === undefined
			? {width: options.width, height: options.height}
			: {width: options.width, height: options.height, frameRate: options.frameRate};
	return {resolution};
}

function buildScreenPublishOptions(options: VoiceEngineV2ScreenEncodingOptions): TrackPublishOptions | undefined {
	const publishOptions: TrackPublishOptions = {};
	if (isVideoCodecValue(options.codec)) {
		publishOptions.videoCodec = options.codec;
	}
	if (options.maxBitrateBps !== undefined) {
		const screenShareEncoding: {maxBitrate: number; maxFramerate?: number} = {
			maxBitrate: options.maxBitrateBps,
		};
		if (options.frameRate !== undefined) {
			screenShareEncoding.maxFramerate = options.frameRate;
		}
		publishOptions.screenShareEncoding = screenShareEncoding;
	}
	return Object.keys(publishOptions).length > 0 ? publishOptions : undefined;
}

export class VoiceEngineV2AppLiveKitExecutionAdapter implements LiveKitPort {
	private readonly media: VoiceEngineV2AppLiveKitMediaDelegate;
	private readonly connection: VoiceEngineV2AppLiveKitConnectionDelegate;
	private readonly screenShare: VoiceEngineV2AppLiveKitScreenShareDelegate;
	private readonly getRoom: VoiceEngineV2AppLiveKitRoomAccessor;
	private readonly audioOutputStore: VoiceEngineV2AppLiveKitAudioOutputStore | null;
	private readonly subscriptions: SubscriptionPort | null;
	private readonly stats: StatsPort | null;
	private readonly logger: VoiceEngineV2AppLiveKitLogger;
	private readonly getActiveChannelId: () => string | null;
	private readonly getActiveGuildId: () => string | null;

	constructor(options: VoiceEngineV2AppLiveKitExecutionAdapterOptions) {
		if (!options) {
			throw new Error(`${ADAPTER_NAME}: options is required`);
		}
		if (typeof options.media?.enableMicrophone !== 'function') {
			throw new Error(`${ADAPTER_NAME}: media.enableMicrophone is required`);
		}
		if (typeof options.media?.disableMicrophone !== 'function') {
			throw new Error(`${ADAPTER_NAME}: media.disableMicrophone is required`);
		}
		if (typeof options.media?.setMicrophoneEnabled !== 'function') {
			throw new Error(`${ADAPTER_NAME}: media.setMicrophoneEnabled is required`);
		}
		if (typeof options.media?.setCameraEnabled !== 'function') {
			throw new Error(`${ADAPTER_NAME}: media.setCameraEnabled is required`);
		}
		if (typeof options.screenShare?.publishControllerScreenViaLiveKitFlows !== 'function') {
			throw new Error(`${ADAPTER_NAME}: screenShare.publishControllerScreenViaLiveKitFlows is required`);
		}
		if (typeof options.screenShare?.unpublishControllerScreenViaLiveKitFlows !== 'function') {
			throw new Error(`${ADAPTER_NAME}: screenShare.unpublishControllerScreenViaLiveKitFlows is required`);
		}
		if (typeof options.screenShare?.updateActiveScreenShareSettings !== 'function') {
			throw new Error(`${ADAPTER_NAME}: screenShare.updateActiveScreenShareSettings is required`);
		}
		if (typeof options.screenShare?.setScreenShareAudioMuted !== 'function') {
			throw new Error(`${ADAPTER_NAME}: screenShare.setScreenShareAudioMuted is required`);
		}
		if (typeof options.getRoom !== 'function') {
			throw new Error(`${ADAPTER_NAME}: getRoom callback is required`);
		}
		this.media = options.media;
		this.connection = options.connection;
		this.screenShare = options.screenShare;
		this.getRoom = options.getRoom;
		this.audioOutputStore = options.audioOutputStore ?? null;
		this.subscriptions = options.subscriptions ?? null;
		this.stats = options.stats ?? null;
		this.logger = options.logger ?? new Logger(ADAPTER_NAME);
		this.getActiveChannelId = options.getActiveChannelId ?? (() => null);
		this.getActiveGuildId = options.getActiveGuildId ?? (() => null);
	}

	async prewarm(): Promise<void> {
		this.logger.debug('prewarm requested');
	}

	async connect(options: VoiceEngineV2ConnectOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('connect', 'options is not an object');
		}
		if (typeof options.url !== 'string' || options.url.length === 0) {
			throw buildOperatingError('connect', 'options.url is missing');
		}
		if (typeof options.token !== 'string' || options.token.length === 0) {
			throw buildOperatingError('connect', 'options.token is missing');
		}
		this.logger.info('connect requested', {url: options.url});
		const voiceServerDelegate = this.connection.connectToVoiceServer;
		if (typeof voiceServerDelegate === 'function') {
			try {
				await voiceServerDelegate.call(this.connection, options);
				return;
			} catch (error) {
				this.logger.warn('connect voice-server delegate threw', {error});
				throw error instanceof Error ? error : buildOperatingError('connect', String(error));
			}
		}
		const delegate = this.connection.startConnection;
		if (typeof delegate !== 'function') {
			throw buildOperatingError('connect', 'connection.connectToVoiceServer is not configured');
		}
		const channelId = this.resolveConnectChannelId(options);
		if (channelId === null) {
			throw buildOperatingError('connect', 'channelId is missing');
		}
		const guildId = this.resolveConnectGuildId(options);
		let accepted = false;
		try {
			accepted = delegate.call(this.connection, guildId, channelId);
		} catch (error) {
			this.logger.warn('connect delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('connect', String(error));
		}
		if (!accepted) {
			throw buildOperatingError('connect', 'connection delegate rejected request');
		}
	}

	async disconnect(reason: VoiceEngineV2DisconnectReason): Promise<void> {
		if (typeof reason !== 'string' || reason.length === 0) {
			throw buildOperatingError('disconnect', 'reason is missing');
		}
		this.logger.info('disconnect requested', {reason});
		const delegate = this.connection.disconnectFromVoiceChannel;
		if (typeof delegate !== 'function') return;
		const mapped = reason === 'user' ? 'user' : reason === 'server' ? 'server' : 'error';
		try {
			delegate.call(this.connection, mapped);
		} catch (error) {
			this.logger.warn('disconnect delegate threw', {error});
		}
	}

	private resolveConnectChannelId(options: VoiceEngineV2ConnectOptions): string | null {
		const metadataChannelId = options.metadata?.channelId;
		if (typeof metadataChannelId === 'string' && metadataChannelId.length > 0) return metadataChannelId;
		const activeChannelId = this.getActiveChannelId();
		if (typeof activeChannelId === 'string' && activeChannelId.length > 0) return activeChannelId;
		return null;
	}

	private resolveConnectGuildId(options: VoiceEngineV2ConnectOptions): string | null {
		const metadataGuildId = options.metadata?.guildId;
		if (typeof metadataGuildId === 'string' && metadataGuildId.length > 0) return metadataGuildId;
		const activeGuildId = this.getActiveGuildId();
		if (typeof activeGuildId === 'string' && activeGuildId.length > 0) return activeGuildId;
		return null;
	}

	async publishMicrophone(options: VoiceEngineV2MicrophoneOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('publishMicrophone', 'options is not an object');
		}
		const room = this.getRoom();
		if (room === null) {
			throw buildOperatingError('publishMicrophone', 'room is not connected');
		}
		const channelId = this.getActiveChannelId();
		try {
			await this.media.enableMicrophone(room, channelId, options);
		} catch (error) {
			this.logger.warn('publishMicrophone delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('publishMicrophone', String(error));
		}
	}

	async unpublishMicrophone(): Promise<void> {
		const room = this.getRoom();
		if (room === null) {
			this.logger.warn('unpublishMicrophone skipped: room is not connected');
			return;
		}
		try {
			await this.media.disableMicrophone(room);
		} catch (error) {
			this.logger.warn('unpublishMicrophone delegate threw', {error});
		}
	}

	async setMicrophoneEnabled(enabled: boolean): Promise<void> {
		if (typeof enabled !== 'boolean') {
			throw buildOperatingError('setMicrophoneEnabled', 'enabled is not a boolean');
		}
		const room = this.getRoom();
		if (room === null) {
			throw buildOperatingError('setMicrophoneEnabled', 'room is not connected');
		}
		const channelId = this.getActiveChannelId();
		try {
			await this.media.setMicrophoneEnabled(enabled, room, channelId);
		} catch (error) {
			this.logger.warn('setMicrophoneEnabled delegate threw', {error, enabled});
			throw error instanceof Error ? error : buildOperatingError('setMicrophoneEnabled', String(error));
		}
	}

	async publishCamera(options: VoiceEngineV2CameraOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('publishCamera', 'options is not an object');
		}
		if (typeof options.width === 'number' && typeof options.height === 'number') {
			assertVoiceEngineV2BridgeVideoOptionsInvariants({width: options.width, height: options.height});
		}
		let outcome: VoiceEngineV2AppCameraTransitionOutcome;
		try {
			const cameraOptions =
				options.deviceId || options.sendUpdate === false
					? {deviceId: options.deviceId, sendUpdate: options.sendUpdate}
					: undefined;
			outcome = await this.media.setCameraEnabled(true, cameraOptions);
		} catch (error) {
			this.logger.warn('publishCamera delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('publishCamera', String(error));
		}
		if (outcome === 'denied') {
			this.logger.warn('publishCamera rejected: camera permission denied');
			throw buildVoiceEngineV2AppCameraPermissionDeniedError();
		}
		if (outcome === 'failed') {
			this.logger.warn('publishCamera rejected: camera transition failed');
			throw buildOperatingError('publishCamera', 'camera transition failed without publishing');
		}
	}

	async updateCameraEncoding(options: VoiceEngineV2CameraEncodingOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('updateCameraEncoding', 'options is not an object');
		}
		if (typeof options.width === 'number' && typeof options.height === 'number') {
			assertVoiceEngineV2BridgeVideoOptionsInvariants({width: options.width, height: options.height});
		}
		try {
			await this.media.updateCameraEncoding(options);
		} catch (error) {
			this.logger.warn('updateCameraEncoding delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('updateCameraEncoding', String(error));
		}
	}

	async unpublishCamera(options?: VoiceEngineV2CameraOptions): Promise<void> {
		try {
			await this.media.setCameraEnabled(false, options?.sendUpdate === false ? {sendUpdate: false} : undefined);
		} catch (error) {
			this.logger.warn('unpublishCamera delegate threw', {error});
		}
	}

	async publishScreen(options: VoiceEngineV2ScreenOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('publishScreen', 'options is not an object');
		}
		if (typeof options.captureId !== 'string' || options.captureId.length === 0) {
			throw buildOperatingError('publishScreen', 'options.captureId is missing');
		}
		assertVoiceEngineV2BridgeVideoOptionsInvariants({width: options.width, height: options.height});
		const room = this.getRoom();
		try {
			await this.screenShare.publishControllerScreenViaLiveKitFlows(room, options);
		} catch (error) {
			this.logger.warn('publishScreen delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('publishScreen', String(error));
		}
	}

	async unpublishScreen(): Promise<void> {
		const room = this.getRoom();
		try {
			await this.screenShare.unpublishControllerScreenViaLiveKitFlows(room);
		} catch (error) {
			this.logger.warn('unpublishScreen delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('unpublishScreen', String(error));
		}
	}

	async updateScreenEncoding(options: VoiceEngineV2ScreenEncodingOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('updateScreenEncoding', 'options is not an object');
		}
		if (typeof options.captureId !== 'string' || options.captureId.length === 0) {
			throw buildOperatingError('updateScreenEncoding', 'options.captureId is missing');
		}
		assertVoiceEngineV2BridgeVideoOptionsInvariants({width: options.width, height: options.height});
		const room = this.getRoom();
		try {
			await this.screenShare.updateActiveScreenShareSettings(
				room,
				buildScreenCaptureOptions(options),
				buildScreenPublishOptions(options),
			);
		} catch (error) {
			this.logger.warn('updateScreenEncoding delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('updateScreenEncoding', String(error));
		}
	}

	async publishScreenAudio(options: VoiceEngineV2ScreenAudioOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('publishScreenAudio', 'options is not an object');
		}
		if (typeof options.sampleRate !== 'number' || options.sampleRate <= 0) {
			throw buildOperatingError('publishScreenAudio', 'options.sampleRate is invalid');
		}
		assertVoiceEngineV2BridgeAudioOptionsInvariants({
			sampleRate: options.sampleRate,
			numChannels: options.numChannels,
		});
		const room = this.getRoom();
		try {
			this.screenShare.setScreenShareAudioMuted(room, false);
		} catch (error) {
			this.logger.warn('publishScreenAudio delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('publishScreenAudio', String(error));
		}
	}

	async unpublishScreenAudio(): Promise<void> {
		const room = this.getRoom();
		try {
			this.screenShare.setScreenShareAudioMuted(room, true);
		} catch (error) {
			this.logger.warn('unpublishScreenAudio delegate threw', {error});
		}
	}

	async setOutputDevice(options: VoiceEngineV2OutputDeviceOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('setOutputDevice', 'options is not an object');
		}
		if (typeof options.deviceId !== 'string' || options.deviceId.length === 0) {
			throw buildOperatingError('setOutputDevice', 'options.deviceId is missing');
		}
		const store = this.audioOutputStore;
		if (store === null) {
			this.logger.warn('setOutputDevice rejected: no audioOutputStore configured');
			throw buildOperatingError('setOutputDevice', 'no audioOutputStore configured');
		}
		try {
			await store.setOutputDevice(options.deviceId);
		} catch (error) {
			this.logger.warn('setOutputDevice store threw', {error});
			throw error instanceof Error ? error : buildOperatingError('setOutputDevice', String(error));
		}
	}

	async setParticipantVolume(options: VoiceEngineV2ParticipantVolumeOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('setParticipantVolume', 'options is not an object');
		}
		if (typeof options.participantIdentity !== 'string' || options.participantIdentity.length === 0) {
			throw buildOperatingError('setParticipantVolume', 'options.participantIdentity is missing');
		}
		if (typeof options.volume !== 'number' || !Number.isFinite(options.volume)) {
			throw buildOperatingError('setParticipantVolume', 'options.volume is not a finite number');
		}
		const delegate = this.subscriptions;
		if (delegate === null) {
			throw buildOperatingError('setParticipantVolume', 'no subscriptions adapter configured');
		}
		try {
			await delegate.setParticipantVolume(options);
		} catch (error) {
			this.logger.warn('setParticipantVolume delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('setParticipantVolume', String(error));
		}
	}

	async setRemoteTrackSubscription(options: VoiceEngineV2RemoteTrackSubscriptionOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('setRemoteTrackSubscription', 'options is not an object');
		}
		if (typeof options.participantIdentity !== 'string' || options.participantIdentity.length === 0) {
			throw buildOperatingError('setRemoteTrackSubscription', 'options.participantIdentity is missing');
		}
		if (typeof options.source !== 'string' || options.source.length === 0) {
			throw buildOperatingError('setRemoteTrackSubscription', 'options.source is missing');
		}
		if (typeof options.subscribed !== 'boolean') {
			throw buildOperatingError('setRemoteTrackSubscription', 'options.subscribed is not a boolean');
		}
		const delegate = this.subscriptions;
		if (delegate === null) {
			throw buildOperatingError('setRemoteTrackSubscription', 'no subscriptions adapter configured');
		}
		try {
			await delegate.setRemoteTrackSubscription(options);
		} catch (error) {
			this.logger.warn('setRemoteTrackSubscription delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('setRemoteTrackSubscription', String(error));
		}
	}

	async collectStats(): Promise<VoiceEngineV2Stats> {
		const delegate = this.stats;
		if (delegate === null) {
			throw buildOperatingError('collectStats', 'no stats adapter configured');
		}
		let snapshot: VoiceEngineV2Stats;
		try {
			snapshot = await delegate.collectStats();
		} catch (error) {
			this.logger.warn('collectStats delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('collectStats', String(error));
		}
		if (typeof snapshot !== 'object' || snapshot === null) {
			throw buildOperatingError('collectStats', 'delegate returned non-object snapshot');
		}
		if (!Array.isArray(snapshot.outbound)) {
			throw buildOperatingError('collectStats', 'snapshot.outbound is not an array');
		}
		if (!Array.isArray(snapshot.inbound)) {
			throw buildOperatingError('collectStats', 'snapshot.inbound is not an array');
		}
		return snapshot;
	}

	async publishData(options: VoiceEngineV2DataOptions): Promise<void> {
		if (typeof options !== 'object' || options === null) {
			throw buildOperatingError('publishData', 'options is not an object');
		}
		if (!isDataPayload(options.payload)) {
			throw buildOperatingError('publishData', 'options.payload is not a buffer');
		}
		const room = this.getRoom();
		if (room === null) {
			throw buildOperatingError('publishData', 'room is not connected');
		}
		const participant = room.localParticipant;
		if (participant === null || participant === undefined) {
			throw buildOperatingError('publishData', 'room.localParticipant is null');
		}
		const data = toUint8Array(options.payload);
		const publishOpts: {
			reliable?: boolean;
			topic?: string;
			destinationIdentities?: Array<string>;
		} = {};
		if (typeof options.reliable === 'boolean') publishOpts.reliable = options.reliable;
		if (typeof options.topic === 'string') publishOpts.topic = options.topic;
		if (Array.isArray(options.destinationIdentities)) {
			publishOpts.destinationIdentities = options.destinationIdentities;
		}
		try {
			await participant.publishData(data, publishOpts);
		} catch (error) {
			this.logger.warn('publishData delegate threw', {error});
			throw error instanceof Error ? error : buildOperatingError('publishData', String(error));
		}
	}
}
