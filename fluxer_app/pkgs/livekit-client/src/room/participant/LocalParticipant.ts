// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {
	AddTrackRequest,
	AudioTrackFeature,
	type BackupCodecPolicy,
	ChatMessage as ChatMessageModel,
	type Codec,
	DataPacket,
	DataPacket_Kind,
	Encryption_Type,
	type JoinResponse,
	type PacketTrailerFeature,
	type ParticipantInfo,
	protoInt64,
	type RequestResponse,
	RequestResponse_Reason,
	SimulcastCodec,
	SipDTMF,
	type SubscribedQualityUpdate,
	type TrackInfo,
	type TrackUnpublishedResponse,
	UserPacket,
	VideoLayer_Mode,
} from '@livekit/protocol';
import {SignalConnectionState} from '../../api/SignalClient.ts';
import {
	getFrameMetadataFeatures,
	getFrameMetadataPublishOptions,
	hasFrameMetadataPublishOptions,
	isFrameMetadataSupported,
} from '../../frameMetadata/utils.ts';
import type {InternalRoomOptions} from '../../options.ts';
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import TypedPromise from '../../utils/TypedPromise.ts';
import type OutgoingDataStreamManager from '../data-stream/outgoing/OutgoingDataStreamManager.ts';
import type {TextStreamWriter} from '../data-stream/outgoing/StreamWriter.ts';
import LocalDataTrack from '../data-track/LocalDataTrack.ts';
import {DataTrackPublishError} from '../data-track/outgoing/errors.ts';
import type OutgoingDataTrackManager from '../data-track/outgoing/OutgoingDataTrackManager.ts';
import type {DataTrackOptions} from '../data-track/outgoing/types.ts';
import {defaultVideoCodec} from '../defaults.ts';
import {
	DeviceUnsupportedError,
	type LivekitError,
	NegotiationError,
	PublishTrackError,
	SignalRequestError,
	TrackInvalidError,
	UnexpectedConnectionState,
} from '../errors.ts';
import {EngineEvent, ParticipantEvent, TrackEvent} from '../events.ts';
import {PCTransportState} from '../PCTransportManager.ts';
import type RTCEngine from '../RTCEngine.ts';
import {DataChannelKind} from '../RTCEngine.ts';
import type {PerformRpcParams, RpcClientManager, RpcError, RpcInvocationData, RpcServerManager} from '../rpc/index.ts';
import CriticalTimers, {type TimerHandle} from '../timers.ts';
import {createLocalTracks} from '../track/create.ts';
import LocalAudioTrack from '../track/LocalAudioTrack.ts';
import type LocalTrack from '../track/LocalTrack.ts';
import LocalTrackPublication from '../track/LocalTrackPublication.ts';
import LocalVideoTrack, {videoLayersFromEncodings} from '../track/LocalVideoTrack.ts';
import type {
	AudioCaptureOptions,
	BackupVideoCodec,
	CreateLocalTracksOptions,
	ScreenShareCaptureOptions,
	TrackPublishOptions,
	VideoCaptureOptions,
} from '../track/options.ts';
import {isBackupCodec, ScreenSharePresets, VideoPresets} from '../track/options.ts';
import {Track} from '../track/Track.ts';
import {
	getLogContextFromTrack,
	getTrackSourceFromProto,
	mergeDefaultOptions,
	mimeTypeToVideoCodecString,
	screenCaptureToDisplayMediaStreamOptions,
	sourceToKind,
} from '../track/utils.ts';
import type {
	ByteStreamInfo,
	ChatMessage,
	DataPublishOptions,
	SendBytesOptions,
	SendFileOptions,
	SendTextOptions,
	StreamBytesOptions,
	StreamTextOptions,
	TextStreamInfo,
} from '../types.ts';
import {
	Future,
	isAudioTrack,
	isE2EESimulcastSupported,
	isFireFox,
	isLocalAudioTrack,
	isLocalTrack,
	isLocalVideoTrack,
	isSafari17Based,
	isSVCCodec,
	isSVCSimulcast,
	isSVCSimulcastSupportedByServer,
	isVideoCodec,
	isVideoTrack,
	isWeb,
	selectPreferredVideoCodec,
	sleep,
	stopTransceiversForSender,
	supportsVideoCodec,
	usesLegacySVCEncodings,
} from '../utils.ts';
import Participant from './Participant.ts';
import type {ParticipantTrackPermission} from './ParticipantTrackPermission.ts';
import {trackPermissionToProto} from './ParticipantTrackPermission.ts';
import {
	computeStartTargetBitrate,
	computeTrackBackupEncodings,
	computeVideoEncodings,
	getDefaultDegradationPreference,
} from './publishUtils.ts';
import type RemoteParticipant from './RemoteParticipant.ts';

export default class LocalParticipant extends Participant {
	override audioTrackPublications: Map<string, LocalTrackPublication>;

	override videoTrackPublications: Map<string, LocalTrackPublication>;

	override trackPublications: Map<string, LocalTrackPublication>;

	engine: RTCEngine;

	activeDeviceMap: Map<MediaDeviceKind, string>;

	private pendingPublishing = new Set<Track.Source>();

	private pendingPublishPromises = new Map<LocalTrack, Promise<LocalTrackPublication>>();

	private republishPromise: Promise<void> | undefined;

	private cameraError: Error | undefined;

	private microphoneError: Error | undefined;

	private participantTrackPermissions: Array<ParticipantTrackPermission> = [];

	private allParticipantsAllowedToSubscribe: boolean = true;

	private roomOptions: InternalRoomOptions;

	private encryptionType: Encryption_Type = Encryption_Type.NONE;

	private e2eeStateMutex = new Mutex();

	private reconnectFuture?: Future<void, Error>;

	private signalConnectedFuture?: Future<void, Error>;

	private activeAgentFuture?: Future<RemoteParticipant, Error>;

	private firstActiveAgent?: RemoteParticipant;

	private roomOutgoingDataStreamManager: OutgoingDataStreamManager;

	private roomOutgoingDataTrackManager: OutgoingDataTrackManager;

	private rpcClientManager: RpcClientManager;

	private rpcServerManager: RpcServerManager;

	private pendingSignalRequests: Map<
		number,
		{
			resolve: (arg: any) => void;
			reject: (reason: LivekitError) => void;
			values: Partial<Record<keyof LocalParticipant, any>>;
		}
	>;

	private enabledPublishVideoCodecs: Array<Codec> = [];

	constructor(
		sid: string,
		identity: string,
		engine: RTCEngine,
		options: InternalRoomOptions,
		roomOutgoingDataStreamManager: OutgoingDataStreamManager,
		roomOutgoingDataTrackManager: OutgoingDataTrackManager,
		rpcClientManager: RpcClientManager,
		rpcServerManager: RpcServerManager,
	) {
		super(sid, identity, undefined, undefined, undefined, {
			loggerName: options.loggerName,
			loggerContextCb: () => this.engine.logContext,
		});
		this.audioTrackPublications = new Map();
		this.videoTrackPublications = new Map();
		this.trackPublications = new Map();
		this.engine = engine;
		this.roomOptions = options;
		this.setupEngine(engine);
		this.activeDeviceMap = new Map([
			['audioinput', 'default'],
			['videoinput', 'default'],
			['audiooutput', 'default'],
		]);
		this.pendingSignalRequests = new Map();
		this.roomOutgoingDataStreamManager = roomOutgoingDataStreamManager;
		this.roomOutgoingDataTrackManager = roomOutgoingDataTrackManager;
		this.rpcClientManager = rpcClientManager;
		this.rpcServerManager = rpcServerManager;
	}

	get lastCameraError(): Error | undefined {
		return this.cameraError;
	}

	get lastMicrophoneError(): Error | undefined {
		return this.microphoneError;
	}

	get isE2EEEnabled(): boolean {
		return this.encryptionType !== Encryption_Type.NONE;
	}

	override getTrackPublication(source: Track.Source): LocalTrackPublication | undefined {
		const track = super.getTrackPublication(source);
		if (track) {
			return track as LocalTrackPublication;
		}
		return undefined;
	}

	override getTrackPublicationByName(name: string): LocalTrackPublication | undefined {
		const track = super.getTrackPublicationByName(name);
		if (track) {
			return track as LocalTrackPublication;
		}
		return undefined;
	}

	setupEngine(engine: RTCEngine) {
		this.engine = engine;
		this.engine.on(EngineEvent.RemoteMute, (trackSid: string, muted: boolean) => {
			const pub = this.trackPublications.get(trackSid);
			if (!pub?.track) {
				return;
			}
			if (muted) {
				pub.mute();
			} else {
				pub.unmute();
			}
		});

		if (this.signalConnectedFuture?.isResolved) {
			this.signalConnectedFuture = undefined;
		}

		this.engine
			.on(EngineEvent.Connected, this.handleReconnected)
			.on(EngineEvent.SignalConnected, this.handleSignalConnected)
			.on(EngineEvent.SignalRestarted, this.handleReconnected)
			.on(EngineEvent.SignalResumed, this.handleReconnected)
			.on(EngineEvent.Restarting, this.handleReconnecting)
			.on(EngineEvent.Resuming, this.handleReconnecting)
			.on(EngineEvent.LocalTrackUnpublished, this.handleLocalTrackUnpublished)
			.on(EngineEvent.SubscribedQualityUpdate, this.handleSubscribedQualityUpdate)
			.on(EngineEvent.Closing, this.handleClosing)
			.on(EngineEvent.SignalRequestResponse, this.handleSignalRequestResponse);
	}

	private handleReconnecting = () => {
		if (!this.reconnectFuture) {
			this.reconnectFuture = new Future<void, Error>();
		}
	};

	private handleReconnected = () => {
		this.reconnectFuture?.resolve?.();
		this.reconnectFuture = undefined;
		this.updateTrackSubscriptionPermissions();
	};

	private handleClosing = () => {
		if (this.reconnectFuture) {
			this.reconnectFuture.promise.catch((e) => this.log.warn(e.message));
			this.reconnectFuture?.reject?.(new Error('Got disconnected during reconnection attempt'));
			this.reconnectFuture = undefined;
		}
		if (this.signalConnectedFuture) {
			this.signalConnectedFuture.reject?.(new Error('Got disconnected without signal connected'));
			this.signalConnectedFuture = undefined;
		}

		this.activeAgentFuture?.reject?.(new Error('Got disconnected without active agent present'));
		this.activeAgentFuture = undefined;
		this.firstActiveAgent = undefined;
	};

	private handleSignalConnected = (joinResponse: JoinResponse) => {
		if (joinResponse.participant) {
			this.updateInfo(joinResponse.participant);
		}
		if (!this.signalConnectedFuture) {
			this.signalConnectedFuture = new Future<void, Error>();
		}

		this.signalConnectedFuture.resolve?.();
	};

	private handleSignalRequestResponse = (response: RequestResponse) => {
		const {requestId, reason, message} = response;
		const targetRequest = this.pendingSignalRequests.get(requestId);
		if (targetRequest) {
			if (reason !== RequestResponse_Reason.OK) {
				targetRequest.reject(new SignalRequestError(message, reason));
			}
			this.pendingSignalRequests.delete(requestId);
		}

		switch (response.request.case) {
			case 'publishDataTrack': {
				let error: ReturnType<
					(typeof DataTrackPublishError)['notAllowed' | 'duplicateName' | 'invalidName' | 'limitReached' | 'unknown']
				>;
				switch (response.reason) {
					case RequestResponse_Reason.NOT_ALLOWED:
						error = DataTrackPublishError.notAllowed(response.message);
						break;
					case RequestResponse_Reason.DUPLICATE_NAME:
						error = DataTrackPublishError.duplicateName(response.message);
						break;
					case RequestResponse_Reason.INVALID_NAME:
						error = DataTrackPublishError.invalidName(response.message);
						break;
					case RequestResponse_Reason.LIMIT_EXCEEDED:
						error = DataTrackPublishError.limitReached(response.message);
						break;
					default:
						error = DataTrackPublishError.unknown(response.reason, response.message);
						break;
				}

				this.roomOutgoingDataTrackManager.receivedSfuPublishResponse(response.request.value.pubHandle, {
					type: 'error',
					error,
				});
				break;
			}
		}
	};

	async setMetadata(metadata: string): Promise<void> {
		await this.requestMetadataUpdate({metadata});
	}

	async setName(name: string): Promise<void> {
		await this.requestMetadataUpdate({name});
	}

	async setAttributes(attributes: Record<string, string>) {
		await this.requestMetadataUpdate({attributes});
	}

	private async requestMetadataUpdate({
		metadata,
		name,
		attributes,
	}: {
		metadata?: string;
		name?: string;
		attributes?: Record<string, string>;
	}) {
		return new TypedPromise<void, Error>(async (resolve, reject) => {
			try {
				let isRejected = false;
				const requestId = await this.engine.client.sendUpdateLocalMetadata(
					metadata ?? this.metadata ?? '',
					name ?? this.name ?? '',
					attributes,
				);
				const startTime = performance.now();
				this.pendingSignalRequests.set(requestId, {
					resolve,
					reject: (error: LivekitError) => {
						reject(error);
						isRejected = true;
					},
					values: {name, metadata, attributes},
				});
				while (performance.now() - startTime < 5_000 && !isRejected) {
					if (
						(!name || this.name === name) &&
						(!metadata || this.metadata === metadata) &&
						(!attributes ||
							Object.entries(attributes).every(
								([key, value]) => this.attributes[key] === value || (value === '' && !this.attributes[key]),
							))
					) {
						this.pendingSignalRequests.delete(requestId);
						resolve();
						return;
					}
					await sleep(50);
				}
				reject(new SignalRequestError('Request to update local metadata timed out', 'TimeoutError'));
			} catch (e: unknown) {
				if (e instanceof Error) {
					reject(e);
				} else {
					reject(new Error(String(e)));
				}
			}
		});
	}

	setCameraEnabled(
		enabled: boolean,
		options?: VideoCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<LocalTrackPublication | undefined> {
		return this.setTrackEnabled(Track.Source.Camera, enabled, options, publishOptions);
	}

	setMicrophoneEnabled(
		enabled: boolean,
		options?: AudioCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<LocalTrackPublication | undefined> {
		return this.setTrackEnabled(Track.Source.Microphone, enabled, options, publishOptions);
	}

	setScreenShareEnabled(
		enabled: boolean,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
		audioPublishOptions?: TrackPublishOptions,
	): Promise<LocalTrackPublication | undefined> {
		return this.setTrackEnabled(Track.Source.ScreenShare, enabled, options, publishOptions, audioPublishOptions);
	}

	async setE2EEEnabled(enabled: boolean) {
		const unlock = await this.e2eeStateMutex.lock();
		try {
			this.encryptionType = enabled ? Encryption_Type.GCM : Encryption_Type.NONE;
			await Promise.all(this.pendingPublishPromises.values());
			if (
				this.trackPublications.size === 0 ||
				Array.from(this.trackPublications.values()).every((pub) => pub.isEncrypted === enabled)
			) {
				return;
			}
			await this.republishAllTracks(undefined, false);
		} finally {
			unlock();
		}
	}

	private async setTrackEnabled(
		source: Extract<Track.Source, Track.Source.Camera>,
		enabled: boolean,
		options?: VideoCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<LocalTrackPublication | undefined>;
	private async setTrackEnabled(
		source: Extract<Track.Source, Track.Source.Microphone>,
		enabled: boolean,
		options?: AudioCaptureOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<LocalTrackPublication | undefined>;
	private async setTrackEnabled(
		source: Extract<Track.Source, Track.Source.ScreenShare>,
		enabled: boolean,
		options?: ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
		audioPublishOptions?: TrackPublishOptions,
	): Promise<LocalTrackPublication | undefined>;
	private async setTrackEnabled(
		source: Track.Source,
		enabled: true,
		options?: VideoCaptureOptions | AudioCaptureOptions | ScreenShareCaptureOptions,
		publishOptions?: TrackPublishOptions,
		audioPublishOptions?: TrackPublishOptions,
	) {
		this.log.debug('setTrackEnabled', {source, enabled});
		if (this.republishPromise) {
			await this.republishPromise;
		}
		let track = this.getTrackPublication(source);
		if (enabled) {
			if (track) {
				await track.unmute();
			} else {
				let localTracks: Array<LocalTrack> | undefined;
				if (this.pendingPublishing.has(source)) {
					const pendingTrack = await this.waitForPendingPublicationOfSource(source);
					if (!pendingTrack) {
						this.log.info('waiting for pending publication promise timed out', {source});
					}
					await pendingTrack?.unmute();
					return pendingTrack;
				}
				this.pendingPublishing.add(source);
				try {
					switch (source) {
						case Track.Source.Camera:
							localTracks = await this.createTracks({
								video: (options as VideoCaptureOptions | undefined) ?? true,
							});

							break;
						case Track.Source.Microphone:
							localTracks = await this.createTracks({
								audio: (options as AudioCaptureOptions | undefined) ?? true,
							});
							break;
						case Track.Source.ScreenShare:
							localTracks = await this.createScreenTracks({
								...(options as ScreenShareCaptureOptions | undefined),
							});
							break;
						default:
							throw new TrackInvalidError(source);
					}
				} catch (e: unknown) {
					localTracks?.forEach((tr) => {
						tr.stop();
					});
					if (e instanceof Error) {
						this.emit(ParticipantEvent.MediaDevicesError, e, sourceToKind(source));
					}
					this.pendingPublishing.delete(source);
					throw e;
				}

				for (const localTrack of localTracks) {
					const opts: TrackPublishOptions = {
						...this.roomOptions.publishDefaults,
						...options,
					};
					if (source === Track.Source.Microphone && isAudioTrack(localTrack) && opts.preConnectBuffer) {
						this.log.info('starting preconnect buffer for microphone');
						localTrack.startPreConnectBuffer();
					}
				}

				try {
					const publishPromises: Array<Promise<LocalTrackPublication>> = [];
					for (const localTrack of localTracks) {
						this.log.info('publishing track', getLogContextFromTrack(localTrack));

						publishPromises.push(
							this.publishTrack(
								localTrack,
								audioPublishOptions && isAudioTrack(localTrack) ? audioPublishOptions : publishOptions,
							),
						);
					}
					const publishedTracks = await Promise.all(publishPromises);

					[track] = publishedTracks;
				} catch (e) {
					localTracks?.forEach((tr) => {
						tr.stop();
					});
					throw e;
				} finally {
					this.pendingPublishing.delete(source);
				}
			}
		} else {
			if (!track?.track && this.pendingPublishing.has(source)) {
				track = await this.waitForPendingPublicationOfSource(source);
				if (!track) {
					this.log.info('waiting for pending publication promise timed out', {source});
				}
			}
			if (track?.track) {
				if (source === Track.Source.ScreenShare) {
					const unpublishPromises = [this.unpublishTrack(track.track)];
					const screenAudioTrack = this.getTrackPublication(Track.Source.ScreenShareAudio);
					if (screenAudioTrack?.track) {
						unpublishPromises.push(this.unpublishTrack(screenAudioTrack.track));
					}
					[track] = await Promise.all(unpublishPromises);
				} else {
					await track.mute();
				}
			}
		}
		return track;
	}

	async enableCameraAndMicrophone() {
		if (this.pendingPublishing.has(Track.Source.Camera) || this.pendingPublishing.has(Track.Source.Microphone)) {
			return;
		}

		this.pendingPublishing.add(Track.Source.Camera);
		this.pendingPublishing.add(Track.Source.Microphone);
		try {
			const tracks: Array<LocalTrack> = await this.createTracks({
				audio: true,
				video: true,
			});

			await Promise.all(tracks.map((track) => this.publishTrack(track)));
		} finally {
			this.pendingPublishing.delete(Track.Source.Camera);
			this.pendingPublishing.delete(Track.Source.Microphone);
		}
	}

	async createTracks(options?: CreateLocalTracksOptions): Promise<Array<LocalTrack>> {
		options ??= {};

		const mergedOptionsWithProcessors = mergeDefaultOptions(
			options,
			this.roomOptions?.audioCaptureDefaults,
			this.roomOptions?.videoCaptureDefaults,
		);

		try {
			const tracks = await createLocalTracks(mergedOptionsWithProcessors, {
				loggerName: this.roomOptions.loggerName,
				loggerContextCb: () => this.logContext,
			});
			const localTracks = tracks.map((track) => {
				if (isAudioTrack(track)) {
					this.microphoneError = undefined;
					track.setAudioContext(this.audioContext);
					track.source = Track.Source.Microphone;
					this.emit(ParticipantEvent.AudioStreamAcquired);
				}
				if (isVideoTrack(track)) {
					this.cameraError = undefined;
					track.source = Track.Source.Camera;
				}
				return track;
			});
			return localTracks;
		} catch (err) {
			if (err instanceof Error) {
				if (options.audio) {
					this.microphoneError = err;
				}
				if (options.video) {
					this.cameraError = err;
				}
			}

			throw err;
		}
	}

	async createScreenTracks(options?: ScreenShareCaptureOptions): Promise<Array<LocalTrack>> {
		if (options === undefined) {
			options = {};
		}

		if (navigator.mediaDevices.getDisplayMedia === undefined) {
			throw new DeviceUnsupportedError('getDisplayMedia not supported');
		}

		if (options.resolution === undefined && !isSafari17Based()) {
			options.resolution = ScreenSharePresets.h1080fps30.resolution;
		}

		const constraints = screenCaptureToDisplayMediaStreamOptions(options);
		const stream: MediaStream = await navigator.mediaDevices.getDisplayMedia(constraints);

		const tracks = stream.getVideoTracks();
		if (tracks.length === 0) {
			throw new TrackInvalidError('no video track found');
		}
		const screenVideo = new LocalVideoTrack(tracks[0], undefined, false, {
			loggerName: this.roomOptions.loggerName,
			loggerContextCb: () => this.logContext,
		});
		screenVideo.source = Track.Source.ScreenShare;
		if (options.contentHint) {
			screenVideo.mediaStreamTrack.contentHint = options.contentHint;
		}

		const localTracks: Array<LocalTrack> = [screenVideo];
		if (stream.getAudioTracks().length > 0) {
			this.emit(ParticipantEvent.AudioStreamAcquired);
			const screenAudio = new LocalAudioTrack(stream.getAudioTracks()[0], undefined, false, this.audioContext, {
				loggerName: this.roomOptions.loggerName,
				loggerContextCb: () => this.logContext,
			});
			screenAudio.source = Track.Source.ScreenShareAudio;
			localTracks.push(screenAudio);
		}
		return localTracks;
	}

	async publishTrack(track: LocalTrack | MediaStreamTrack, options?: TrackPublishOptions) {
		return this.publishOrRepublishTrack(track, options);
	}

	private waitForNextEngineRestart(timeoutMs = 15_000): Promise<void> {
		return new Promise<void>((resolve, reject) => {
			const cleanup = () => {
				clearTimeout(timeout);
				this.engine.off(EngineEvent.Restarted, onRestarted);
				this.engine.off(EngineEvent.Closing, onClosing);
			};
			const onRestarted = () => {
				cleanup();
				resolve();
			};
			const onClosing = () => {
				cleanup();
				reject(new Error('engine closed before restart completed'));
			};
			const timeout = setTimeout(() => {
				cleanup();
				reject(new Error('timed out waiting for engine restart'));
			}, timeoutMs);
			this.engine.once(EngineEvent.Restarted, onRestarted);
			this.engine.once(EngineEvent.Closing, onClosing);
		});
	}

	private async publishOrRepublishTrack(
		track: LocalTrack | MediaStreamTrack,
		options?: TrackPublishOptions,
		isRepublish = false,
		hasRetriedAfterNegotiationError = false,
	): Promise<LocalTrackPublication> {
		if (isLocalAudioTrack(track)) {
			track.setAudioContext(this.audioContext);
		}

		await this.reconnectFuture?.promise;
		if (this.republishPromise && !isRepublish) {
			await this.republishPromise;
		}
		if (isLocalTrack(track) && this.pendingPublishPromises.has(track)) {
			await this.pendingPublishPromises.get(track);
		}
		let defaultConstraints: MediaTrackConstraints | undefined;
		if (track instanceof MediaStreamTrack) {
			defaultConstraints = track.getConstraints();
		} else {
			defaultConstraints = track.constraints;
			let deviceKind: MediaDeviceKind | undefined;
			switch (track.source) {
				case Track.Source.Microphone:
					deviceKind = 'audioinput';
					break;
				case Track.Source.Camera:
					deviceKind = 'videoinput';
					break;
				default:
					break;
			}
			if (deviceKind && this.activeDeviceMap.has(deviceKind)) {
				defaultConstraints = {
					...defaultConstraints,
					deviceId: this.activeDeviceMap.get(deviceKind),
				};
			}
		}
		if (track instanceof MediaStreamTrack) {
			switch (track.kind) {
				case 'audio':
					track = new LocalAudioTrack(track, defaultConstraints, true, this.audioContext, {
						loggerName: this.roomOptions.loggerName,
						loggerContextCb: () => this.logContext,
					});
					break;
				case 'video':
					track = new LocalVideoTrack(track, defaultConstraints, true, {
						loggerName: this.roomOptions.loggerName,
						loggerContextCb: () => this.logContext,
					});
					break;
				default:
					throw new TrackInvalidError(`unsupported MediaStreamTrack kind ${track.kind}`);
			}
		} else {
			track.updateLoggerOptions({
				loggerName: this.roomOptions.loggerName,
				loggerContextCb: () => this.logContext,
			});
		}

		let existingPublication: LocalTrackPublication | undefined;
		this.trackPublications.forEach((publication) => {
			if (!publication.track) {
				return;
			}
			if (publication.track === track) {
				existingPublication = <LocalTrackPublication>publication;
			}
		});

		if (existingPublication) {
			this.log.warn('track has already been published, skipping', getLogContextFromTrack(existingPublication));
			return existingPublication;
		}

		const opts: TrackPublishOptions = {
			...this.roomOptions.publishDefaults,
			...options,
		};
		const isStereoInput =
			('channelCount' in track.mediaStreamTrack.getSettings() &&
				track.mediaStreamTrack.getSettings().channelCount === 2) ||
			track.mediaStreamTrack.getConstraints().channelCount === 2;
		const isStereo = opts.forceStereo ?? isStereoInput;

		if (isStereo) {
			if (opts.dtx === undefined) {
				this.log.debug(
					`Opus DTX will be disabled for stereo tracks by default. Enable them explicitly to make it work.`,
					getLogContextFromTrack(track),
				);
			}
			if (opts.red === undefined) {
				this.log.debug(
					`Opus RED will be disabled for stereo tracks by default. Enable them explicitly to make it work.`,
				);
			}
			opts.dtx ??= false;
			opts.red ??= false;
		}

		if (!isE2EESimulcastSupported() && this.roomOptions.e2ee) {
			this.log.info(
				`End-to-end encryption is set up, simulcast publishing will be disabled on Safari versions and iOS browsers running iOS < v17.2`,
			);
			opts.simulcast = false;
		}

		if (opts.source) {
			track.source = opts.source;
		}
		const publishPromise = (async (): Promise<LocalTrackPublication> => {
			if (this.engine.client.currentState !== SignalConnectionState.CONNECTED) {
				this.log.debug('deferring track publication until signal is connected', {
					track: getLogContextFromTrack(track),
				});

				let timeout: TimerHandle | undefined;
				try {
					await Promise.race([
						this.waitUntilEngineConnected(),
						new Promise<never>((_, reject) => {
							timeout = CriticalTimers.setTimeout(() => {
								track.stop();
								reject(new PublishTrackError('publishing rejected as engine not connected within timeout', 408));
							}, 15_000);
						}),
					]);
				} finally {
					if (timeout !== undefined) {
						CriticalTimers.clearTimeout(timeout);
					}
				}
			}
			return this.publish(track, opts, isStereo);
		})();
		this.pendingPublishPromises.set(track, publishPromise);
		try {
			const publication = await publishPromise;
			return publication;
		} catch (e) {
			if (!hasRetriedAfterNegotiationError && e instanceof NegotiationError) {
				this.log.warn('negotiation due to track publish failed, retrying after reconnect', {
					error: e,
				});
				this.pendingPublishPromises.delete(track);
				await this.waitForNextEngineRestart();
				return await this.publishOrRepublishTrack(track, options, isRepublish, true);
			}
			throw e;
		} finally {
			this.pendingPublishPromises.delete(track);
		}
	}

	private waitUntilEngineConnected() {
		if (!this.signalConnectedFuture) {
			this.signalConnectedFuture = new Future<void, Error>();
		}
		return this.signalConnectedFuture.promise;
	}

	private hasPermissionsToPublish(track: LocalTrack): boolean {
		if (!this.permissions) {
			this.log.warn('no permissions present for publishing track', getLogContextFromTrack(track));
			return false;
		}
		const {canPublish, canPublishSources} = this.permissions;
		if (
			canPublish &&
			(canPublishSources.length === 0 ||
				canPublishSources.map((source) => getTrackSourceFromProto(source)).includes(track.source))
		) {
			return true;
		}
		this.log.warn('insufficient permissions to publish', getLogContextFromTrack(track));
		return false;
	}

	private async publish(track: LocalTrack, opts: TrackPublishOptions, isStereo: boolean) {
		if (!this.hasPermissionsToPublish(track)) {
			throw new PublishTrackError('failed to publish track, insufficient permissions', 403);
		}
		const existingTrackOfSource = Array.from(this.trackPublications.values()).find(
			(publishedTrack) => isLocalTrack(track) && publishedTrack.source === track.source,
		);
		if (existingTrackOfSource && track.source !== Track.Source.Unknown) {
			this.log.info(`publishing a second track with the same source: ${track.source}`, getLogContextFromTrack(track));
		}
		if (opts.stopMicTrackOnMute && isAudioTrack(track)) {
			track.stopOnMute = true;
		}

		if (track.source === Track.Source.ScreenShare && isFireFox()) {
			opts.simulcast = false;
		}

		const requestedVideoCodec = opts.videoCodec ?? defaultVideoCodec;
		opts.videoCodec = supportsVideoCodec(requestedVideoCodec) ? requestedVideoCodec : selectPreferredVideoCodec();
		if (this.enabledPublishVideoCodecs.length > 0) {
			const enabledVideoCodecs = this.enabledPublishVideoCodecs
				.map((codec) => mimeTypeToVideoCodecString(codec.mime))
				.filter(isVideoCodec);
			if (!enabledVideoCodecs.some((codec) => opts.videoCodec === codec)) {
				opts.videoCodec = selectPreferredVideoCodec(enabledVideoCodecs);
			}
		}

		const videoCodec = opts.videoCodec;

		track.on(TrackEvent.Muted, this.onTrackMuted);
		track.on(TrackEvent.Unmuted, this.onTrackUnmuted);
		track.on(TrackEvent.Ended, this.handleTrackEnded);
		track.on(TrackEvent.UpstreamPaused, this.onTrackUpstreamPaused);
		track.on(TrackEvent.UpstreamResumed, this.onTrackUpstreamResumed);
		track.on(TrackEvent.AudioTrackFeatureUpdate, this.onTrackFeatureUpdate);

		const audioFeatures: Array<AudioTrackFeature> = [];
		const disableDtx = !(opts.dtx ?? true);

		const settings = track.getSourceTrackSettings();

		if (settings.autoGainControl) {
			audioFeatures.push(AudioTrackFeature.TF_AUTO_GAIN_CONTROL);
		}
		if (settings.echoCancellation) {
			audioFeatures.push(AudioTrackFeature.TF_ECHO_CANCELLATION);
		}
		if (settings.noiseSuppression) {
			audioFeatures.push(AudioTrackFeature.TF_NOISE_SUPPRESSION);
		}
		if (settings.channelCount && settings.channelCount > 1) {
			audioFeatures.push(AudioTrackFeature.TF_STEREO);
		}
		if (disableDtx) {
			audioFeatures.push(AudioTrackFeature.TF_NO_DTX);
		}
		if (isLocalAudioTrack(track) && track.hasPreConnectBuffer) {
			audioFeatures.push(AudioTrackFeature.TF_PRECONNECT_BUFFER);
		}
		const packetTrailerFeatures: Array<PacketTrailerFeature> = this.normalizeRequestedFrameMetadataOptions(track, opts);

		const req = new AddTrackRequest({
			cid: track.mediaStreamTrack.id,
			name: opts.name,
			type: Track.kindToProto(track.kind),
			muted: track.isMuted,
			source: Track.sourceToProto(track.source),
			disableDtx,
			encryption: this.encryptionType,
			stereo: isStereo,
			disableRed: this.isE2EEEnabled || !(opts.red ?? true),
			stream: opts?.stream,
			backupCodecPolicy: opts?.backupCodecPolicy as BackupCodecPolicy,
			audioFeatures,
			packetTrailerFeatures,
		});

		let encodings: Array<RTCRtpEncodingParameters> | undefined;
		if (track.kind === Track.Kind.Video) {
			let dims: Track.Dimensions;
			try {
				dims = await track.waitForDimensions();
			} catch (_e) {
				const defaultRes = this.roomOptions.videoCaptureDefaults?.resolution ?? VideoPresets.h720.resolution;
				dims = {
					width: defaultRes.width,
					height: defaultRes.height,
				};
				this.log.error('could not determine track dimensions, using defaults', {
					...getLogContextFromTrack(track),
					dims,
				});
			}
			req.width = dims.width;
			req.height = dims.height;
			if (isLocalVideoTrack(track)) {
				if (
					isSVCSimulcast(videoCodec, opts) &&
					(usesLegacySVCEncodings() || !isSVCSimulcastSupportedByServer(this.engine?.serverVersion))
				) {
					opts.simulcast = false;
					this.log.info('SVC simulcast is not supported, disabling simulcast.', getLogContextFromTrack(track));
				}

				const svcSimulcast = isSVCSimulcast(videoCodec, opts);
				if (isSVCCodec(videoCodec) && !svcSimulcast && track.source !== Track.Source.ScreenShare) {
					opts.scalabilityMode = opts.scalabilityMode ?? 'L3T3_KEY';
				}

				const primaryCodec = new SimulcastCodec({
					codec: videoCodec,
					cid: track.mediaStreamTrack.id,
				});
				if (svcSimulcast) {
					primaryCodec.videoLayerMode = VideoLayer_Mode.ONE_SPATIAL_LAYER_PER_STREAM;
				}
				req.simulcastCodecs = [primaryCodec];

				if (opts.backupCodec === true) {
					opts.backupCodec = {codec: 'h264'};
				}
				const backupCodec = opts.backupCodec;
				if (backupCodec && videoCodec !== backupCodec.codec && isBackupCodec(backupCodec.codec)) {
					if (!this.roomOptions.dynacast) {
						this.roomOptions.dynacast = true;
					}
					req.simulcastCodecs.push(
						new SimulcastCodec({
							codec: backupCodec.codec,
							cid: '',
						}),
					);
				}
			}

			encodings = computeVideoEncodings(track.source === Track.Source.ScreenShare, req.width, req.height, opts);
			const usesSvcLayers =
				isSVCCodec(opts.videoCodec) &&
				!isSVCSimulcast(opts.videoCodec, opts) &&
				encodings.some(
					(encoding) => typeof encoding.scalabilityMode === 'string' && encoding.scalabilityMode.length > 0,
				);
			req.layers = videoLayersFromEncodings(req.width, req.height, encodings, usesSvcLayers);
		} else if (track.kind === Track.Kind.Audio) {
			encodings = [
				{
					maxBitrate: opts.audioPreset?.maxBitrate,
					priority: opts.audioPreset?.priority ?? 'high',
					networkPriority: opts.audioPreset?.priority ?? 'high',
				},
			];
		}

		if (!this.engine || this.engine.isClosed) {
			throw new UnexpectedConnectionState('cannot publish track when not connected');
		}

		const negotiate = async () => {
			if (!this.engine.pcManager) {
				throw new UnexpectedConnectionState('pcManager is not ready');
			}

			track.sender = await this.engine.createSender(track, opts, encodings);
			if (isLocalVideoTrack(track)) {
				track.publishOptions = opts;
			}
			this.emit(ParticipantEvent.LocalSenderCreated, track.sender, track, opts.videoCodec, track.mediaStreamID);

			if (isLocalVideoTrack(track)) {
				opts.degradationPreference ??= getDefaultDegradationPreference(track);
				track.setDegradationPreference(opts.degradationPreference);
			}

			if (encodings) {
				if (track.kind === Track.Kind.Audio) {
					let trackTransceiver: RTCRtpTransceiver | undefined;
					for (const transceiver of this.engine.pcManager.publisher.getTransceivers()) {
						if (transceiver.sender === track.sender) {
							trackTransceiver = transceiver;
							break;
						}
					}
					if (trackTransceiver) {
						this.engine.pcManager.publisher.setTrackCodecBitrate({
							transceiver: trackTransceiver,
							codec: 'opus',
							maxbr: encodings[0]?.maxBitrate ? encodings[0].maxBitrate / 1000 : 0,
							stereo: isStereo,
						});
					}
				} else if (track.codec && isVideoCodec(track.codec)) {
					const targetBitrate = computeStartTargetBitrate(track.codec, opts, encodings);
					if (targetBitrate > 0) {
						this.engine.pcManager.publisher.setTrackCodecBitrate({
							cid: req.cid,
							codec: track.codec,
							maxbr: targetBitrate / 1000,
							isScreenShare: track.source === Track.Source.ScreenShare,
						});
					}
				}
			}

			await this.engine.negotiate();
		};

		let ti: TrackInfo;
		const addTrackPromise = (async (): Promise<TrackInfo> => {
			try {
				return await this.engine.addTrack(req);
			} catch (err) {
				if (track.sender && this.engine.pcManager?.publisher) {
					try {
						this.engine.pcManager.publisher.removeTrack(track.sender);
					} catch (e) {
						this.log.error(e);
					}
					await this.engine.negotiate().catch((negotiateErr) => {
						this.log.error('failed to negotiate after removing track due to failed add track request', {
							...getLogContextFromTrack(track),
							error: negotiateErr,
						});
					});
				}
				throw err;
			}
		})();
		if (this.enabledPublishVideoCodecs.length > 0 && packetTrailerFeatures.length === 0) {
			const rets = await Promise.all([addTrackPromise, negotiate()]);
			ti = rets[0];
		} else {
			ti = await addTrackPromise;
			let primaryCodecMime: string | undefined;
			ti.codecs.forEach((codec) => {
				if (primaryCodecMime === undefined) {
					primaryCodecMime = codec.mimeType;
				}
			});
			if (primaryCodecMime && track.kind === Track.Kind.Video) {
				const updatedCodec = mimeTypeToVideoCodecString(primaryCodecMime);
				if (updatedCodec !== videoCodec) {
					this.log.debug('falling back to server selected codec', {
						...getLogContextFromTrack(track),
						codec: updatedCodec,
					});
					opts.videoCodec = updatedCodec;

					encodings = computeVideoEncodings(track.source === Track.Source.ScreenShare, req.width, req.height, opts);
				}
			}
			await negotiate();
		}

		const publication = new LocalTrackPublication(track.kind, ti, track, {
			loggerName: this.roomOptions.loggerName,
			loggerContextCb: () => this.logContext,
		});
		publication.on(TrackEvent.CpuConstrained, (constrainedTrack) =>
			this.onTrackCpuConstrained(constrainedTrack, publication),
		);
		publication.options = opts;
		track.sid = ti.sid;

		if (isLocalVideoTrack(track)) {
			track.publishOptions = opts;
			if (req.width && req.height) {
				track.lastEncodedDimensions = {width: req.width, height: req.height};
			}
		}

		this.log.debug(`publishing ${track.kind} with encodings`, {encodings, trackInfo: ti});

		if (isLocalVideoTrack(track)) {
			track.startMonitor(this.engine.client);
		} else if (isLocalAudioTrack(track)) {
			track.startMonitor();
		}

		this.addTrackPublication(publication);
		this.emit(ParticipantEvent.LocalTrackPublished, publication);

		if (isLocalAudioTrack(track) && ti.audioFeatures.includes(AudioTrackFeature.TF_PRECONNECT_BUFFER)) {
			const stream = track.getPreConnectBuffer();
			const mimeType = track.getPreConnectBufferMimeType();
			this.on(ParticipantEvent.LocalTrackSubscribed, (pub) => {
				if (pub.trackSid === ti.sid) {
					if (!track.hasPreConnectBuffer) {
						this.log.warn('subscribe event came to late, buffer already closed');
						return;
					}
					this.log.debug('finished recording preconnect buffer', getLogContextFromTrack(track));
					track.stopPreConnectBuffer();
				}
			});

			if (stream) {
				const bufferStreamPromise = (async (): Promise<void> => {
					this.log.debug('waiting for agent', getLogContextFromTrack(track));
					let agentActiveTimeout: TimerHandle | undefined;
					let agent: RemoteParticipant;
					try {
						agent = await Promise.race([
							this.waitUntilActiveAgentPresent(),
							new Promise<never>((_, reject) => {
								agentActiveTimeout = CriticalTimers.setTimeout(() => {
									reject(new Error('agent not active within 10 seconds'));
								}, 10_000);
							}),
						]);
					} finally {
						if (agentActiveTimeout !== undefined) {
							CriticalTimers.clearTimeout(agentActiveTimeout);
						}
					}
					this.log.debug('sending preconnect buffer', getLogContextFromTrack(track));
					const writer = await this.streamBytes({
						name: 'preconnect-buffer',
						mimeType,
						topic: 'lk.agent.pre-connect-audio-buffer',
						destinationIdentities: [agent.identity],
						attributes: {
							trackId: publication.trackSid,
							sampleRate: String(settings.sampleRate ?? '48000'),
							channels: String(settings.channelCount ?? '1'),
						},
					});
					for await (const chunk of stream) {
						await writer.write(chunk);
					}
					await writer.close();
				})();
				bufferStreamPromise
					.then(() => {
						this.log.debug('preconnect buffer sent successfully', getLogContextFromTrack(track));
					})
					.catch((e) => {
						this.log.error('error sending preconnect buffer', {
							...getLogContextFromTrack(track),
							error: e,
						});
					});
			}
		}
		return publication;
	}

	private canPublishFrameMetadata() {
		return !!(
			this.roomOptions.e2ee ||
			this.roomOptions.encryption ||
			isFrameMetadataSupported(this.roomOptions.frameMetadata ?? this.roomOptions.packetTrailer)
		);
	}

	private normalizeRequestedFrameMetadataOptions(track: LocalTrack, opts: TrackPublishOptions) {
		const fmOpts = opts.frameMetadata ?? opts.packetTrailer;
		if (track.kind !== Track.Kind.Video || !hasFrameMetadataPublishOptions(fmOpts)) {
			opts.frameMetadata = undefined;
			opts.packetTrailer = undefined;
			return [];
		}

		if (!this.canPublishFrameMetadata()) {
			this.log.warn('frame metadata transform not supported; not advertising features', {
				...this.logContext,
				...getLogContextFromTrack(track),
			});
			opts.frameMetadata = undefined;
			opts.packetTrailer = undefined;
			return [];
		}

		const features = getFrameMetadataFeatures(fmOpts);
		const normalized = getFrameMetadataPublishOptions(features);
		opts.frameMetadata = normalized;
		opts.packetTrailer = normalized;
		return features;
	}

	override get isLocal(): boolean {
		return true;
	}

	async publishAdditionalCodecForTrack(
		track: LocalTrack | MediaStreamTrack,
		videoCodec: BackupVideoCodec,
		options?: TrackPublishOptions,
	) {
		let existingPublication: LocalTrackPublication | undefined;
		this.trackPublications.forEach((publication) => {
			if (!publication.track) {
				return;
			}
			if (publication.track === track) {
				existingPublication = <LocalTrackPublication>publication;
			}
		});
		if (!existingPublication) {
			throw new TrackInvalidError('track is not published');
		}

		if (!isLocalVideoTrack(track)) {
			throw new TrackInvalidError('track is not a video track');
		}

		const opts: TrackPublishOptions = {
			...this.roomOptions?.publishDefaults,
			...options,
		};

		const encodings = computeTrackBackupEncodings(track, videoCodec, opts);
		if (!encodings) {
			this.log.info(
				`backup codec has been disabled, ignoring request to add additional codec for track`,
				getLogContextFromTrack(track),
			);
			return;
		}
		const simulcastTrack = track.addSimulcastTrack(videoCodec, encodings);
		if (!simulcastTrack) {
			return;
		}
		const packetTrailerFeatures = this.normalizeRequestedFrameMetadataOptions(track, opts);

		const req = new AddTrackRequest({
			cid: simulcastTrack.mediaStreamTrack.id,
			type: Track.kindToProto(track.kind),
			muted: track.isMuted,
			source: Track.sourceToProto(track.source),
			sid: track.sid,
			encryption: this.encryptionType,
			packetTrailerFeatures,
			simulcastCodecs: [
				{
					codec: opts.videoCodec,
					cid: simulcastTrack.mediaStreamTrack.id,
				},
			],
		});
		req.layers = videoLayersFromEncodings(req.width, req.height, encodings);

		if (!this.engine || this.engine.isClosed) {
			throw new UnexpectedConnectionState('cannot publish track when not connected');
		}

		const negotiate = async () => {
			const transceiverInit: RTCRtpTransceiverInit = {direction: 'sendonly'};
			if (encodings) {
				transceiverInit.sendEncodings = encodings;
			}
			const sender = await this.engine.createSimulcastSender(track, simulcastTrack, opts, encodings);
			if (sender) {
				this.emit(ParticipantEvent.LocalSenderCreated, sender, track, videoCodec, simulcastTrack.mediaStreamTrack.id);
			}

			await this.engine.negotiate();
		};

		try {
			const rets = await Promise.all([this.engine.addTrack(req), negotiate()]);
			const ti = rets[0];

			this.log.debug(`published ${videoCodec} for track ${track.sid}`, {
				encodings,
				trackInfo: ti,
			});
		} catch (e) {
			const scInfo = track.simulcastCodecs.get(videoCodec);
			if (scInfo) {
				scInfo.mediaStreamTrack.stop();
				track.simulcastCodecs.delete(videoCodec);
			}
			throw e;
		}
	}

	async unpublishTrack(
		track: LocalTrack | MediaStreamTrack,
		stopOnUnpublish?: boolean,
	): Promise<LocalTrackPublication | undefined> {
		if (isLocalTrack(track)) {
			const publishPromise = this.pendingPublishPromises.get(track);
			if (publishPromise) {
				this.log.debug('awaiting publish promise before attempting to unpublish', getLogContextFromTrack(track));
				await publishPromise;
			}
		}
		const publication = this.getPublicationForTrack(track);

		const pubLogContext = publication ? getLogContextFromTrack(publication) : undefined;

		this.log.info('unpublishing track', pubLogContext);

		if (!publication?.track) {
			this.log.warn('track was not unpublished because no publication was found', pubLogContext);
			return undefined;
		}

		track = publication.track;
		track.off(TrackEvent.Muted, this.onTrackMuted);
		track.off(TrackEvent.Unmuted, this.onTrackUnmuted);
		track.off(TrackEvent.Ended, this.handleTrackEnded);
		track.off(TrackEvent.UpstreamPaused, this.onTrackUpstreamPaused);
		track.off(TrackEvent.UpstreamResumed, this.onTrackUpstreamResumed);
		track.off(TrackEvent.AudioTrackFeatureUpdate, this.onTrackFeatureUpdate);

		if (stopOnUnpublish === undefined) {
			stopOnUnpublish = this.roomOptions?.stopLocalTrackOnUnpublish ?? true;
		}
		if (stopOnUnpublish) {
			track.stop();
		} else {
			track.stopMonitor();
		}

		let negotiationNeeded = false;
		const trackSender = track.sender;
		track.sender = undefined;
		if (this.engine.pcManager && this.engine.pcManager.currentState < PCTransportState.FAILED && trackSender) {
			const publisher = this.engine.pcManager.publisher;
			try {
				try {
					negotiationNeeded = this.engine.removeTrack(trackSender);
				} catch (e) {
					this.log.warn(e);
					negotiationNeeded = true;
				}
				if (stopTransceiversForSender(publisher.getTransceivers(), trackSender)) {
					negotiationNeeded = true;
				}

				if (isLocalVideoTrack(track)) {
					for (const [, trackInfo] of track.simulcastCodecs) {
						if (trackInfo.sender) {
							try {
								negotiationNeeded = this.engine.removeTrack(trackInfo.sender) || negotiationNeeded;
							} catch (e) {
								this.log.warn(e);
								negotiationNeeded = true;
							}
							if (stopTransceiversForSender(publisher.getTransceivers(), trackInfo.sender)) {
								negotiationNeeded = true;
							}
							trackInfo.sender = undefined;
						}
					}
					track.simulcastCodecs.clear();
				}
			} catch (e) {
				this.log.warn('failed to unpublish track', {...pubLogContext, error: e});
			}
		}

		this.trackPublications.delete(publication.trackSid);
		switch (publication.kind) {
			case Track.Kind.Audio:
				this.audioTrackPublications.delete(publication.trackSid);
				break;
			case Track.Kind.Video:
				this.videoTrackPublications.delete(publication.trackSid);
				break;
			default:
				break;
		}

		this.emit(ParticipantEvent.LocalTrackUnpublished, publication);
		publication.setTrack(undefined);

		if (negotiationNeeded) {
			await this.engine.negotiate();
		}
		return publication;
	}

	async unpublishTracks(tracks: Array<LocalTrack> | Array<MediaStreamTrack>): Promise<Array<LocalTrackPublication>> {
		const results = await Promise.all(tracks.map((track) => this.unpublishTrack(track)));
		return results.filter((track) => !!track);
	}

	async republishAllTracks(options?: TrackPublishOptions, restartTracks: boolean = true) {
		if (this.republishPromise) {
			await this.republishPromise;
		}
		this.republishPromise = new TypedPromise<void, Error>(async (resolve, reject) => {
			try {
				const localPubs: Array<LocalTrackPublication> = [];
				this.trackPublications.forEach((pub) => {
					if (pub.track) {
						if (options) {
							pub.options = {...pub.options, ...options};
						}
						localPubs.push(pub);
					}
				});

				await Promise.all(
					localPubs.map(async (pub) => {
						const track = pub.track!;
						await this.unpublishTrack(track, false);
						if (
							restartTracks &&
							!track.isMuted &&
							track.source !== Track.Source.ScreenShare &&
							track.source !== Track.Source.ScreenShareAudio &&
							(isLocalAudioTrack(track) || isLocalVideoTrack(track)) &&
							!track.isUserProvided
						) {
							this.log.debug('restarting existing track', {track: pub.trackSid});
							await track.restartTrack();
						}
						await this.publishOrRepublishTrack(track, pub.options, true);
					}),
				);
				resolve();
			} catch (error: unknown) {
				if (error instanceof Error) {
					reject(error);
				} else {
					reject(new Error(String(error)));
				}
			} finally {
				this.republishPromise = undefined;
			}
		});

		await this.republishPromise;
	}

	async publishData(data: NonSharedUint8Array, options: DataPublishOptions = {}): Promise<void> {
		const kind = options.reliable ? DataChannelKind.RELIABLE : DataChannelKind.LOSSY;
		const dataPacketKind = options.reliable ? DataPacket_Kind.RELIABLE : DataPacket_Kind.LOSSY;
		const destinationIdentities = options.destinationIdentities;
		const topic = options.topic;

		const userPacket = new UserPacket({
			participantIdentity: this.identity,
			payload: data,
			destinationIdentities,
			topic,
		});

		const packet = new DataPacket({
			kind: dataPacketKind,
			value: {
				case: 'user',
				value: userPacket,
			},
		});

		await this.engine.sendDataPacket(packet, kind);
	}

	async publishDtmf(code: number, digit: string): Promise<void> {
		const packet = new DataPacket({
			kind: DataPacket_Kind.RELIABLE,
			value: {
				case: 'sipDtmf',
				value: new SipDTMF({
					code: code,
					digit: digit,
				}),
			},
		});

		await this.engine.sendDataPacket(packet, DataChannelKind.RELIABLE);
	}

	async sendChatMessage(text: string, options?: SendTextOptions): Promise<ChatMessage> {
		const msg = {
			id: crypto.randomUUID(),
			message: text,
			timestamp: Date.now(),
			attachedFiles: options?.attachments,
		} as const satisfies ChatMessage;
		const packet = new DataPacket({
			value: {
				case: 'chatMessage',
				value: new ChatMessageModel({
					...msg,
					timestamp: protoInt64.parse(msg.timestamp),
				}),
			},
		});
		await this.engine.sendDataPacket(packet, DataChannelKind.RELIABLE);

		this.emit(ParticipantEvent.ChatMessage, msg);
		return msg;
	}

	async editChatMessage(editText: string, originalMessage: ChatMessage) {
		const msg = {
			...originalMessage,
			message: editText,
			editTimestamp: Date.now(),
		} as const satisfies ChatMessage;
		const packet = new DataPacket({
			value: {
				case: 'chatMessage',
				value: new ChatMessageModel({
					...msg,
					timestamp: protoInt64.parse(msg.timestamp),
					editTimestamp: protoInt64.parse(msg.editTimestamp),
				}),
			},
		});
		await this.engine.sendDataPacket(packet, DataChannelKind.RELIABLE);
		this.emit(ParticipantEvent.ChatMessage, msg);
		return msg;
	}

	async sendText(text: string, options?: SendTextOptions): Promise<TextStreamInfo> {
		return this.roomOutgoingDataStreamManager.sendText(text, options);
	}

	async streamText(options?: StreamTextOptions): Promise<TextStreamWriter> {
		return this.roomOutgoingDataStreamManager.streamText(options);
	}

	async sendFile(file: File, options?: SendFileOptions): Promise<{id: string}> {
		return this.roomOutgoingDataStreamManager.sendFile(file, options);
	}

	async sendBytes(bytes: Uint8Array, options?: SendBytesOptions): Promise<ByteStreamInfo> {
		return this.roomOutgoingDataStreamManager.sendBytes(bytes, options);
	}

	async streamBytes(options?: StreamBytesOptions) {
		return this.roomOutgoingDataStreamManager.streamBytes(options);
	}

	performRpc(params: PerformRpcParams): TypedPromise<string, RpcError> {
		return this.rpcClientManager.performRpc(params).then(([_id, completionPromise]) => {
			return completionPromise;
		});
	}

	registerRpcMethod(method: string, handler: (data: RpcInvocationData) => Promise<string>) {
		this.rpcServerManager.registerRpcMethod(method, handler);
	}

	unregisterRpcMethod(method: string) {
		this.rpcServerManager.unregisterRpcMethod(method);
	}

	setTrackSubscriptionPermissions(
		allParticipantsAllowed: boolean,
		participantTrackPermissions: Array<ParticipantTrackPermission> = [],
	) {
		this.participantTrackPermissions = participantTrackPermissions;
		this.allParticipantsAllowedToSubscribe = allParticipantsAllowed;
		if (!this.engine.client.isDisconnected) {
			this.updateTrackSubscriptionPermissions();
		}
	}

	setEnabledPublishCodecs(codecs: Array<Codec>) {
		this.enabledPublishVideoCodecs = codecs.filter((c) => c.mime.split('/')[0].toLowerCase() === 'video');
	}

	override updateInfo(info: ParticipantInfo): boolean {
		if (!super.updateInfo(info)) {
			return false;
		}

		info.tracks.forEach((ti) => {
			const pub = this.trackPublications.get(ti.sid);

			if (pub) {
				const mutedOnServer = pub.isMuted || (pub.track?.isUpstreamPaused ?? false);
				if (mutedOnServer !== ti.muted) {
					this.log.debug('updating server mute state after reconcile', {
						...getLogContextFromTrack(pub),
						mutedOnServer,
					});
					this.engine.client.sendMuteTrack(ti.sid, mutedOnServer);
				}
			}
		});
		return true;
	}

	private updateTrackSubscriptionPermissions = () => {
		this.log.debug('updating track subscription permissions', {
			allParticipantsAllowed: this.allParticipantsAllowedToSubscribe,
			participantTrackPermissions: this.participantTrackPermissions,
		});
		this.engine.client.sendUpdateSubscriptionPermissions(
			this.allParticipantsAllowedToSubscribe,
			this.participantTrackPermissions.map((p) => trackPermissionToProto(p)),
		);
	};

	setActiveAgent(agent: RemoteParticipant | undefined) {
		this.firstActiveAgent = agent;
		if (agent && !this.firstActiveAgent) {
			this.firstActiveAgent = agent;
		}
		if (agent) {
			this.activeAgentFuture?.resolve?.(agent);
		} else {
			this.activeAgentFuture?.reject?.(new Error('Agent disconnected'));
		}
		this.activeAgentFuture = undefined;
	}

	private waitUntilActiveAgentPresent() {
		if (this.firstActiveAgent) {
			return Promise.resolve(this.firstActiveAgent);
		}
		if (!this.activeAgentFuture) {
			this.activeAgentFuture = new Future<RemoteParticipant, Error>();
		}
		return this.activeAgentFuture.promise;
	}

	private onTrackUnmuted = (track: LocalTrack) => {
		this.onTrackMuted(track, track.isUpstreamPaused);
	};

	private onTrackMuted = (track: LocalTrack, muted?: boolean) => {
		if (muted === undefined) {
			muted = true;
		}

		if (!track.sid) {
			this.log.error('could not update mute status for unpublished track', getLogContextFromTrack(track));
			return;
		}

		this.engine.updateMuteStatus(track.sid, muted);
	};

	private onTrackUpstreamPaused = (track: LocalTrack) => {
		this.log.debug('upstream paused', getLogContextFromTrack(track));
		this.onTrackMuted(track, true);
	};

	private onTrackUpstreamResumed = (track: LocalTrack) => {
		this.log.debug('upstream resumed', getLogContextFromTrack(track));
		this.onTrackMuted(track, track.isMuted);
	};

	private onTrackFeatureUpdate = (track: LocalAudioTrack) => {
		const pub = this.audioTrackPublications.get(track.sid!);
		if (!pub) {
			this.log.warn(`Could not update local audio track settings, missing publication for track ${track.sid}`);
			return;
		}
		this.engine.client.sendUpdateLocalAudioTrack(pub.trackSid, pub.getTrackFeatures());
	};

	private onTrackCpuConstrained = (track: LocalVideoTrack, publication: LocalTrackPublication) => {
		this.log.debug('track cpu constrained', getLogContextFromTrack(publication));
		this.emit(ParticipantEvent.LocalTrackCpuConstrained, track, publication);
	};

	private handleSubscribedQualityUpdate = async (update: SubscribedQualityUpdate) => {
		if (!this.roomOptions?.dynacast) {
			return;
		}
		const pub = this.videoTrackPublications.get(update.trackSid);
		if (!pub) {
			this.log.warn('received subscribed quality update for unknown track', {
				trackSid: update.trackSid,
			});
			return;
		}
		if (!pub.videoTrack) {
			return;
		}
		const newCodecs = await pub.videoTrack.setPublishingCodecs(update.subscribedCodecs);
		for await (const codec of newCodecs) {
			if (isBackupCodec(codec)) {
				this.log.debug(`publish ${codec} for ${pub.videoTrack.sid}`, getLogContextFromTrack(pub));
				try {
					await this.publishAdditionalCodecForTrack(pub.videoTrack, codec, pub.options);
				} catch (e) {
					this.log.warn(`failed to publish backup codec ${codec} for ${pub.videoTrack.sid}`, {
						...getLogContextFromTrack(pub),
						error: e,
					});
				}
			}
		}
	};

	private handleLocalTrackUnpublished = (unpublished: TrackUnpublishedResponse) => {
		const track = this.trackPublications.get(unpublished.trackSid);
		if (!track) {
			this.log.warn('received unpublished event for unknown track', {
				trackSid: unpublished.trackSid,
			});
			return;
		}
		this.unpublishTrack(track.track!);
	};

	private handleTrackEnded = async (track: LocalTrack) => {
		if (track.source === Track.Source.ScreenShare || track.source === Track.Source.ScreenShareAudio) {
			this.log.debug('unpublishing local track due to TrackEnded', getLogContextFromTrack(track));
			this.unpublishTrack(track);
		} else if (track.isUserProvided) {
			await track.mute();
		} else if (isLocalAudioTrack(track) || isLocalVideoTrack(track)) {
			try {
				if (isWeb()) {
					try {
						const currentPermissions = await navigator?.permissions.query({
							name: track.source === Track.Source.Camera ? 'camera' : 'microphone',
						});
						if (currentPermissions && currentPermissions.state === 'denied') {
							this.log.warn(`user has revoked access to ${track.source}`, getLogContextFromTrack(track));

							currentPermissions.onchange = () => {
								if (currentPermissions.state !== 'denied') {
									if (!track.isMuted) {
										track.restartTrack();
									}
									currentPermissions.onchange = null;
								}
							};
							throw new Error('GetUserMedia Permission denied');
						}
					} catch (_e: any) {}
				}
				if (!track.isMuted) {
					this.log.debug('track ended, attempting to use a different device', getLogContextFromTrack(track));
					if (isLocalAudioTrack(track)) {
						await track.restartTrack({deviceId: 'default'});
					} else {
						await track.restartTrack();
					}
				}
			} catch (_e) {
				this.log.warn(`could not restart track, muting instead`, getLogContextFromTrack(track));
				await track.mute();
			}
		}
	};

	private getPublicationForTrack(track: LocalTrack | MediaStreamTrack): LocalTrackPublication | undefined {
		let publication: LocalTrackPublication | undefined;
		this.trackPublications.forEach((pub) => {
			const localTrack = pub.track;
			if (!localTrack) {
				return;
			}

			if (track instanceof MediaStreamTrack) {
				if (isLocalAudioTrack(localTrack) || isLocalVideoTrack(localTrack)) {
					if (localTrack.mediaStreamTrack === track) {
						publication = <LocalTrackPublication>pub;
					}
				}
			} else if (track === localTrack) {
				publication = <LocalTrackPublication>pub;
			}
		});
		return publication;
	}

	private async waitForPendingPublicationOfSource(source: Track.Source): Promise<LocalTrackPublication | undefined> {
		const waitForPendingTimeout = 10_000;
		const startTime = Date.now();

		while (Date.now() < startTime + waitForPendingTimeout) {
			const publishPromiseEntry = Array.from(this.pendingPublishPromises.entries()).find(
				([pendingTrack]) => pendingTrack.source === source,
			);
			if (publishPromiseEntry) {
				return publishPromiseEntry[1];
			}
			await sleep(20);
		}
		return undefined;
	}

	async publishDataTrack(options: DataTrackOptions): Promise<LocalDataTrack> {
		const track = new LocalDataTrack(options, this.roomOutgoingDataTrackManager);
		await track.publish();

		return track;
	}
}
