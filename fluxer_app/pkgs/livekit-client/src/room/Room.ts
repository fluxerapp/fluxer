// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {
	type ChatMessage as ChatMessageModel,
	ClientInfo_Capability,
	type ConnectionQualityUpdate,
	type DataPacket,
	type DataPacket_Kind,
	DisconnectReason,
	Encryption_Type,
	type JoinResponse,
	LeaveRequest,
	LeaveRequest_Action,
	type MetricsBatch,
	ParticipantInfo,
	ParticipantInfo_State,
	type ParticipantPermission,
	protoInt64,
	Room as RoomModel,
	type ServerInfo,
	SimulateScenario,
	type SipDTMF,
	type SpeakerInfo,
	type StreamStateUpdate,
	type SubscriptionError,
	type SubscriptionPermissionUpdate,
	type SubscriptionResponse,
	TrackInfo,
	TrackSource,
	TrackType,
	type Transcription as TranscriptionModel,
	type TranscriptionSegment as TranscriptionSegmentModel,
	type UserPacket,
} from '@livekit/protocol';
import {EventEmitter} from 'events';
import 'webrtc-adapter';
import type TypedEmitter from 'typed-emitter';
import {ensureTrailingSlash} from '../api/utils.ts';
import {type BaseE2EEManager, E2EEManager} from '../e2ee/E2eeManager.ts';
import {EncryptionEvent} from '../e2ee/index.ts';
import {FrameMetadataManager} from '../frameMetadata/FrameMetadataManager.ts';
import {isFrameMetadataSupported} from '../frameMetadata/utils.ts';
import log, {getLogger, LoggerNames} from '../logger.ts';
import type {InternalRoomConnectOptions, InternalRoomOptions, RoomConnectOptions, RoomOptions} from '../options.ts';
import type {NonSharedUint8Array} from '../type-polyfills/non-shared-typed-arrays.ts';
import {getBrowser} from '../utils/browserParser.ts';
import TypedPromise from '../utils/TypedPromise.ts';
import {CLIENT_PROTOCOL_DEFAULT} from '../version.ts';
import {BackOffStrategy} from './BackOffStrategy.ts';
import DeviceManager from './DeviceManager.ts';
import IncomingDataStreamManager from './data-stream/incoming/IncomingDataStreamManager.ts';
import type {ByteStreamHandler, TextStreamHandler} from './data-stream/incoming/StreamReader.ts';
import OutgoingDataStreamManager from './data-stream/outgoing/OutgoingDataStreamManager.ts';
import IncomingDataTrackManager from './data-track/incoming/IncomingDataTrackManager.ts';
import type LocalDataTrack from './data-track/LocalDataTrack.ts';
import OutgoingDataTrackManager from './data-track/outgoing/OutgoingDataTrackManager.ts';
import type RemoteDataTrack from './data-track/RemoteDataTrack.ts';
import {DataTrackInfo, type DataTrackSid} from './data-track/types.ts';
import {
	audioDefaults,
	publishDefaults,
	roomConnectOptionDefaults,
	roomOptionDefaults,
	screenShareDeliveryPublishDefaults,
	videoDefaults,
} from './defaults.ts';
import {ConnectionError, ConnectionErrorReason, UnexpectedConnectionState, UnsupportedServer} from './errors.ts';
import {EngineEvent, ParticipantEvent, RoomEvent, TrackEvent} from './events.ts';
import LocalParticipant from './participant/LocalParticipant.ts';
import Participant, {type ConnectionQuality, ParticipantKind} from './participant/Participant.ts';
import RemoteParticipant from './participant/RemoteParticipant.ts';
import {DEFAULT_MAX_AGE_MS, RegionUrlProvider} from './RegionUrlProvider.ts';
import RTCEngine, {DataChannelKind, type RegionStrategy} from './RTCEngine.ts';
import {
	RPC_REQUEST_DATA_STREAM_TOPIC,
	RPC_RESPONSE_DATA_STREAM_TOPIC,
	RpcClientManager,
	RpcError,
	type RpcInvocationData,
	RpcServerManager,
} from './rpc/index.ts';
import {summarizeStatsReport} from './statsSummary.ts';
import CriticalTimers, {type TimerHandle} from './timers.ts';
import LocalAudioTrack from './track/LocalAudioTrack.ts';
import type LocalTrack from './track/LocalTrack.ts';
import LocalTrackPublication from './track/LocalTrackPublication.ts';
import LocalVideoTrack from './track/LocalVideoTrack.ts';
import type RemoteTrack from './track/RemoteTrack.ts';
import type RemoteTrackPublication from './track/RemoteTrackPublication.ts';
import {Track, type TrackProcessorEventValue} from './track/Track.ts';
import type {TrackPublication} from './track/TrackPublication.ts';
import type {AdaptiveStreamSettings} from './track/types.ts';
import {getNewAudioContext, kindToSource, sourceToKind} from './track/utils.ts';
import type {ChatMessage, SimulationOptions, SimulationScenario, TranscriptionSegment} from './types.ts';
import {
	createDummyVideoStreamTrack,
	extractChatMessage,
	extractTrackSid,
	extractTranscriptionSegments,
	Future,
	getDisconnectReasonFromConnectionError,
	getEmptyAudioStreamTrack,
	isBrowserSupported,
	isCloud,
	isCompressionStreamSupported,
	isLocalAudioTrack,
	isLocalParticipant,
	isReactNative,
	isRemotePub,
	isSafariBased,
	isWeb,
	numberToBigInt,
	sleep,
	supportsSetSinkId,
	toHttpUrl,
	unpackStreamId,
	unwrapConstraint,
} from './utils.ts';

export enum ConnectionState {
	Disconnected = 'disconnected',
	Connecting = 'connecting',
	Connected = 'connected',
	Reconnecting = 'reconnecting',
	SignalReconnecting = 'signalReconnecting',
}

const CONNECTION_RECONCILE_FREQUENCY_MS = 4 * 1000;
type RoomEventArguments<E extends keyof RoomEventCallbacks> = Parameters<RoomEventCallbacks[E]>;
type BufferedRoomEvent = {
	[E in keyof RoomEventCallbacks]: {
		event: E;
		args: RoomEventArguments<E>;
	};
}[keyof RoomEventCallbacks];
const STATS_LOG_FREQUENCY_MS = 30 * 1000;

class Room extends (EventEmitter as new () => TypedEmitter<RoomEventCallbacks>) {
	state: ConnectionState = ConnectionState.Disconnected;

	remoteParticipants: Map<string, RemoteParticipant>;

	activeSpeakers: Array<Participant> = [];

	engine!: RTCEngine;

	localParticipant: LocalParticipant;

	options: InternalRoomOptions;

	isE2EEEnabled: boolean = false;

	serverInfo?: Partial<ServerInfo>;

	private roomInfo?: RoomModel;

	private sidToIdentity: Map<string, string>;

	private connOptions?: InternalRoomConnectOptions;

	private audioEnabled = true;

	private audioContext?: AudioContext;

	private abortController?: AbortController;

	private connectFuture?: Future<void, Error>;

	private disconnectLock: Mutex;

	private e2eeManager: BaseE2EEManager | undefined;
	private frameMetadataManager: FrameMetadataManager | undefined;
	private e2eeStateMutex: Mutex = new Mutex();
	private connectionReconcileInterval?: TimerHandle;
	private statsLogInterval?: TimerHandle;

	private regionUrlProvider?: RegionUrlProvider;

	private regionUrl?: string;

	private isVideoPlaybackBlocked: boolean = false;

	private log = log;
	private statsLog = log;
	private bufferedEvents: Array<BufferedRoomEvent> = [];

	private isResuming: boolean = false;

	private pendingTrackAddedCallbacks = new Map<Track.SID, Set<() => void>>();

	private transcriptionReceivedTimes: Map<string, number>;

	private incomingDataStreamManager: IncomingDataStreamManager;

	private outgoingDataStreamManager: OutgoingDataStreamManager;

	private incomingDataTrackManager: IncomingDataTrackManager;

	private outgoingDataTrackManager: OutgoingDataTrackManager;

	private rpcClientManager: RpcClientManager;

	private rpcServerManager: RpcServerManager;

	get hasE2EESetup(): boolean {
		return this.e2eeManager !== undefined;
	}

	constructor(options?: RoomOptions) {
		super();
		this.setMaxListeners(100);
		this.remoteParticipants = new Map();
		this.sidToIdentity = new Map();
		this.options = {...roomOptionDefaults, ...options};

		this.log = getLogger(this.options.loggerName ?? LoggerNames.Room, () => this.logContext);
		this.statsLog = getLogger(LoggerNames.Stats, () => this.logContext);
		this.transcriptionReceivedTimes = new Map();

		this.options.audioCaptureDefaults = {
			...audioDefaults,
			...options?.audioCaptureDefaults,
		};
		this.options.videoCaptureDefaults = {
			...videoDefaults,
			...options?.videoCaptureDefaults,
		};
		this.options.publishDefaults = {
			...(this.options.screenShareDelivery ? screenShareDeliveryPublishDefaults : publishDefaults),
			...options?.publishDefaults,
		};

		this.maybeCreateEngine();

		this.incomingDataStreamManager = new IncomingDataStreamManager(this.options.dataStream?.maxPayloadByteLength);
		this.outgoingDataStreamManager = new OutgoingDataStreamManager(
			this.engine,
			this.log,
			this.getRemoteParticipantClientProtocol,
			this.getRemoteParticipantCapabilities,
			this.getAllRemoteParticipantIdentities,
		);

		this.incomingDataTrackManager = new IncomingDataTrackManager({e2eeManager: this.e2eeManager});
		this.incomingDataTrackManager
			.on('sfuUpdateSubscription', (event) => {
				this.engine.client.sendUpdateDataSubscription(event.sid, event.subscribe);
			})
			.on('trackPublished', (event) => {
				if (event.track.publisherIdentity === this.localParticipant.identity) {
					return;
				}
				this.emit(RoomEvent.DataTrackPublished, event.track);
				this.remoteParticipants.get(event.track.publisherIdentity)?.addRemoteDataTrack(event.track);
			})
			.on('trackUnpublished', (event) => {
				if (event.publisherIdentity === this.localParticipant.identity) {
					return;
				}
				this.emit(RoomEvent.DataTrackUnpublished, event.sid);
				this.remoteParticipants.get(event.publisherIdentity)?.removeRemoteDataTrack(event.sid);
			});

		this.outgoingDataTrackManager = new OutgoingDataTrackManager({e2eeManager: this.e2eeManager});
		this.outgoingDataTrackManager
			.on('sfuPublishRequest', (event) => {
				this.engine.client.sendPublishDataTrackRequest(event.handle, event.name, event.usesE2ee);
			})
			.on('sfuUnpublishRequest', (event) => {
				this.engine.client.sendUnPublishDataTrackRequest(event.handle);
			})
			.on('trackPublished', (event) => {
				this.emit(RoomEvent.LocalDataTrackPublished, event.track);
			})
			.on('trackUnpublished', (event) => {
				this.emit(RoomEvent.LocalDataTrackUnpublished, event.sid);
			})
			.on('packetAvailable', ({handle, bytes}) => {
				this.engine
					.sendDataTrackFrame(bytes)
					.finally(() => this.outgoingDataTrackManager.handlePacketSendComplete(handle));
			});

		this.registerRpcDataStreamHandler();

		this.rpcClientManager = new RpcClientManager(
			this.log,
			this.outgoingDataStreamManager,
			this.getRemoteParticipantClientProtocol,
			() => this.engine?.serverVersion,
		);
		this.rpcClientManager.on('sendDataPacket', ({packet}) => {
			this.engine?.sendDataPacket(packet, DataChannelKind.RELIABLE);
		});
		this.rpcServerManager = new RpcServerManager(
			this.log,
			this.outgoingDataStreamManager,
			this.getRemoteParticipantClientProtocol,
		);
		this.rpcServerManager.on('sendDataPacket', ({packet}) => {
			this.engine?.sendDataPacket(packet, DataChannelKind.RELIABLE);
		});

		this.disconnectLock = new Mutex();
		this.localParticipant = new LocalParticipant(
			'',
			'',
			this.engine,
			this.options,
			this.outgoingDataStreamManager,
			this.outgoingDataTrackManager,
			this.rpcClientManager,
			this.rpcServerManager,
		);

		this.setupFrameMetadata();

		if (this.options.e2ee || this.options.encryption) {
			this.setupE2EE();
		}

		this.engine.e2eeManager = this.e2eeManager;
		this.incomingDataTrackManager.updateE2eeManager(this.e2eeManager ?? null);
		this.outgoingDataTrackManager.updateE2eeManager(this.e2eeManager ?? null);

		if (this.options.videoCaptureDefaults.deviceId) {
			this.localParticipant.activeDeviceMap.set(
				'videoinput',
				unwrapConstraint(this.options.videoCaptureDefaults.deviceId),
			);
		}
		if (this.options.audioCaptureDefaults.deviceId) {
			this.localParticipant.activeDeviceMap.set(
				'audioinput',
				unwrapConstraint(this.options.audioCaptureDefaults.deviceId),
			);
		}
		if (this.options.audioOutput?.deviceId) {
			this.switchActiveDevice('audiooutput', unwrapConstraint(this.options.audioOutput.deviceId)).catch((e) =>
				this.log.warn(`Could not set audio output: ${e.message}`),
			);
		}

		if (isWeb()) {
			const cleanupController = new AbortController();
			let onDeviceChange: () => void;

			if (Room.cleanupRegistry) {
				const roomRef = new WeakRef(this);
				onDeviceChange = () => {
					const self = roomRef.deref();
					if (!self) {
						return;
					}
					self.handleDeviceChange();
				};
				Room.cleanupRegistry.register(this, () => {
					cleanupController.abort();
				});
			} else {
				onDeviceChange = this.handleDeviceChange;
			}

			navigator.mediaDevices?.addEventListener?.('devicechange', onDeviceChange, {
				signal: cleanupController.signal,
			});
		}
	}

	registerTextStreamHandler(topic: string, callback: TextStreamHandler) {
		return this.incomingDataStreamManager.registerTextStreamHandler(topic, callback);
	}

	unregisterTextStreamHandler(topic: string) {
		return this.incomingDataStreamManager.unregisterTextStreamHandler(topic);
	}

	registerByteStreamHandler(topic: string, callback: ByteStreamHandler) {
		return this.incomingDataStreamManager.registerByteStreamHandler(topic, callback);
	}

	unregisterByteStreamHandler(topic: string) {
		return this.incomingDataStreamManager.unregisterByteStreamHandler(topic);
	}

	registerRpcMethod(method: string, handler: (data: RpcInvocationData) => Promise<string>) {
		this.rpcServerManager.registerRpcMethod(method, handler);
	}

	unregisterRpcMethod(method: string) {
		this.rpcServerManager.unregisterRpcMethod(method);
	}

	async setE2EEEnabled(enabled: boolean) {
		const unlock = await this.e2eeStateMutex.lock();
		try {
			if (this.e2eeManager) {
				if (this.isE2EEEnabled !== enabled) {
					await this.localParticipant.setE2EEEnabled(enabled);

					if (this.localParticipant.identity !== '') {
						this.e2eeManager.setParticipantCryptorEnabled(enabled, this.localParticipant.identity);
					}
				}
			} else {
				throw Error('e2ee not configured, please set e2ee settings within the room options');
			}
		} finally {
			unlock();
		}
	}

	private setupE2EE() {
		const dcEncryptionEnabled = !!this.options.encryption;
		const e2eeOptions = this.options.encryption || this.options.e2ee;

		if (e2eeOptions) {
			if ('e2eeManager' in e2eeOptions) {
				this.e2eeManager = e2eeOptions.e2eeManager;
				this.e2eeManager.isDataChannelEncryptionEnabled = dcEncryptionEnabled;
			} else {
				this.e2eeManager = new E2EEManager(e2eeOptions, dcEncryptionEnabled);
			}
			this.e2eeManager.on(EncryptionEvent.ParticipantEncryptionStatusChanged, (enabled, participant) => {
				if (isLocalParticipant(participant)) {
					this.isE2EEEnabled = enabled;
				}
				this.emit(RoomEvent.ParticipantEncryptionStatusChanged, enabled, participant);
			});
			this.e2eeManager.on(EncryptionEvent.EncryptionError, (error, participantIdentity) => {
				const participant = participantIdentity ? this.getParticipantByIdentity(participantIdentity) : undefined;
				this.emit(RoomEvent.EncryptionError, error, participant);
			});
			this.e2eeManager?.setup(this);
			this.e2eeManager?.setupEngine(this.engine);
		}
	}

	private setupFrameMetadata() {
		const opts = this.options.frameMetadata ?? this.options.packetTrailer;
		this.frameMetadataManager = new FrameMetadataManager(opts);
		this.frameMetadataManager.setup(this);
	}

	private get logContext() {
		return {
			room: this.name,
			roomID: this.roomInfo?.sid,
			participant: this.localParticipant?.identity,
			participantID: this.localParticipant?.sid,
		};
	}

	get isRecording(): boolean {
		return this.roomInfo?.activeRecording ?? false;
	}

	getSid(): TypedPromise<string, UnexpectedConnectionState> {
		if (this.state === ConnectionState.Disconnected) {
			return TypedPromise.resolve('');
		}
		if (this.roomInfo && this.roomInfo.sid !== '') {
			return TypedPromise.resolve(this.roomInfo.sid);
		}
		return new TypedPromise<string, UnexpectedConnectionState>((resolve, reject) => {
			const handleRoomUpdate = (roomInfo: RoomModel) => {
				if (roomInfo.sid !== '') {
					this.engine.off(EngineEvent.RoomUpdate, handleRoomUpdate);
					resolve(roomInfo.sid);
				}
			};
			this.engine.on(EngineEvent.RoomUpdate, handleRoomUpdate);
			this.once(RoomEvent.Disconnected, () => {
				this.engine.off(EngineEvent.RoomUpdate, handleRoomUpdate);
				reject(new UnexpectedConnectionState('Room disconnected before room server id was available'));
			});
		});
	}

	get name(): string {
		return this.roomInfo?.name ?? '';
	}

	get metadata(): string | undefined {
		return this.roomInfo?.metadata;
	}

	get numParticipants(): number {
		return this.roomInfo?.numParticipants ?? 0;
	}

	get numPublishers(): number {
		return this.roomInfo?.numPublishers ?? 0;
	}

	private maybeCreateEngine() {
		if (this.engine && (this.engine.isNewlyCreated || !this.engine.isClosed)) {
			return;
		}

		this.engine = new RTCEngine(this.options);
		this.engine.e2eeManager = this.e2eeManager;

		this.engine
			.on(EngineEvent.ParticipantUpdate, this.handleParticipantUpdates)
			.on(EngineEvent.RoomUpdate, this.handleRoomUpdate)
			.on(EngineEvent.SpeakersChanged, this.handleSpeakersChanged)
			.on(EngineEvent.StreamStateChanged, this.handleStreamStateUpdate)
			.on(EngineEvent.ConnectionQualityUpdate, this.handleConnectionQualityUpdate)
			.on(EngineEvent.SubscriptionError, this.handleSubscriptionError)
			.on(EngineEvent.SubscriptionPermissionUpdate, this.handleSubscriptionPermissionUpdate)
			.on(
				EngineEvent.MediaTrackAdded,
				(mediaTrack: MediaStreamTrack, stream: MediaStream, receiver: RTCRtpReceiver) => {
					this.onTrackAdded(mediaTrack, stream, receiver);
				},
			)
			.on(EngineEvent.Disconnected, (reason?: DisconnectReason) => {
				this.handleDisconnect(this.options.stopLocalTrackOnUnpublish, reason);
			})
			.on(EngineEvent.ActiveSpeakersUpdate, this.handleActiveSpeakersUpdate)
			.on(EngineEvent.DataPacketReceived, this.handleDataPacket)
			.on(EngineEvent.Resuming, () => {
				this.clearConnectionReconcile();
				this.isResuming = true;
				this.log.debug('Resuming signal connection');
				if (this.setAndEmitConnectionState(ConnectionState.SignalReconnecting)) {
					this.emit(RoomEvent.SignalReconnecting);
				}
			})
			.on(EngineEvent.Resumed, () => {
				this.registerConnectionReconcile();
				this.isResuming = false;
				this.log.debug('Resumed signal connection');
				this.updateSubscriptions();
				if (this.setAndEmitConnectionState(ConnectionState.Connected)) {
					this.emit(RoomEvent.Reconnected);
				}
				this.emitBufferedEvents();
			})
			.on(EngineEvent.SignalResumed, () => {
				if (this.state === ConnectionState.Reconnecting || this.isResuming) {
					this.sendSyncState();
				}
				this.emitBufferedEvents();
			})
			.on(EngineEvent.Restarting, this.handleRestarting)
			.on(EngineEvent.Restarted, this.handleRestarted)
			.on(EngineEvent.SignalRestarted, this.handleSignalRestarted)
			.on(EngineEvent.Offline, () => {
				if (this.setAndEmitConnectionState(ConnectionState.Reconnecting)) {
					this.emit(RoomEvent.Reconnecting);
				}
			})
			.on(EngineEvent.DCBufferStatusChanged, (status, kind) => {
				this.emit(RoomEvent.DCBufferStatusChanged, status, kind);
			})
			.on(EngineEvent.LocalTrackSubscribed, (subscribedSid) => {
				this.handleLocalTrackSubscribed(subscribedSid);
			})
			.on(EngineEvent.RoomMoved, (roomMoved) => {
				this.log.debug('room moved', roomMoved);

				if (roomMoved.room) {
					this.handleRoomUpdate(roomMoved.room);
				}

				this.remoteParticipants.forEach((participant, identity) => {
					this.handleParticipantDisconnected(identity, participant);
				});

				this.emit(RoomEvent.Moved, roomMoved.room!.name);

				if (roomMoved.participant) {
					this.handleParticipantUpdates([roomMoved.participant, ...roomMoved.otherParticipants]);
				} else {
					this.handleParticipantUpdates(roomMoved.otherParticipants);
				}
			})
			.on(EngineEvent.PublishDataTrackResponse, (event) => {
				if (!event.info) {
					this.log.warn(`received PublishDataTrackResponse, but event.info was ${event.info}, so skipping.`);
					return;
				}

				this.outgoingDataTrackManager.receivedSfuPublishResponse(event.info.pubHandle, {
					type: 'ok',
					data: {
						sid: event.info.sid,
						pubHandle: event.info.pubHandle,
						name: event.info.name,
						usesE2ee: event.info.encryption !== Encryption_Type.NONE,
					},
				});
			})
			.on(EngineEvent.UnPublishDataTrackResponse, (event) => {
				if (!event.info) {
					this.log.warn(`received UnPublishDataTrackResponse, but event.info was ${event.info}, so skipping.`);
					return;
				}

				this.outgoingDataTrackManager.receivedSfuUnpublishResponse(event.info.pubHandle);
			})
			.on(EngineEvent.DataTrackSubscriberHandles, (event) => {
				const handleToSidMapping = new Map(
					Object.entries(event.subHandles).map(([key, value]) => {
						return [parseInt(key, 10), value.trackSid];
					}),
				);

				this.incomingDataTrackManager.receivedSfuSubscriberHandles(handleToSidMapping);
			})
			.on(EngineEvent.DataTrackPacketReceived, (packetBytes) => {
				this.incomingDataTrackManager.packetReceived(packetBytes);
			})
			.on(EngineEvent.Joined, (joinResponse) => {
				const mapped = new Map(
					joinResponse.otherParticipants.map((participant) => {
						return [participant.identity, participant.dataTracks.map((info) => DataTrackInfo.from(info))];
					}),
				);
				this.incomingDataTrackManager.receiveSfuPublicationUpdates(mapped);
			})
			.on(EngineEvent.TokenRefreshed, (token) => {
				this.regionUrlProvider?.updateToken(token);
			})
			.on(EngineEvent.ServerRegionsReported, (regions) => {
				this.regionUrlProvider?.setServerReportedRegions({
					regionSettings: regions,
					updatedAtInMs: Date.now(),
					maxAgeInMs: DEFAULT_MAX_AGE_MS,
				});
			});

		if (this.localParticipant) {
			this.localParticipant.setupEngine(this.engine);
		}
		if (this.e2eeManager) {
			this.e2eeManager.setupEngine(this.engine);
		}
		if (this.outgoingDataStreamManager) {
			this.outgoingDataStreamManager.setupEngine(this.engine);
		}
	}

	private createRegionStrategy(): RegionStrategy {
		return {
			getNextUrl: async (signal?: AbortSignal) =>
				this.regionUrlProvider ? this.regionUrlProvider.getNextBestRegionUrl(signal) : null,
			resetAttempts: () => this.regionUrlProvider?.resetAttempts(),
		};
	}

	static getLocalDevices(kind?: MediaDeviceKind, requestPermissions: boolean = true): Promise<Array<MediaDeviceInfo>> {
		return DeviceManager.getInstance().getDevices(kind, requestPermissions);
	}

	static cleanupRegistry =
		typeof FinalizationRegistry !== 'undefined' &&
		typeof WeakRef !== 'undefined' &&
		new FinalizationRegistry((cleanup: () => void) => {
			cleanup();
		});

	async prepareConnection(url: string, token?: string) {
		if (this.state !== ConnectionState.Disconnected) {
			return;
		}
		this.log.debug(`prepareConnection to ${url}`);
		try {
			if (isCloud(new URL(url)) && token) {
				this.regionUrlProvider = new RegionUrlProvider(url, token);
				const regionUrl = await this.regionUrlProvider.getNextBestRegionUrl();
				if (regionUrl && this.state === ConnectionState.Disconnected) {
					this.regionUrl = regionUrl;
					await fetch(toHttpUrl(regionUrl), {method: 'HEAD'});
					this.log.debug(`prepared connection to ${regionUrl}`);
				}
			} else {
				await fetch(toHttpUrl(url), {method: 'HEAD'});
			}
		} catch (e) {
			this.log.warn('could not prepare connection', {error: e});
		}
	}

	connect = async (url: string, token: string, opts?: RoomConnectOptions): Promise<void> => {
		if (!isBrowserSupported()) {
			if (isReactNative()) {
				throw Error("WebRTC isn't detected, have you called registerGlobals?");
			} else {
				throw Error(
					"LiveKit doesn't seem to be supported on this browser. Try to update your browser and make sure no browser extensions are disabling webRTC.",
				);
			}
		}

		const unlockDisconnect = await this.disconnectLock.lock();

		if (this.state === ConnectionState.Connected) {
			this.log.info(`already connected to room ${this.name}`);
			unlockDisconnect();
			return Promise.resolve();
		}

		if (this.connectFuture) {
			unlockDisconnect();
			return this.connectFuture.promise;
		}

		this.setAndEmitConnectionState(ConnectionState.Connecting);
		if (this.regionUrlProvider?.getServerUrl().toString() !== ensureTrailingSlash(url)) {
			this.regionUrl = undefined;
			this.regionUrlProvider = undefined;
		}
		if (isCloud(new URL(url))) {
			if (this.regionUrlProvider === undefined) {
				this.regionUrlProvider = new RegionUrlProvider(url, token);
			} else {
				this.regionUrlProvider.updateToken(token);
			}
			this.regionUrlProvider
				.fetchRegionSettings()
				.then((settings) => {
					this.regionUrlProvider?.setServerReportedRegions(settings);
				})
				.catch((e) => {
					this.log.warn('could not fetch region settings', {error: e});
				});
		}

		const connectFn = async (resolve: () => void, reject: (reason: Error) => void, regionUrl?: string) => {
			if (this.abortController) {
				this.abortController.abort();
			}

			const abortController = new AbortController();
			this.abortController = abortController;

			unlockDisconnect?.();

			try {
				await BackOffStrategy.getInstance().getBackOffPromise(url);
				if (abortController.signal.aborted) {
					throw ConnectionError.cancelled('Connection attempt aborted');
				}
				await this.attemptConnection(regionUrl ?? url, token, opts, abortController);
				this.abortController = undefined;
				resolve();
			} catch (error) {
				if (
					this.regionUrlProvider &&
					error instanceof ConnectionError &&
					error.reason !== ConnectionErrorReason.Cancelled &&
					error.reason !== ConnectionErrorReason.NotAllowed
				) {
					let nextUrl: string | null = null;
					try {
						this.log.debug('Fetching next region');
						nextUrl = await this.regionUrlProvider.getNextBestRegionUrl(this.abortController?.signal);
					} catch (regionFetchError) {
						if (
							regionFetchError instanceof ConnectionError &&
							(regionFetchError.status === 401 || regionFetchError.reason === ConnectionErrorReason.Cancelled)
						) {
							this.handleDisconnect(this.options.stopLocalTrackOnUnpublish);
							reject(regionFetchError);
							return;
						}
					}
					if (
						[
							ConnectionErrorReason.InternalError,
							ConnectionErrorReason.ServerUnreachable,
							ConnectionErrorReason.Timeout,
						].includes(error.reason)
					) {
						this.log.debug('Adding failed connection attempt to back off');
						BackOffStrategy.getInstance().addFailedConnectionAttempt(url);
					}
					if (nextUrl && !this.abortController?.signal.aborted) {
						this.log.info(
							`Initial connection failed with ConnectionError: ${error.message}. Retrying with another region: ${nextUrl}`,
						);
						this.recreateEngine(true);
						await connectFn(resolve, reject, nextUrl);
					} else {
						this.handleDisconnect(
							this.options.stopLocalTrackOnUnpublish,
							getDisconnectReasonFromConnectionError(error),
						);
						reject(error);
					}
				} else {
					let disconnectReason = DisconnectReason.UNKNOWN_REASON;
					if (error instanceof ConnectionError) {
						disconnectReason = getDisconnectReasonFromConnectionError(error);
					}
					this.handleDisconnect(this.options.stopLocalTrackOnUnpublish, disconnectReason);
					reject(error instanceof Error ? error : new Error(String(error)));
				}
			}
		};

		const regionUrl = this.regionUrl;
		this.regionUrl = undefined;
		this.connectFuture = new Future(
			(resolve, reject) => {
				connectFn(resolve, reject, regionUrl);
			},
			() => {
				this.clearConnectionFutures();
			},
		);

		return this.connectFuture.promise;
	};

	private connectSignal = async (
		url: string,
		token: string,
		engine: RTCEngine,
		connectOptions: InternalRoomConnectOptions,
		roomOptions: InternalRoomOptions,
		abortController: AbortController,
	): Promise<JoinResponse> => {
		const {joinResponse, serverInfo} = await engine.join(
			url,
			token,
			{
				autoSubscribe: connectOptions.autoSubscribe,
				adaptiveStream: typeof roomOptions.adaptiveStream === 'object' ? true : roomOptions.adaptiveStream,
				clientInfoCapabilities: this.getClientInfoCapabilities(roomOptions),
				maxRetries: connectOptions.maxRetries,
				e2eeEnabled: !!this.e2eeManager,
				websocketTimeout: connectOptions.websocketTimeout,
			},
			abortController.signal,
			!roomOptions.singlePeerConnection,
		);

		this.serverInfo = serverInfo;

		if (!serverInfo.version) {
			throw new UnsupportedServer('unknown server version');
		}

		if (serverInfo.version === '0.15.1' && this.options.dynacast) {
			this.log.debug('disabling dynacast due to server version');
			roomOptions.dynacast = false;
		}

		return joinResponse;
	};

	private applyJoinResponse = (joinResponse: JoinResponse) => {
		const pi = joinResponse.participant!;

		this.localParticipant.sid = pi.sid;
		this.localParticipant.identity = pi.identity;
		this.localParticipant.setEnabledPublishCodecs(joinResponse.enabledPublishCodecs);

		if (this.e2eeManager) {
			try {
				this.e2eeManager.setSifTrailer(joinResponse.sifTrailer as NonSharedUint8Array);
			} catch (e: unknown) {
				this.log.error(e instanceof Error ? e.message : 'Could not set SifTrailer', {
					error: e,
				});
			}
		}

		this.handleParticipantUpdates([pi, ...joinResponse.otherParticipants]);

		if (joinResponse.room) {
			this.handleRoomUpdate(joinResponse.room);
		}
	};

	private attemptConnection = async (
		url: string,
		token: string,
		opts: RoomConnectOptions | undefined,
		abortController: AbortController,
	) => {
		if (this.state === ConnectionState.Reconnecting || this.isResuming || this.engine?.pendingReconnect) {
			this.log.info('Reconnection attempt replaced by new connection attempt');
			this.recreateEngine(true);
		} else {
			this.maybeCreateEngine();
		}
		if (this.regionUrlProvider?.isCloud()) {
			this.engine.setRegionStrategy(this.createRegionStrategy());
		}

		this.acquireAudioContext();

		this.connOptions = {...roomConnectOptionDefaults, ...opts} as InternalRoomConnectOptions;

		if (this.connOptions.rtcConfig) {
			this.engine.rtcConfig = this.connOptions.rtcConfig;
		}
		if (this.connOptions.peerConnectionTimeout) {
			this.engine.peerConnectionTimeout = this.connOptions.peerConnectionTimeout;
		}

		try {
			const joinResponse = await this.connectSignal(
				url,
				token,
				this.engine,
				this.connOptions,
				this.options,
				abortController,
			);

			this.applyJoinResponse(joinResponse);
			this.setupLocalParticipantEvents();
			this.emit(RoomEvent.SignalConnected);
		} catch (err) {
			await this.engine.close();
			this.recreateEngine();

			const resultingError = abortController.signal.aborted
				? ConnectionError.cancelled('Signal connection aborted')
				: ConnectionError.serverUnreachable('could not establish signal connection');

			if (err instanceof Error) {
				resultingError.message = `${resultingError.message}: ${err.message}`;
			}
			if (err instanceof ConnectionError) {
				resultingError.reason = err.reason;
				resultingError.status = err.status;
			}
			this.log.debug(`error trying to establish signal connection`, {
				error: err,
			});
			throw resultingError;
		}

		if (abortController.signal.aborted) {
			await this.engine.close();
			this.recreateEngine();
			throw ConnectionError.cancelled(`Connection attempt aborted`);
		}

		try {
			await this.engine.waitForPCInitialConnection(this.connOptions.peerConnectionTimeout, abortController);
		} catch (e) {
			await this.engine.close();
			this.recreateEngine();
			throw e;
		}

		if (isWeb() && this.options.disconnectOnPageLeave) {
			window.addEventListener('pagehide', this.onPageLeave);
			window.addEventListener('beforeunload', this.onPageLeave);
		}
		if (isWeb()) {
			window.addEventListener('freeze', this.onPageLeave);
		}
		this.setAndEmitConnectionState(ConnectionState.Connected);
		this.emit(RoomEvent.Connected);
		BackOffStrategy.getInstance().resetFailedConnectionAttempts(url);
		this.registerConnectionReconcile();

		if (this.regionUrlProvider) {
			this.regionUrlProvider.notifyConnected();
		}
	};

	disconnect = async (stopTracks = true) => {
		const unlock = await this.disconnectLock.lock();
		try {
			if (this.state === ConnectionState.Disconnected) {
				this.log.debug('already disconnected');
				return;
			}
			this.log.info('disconnect from room');
			if (this.state === ConnectionState.Connecting || this.state === ConnectionState.Reconnecting || this.isResuming) {
				const msg = 'Abort connection attempt due to user initiated disconnect';
				this.log.warn(msg);
				this.abortController?.abort(msg);
				this.connectFuture?.reject?.(ConnectionError.cancelled('Client initiated disconnect'));
				this.connectFuture = undefined;
			}

			if (this.engine) {
				if (!this.engine.client.isDisconnected) {
					await this.engine.client.sendLeave();
				}
				await this.engine.close();
			}
			this.handleDisconnect(stopTracks, DisconnectReason.CLIENT_INITIATED);
			this.engine = undefined!;
		} finally {
			unlock();
		}
	};

	getParticipantByIdentity(identity: string): Participant | undefined {
		if (this.localParticipant.identity === identity) {
			return this.localParticipant;
		}
		return this.remoteParticipants.get(identity);
	}

	private clearConnectionFutures() {
		this.connectFuture = undefined;
	}

	async simulateScenario(scenario: SimulationScenario, arg?: unknown) {
		let postAction = async () => {};
		let req: SimulateScenario | undefined;
		switch (scenario) {
			case 'signal-reconnect':
				await this.engine.client.handleOnClose('simulate disconnect');
				break;
			case 'fail-on-v1-path':
				this.engine.failNextV1Path();
				break;
			case 'speaker':
				req = new SimulateScenario({
					scenario: {
						case: 'speakerUpdate',
						value: 3,
					},
				});
				break;
			case 'node-failure':
				req = new SimulateScenario({
					scenario: {
						case: 'nodeFailure',
						value: true,
					},
				});
				break;
			case 'server-leave':
				req = new SimulateScenario({
					scenario: {
						case: 'serverLeave',
						value: true,
					},
				});
				break;
			case 'migration':
				req = new SimulateScenario({
					scenario: {
						case: 'migration',
						value: true,
					},
				});
				break;
			case 'resume-reconnect':
				this.engine.failNext();
				await this.engine.client.handleOnClose('simulate resume-disconnect');
				break;
			case 'disconnect-signal-on-resume':
				postAction = async () => {
					await this.engine.client.handleOnClose('simulate resume-disconnect');
				};
				req = new SimulateScenario({
					scenario: {
						case: 'disconnectSignalOnResume',
						value: true,
					},
				});
				break;
			case 'disconnect-signal-on-resume-no-messages':
				postAction = async () => {
					await this.engine.client.handleOnClose('simulate resume-disconnect');
				};
				req = new SimulateScenario({
					scenario: {
						case: 'disconnectSignalOnResumeNoMessages',
						value: true,
					},
				});
				break;
			case 'full-reconnect':
				this.engine.fullReconnectOnNext = true;
				await this.engine.client.handleOnClose('simulate full-reconnect');
				break;
			case 'force-tcp':
			case 'force-tls':
				req = new SimulateScenario({
					scenario: {
						case: 'switchCandidateProtocol',
						value: scenario === 'force-tls' ? 2 : 1,
					},
				});
				postAction = async () => {
					const onLeave = this.engine.client.onLeave;
					if (onLeave) {
						onLeave(
							new LeaveRequest({
								reason: DisconnectReason.CLIENT_INITIATED,
								action: LeaveRequest_Action.RECONNECT,
							}),
						);
					}
				};
				break;
			case 'subscriber-bandwidth':
				if (arg === undefined || typeof arg !== 'number') {
					throw new Error('subscriber-bandwidth requires a number as argument');
				}
				req = new SimulateScenario({
					scenario: {
						case: 'subscriberBandwidth',
						value: numberToBigInt(arg),
					},
				});
				break;
			case 'leave-full-reconnect':
				req = new SimulateScenario({
					scenario: {
						case: 'leaveRequestFullReconnect',
						value: true,
					},
				});
				break;
			default:
		}
		if (req) {
			await this.engine.client.sendSimulateScenario(req);
			await postAction();
		}
	}

	private onPageLeave = async () => {
		this.log.info('Page leave detected, disconnecting');
		await this.disconnect();
	};

	startAudio = async () => {
		const elements: Array<HTMLMediaElement> = [];
		const browser = getBrowser();
		if (browser && browser.os === 'iOS') {
			const audioId = 'livekit-dummy-audio-el';
			let dummyAudioEl = document.getElementById(audioId) as HTMLAudioElement | null;
			if (!dummyAudioEl) {
				dummyAudioEl = document.createElement('audio');
				dummyAudioEl.id = audioId;
				dummyAudioEl.autoplay = true;
				dummyAudioEl.hidden = true;
				const track = getEmptyAudioStreamTrack();
				track.enabled = true;
				const stream = new MediaStream([track]);
				dummyAudioEl.srcObject = stream;
				document.addEventListener('visibilitychange', () => {
					if (!dummyAudioEl) {
						return;
					}
					dummyAudioEl.srcObject = document.hidden ? null : stream;
					if (!document.hidden) {
						this.log.debug('page visible again, triggering startAudio to resume playback and update playback status');
						this.startAudio();
					}
				});
				document.body.append(dummyAudioEl);
				this.once(RoomEvent.Disconnected, () => {
					dummyAudioEl?.remove();
					dummyAudioEl = null;
				});
			}
			elements.push(dummyAudioEl);
		}

		this.remoteParticipants.forEach((p) => {
			p.audioTrackPublications.forEach((t) => {
				if (t.track) {
					t.track.attachedElements.forEach((e) => {
						elements.push(e);
					});
				}
			});
		});

		try {
			await Promise.all([
				this.acquireAudioContext(),
				...elements.map((e) => {
					if (!this.options.webAudioMix) {
						e.muted = false;
					}
					return e.play();
				}),
			]);
			this.handleAudioPlaybackStarted();
		} catch (err) {
			this.handleAudioPlaybackFailed(err);
			throw err;
		}
	};

	startVideo = async () => {
		const elements: Array<HTMLMediaElement> = [];
		for (const p of this.remoteParticipants.values()) {
			p.videoTrackPublications.forEach((tr) => {
				tr.track?.attachedElements.forEach((el) => {
					if (!elements.includes(el)) {
						elements.push(el);
					}
				});
			});
		}
		await Promise.all(elements.map((el) => el.play()))
			.then(() => {
				this.handleVideoPlaybackStarted();
			})
			.catch((e) => {
				if (e.name === 'NotAllowedError') {
					this.handleVideoPlaybackFailed();
				} else {
					this.log.warn(
						'Resuming video playback failed, make sure you call `startVideo` directly in a user gesture handler',
					);
				}
			});
	};

	get canPlaybackAudio(): boolean {
		return this.audioEnabled;
	}

	get canPlaybackVideo(): boolean {
		return !this.isVideoPlaybackBlocked;
	}

	getActiveDevice(kind: MediaDeviceKind): string | undefined {
		return this.localParticipant.activeDeviceMap.get(kind);
	}

	async switchActiveDevice(kind: MediaDeviceKind, deviceId: string, exact: boolean = true) {
		let success = true;
		let shouldTriggerImmediateDeviceChange = false;
		const deviceConstraint = exact ? {exact: deviceId} : deviceId;
		if (kind === 'audioinput') {
			shouldTriggerImmediateDeviceChange = this.localParticipant.audioTrackPublications.size === 0;
			const prevDeviceId = this.getActiveDevice(kind) ?? this.options.audioCaptureDefaults!.deviceId;
			this.options.audioCaptureDefaults!.deviceId = deviceConstraint;
			const tracks = Array.from(this.localParticipant.audioTrackPublications.values()).filter(
				(track) => track.source === Track.Source.Microphone,
			);
			try {
				success = (await Promise.all(tracks.map((t) => t.audioTrack?.setDeviceId(deviceConstraint)))).every(
					(val) => val === true,
				);
			} catch (e) {
				this.options.audioCaptureDefaults!.deviceId = prevDeviceId;
				throw e;
			}
			const isMuted = tracks.some((t) => t.track?.isMuted ?? false);
			if (success && isMuted) shouldTriggerImmediateDeviceChange = true;
		} else if (kind === 'videoinput') {
			shouldTriggerImmediateDeviceChange = this.localParticipant.videoTrackPublications.size === 0;
			const prevDeviceId = this.getActiveDevice(kind) ?? this.options.videoCaptureDefaults!.deviceId;
			this.options.videoCaptureDefaults!.deviceId = deviceConstraint;
			const tracks = Array.from(this.localParticipant.videoTrackPublications.values()).filter(
				(track) => track.source === Track.Source.Camera,
			);
			try {
				success = (await Promise.all(tracks.map((t) => t.videoTrack?.setDeviceId(deviceConstraint)))).every(
					(val) => val === true,
				);
			} catch (e) {
				this.options.videoCaptureDefaults!.deviceId = prevDeviceId;
				throw e;
			}
			const isMuted = tracks.some((t) => t.track?.isMuted ?? false);
			if (success && isMuted) shouldTriggerImmediateDeviceChange = true;
		} else if (kind === 'audiooutput') {
			shouldTriggerImmediateDeviceChange = true;
			if (
				(!supportsSetSinkId() && !this.options.webAudioMix) ||
				(this.options.webAudioMix && this.audioContext && !('setSinkId' in this.audioContext))
			) {
				throw new Error('cannot switch audio output, the current browser does not support it');
			}
			if (this.options.webAudioMix) {
				deviceId = (await DeviceManager.getInstance().normalizeDeviceId('audiooutput', deviceId)) ?? '';
			}
			this.options.audioOutput ??= {};
			const prevDeviceId = this.getActiveDevice(kind) ?? this.options.audioOutput.deviceId;
			this.options.audioOutput.deviceId = deviceId;

			try {
				if (this.options.webAudioMix) {
					(this.audioContext as AudioContext & {setSinkId?: (id: string) => Promise<void>})?.setSinkId?.(deviceId);
				}

				await Promise.all(Array.from(this.remoteParticipants.values()).map((p) => p.setAudioOutput({deviceId})));
			} catch (e) {
				this.options.audioOutput.deviceId = prevDeviceId;
				throw e;
			}
		}

		if (shouldTriggerImmediateDeviceChange) {
			this.localParticipant.activeDeviceMap.set(kind, deviceId);
			this.emit(RoomEvent.ActiveDeviceChanged, kind, deviceId);
		}

		return success;
	}

	private setupLocalParticipantEvents() {
		this.localParticipant
			.on(ParticipantEvent.ParticipantMetadataChanged, this.onLocalParticipantMetadataChanged)
			.on(ParticipantEvent.ParticipantNameChanged, this.onLocalParticipantNameChanged)
			.on(ParticipantEvent.AttributesChanged, this.onLocalAttributesChanged)
			.on(ParticipantEvent.TrackMuted, this.onLocalTrackMuted)
			.on(ParticipantEvent.TrackUnmuted, this.onLocalTrackUnmuted)
			.on(ParticipantEvent.LocalTrackPublished, this.onLocalTrackPublished)
			.on(ParticipantEvent.LocalTrackUnpublished, this.onLocalTrackUnpublished)
			.on(ParticipantEvent.ConnectionQualityChanged, this.onLocalConnectionQualityChanged)
			.on(ParticipantEvent.MediaDevicesError, this.onMediaDevicesError)
			.on(ParticipantEvent.AudioStreamAcquired, this.startAudio)
			.on(ParticipantEvent.ChatMessage, this.onLocalChatMessageSent)
			.on(ParticipantEvent.ParticipantPermissionsChanged, this.onLocalParticipantPermissionsChanged);
	}
	private recreateEngine(sendLeave?: boolean) {
		const oldEngine = this.engine;
		if (sendLeave && oldEngine && !oldEngine.client.isDisconnected) {
			oldEngine.client.sendLeave().finally(() => oldEngine.close());
		} else {
			oldEngine?.close();
		}
		this.engine = undefined!;
		this.isResuming = false;

		this.remoteParticipants.clear();
		this.sidToIdentity.clear();
		this.bufferedEvents = [];
		this.maybeCreateEngine();
	}

	private onTrackAdded(mediaTrack: MediaStreamTrack, stream: MediaStream, receiver: RTCRtpReceiver) {
		if ([ConnectionState.Connecting, ConnectionState.Reconnecting].includes(this.state)) {
			const pendingTrackSid = extractTrackSid(mediaTrack, stream);
			this.log.debug('deferring on track for later', {
				mediaTrackId: mediaTrack.id,
				mediaStreamId: stream.id,
				tracksInStream: stream.getTracks().map((track) => track.id),
			});
			const reconnectedHandler = () => {
				cleanup();
				this.onTrackAdded(mediaTrack, stream, receiver);
			};
			const cleanup = () => {
				this.off(RoomEvent.Reconnected, reconnectedHandler);
				this.off(RoomEvent.Connected, reconnectedHandler);
				this.off(RoomEvent.Disconnected, cleanup);
				if (pendingTrackSid) {
					const pendingCallbacks = this.pendingTrackAddedCallbacks.get(pendingTrackSid);
					pendingCallbacks?.delete(cleanup);
					if (pendingCallbacks?.size === 0) {
						this.pendingTrackAddedCallbacks.delete(pendingTrackSid);
					}
				}
			};
			this.once(RoomEvent.Reconnected, reconnectedHandler);
			this.once(RoomEvent.Connected, reconnectedHandler);
			this.once(RoomEvent.Disconnected, cleanup);
			if (pendingTrackSid) {
				const pendingCallbacks = this.pendingTrackAddedCallbacks.get(pendingTrackSid) ?? new Set<() => void>();
				pendingCallbacks.add(cleanup);
				this.pendingTrackAddedCallbacks.set(pendingTrackSid, pendingCallbacks);
			}
			return;
		}
		if (this.state === ConnectionState.Disconnected) {
			this.log.warn('skipping incoming track after Room disconnected');
			return;
		}
		if (mediaTrack.readyState === 'ended') {
			this.log.debug('skipping incoming track as it already ended');
			return;
		}
		const parts = unpackStreamId(stream.id);
		const participantSid = parts[0];
		const streamId = parts[1];
		let trackId = extractTrackSid(mediaTrack, stream) ?? mediaTrack.id;

		if (participantSid === this.localParticipant.sid) {
			this.log.warn('tried to create RemoteParticipant for local participant');
			return;
		}

		const participant = Array.from(this.remoteParticipants.values()).find((p) => p.sid === participantSid) as
			| RemoteParticipant
			| undefined;

		if (!participant) {
			if (participantSid.startsWith('PA')) {
				this.log.error(`Tried to add a track for a participant, that's not present. Sid: ${participantSid}`);
			}
			return;
		}

		if (!trackId.startsWith('TR')) {
			const id = this.engine.getTrackIdForReceiver(receiver);
			if (!id) {
				this.log.error(
					`Tried to add a track whose 'sid' could not be found for a participant, that's not present. Sid: ${participantSid}`,
				);
				return;
			}

			trackId = id;
		}
		if (!trackId.startsWith('TR')) {
			this.log.warn(
				`Tried to add a track whose 'sid' could not be determined for a participant, that's not present. Sid: ${participantSid}, streamId: ${streamId}, trackId: ${trackId}`,
				{remoteParticipantID: participantSid, streamId, trackId},
			);
		}

		let adaptiveStreamSettings: AdaptiveStreamSettings | undefined;
		if (this.options.adaptiveStream) {
			if (typeof this.options.adaptiveStream === 'object') {
				adaptiveStreamSettings = this.options.adaptiveStream;
			} else {
				adaptiveStreamSettings = {};
			}
		}

		const publication = participant.addSubscribedMediaTrack(
			mediaTrack,
			trackId,
			stream,
			receiver,
			adaptiveStreamSettings,
		);

		if (publication?.isEncrypted && !this.e2eeManager) {
			this.emit(
				RoomEvent.EncryptionError,
				new Error(
					`Encrypted ${publication.source} track received from participant ${participant.sid}, but room does not have encryption enabled!`,
				),
			);
		}
	}

	private cancelPendingTrackAdded(trackSid: Track.SID) {
		this.pendingTrackAddedCallbacks.get(trackSid)?.forEach((cleanup) => cleanup());
	}

	private handleLocalTrackSubscribed(subscribedSid: string) {
		const findPublication = () =>
			this.localParticipant.getTrackPublications().find(({trackSid}) => trackSid === subscribedSid) as
				| LocalTrackPublication
				| undefined;

		const trackPublication = findPublication();
		if (trackPublication) {
			this.emitLocalTrackSubscribed(trackPublication);
			return;
		}

		this.log.debug('deferring LocalTrackSubscribed, publication not yet available', {
			subscribedSid,
		});

		const TIMEOUT_MS = 10_000;
		let timer: ReturnType<typeof setTimeout>;

		const onPublished = (pub: LocalTrackPublication) => {
			if (pub.trackSid === subscribedSid) {
				cleanup();
				this.emitLocalTrackSubscribed(pub);
			}
		};

		const cleanup = () => {
			clearTimeout(timer);
			this.localParticipant.off(ParticipantEvent.LocalTrackPublished, onPublished);
			this.off(RoomEvent.Disconnected, cleanup);
		};

		this.localParticipant.on(ParticipantEvent.LocalTrackPublished, onPublished);
		this.once(RoomEvent.Disconnected, cleanup);

		timer = setTimeout(() => {
			cleanup();
			const pub = findPublication();
			if (pub) {
				this.emitLocalTrackSubscribed(pub);
			} else {
				this.log.warn('could not find local track publication for LocalTrackSubscribed event after timeout', {
					subscribedSid,
				});
			}
		}, TIMEOUT_MS);
	}

	private emitLocalTrackSubscribed(trackPublication: LocalTrackPublication) {
		this.localParticipant.emit(ParticipantEvent.LocalTrackSubscribed, trackPublication);
		this.emitWhenConnected(RoomEvent.LocalTrackSubscribed, trackPublication, this.localParticipant);
	}

	private handleRestarting = () => {
		this.clearConnectionReconcile();
		this.isResuming = false;

		for (const p of this.remoteParticipants.values()) {
			this.handleParticipantDisconnected(p.identity, p);
		}

		if (this.setAndEmitConnectionState(ConnectionState.Reconnecting)) {
			this.emit(RoomEvent.Reconnecting);
		}
	};

	private handleRestarted = () => {
		this.outgoingDataTrackManager.sfuWillRepublishTracks();
		this.incomingDataTrackManager.resendSubscriptionUpdates();
	};

	private handleSignalRestarted = async (joinResponse: JoinResponse) => {
		this.log.debug(`signal reconnected to server, region ${joinResponse.serverRegion}`, {
			region: joinResponse.serverRegion,
		});
		this.bufferedEvents = [];

		this.applyJoinResponse(joinResponse);

		try {
			await this.localParticipant.republishAllTracks(undefined, true);
		} catch (error) {
			this.log.error('error trying to re-publish tracks after reconnection', {error});
		}

		try {
			await this.engine.waitForRestarted();
			this.log.debug(`fully reconnected to server`, {region: joinResponse.serverRegion});
		} catch {
			return;
		}
		this.setAndEmitConnectionState(ConnectionState.Connected);
		this.emit(RoomEvent.Reconnected);
		this.registerConnectionReconcile();
		this.emitBufferedEvents();
	};

	private handleDisconnect(shouldStopTracks = true, reason?: DisconnectReason) {
		this.clearConnectionReconcile();
		this.isResuming = false;
		this.bufferedEvents = [];
		this.transcriptionReceivedTimes.clear();
		this.incomingDataStreamManager.clearControllers();
		this.incomingDataTrackManager.reset();
		this.outgoingDataTrackManager.reset();
		if (this.state === ConnectionState.Disconnected) {
			return;
		}

		this.regionUrl = undefined;

		if (this.regionUrlProvider) {
			this.regionUrlProvider.notifyDisconnected();
		}

		try {
			this.remoteParticipants.forEach((p) => {
				p.trackPublications.forEach((pub) => {
					p.unpublishTrack(pub.trackSid);
				});
			});

			this.localParticipant.trackPublications.forEach((pub) => {
				if (pub.track) {
					this.localParticipant.unpublishTrack(pub.track, shouldStopTracks);
				}
				if (shouldStopTracks) {
					pub.track?.detach();
					pub.track?.stop();
				} else {
					pub.track?.stopMonitor();
				}
			});

			this.localParticipant
				.off(ParticipantEvent.ParticipantMetadataChanged, this.onLocalParticipantMetadataChanged)
				.off(ParticipantEvent.ParticipantNameChanged, this.onLocalParticipantNameChanged)
				.off(ParticipantEvent.AttributesChanged, this.onLocalAttributesChanged)
				.off(ParticipantEvent.TrackMuted, this.onLocalTrackMuted)
				.off(ParticipantEvent.TrackUnmuted, this.onLocalTrackUnmuted)
				.off(ParticipantEvent.LocalTrackPublished, this.onLocalTrackPublished)
				.off(ParticipantEvent.LocalTrackUnpublished, this.onLocalTrackUnpublished)
				.off(ParticipantEvent.ConnectionQualityChanged, this.onLocalConnectionQualityChanged)
				.off(ParticipantEvent.MediaDevicesError, this.onMediaDevicesError)
				.off(ParticipantEvent.AudioStreamAcquired, this.startAudio)
				.off(ParticipantEvent.ChatMessage, this.onLocalChatMessageSent)
				.off(ParticipantEvent.ParticipantPermissionsChanged, this.onLocalParticipantPermissionsChanged);

			this.localParticipant.trackPublications.clear();
			this.localParticipant.videoTrackPublications.clear();
			this.localParticipant.audioTrackPublications.clear();

			this.remoteParticipants.clear();
			this.sidToIdentity.clear();
			this.activeSpeakers = [];
			if (this.audioContext && typeof this.options.webAudioMix === 'boolean') {
				this.audioContext.close();
				this.audioContext = undefined;
			}
			if (isWeb()) {
				window.removeEventListener('beforeunload', this.onPageLeave);
				window.removeEventListener('pagehide', this.onPageLeave);
				window.removeEventListener('freeze', this.onPageLeave);
				navigator.mediaDevices?.removeEventListener?.('devicechange', this.handleDeviceChange);
			}
		} finally {
			this.setAndEmitConnectionState(ConnectionState.Disconnected);
			this.emit(RoomEvent.Disconnected, reason);
		}
	}

	private handleParticipantUpdates = (participantInfos: Array<ParticipantInfo>) => {
		for (const info of participantInfos) {
			if (info.identity === this.localParticipant.identity) {
				this.localParticipant.updateInfo(info);
				continue;
			}

			if (info.identity === '') {
				info.identity = this.sidToIdentity.get(info.sid) ?? '';
			}

			const remoteParticipant = this.remoteParticipants.get(info.identity);

			if (info.state === ParticipantInfo_State.DISCONNECTED) {
				this.handleParticipantDisconnected(
					info.identity,
					remoteParticipant,
					info.disconnectReason === DisconnectReason.UNKNOWN_REASON ? undefined : info.disconnectReason,
				);
			} else {
				this.getOrCreateParticipant(info.identity, info);
			}
		}

		const mapped = new Map(
			participantInfos
				.filter((p) => p.identity !== this.localParticipant.identity)
				.map((info) => {
					return [info.identity, info.dataTracks.map((dataTrack) => DataTrackInfo.from(dataTrack))];
				}),
		);
		this.incomingDataTrackManager.receiveSfuPublicationUpdates(mapped);
	};

	private handleParticipantDisconnected(
		identity: string,
		participant?: RemoteParticipant,
		disconnectReason?: DisconnectReason,
	) {
		this.remoteParticipants.delete(identity);
		if (!participant) {
			return;
		}

		this.incomingDataStreamManager.validateParticipantHasNoActiveDataStreams(identity);
		this.incomingDataTrackManager.handleRemoteParticipantDisconnected(identity);

		participant.trackPublications.forEach((publication) => {
			participant.unpublishTrack(publication.trackSid, true);
		});
		this.emit(RoomEvent.ParticipantDisconnected, participant, disconnectReason);
		participant.setDisconnected();
		this.rpcClientManager.handleParticipantDisconnected(participant.identity);
	}

	private handleActiveSpeakersUpdate = (speakers: Array<SpeakerInfo>) => {
		const activeSpeakers: Array<Participant> = [];
		const seenSids = new Set<string>();
		speakers.forEach((speaker) => {
			seenSids.add(speaker.sid);
			if (speaker.sid === this.localParticipant.sid) {
				this.localParticipant.audioLevel = speaker.level;
				this.localParticipant.setIsSpeaking(true);
				activeSpeakers.push(this.localParticipant);
			} else {
				const p = this.getRemoteParticipantBySid(speaker.sid);
				if (p) {
					p.audioLevel = speaker.level;
					p.setIsSpeaking(true);
					activeSpeakers.push(p);
				}
			}
		});

		if (!seenSids.has(this.localParticipant.sid)) {
			this.localParticipant.audioLevel = 0;
			this.localParticipant.setIsSpeaking(false);
		}
		this.remoteParticipants.forEach((p) => {
			if (!seenSids.has(p.sid)) {
				p.audioLevel = 0;
				p.setIsSpeaking(false);
			}
		});

		this.activeSpeakers = activeSpeakers;
		this.emitWhenConnected(RoomEvent.ActiveSpeakersChanged, activeSpeakers);
	};

	private handleSpeakersChanged = (speakerUpdates: Array<SpeakerInfo>) => {
		const lastSpeakers = new Map<string, Participant>();
		this.activeSpeakers.forEach((p) => {
			const remoteParticipant = this.remoteParticipants.get(p.identity);
			if (remoteParticipant && remoteParticipant.sid !== p.sid) {
				return;
			}
			lastSpeakers.set(p.sid, p);
		});
		speakerUpdates.forEach((speaker) => {
			let p: Participant | undefined = this.getRemoteParticipantBySid(speaker.sid);
			if (speaker.sid === this.localParticipant.sid) {
				p = this.localParticipant;
			}
			if (!p) {
				return;
			}
			p.audioLevel = speaker.level;
			p.setIsSpeaking(speaker.active);

			if (speaker.active) {
				lastSpeakers.set(speaker.sid, p);
			} else {
				lastSpeakers.delete(speaker.sid);
			}
		});
		const activeSpeakers = Array.from(lastSpeakers.values());
		activeSpeakers.sort((a, b) => b.audioLevel - a.audioLevel);
		this.activeSpeakers = activeSpeakers;
		this.emitWhenConnected(RoomEvent.ActiveSpeakersChanged, activeSpeakers);
	};

	private handleStreamStateUpdate = (streamStateUpdate: StreamStateUpdate) => {
		streamStateUpdate.streamStates.forEach((streamState) => {
			const participant = this.getRemoteParticipantBySid(streamState.participantSid);
			if (!participant) {
				return;
			}
			const pub = participant.getTrackPublicationBySid(streamState.trackSid);
			if (!pub?.track) {
				return;
			}
			const newStreamState = Track.streamStateFromProto(streamState.state);
			const prevStreamState = pub.track.streamState;
			pub.track.setStreamState(newStreamState);
			if (newStreamState !== prevStreamState) {
				participant.emit(ParticipantEvent.TrackStreamStateChanged, pub, pub.track.streamState);
				this.emitWhenConnected(RoomEvent.TrackStreamStateChanged, pub, pub.track.streamState, participant);
			}
		});
	};

	private handleSubscriptionPermissionUpdate = (update: SubscriptionPermissionUpdate) => {
		const participant = this.getRemoteParticipantBySid(update.participantSid);
		if (!participant) {
			return;
		}
		const pub = participant.getTrackPublicationBySid(update.trackSid);
		if (!pub) {
			return;
		}

		pub.setAllowed(update.allowed);
	};

	private handleSubscriptionError = (update: SubscriptionResponse) => {
		this.cancelPendingTrackAdded(update.trackSid);

		const participant = Array.from(this.remoteParticipants.values()).find((p) =>
			p.trackPublications.has(update.trackSid),
		);
		if (!participant) {
			return;
		}
		const pub = participant.getTrackPublicationBySid(update.trackSid);
		if (!pub) {
			return;
		}

		pub.setSubscriptionError(update.err);
	};

	private handleDataPacket = (packet: DataPacket, encryptionType: Encryption_Type) => {
		const participant = this.remoteParticipants.get(packet.participantIdentity);
		if (packet.value.case === 'user') {
			this.handleUserPacket(participant, packet.value.value, packet.kind, encryptionType);
		} else if (packet.value.case === 'transcription') {
			this.handleTranscription(participant, packet.value.value);
		} else if (packet.value.case === 'sipDtmf') {
			this.handleSipDtmf(participant, packet.value.value);
		} else if (packet.value.case === 'chatMessage') {
			this.handleChatMessage(participant, packet.value.value);
		} else if (packet.value.case === 'metrics') {
			this.handleMetrics(packet.value.value, participant);
		} else if (
			packet.value.case === 'streamHeader' ||
			packet.value.case === 'streamChunk' ||
			packet.value.case === 'streamTrailer'
		) {
			this.handleDataStream(packet, encryptionType);
		} else if (packet.value.case === 'rpcRequest') {
			const rpc = packet.value.value;
			this.rpcServerManager.handleIncomingRpcRequest(packet.participantIdentity, rpc);
		} else if (packet.value.case === 'rpcResponse') {
			const rpcResponse = packet.value.value;
			switch (rpcResponse.value.case) {
				case 'payload':
					this.rpcClientManager.handleIncomingRpcResponseSuccess(rpcResponse.requestId, rpcResponse.value.value);
					break;
				case 'error':
					this.rpcClientManager.handleIncomingRpcResponseFailure(
						rpcResponse.requestId,
						RpcError.fromProto(rpcResponse.value.value),
					);
					break;
				default:
					this.log.warn(`Unknown rpcResponse.value.case: ${rpcResponse.value.case}`, this.logContext);
					break;
			}
		} else if (packet.value.case === 'rpcAck') {
			this.rpcClientManager.handleIncomingRpcAck(packet.value.value.requestId);
		}
	};

	private handleUserPacket = (
		participant: RemoteParticipant | undefined,
		userPacket: UserPacket,
		kind: DataPacket_Kind,
		encryptionType: Encryption_Type,
	) => {
		this.emit(
			RoomEvent.DataReceived,
			userPacket.payload as NonSharedUint8Array,
			participant,
			kind,
			userPacket.topic,
			encryptionType,
		);

		participant?.emit(ParticipantEvent.DataReceived, userPacket.payload as NonSharedUint8Array, kind, encryptionType);
	};

	private handleSipDtmf = (participant: RemoteParticipant | undefined, dtmf: SipDTMF) => {
		this.emit(RoomEvent.SipDTMFReceived, dtmf, participant);

		participant?.emit(ParticipantEvent.SipDTMFReceived, dtmf);
	};

	private handleTranscription = (
		_remoteParticipant: RemoteParticipant | undefined,
		transcription: TranscriptionModel,
	) => {
		const participant =
			transcription.transcribedParticipantIdentity === this.localParticipant.identity
				? this.localParticipant
				: this.getParticipantByIdentity(transcription.transcribedParticipantIdentity);
		const publication = participant?.trackPublications.get(transcription.trackId);

		const segments = extractTranscriptionSegments(transcription, this.transcriptionReceivedTimes);

		publication?.emit(TrackEvent.TranscriptionReceived, segments);
		participant?.emit(ParticipantEvent.TranscriptionReceived, segments, publication);
		this.emit(RoomEvent.TranscriptionReceived, segments, participant, publication);
	};

	private handleChatMessage = (participant: RemoteParticipant | undefined, chatMessage: ChatMessageModel) => {
		const msg = extractChatMessage(chatMessage);
		this.emit(RoomEvent.ChatMessage, msg, participant);
	};

	private handleMetrics = (metrics: MetricsBatch, participant?: Participant) => {
		this.emit(RoomEvent.MetricsReceived, metrics, participant);
	};

	private handleDataStream = (packet: DataPacket, encryptionType: Encryption_Type) => {
		this.incomingDataStreamManager.handleDataStreamPacket(packet, encryptionType);
	};

	bufferedSegments: Map<string, TranscriptionSegmentModel> = new Map();

	private handleAudioPlaybackStarted = () => {
		if (this.canPlaybackAudio) {
			return;
		}
		this.audioEnabled = true;
		this.emit(RoomEvent.AudioPlaybackStatusChanged, true);
	};
	private handleAudioPlaybackFailed = (e: unknown) => {
		this.log.warn('could not playback audio', {error: e});
		if (!this.canPlaybackAudio) {
			return;
		}
		this.audioEnabled = false;
		this.emit(RoomEvent.AudioPlaybackStatusChanged, false);
	};

	private handleVideoPlaybackStarted = () => {
		if (this.isVideoPlaybackBlocked) {
			this.isVideoPlaybackBlocked = false;
			this.emit(RoomEvent.VideoPlaybackStatusChanged, true);
		}
	};

	private handleVideoPlaybackFailed = () => {
		if (!this.isVideoPlaybackBlocked) {
			this.isVideoPlaybackBlocked = true;
			this.emit(RoomEvent.VideoPlaybackStatusChanged, false);
		}
	};

	private async selectDefaultDevices() {
		const previousDevices = DeviceManager.getInstance().previousDevices;
		const availableDevices = await DeviceManager.getInstance().getDevices(undefined, false);
		const browser = getBrowser();
		if (browser?.name === 'Chrome' && browser.os !== 'iOS') {
			for (const availableDevice of availableDevices) {
				const previousDevice = previousDevices.find((info) => info.deviceId === availableDevice.deviceId);
				if (
					previousDevice &&
					previousDevice.label !== '' &&
					previousDevice.kind === availableDevice.kind &&
					previousDevice.label !== availableDevice.label
				) {
					if (this.getActiveDevice(availableDevice.kind) === 'default') {
						this.emit(RoomEvent.ActiveDeviceChanged, availableDevice.kind, availableDevice.deviceId);
					}
				}
			}
		}

		const kinds: Array<MediaDeviceKind> = ['audiooutput', 'audioinput', 'videoinput'];
		for (const kind of kinds) {
			const targetSource = kindToSource(kind);
			const targetPublication = this.localParticipant.getTrackPublication(targetSource);
			if (targetPublication?.track?.isUserProvided) {
				continue;
			}
			const devicesOfKind = availableDevices.filter((d) => d.kind === kind);
			const activeDevice = this.getActiveDevice(kind);

			if (activeDevice === previousDevices.filter((info) => info.kind === kind)[0]?.deviceId) {
				if (devicesOfKind.length > 0 && devicesOfKind[0]?.deviceId !== activeDevice) {
					await this.switchActiveDevice(kind, devicesOfKind[0].deviceId);
					continue;
				}
			}

			if ((kind === 'audioinput' && !isSafariBased()) || kind === 'videoinput') {
				continue;
			}
			if (
				devicesOfKind.length > 0 &&
				!devicesOfKind.find((deviceInfo) => deviceInfo.deviceId === this.getActiveDevice(kind)) &&
				(kind !== 'audiooutput' || !isSafariBased())
			) {
				await this.switchActiveDevice(kind, devicesOfKind[0].deviceId);
			}
		}
	}

	private handleDeviceChange = async () => {
		if (getBrowser()?.os !== 'iOS') {
			await this.selectDefaultDevices();
		}
		this.emit(RoomEvent.MediaDevicesChanged);
	};

	private handleRoomUpdate = (room: RoomModel) => {
		const oldRoom = this.roomInfo;
		this.roomInfo = room;
		if (oldRoom && oldRoom.metadata !== room.metadata) {
			this.emitWhenConnected(RoomEvent.RoomMetadataChanged, room.metadata);
		}
		if (oldRoom?.activeRecording !== room.activeRecording) {
			this.emitWhenConnected(RoomEvent.RecordingStatusChanged, room.activeRecording);
		}
	};

	private handleConnectionQualityUpdate = (update: ConnectionQualityUpdate) => {
		update.updates.forEach((info) => {
			if (info.participantSid === this.localParticipant.sid) {
				this.localParticipant.setConnectionQuality(info.quality);
				return;
			}
			const participant = this.getRemoteParticipantBySid(info.participantSid);
			if (participant) {
				participant.setConnectionQuality(info.quality);
			}
		});
	};

	private async acquireAudioContext() {
		if (typeof this.options.webAudioMix !== 'boolean' && this.options.webAudioMix.audioContext) {
			this.audioContext = this.options.webAudioMix.audioContext;
		} else if (!this.audioContext || this.audioContext.state === 'closed') {
			this.audioContext = getNewAudioContext() ?? undefined;
		}

		if (this.options.webAudioMix) {
			this.remoteParticipants.forEach((participant) => participant.setAudioContext(this.audioContext));
		}

		this.localParticipant.setAudioContext(this.audioContext);

		if (this.audioContext && this.audioContext.state === 'suspended') {
			try {
				await Promise.race([this.audioContext.resume(), sleep(200)]);
			} catch (e: unknown) {
				this.log.warn('Could not resume audio context', {error: e});
			}
		}

		const newContextIsRunning = this.audioContext?.state === 'running';
		if (newContextIsRunning !== this.canPlaybackAudio) {
			this.audioEnabled = newContextIsRunning;
			this.emit(RoomEvent.AudioPlaybackStatusChanged, newContextIsRunning);
		}
	}

	private createParticipant(identity: string, info?: ParticipantInfo): RemoteParticipant {
		let participant: RemoteParticipant;
		if (info) {
			participant = RemoteParticipant.fromParticipantInfo(
				this.engine.client,
				info,
				{
					loggerContextCb: () => this.logContext,
					loggerName: this.options.loggerName,
				},
				this.incomingDataTrackManager,
			);
		} else {
			participant = new RemoteParticipant(this.engine.client, '', identity, undefined, undefined, undefined, {
				loggerContextCb: () => this.logContext,
				loggerName: this.options.loggerName,
			});
		}
		if (this.options.webAudioMix) {
			participant.setAudioContext(this.audioContext);
		}
		if (this.options.audioOutput?.deviceId) {
			participant
				.setAudioOutput(this.options.audioOutput)
				.catch((e) => this.log.warn(`Could not set audio output: ${e.message}`));
		}
		return participant;
	}

	private getOrCreateParticipant(identity: string, info: ParticipantInfo): RemoteParticipant {
		if (this.remoteParticipants.has(identity)) {
			const existingParticipant = this.remoteParticipants.get(identity)!;
			if (info) {
				const wasUpdated = existingParticipant.updateInfo(info);
				if (wasUpdated) {
					this.sidToIdentity.set(info.sid, info.identity);
				}
			}
			return existingParticipant;
		}
		const participant = this.createParticipant(identity, info);
		this.remoteParticipants.set(identity, participant);

		this.sidToIdentity.set(info.sid, info.identity);
		this.emitWhenConnected(RoomEvent.ParticipantConnected, participant);

		participant
			.on(ParticipantEvent.TrackPublished, (trackPublication: RemoteTrackPublication) => {
				this.emitWhenConnected(RoomEvent.TrackPublished, trackPublication, participant);
			})
			.on(ParticipantEvent.TrackSubscribed, (track: RemoteTrack, publication: RemoteTrackPublication) => {
				if (track.kind === Track.Kind.Audio) {
					track.on(TrackEvent.AudioPlaybackStarted, this.handleAudioPlaybackStarted);
					track.on(TrackEvent.AudioPlaybackFailed, this.handleAudioPlaybackFailed);
				} else if (track.kind === Track.Kind.Video) {
					track.on(TrackEvent.VideoPlaybackFailed, this.handleVideoPlaybackFailed);
					track.on(TrackEvent.VideoPlaybackStarted, this.handleVideoPlaybackStarted);
				}
				this.emitWhenConnected(RoomEvent.TrackSubscribed, track, publication, participant);
			})
			.on(ParticipantEvent.TrackUnpublished, (publication: RemoteTrackPublication) => {
				this.cancelPendingTrackAdded(publication.trackSid);
				this.emit(RoomEvent.TrackUnpublished, publication, participant);
			})
			.on(ParticipantEvent.TrackUnsubscribed, (track: RemoteTrack, publication: RemoteTrackPublication) => {
				this.emit(RoomEvent.TrackUnsubscribed, track, publication, participant);
			})
			.on(ParticipantEvent.TrackMuted, (pub: TrackPublication) => {
				this.emitWhenConnected(RoomEvent.TrackMuted, pub, participant);
			})
			.on(ParticipantEvent.TrackUnmuted, (pub: TrackPublication) => {
				this.emitWhenConnected(RoomEvent.TrackUnmuted, pub, participant);
			})
			.on(ParticipantEvent.ParticipantMetadataChanged, (metadata: string | undefined) => {
				this.emitWhenConnected(RoomEvent.ParticipantMetadataChanged, metadata, participant);
			})
			.on(ParticipantEvent.ParticipantNameChanged, (name) => {
				this.emitWhenConnected(RoomEvent.ParticipantNameChanged, name, participant);
			})
			.on(ParticipantEvent.AttributesChanged, (changedAttributes: Record<string, string>) => {
				this.emitWhenConnected(RoomEvent.ParticipantAttributesChanged, changedAttributes, participant);
			})
			.on(ParticipantEvent.ConnectionQualityChanged, (quality: ConnectionQuality) => {
				this.emitWhenConnected(RoomEvent.ConnectionQualityChanged, quality, participant);
			})
			.on(ParticipantEvent.ParticipantPermissionsChanged, (prevPermissions?: ParticipantPermission) => {
				this.emitWhenConnected(RoomEvent.ParticipantPermissionsChanged, prevPermissions, participant);
			})
			.on(ParticipantEvent.TrackSubscriptionStatusChanged, (pub, status) => {
				this.emitWhenConnected(RoomEvent.TrackSubscriptionStatusChanged, pub, status, participant);
			})
			.on(ParticipantEvent.TrackSubscriptionFailed, (trackSid, error) => {
				this.emit(RoomEvent.TrackSubscriptionFailed, trackSid, participant, error);
			})
			.on(ParticipantEvent.TrackSubscriptionPermissionChanged, (pub, status) => {
				this.emitWhenConnected(RoomEvent.TrackSubscriptionPermissionChanged, pub, status, participant);
			})
			.on(ParticipantEvent.Active, () => {
				this.emitWhenConnected(RoomEvent.ParticipantActive, participant);
				if (participant.kind === ParticipantKind.AGENT) {
					this.localParticipant.setActiveAgent(participant);
				}
			});

		if (info) {
			participant.updateInfo(info);
		}
		return participant;
	}

	private sendSyncState() {
		const remoteTracks = Array.from(this.remoteParticipants.values()).reduce(
			(acc, participant) => {
				acc.push(...(participant.getTrackPublications() as Array<RemoteTrackPublication>));
				return acc;
			},
			[] as Array<RemoteTrackPublication>,
		);
		const localTracks = this.localParticipant.getTrackPublications() as Array<LocalTrackPublication>;
		const localDataTrackInfos = this.outgoingDataTrackManager.queryPublished();
		this.engine.sendSyncState(remoteTracks, localTracks, localDataTrackInfos);
	}

	private updateSubscriptions() {
		for (const p of this.remoteParticipants.values()) {
			for (const pub of p.videoTrackPublications.values()) {
				if (pub.isSubscribed && isRemotePub(pub)) {
					pub.emitTrackUpdate();
				}
			}
		}
	}

	private getRemoteParticipantBySid(sid: string): RemoteParticipant | undefined {
		const identity = this.sidToIdentity.get(sid);
		if (identity) {
			return this.remoteParticipants.get(identity);
		}
		return undefined;
	}

	private getClientInfoCapabilities(roomOptions: InternalRoomOptions): Array<ClientInfo_Capability> {
		const capabilities: Array<ClientInfo_Capability> = [];
		if (isFrameMetadataSupported(roomOptions.frameMetadata ?? roomOptions.packetTrailer) || this.e2eeManager) {
			capabilities.push(ClientInfo_Capability.CAP_PACKET_TRAILER);
		}
		if (isCompressionStreamSupported()) {
			capabilities.push(ClientInfo_Capability.CAP_COMPRESSION_DEFLATE_RAW);
		}
		return capabilities;
	}

	private getRemoteParticipantClientProtocol = (identity: Participant['identity']) => {
		return this.remoteParticipants.get(identity)?.clientProtocol ?? CLIENT_PROTOCOL_DEFAULT;
	};

	private getRemoteParticipantCapabilities = (identity: Participant['identity']): Array<ClientInfo_Capability> => {
		return this.remoteParticipants.get(identity)?.capabilities ?? [];
	};

	private getAllRemoteParticipantIdentities = () => {
		return Array.from(this.remoteParticipants.keys());
	};

	private registerRpcDataStreamHandler() {
		this.incomingDataStreamManager.registerTextStreamHandler(
			RPC_REQUEST_DATA_STREAM_TOPIC,
			async (reader, {identity}) => {
				const attributes = reader.info.attributes ?? {};
				await this.rpcServerManager.handleIncomingDataStream(reader, identity, attributes);
			},
		);
		this.incomingDataStreamManager.registerTextStreamHandler(
			RPC_RESPONSE_DATA_STREAM_TOPIC,
			async (reader, {identity}) => {
				const attributes = reader.info.attributes ?? {};
				await this.rpcClientManager.handleIncomingDataStream(reader, identity, attributes);
			},
		);
	}

	private setStatsLogging(enabled: boolean) {
		if (enabled) {
			if (!this.statsLogInterval) {
				this.statsLogInterval = CriticalTimers.setInterval(() => {
					this.logWebRTCStats();
				}, STATS_LOG_FREQUENCY_MS);
			}
		} else if (this.statsLogInterval) {
			CriticalTimers.clearInterval(this.statsLogInterval);
			this.statsLogInterval = undefined;
		}
	}

	private logWebRTCStats = async () => {
		const pcManager = this.engine?.pcManager;
		if (!pcManager) {
			return;
		}
		try {
			const [publisher, subscriber] = await Promise.all([
				pcManager.publisher.getStats(),
				pcManager.subscriber?.getStats(),
			]);
			const publisherStats = publisher && summarizeStatsReport(publisher);
			const subscriberStats = subscriber && summarizeStatsReport(subscriber);
			this.statsLog.info(`webrtc stats`, {
				publisher: publisherStats?.connection,
				subscriber: subscriberStats?.connection,
				inbound: [...(publisherStats?.inbound ?? []), ...(subscriberStats?.inbound ?? [])],
				outbound: publisherStats?.outbound,
			});
		} catch (error) {
			this.statsLog.debug('could not collect webrtc stats', {error});
		}
	};

	private registerConnectionReconcile() {
		this.clearConnectionReconcile();
		let consecutiveFailures = 0;
		this.connectionReconcileInterval = CriticalTimers.setInterval(() => {
			if (!this.engine || this.engine.isClosed || !this.engine.verifyTransport()) {
				consecutiveFailures++;
				this.log.warn('detected connection state mismatch', {
					numFailures: consecutiveFailures,
					engine: this.engine
						? {
								closed: this.engine.isClosed,
								transportsConnectedOrConnecting: this.engine.verifyTransport(),
							}
						: undefined,
				});
				if (consecutiveFailures >= 3) {
					this.clearConnectionReconcile();
					if (this.engine && !this.engine.isClosed) {
						this.log.warn('detected connection state mismatch, attempting full reconnect');
						this.engine.reconnect();
					} else {
						this.recreateEngine();
						this.handleDisconnect(this.options.stopLocalTrackOnUnpublish, DisconnectReason.STATE_MISMATCH);
					}
				}
			} else {
				consecutiveFailures = 0;
			}
		}, CONNECTION_RECONCILE_FREQUENCY_MS);
	}

	private clearConnectionReconcile() {
		if (this.connectionReconcileInterval) {
			CriticalTimers.clearInterval(this.connectionReconcileInterval);
		}
	}

	private setAndEmitConnectionState(state: ConnectionState): boolean {
		if (state === this.state) {
			return false;
		}
		this.log.info(`connection state changed: ${this.state} -> ${state}`);
		this.state = state;
		this.incomingDataStreamManager.setConnected(state === ConnectionState.Connected);
		this.setStatsLogging(state === ConnectionState.Connected);

		this.emit(RoomEvent.ConnectionStateChanged, this.state);

		return true;
	}

	private emitBufferedEvents() {
		this.bufferedEvents.forEach(({event, args}) => this.emitBufferedEvent(event, args));
		this.bufferedEvents = [];
	}

	private emitBufferedEvent<E extends keyof RoomEventCallbacks>(event: E, args: RoomEventArguments<E>) {
		this.emit(event, ...args);
	}

	private emitWhenConnected<E extends keyof RoomEventCallbacks>(event: E, ...args: RoomEventArguments<E>): boolean {
		if (
			this.state === ConnectionState.Reconnecting ||
			this.isResuming ||
			!this.engine ||
			this.engine.pendingReconnect
		) {
			this.bufferedEvents.push({event, args} as BufferedRoomEvent);
		} else if (this.state === ConnectionState.Connected) {
			return this.emit(event, ...args);
		}
		return false;
	}

	private onLocalParticipantMetadataChanged = (metadata: string | undefined) => {
		this.emit(RoomEvent.ParticipantMetadataChanged, metadata, this.localParticipant);
	};

	private onLocalParticipantNameChanged = (name: string) => {
		this.emit(RoomEvent.ParticipantNameChanged, name, this.localParticipant);
	};

	private onLocalAttributesChanged = (changedAttributes: Record<string, string>) => {
		this.emit(RoomEvent.ParticipantAttributesChanged, changedAttributes, this.localParticipant);
	};

	private onLocalTrackMuted = (pub: TrackPublication) => {
		this.emit(RoomEvent.TrackMuted, pub, this.localParticipant);
	};

	private onLocalTrackUnmuted = (pub: TrackPublication) => {
		this.emit(RoomEvent.TrackUnmuted, pub, this.localParticipant);
	};

	private onTrackProcessorUpdate = (processor?: TrackProcessorEventValue) => {
		processor?.onPublish?.(this);
	};

	private onLocalTrackPublished = async (pub: LocalTrackPublication) => {
		pub.track?.on(TrackEvent.TrackProcessorUpdate, this.onTrackProcessorUpdate);
		pub.track?.on(TrackEvent.Restarted, this.onLocalTrackRestarted);
		pub.track?.getProcessor()?.onPublish?.(this);

		this.emit(RoomEvent.LocalTrackPublished, pub, this.localParticipant);

		if (isLocalAudioTrack(pub.track)) {
			const trackIsSilent = await pub.track.checkForSilence();
			if (trackIsSilent) {
				this.emit(RoomEvent.LocalAudioSilenceDetected, pub);
			}
		}
		const deviceId = await pub.track?.getDeviceId(false);
		const deviceKind = sourceToKind(pub.source);
		if (deviceKind && deviceId && deviceId !== this.localParticipant.activeDeviceMap.get(deviceKind)) {
			this.localParticipant.activeDeviceMap.set(deviceKind, deviceId);
			this.emit(RoomEvent.ActiveDeviceChanged, deviceKind, deviceId);
		}
	};

	private onLocalTrackUnpublished = (pub: LocalTrackPublication) => {
		pub.track?.off(TrackEvent.TrackProcessorUpdate, this.onTrackProcessorUpdate);
		pub.track?.off(TrackEvent.Restarted, this.onLocalTrackRestarted);
		this.emit(RoomEvent.LocalTrackUnpublished, pub, this.localParticipant);
	};

	private onLocalTrackRestarted = async (track: LocalTrack) => {
		const deviceId = await track.getDeviceId(false);
		const deviceKind = sourceToKind(track.source);
		if (deviceKind && deviceId && deviceId !== this.localParticipant.activeDeviceMap.get(deviceKind)) {
			this.log.debug(`local track restarted, setting ${deviceKind} ${deviceId} active`);
			this.localParticipant.activeDeviceMap.set(deviceKind, deviceId);
			this.emit(RoomEvent.ActiveDeviceChanged, deviceKind, deviceId);
		}
	};

	private onLocalConnectionQualityChanged = (quality: ConnectionQuality) => {
		this.emit(RoomEvent.ConnectionQualityChanged, quality, this.localParticipant);
	};

	private onMediaDevicesError = (e: Error, kind?: MediaDeviceKind) => {
		this.emit(RoomEvent.MediaDevicesError, e, kind);
	};

	private onLocalParticipantPermissionsChanged = (prevPermissions?: ParticipantPermission) => {
		this.emit(RoomEvent.ParticipantPermissionsChanged, prevPermissions, this.localParticipant);
	};

	private onLocalChatMessageSent = (msg: ChatMessage) => {
		this.emit(RoomEvent.ChatMessage, msg, this.localParticipant);
	};

	async simulateParticipants(options: SimulationOptions) {
		const publishOptions = {
			audio: true,
			video: true,
			useRealTracks: false,
			...options.publish,
		};
		const participantOptions = {
			count: 9,
			audio: false,
			video: true,
			aspectRatios: [1.66, 1.7, 1.3],
			...options.participants,
		};
		this.handleDisconnect();
		this.roomInfo = new RoomModel({
			sid: 'RM_SIMULATED',
			name: 'simulated-room',
			emptyTimeout: 0,
			maxParticipants: 0,
			creationTime: protoInt64.parse(Date.now()),
			metadata: '',
			numParticipants: 1,
			numPublishers: 1,
			turnPassword: '',
			enabledCodecs: [],
			activeRecording: false,
		});

		this.localParticipant.updateInfo(
			new ParticipantInfo({
				identity: 'simulated-local',
				name: 'local-name',
			}),
		);
		this.setupLocalParticipantEvents();
		this.emit(RoomEvent.SignalConnected);
		this.emit(RoomEvent.Connected);
		this.setAndEmitConnectionState(ConnectionState.Connected);
		if (publishOptions.video) {
			const camPub = new LocalTrackPublication(
				Track.Kind.Video,
				new TrackInfo({
					source: TrackSource.CAMERA,
					sid: Math.floor(Math.random() * 10_000).toString(),
					type: TrackType.AUDIO,
					name: 'video-dummy',
				}),
				new LocalVideoTrack(
					publishOptions.useRealTracks && window.navigator.mediaDevices?.getUserMedia
						? (await window.navigator.mediaDevices.getUserMedia({video: true})).getVideoTracks()[0]
						: createDummyVideoStreamTrack(160 * (participantOptions.aspectRatios[0] ?? 1), 160, true, true),
					undefined,
					false,
					{loggerName: this.options.loggerName, loggerContextCb: () => this.logContext},
				),
				{loggerName: this.options.loggerName, loggerContextCb: () => this.logContext},
			);
			this.localParticipant.addTrackPublication(camPub);
			this.localParticipant.emit(ParticipantEvent.LocalTrackPublished, camPub);
		}
		if (publishOptions.audio) {
			const audioPub = new LocalTrackPublication(
				Track.Kind.Audio,
				new TrackInfo({
					source: TrackSource.MICROPHONE,
					sid: Math.floor(Math.random() * 10_000).toString(),
					type: TrackType.AUDIO,
				}),
				new LocalAudioTrack(
					publishOptions.useRealTracks && navigator.mediaDevices?.getUserMedia
						? (await navigator.mediaDevices.getUserMedia({audio: true})).getAudioTracks()[0]
						: getEmptyAudioStreamTrack(),
					undefined,
					false,
					this.audioContext,
					{loggerName: this.options.loggerName, loggerContextCb: () => this.logContext},
				),
				{loggerName: this.options.loggerName, loggerContextCb: () => this.logContext},
			);
			this.localParticipant.addTrackPublication(audioPub);
			this.localParticipant.emit(ParticipantEvent.LocalTrackPublished, audioPub);
		}

		for (let i = 0; i < participantOptions.count - 1; i += 1) {
			const info: ParticipantInfo = new ParticipantInfo({
				sid: Math.floor(Math.random() * 10_000).toString(),
				identity: `simulated-${i}`,
				state: ParticipantInfo_State.ACTIVE,
				tracks: [],
				joinedAt: protoInt64.parse(Date.now()),
			});
			const p = this.getOrCreateParticipant(info.identity, info);
			if (participantOptions.video) {
				const dummyVideo = createDummyVideoStreamTrack(
					160 * (participantOptions.aspectRatios[i % participantOptions.aspectRatios.length] ?? 1),
					160,
					false,
					true,
				);
				const videoTrack = new TrackInfo({
					source: TrackSource.CAMERA,
					sid: Math.floor(Math.random() * 10_000).toString(),
					type: TrackType.AUDIO,
				});
				p.addSubscribedMediaTrack(dummyVideo, videoTrack.sid, new MediaStream([dummyVideo]), new RTCRtpReceiver());
				info.tracks = [...info.tracks, videoTrack];
			}
			if (participantOptions.audio) {
				const dummyTrack = getEmptyAudioStreamTrack();
				const audioTrack = new TrackInfo({
					source: TrackSource.MICROPHONE,
					sid: Math.floor(Math.random() * 10_000).toString(),
					type: TrackType.AUDIO,
				});
				p.addSubscribedMediaTrack(dummyTrack, audioTrack.sid, new MediaStream([dummyTrack]), new RTCRtpReceiver());
				info.tracks = [...info.tracks, audioTrack];
			}

			p.updateInfo(info);
		}
	}

	override emit<E extends keyof RoomEventCallbacks>(event: E, ...args: RoomEventArguments<E>): boolean {
		if (event !== RoomEvent.ActiveSpeakersChanged && event !== RoomEvent.TranscriptionReceived) {
			const minimizedArgs = mapArgs(args).filter((arg: unknown) => arg !== undefined);
			if (event === RoomEvent.TrackSubscribed || event === RoomEvent.TrackUnsubscribed) {
				this.log.trace(`subscribe trace: ${event}`, {event, args: minimizedArgs});
			}
			this.log.debug(`room event ${event}`, {event, args: minimizedArgs});
		}
		return EventEmitter.prototype.emit.call(this, event, ...args);
	}
}

function mapArgs(args: Array<unknown>): Array<unknown> {
	return args.map((arg: unknown) => {
		if (!arg) {
			return;
		}
		if (Array.isArray(arg)) {
			return mapArgs(arg);
		}
		if (typeof arg === 'object') {
			return 'logContext' in arg ? arg.logContext : undefined;
		}
		return arg;
	});
}

export default Room;
export type RoomEventCallbacks = {
	connected: () => void;
	reconnecting: () => void;
	signalReconnecting: () => void;
	reconnected: () => void;
	disconnected: (reason?: DisconnectReason) => void;
	connectionStateChanged: (state: ConnectionState) => void;
	moved: (name: string) => void;
	mediaDevicesChanged: () => void;
	participantConnected: (participant: RemoteParticipant) => void;
	participantDisconnected: (participant: RemoteParticipant, disconnectReason?: DisconnectReason) => void;
	trackPublished: (publication: RemoteTrackPublication, participant: RemoteParticipant) => void;
	trackSubscribed: (track: RemoteTrack, publication: RemoteTrackPublication, participant: RemoteParticipant) => void;
	trackSubscriptionFailed: (trackSid: string, participant: RemoteParticipant, reason?: SubscriptionError) => void;
	trackUnpublished: (publication: RemoteTrackPublication, participant: RemoteParticipant) => void;
	trackUnsubscribed: (track: RemoteTrack, publication: RemoteTrackPublication, participant: RemoteParticipant) => void;
	trackMuted: (publication: TrackPublication, participant: Participant) => void;
	trackUnmuted: (publication: TrackPublication, participant: Participant) => void;
	localTrackPublished: (publication: LocalTrackPublication, participant: LocalParticipant) => void;
	localTrackUnpublished: (publication: LocalTrackPublication, participant: LocalParticipant) => void;
	localAudioSilenceDetected: (publication: LocalTrackPublication) => void;
	participantMetadataChanged: (metadata: string | undefined, participant: RemoteParticipant | LocalParticipant) => void;
	participantNameChanged: (name: string, participant: RemoteParticipant | LocalParticipant) => void;
	participantPermissionsChanged: (
		prevPermissions: ParticipantPermission | undefined,
		participant: RemoteParticipant | LocalParticipant,
	) => void;
	participantAttributesChanged: (
		changedAttributes: Record<string, string>,
		participant: RemoteParticipant | LocalParticipant,
	) => void;
	activeSpeakersChanged: (speakers: Array<Participant>) => void;
	roomMetadataChanged: (metadata: string) => void;
	dataReceived: (
		payload: NonSharedUint8Array,
		participant?: RemoteParticipant,
		kind?: DataPacket_Kind,
		topic?: string,
		encryptionType?: Encryption_Type,
	) => void;
	sipDTMFReceived: (dtmf: SipDTMF, participant?: RemoteParticipant) => void;
	transcriptionReceived: (
		transcription: Array<TranscriptionSegment>,
		participant?: Participant,
		publication?: TrackPublication,
	) => void;
	connectionQualityChanged: (quality: ConnectionQuality, participant: Participant) => void;
	mediaDevicesError: (error: Error, kind?: MediaDeviceKind) => void;
	trackStreamStateChanged: (
		publication: RemoteTrackPublication,
		streamState: Track.StreamState,
		participant: RemoteParticipant,
	) => void;
	trackSubscriptionPermissionChanged: (
		publication: RemoteTrackPublication,
		status: TrackPublication.PermissionStatus,
		participant: RemoteParticipant,
	) => void;
	trackSubscriptionStatusChanged: (
		publication: RemoteTrackPublication,
		status: TrackPublication.SubscriptionStatus,
		participant: RemoteParticipant,
	) => void;
	audioPlaybackChanged: (playing: boolean) => void;
	videoPlaybackChanged: (playing: boolean) => void;
	signalConnected: () => void;
	recordingStatusChanged: (recording: boolean) => void;
	participantEncryptionStatusChanged: (encrypted: boolean, participant?: Participant) => void;
	encryptionError: (error: Error, participant?: Participant) => void;
	dcBufferStatusChanged: (isLow: boolean, kind: DataChannelKind) => void;
	activeDeviceChanged: (kind: MediaDeviceKind, deviceId: string) => void;
	chatMessage: (message: ChatMessage, participant?: RemoteParticipant | LocalParticipant) => void;
	localTrackSubscribed: (publication: LocalTrackPublication, participant: LocalParticipant) => void;
	metricsReceived: (metrics: MetricsBatch, participant?: Participant) => void;
	participantActive: (participant: Participant) => void;
	dataTrackPublished: (track: RemoteDataTrack) => void;
	dataTrackUnpublished: (sid: DataTrackSid) => void;
	localDataTrackPublished: (track: LocalDataTrack) => void;
	localDataTrackUnpublished: (sid: DataTrackSid) => void;
};
