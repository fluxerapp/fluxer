// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {
	type AddTrackRequest,
	ClientConfigSetting,
	type ClientConfiguration,
	type ConnectionQualityUpdate,
	DataChannelInfo,
	DataChannelReceiveState,
	DataPacket,
	DataPacket_Kind,
	type DataTrackSubscriberHandles,
	DisconnectReason,
	EncryptedPacket,
	EncryptedPacketPayload,
	Encryption_Type,
	type JoinResponse,
	type LeaveRequest,
	LeaveRequest_Action,
	type MediaSectionsRequirement,
	type ParticipantInfo,
	ConnectionQuality as ProtoConnectionQuality,
	PublishDataTrackResponse,
	ReconnectReason,
	type ReconnectResponse,
	type RegionSettings,
	type RequestResponse,
	type Room as RoomModel,
	type RoomMovedResponse,
	RpcAck,
	type ServerInfo,
	type SessionDescription,
	SignalTarget,
	type SpeakerInfo,
	type StreamStateUpdate,
	type SubscribedQualityUpdate,
	type SubscriptionPermissionUpdate,
	type SubscriptionResponse,
	SyncState,
	type TrackInfo,
	type TrackPublishedResponse,
	type TrackUnpublishedResponse,
	type Transcription,
	type UnpublishDataTrackResponse,
	UpdateSubscription,
	type UserPacket,
} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type {MediaAttributes} from 'sdp-transform';
import type TypedEventEmitter from 'typed-emitter';
import type {SignalOptions} from '../api/SignalClient.ts';
import {SignalClient, SignalConnectionState, toProtoSessionDescription} from '../api/SignalClient.ts';
import type {BaseE2EEManager} from '../e2ee/E2eeManager.ts';
import {asEncryptablePacket, isInsertableStreamSupported} from '../e2ee/utils.ts';
import {
	hasFrameMetadataPublishOptions,
	isFrameMetadataSupported,
	shouldUseFrameMetadataScriptTransform,
} from '../frameMetadata/utils.ts';
import log, {getLogger, LoggerNames} from '../logger.ts';
import type {InternalRoomOptions} from '../options.ts';
import type {NonSharedUint8Array} from '../type-polyfills/non-shared-typed-arrays.ts';
import TypedPromise from '../utils/TypedPromise.ts';
import {TTLMap} from '../utils/ttlmap.ts';
import {DataChannelManager} from './data-channel/DataChannelManager.ts';
import type {FlowControlledDataChannel} from './data-channel/FlowControlledDataChannel.ts';
import type {LossyDataChannel} from './data-channel/LossyDataChannel.ts';
import type {ReliableDataChannel} from './data-channel/ReliableDataChannel.ts';
import {DataChannelKind} from './data-channel/types.ts';
import {DataTrackInfo} from './data-track/types.ts';
import {roomConnectOptionDefaults} from './defaults.ts';
import {
	ConnectionError,
	ConnectionErrorReason,
	NegotiationError,
	PublishDataError,
	SignalReconnectError,
	TrackInvalidError,
	UnexpectedConnectionState,
} from './errors.ts';
import {EngineEvent} from './events.ts';
import type PCTransport from './PCTransport.ts';
import {PCEvents} from './PCTransport.ts';
import {PCTransportManager, PCTransportState} from './PCTransportManager.ts';
import type {ReconnectContext, ReconnectPolicy} from './ReconnectPolicy.ts';
import CriticalTimers, {type TimerHandle} from './timers.ts';
import type LocalTrack from './track/LocalTrack.ts';
import type LocalTrackPublication from './track/LocalTrackPublication.ts';
import type LocalVideoTrack from './track/LocalVideoTrack.ts';
import type {SimulcastTrackInfo} from './track/LocalVideoTrack.ts';
import type {TrackPublishOptions, VideoCodec} from './track/options.ts';
import type RemoteTrackPublication from './track/RemoteTrackPublication.ts';
import type {Track} from './track/Track.ts';
import {getTrackPublicationInfo} from './track/utils.ts';
import type {LoggerOptions} from './types.ts';
import {
	isPublisherOfferWithJoinSupported,
	isReactNative,
	isVideoCodec,
	isVideoTrack,
	isWeb,
	negotiateDependencyDescriptor,
	sleep,
	supportsAddTrack,
	supportsTransceiver,
	toHttpUrl,
} from './utils.ts';

const minReconnectWait = 2 * 1000;
const leaveReconnect = 'leave-reconnect';

const connectionQualityLostTimeout = 10 * 1000;
const reliabeReceiveStateTTL = 30_000;
const initialMediaSectionsAudio = 3;
const initialMediaSectionsVideo = 3;
const videoCodecMimeTypes: Record<VideoCodec, Array<string>> = {
	av1: ['video/av1', 'video/av1x'],
	h265: ['video/h265'],
	h264: ['video/h264'],
	vp9: ['video/vp9'],
	vp8: ['video/vp8'],
};
const h264OpenH264ProfileLevelId = '42e01f';
const h264PreferredHardwareProfileLevelIds = new Set(['42001f']);
type RtpCodecCapability = RTCRtpCapabilities['codecs'][number] & {sdpFmtpLine?: string};

enum PCState {
	New,
	Connected,
	Disconnected,
	Reconnecting,
	Closed,
}

export {DataChannelKind};

const DEFAULT_MAX_MESSAGE_SIZE = 64_000;

export default class RTCEngine extends (EventEmitter as new () => TypedEventEmitter<EngineEventCallbacks>) {
	client: SignalClient;

	rtcConfig: RTCConfiguration = {};

	peerConnectionTimeout: number = roomConnectOptionDefaults.peerConnectionTimeout;

	fullReconnectOnNext: boolean = false;

	pcManager?: PCTransportManager;

	latestJoinResponse?: JoinResponse;

	latestRemoteOfferId: number = 0;

	e2eeManager: BaseE2EEManager | undefined;

	get isClosed() {
		return this._isClosed;
	}

	get isNewlyCreated() {
		return this._isNewlyCreated;
	}

	get pendingReconnect() {
		return !!this.reconnectTimeout;
	}
	get serverVersion(): string | undefined {
		return this.latestJoinResponse?.serverInfo?.version || this.latestJoinResponse?.serverVersion || undefined;
	}
	private dataChannels: DataChannelManager;
	private get reliableChannel(): ReliableDataChannel {
		return this.dataChannels.reliable;
	}
	private get lossyChannel(): LossyDataChannel {
		return this.dataChannels.lossy;
	}
	private get dataTrackChannel(): LossyDataChannel {
		return this.dataChannels.dataTrack;
	}

	private subscriberPrimary: boolean = false;

	private pcState: PCState = PCState.New;

	private _isClosed: boolean = true;

	private _isNewlyCreated: boolean = true;

	private pendingTrackResolvers: {
		[key: string]: {resolve: (info: TrackInfo) => void; reject: () => void};
	} = {};

	private url?: string;

	private token?: string;

	private signalOpts?: SignalOptions;

	private reconnectAttempts: number = 0;

	private reconnectStart: number = 0;

	private clientConfiguration?: ClientConfiguration;

	private attemptingReconnect: boolean = false;

	private reconnectPolicy: ReconnectPolicy;

	private reconnectTimeout?: TimerHandle;

	private participantSid?: string;

	private joinAttempts: number = 0;

	private maxJoinAttempts: number = 1;

	private closingLock: Mutex;

	private dataProcessLock: Mutex;

	private shouldFailNext: boolean = false;

	private shouldFailOnV1Path: boolean = false;

	private regionStrategy?: RegionStrategy;

	private log = log;

	private loggerOptions: LoggerOptions;

	private publisherConnectionPromise: Promise<void> | undefined;

	private reliableReceivedState: TTLMap<string, number> = new TTLMap(reliabeReceiveStateTTL);

	private midToTrackId: {[key: string]: string} = {};

	private isWaitingForNetworkReconnect: boolean = false;

	private lostQualityTimeout?: TimerHandle;

	private transportConnectingSince?: number;

	constructor(private options: InternalRoomOptions) {
		super();
		this.log = getLogger(options.loggerName ?? LoggerNames.Engine, () => this.logContext);
		this.loggerOptions = {
			loggerName: options.loggerName,
			loggerContextCb: () => this.logContext,
		};
		this.client = new SignalClient(undefined, this.loggerOptions);
		this.client.signalLatency = this.options.expSignalLatency;
		this.reconnectPolicy = this.options.reconnectPolicy;
		this.closingLock = new Mutex();
		this.dataProcessLock = new Mutex();
		this.dataChannels = new DataChannelManager({
			isEngineClosed: () => this.isClosed,
			isReconnecting: () => this.attemptingReconnect,
			onDataMessage: (message) => this.handleDataMessage(message),
			onDataTrackMessage: (message) => this.handleDataTrackMessage(message),
			onDataError: (event) => this.handleDataError(event),
			onChannelClose: (kind) => this.handleDataChannelClose(kind)(),
			onBufferStatusChanged: (kind, isLow) => this.emit(EngineEvent.DCBufferStatusChanged, isLow, kind),
		});

		this.client.onParticipantUpdate = (updates) => this.emit(EngineEvent.ParticipantUpdate, updates);
		this.client.onConnectionQuality = (update) => {
			this.handleLocalConnectionQuality(update);
			this.emit(EngineEvent.ConnectionQualityUpdate, update);
		};
		this.client.onRoomUpdate = (update) => this.emit(EngineEvent.RoomUpdate, update);
		this.client.onSubscriptionError = (resp) => this.emit(EngineEvent.SubscriptionError, resp);
		this.client.onSubscriptionPermissionUpdate = (update) =>
			this.emit(EngineEvent.SubscriptionPermissionUpdate, update);
		this.client.onSpeakersChanged = (update) => this.emit(EngineEvent.SpeakersChanged, update);
		this.client.onStreamStateUpdate = (update) => this.emit(EngineEvent.StreamStateChanged, update);
		this.client.onRequestResponse = (response) => this.emit(EngineEvent.SignalRequestResponse, response);
		this.client.onParticipantUpdate = (updates) => this.emit(EngineEvent.ParticipantUpdate, updates);
		this.client.onJoined = (joinResponse) => this.emit(EngineEvent.Joined, joinResponse);
	}

	get logContext() {
		return {
			room: this.latestJoinResponse?.room?.name,
			roomID: this.latestJoinResponse?.room?.sid,
			participant: this.latestJoinResponse?.participant?.identity,
			participantID: this.participantSid,
		};
	}

	async join(
		url: string,
		token: string,
		opts: SignalOptions,
		abortSignal?: AbortSignal,
		useV0Path: boolean = false,
	): Promise<{joinResponse: JoinResponse; serverInfo: Partial<ServerInfo>}> {
		this._isNewlyCreated = false;
		this.url = url;
		this.token = token;
		this.signalOpts = opts;
		this.maxJoinAttempts = opts.maxRetries;
		try {
			this.joinAttempts += 1;

			this.setupSignalClientCallbacks();
			const sendOfferWithJoin = !useV0Path && isPublisherOfferWithJoinSupported();

			let offerProto: SessionDescription | undefined;
			if (sendOfferWithJoin) {
				if (!this.pcManager) {
					await this.configure();
					this.applyInitialPublisherLayout();
				}
				const offer = await this.pcManager?.publisher.createInitialOffer();
				if (offer) {
					offerProto = toProtoSessionDescription(offer.offer, offer.offerId);
				}
			}

			if (abortSignal?.aborted) {
				throw ConnectionError.cancelled('Connection aborted');
			}

			if (!useV0Path && this.shouldFailOnV1Path) {
				this.shouldFailOnV1Path = false;
				throw ConnectionError.serviceNotFound('Simulated v1 path failure', 'v0-rtc');
			}
			const joinResponse = await this.client.join(url, token, opts, abortSignal, useV0Path, offerProto);
			this._isClosed = false;
			this.latestJoinResponse = joinResponse;
			this.participantSid = joinResponse.participant?.sid;

			this.subscriberPrimary = joinResponse.subscriberPrimary;
			if (sendOfferWithJoin) {
				this.pcManager?.updateConfiguration(this.makeRTCConfiguration(joinResponse));
			} else {
				if (!this.pcManager) {
					await this.configure(joinResponse, !useV0Path);
					if (!useV0Path) {
						this.applyInitialPublisherLayout();
					}
				}
				if (!this.subscriberPrimary || joinResponse.fastPublish) {
					this.negotiate().catch((err) => {
						this.log.error(err);
					});
				}
			}

			this.registerOnLineListener();
			this.clientConfiguration = joinResponse.clientConfiguration;
			this.emit(EngineEvent.SignalConnected, joinResponse);

			let serverInfo: Partial<ServerInfo> | undefined = joinResponse.serverInfo;
			if (!serverInfo) {
				serverInfo = {version: joinResponse.serverVersion, region: joinResponse.serverRegion};
			}
			this.log.info(
				`connected to Livekit Server ${Object.entries(serverInfo)
					.map(([key, value]) => `${key}: ${value}`)
					.join(', ')}`,
			);

			return {joinResponse, serverInfo};
		} catch (e) {
			if (e instanceof ConnectionError) {
				if (e.reason === ConnectionErrorReason.ServerUnreachable) {
					this.log.warn(`Couldn't connect to server, attempt ${this.joinAttempts} of ${this.maxJoinAttempts}`);
					if (this.joinAttempts < this.maxJoinAttempts) {
						return this.join(url, token, opts, abortSignal, useV0Path);
					}
				} else if (e.reason === ConnectionErrorReason.ServiceNotFound) {
					this.log.warn(`Initial connection failed: ${e.message} – Retrying`);
					if (this.pcManager) {
						this.pcManager.onStateChange = undefined;
						await this.cleanupPeerConnections();
					}
					return this.join(url, token, opts, abortSignal, true);
				}
			}
			throw e;
		}
	}

	async close(reason?: string) {
		const unlock = await this.closingLock.lock();
		if (this.isClosed) {
			unlock();
			return;
		}
		try {
			this._isClosed = true;
			this.joinAttempts = 0;
			this.emit(EngineEvent.Closing);
			this.removeAllListeners();
			this.deregisterOnLineListener();
			this.clearPendingReconnect();
			this.clearLostQualityTimeout();
			this.cleanupLossyDataStats();
			await this.cleanupPeerConnections();
			await this.cleanupClient(reason);
		} finally {
			unlock();
		}
	}

	async cleanupPeerConnections() {
		this.dataChannels.teardown();

		await this.pcManager?.close();
		this.pcManager = undefined;
		this.transportConnectingSince = undefined;

		this.reliableReceivedState.clear();
	}

	cleanupLossyDataStats() {
		this.lossyChannel.stopThresholdTuning();
	}

	async cleanupClient(reason?: string) {
		await this.client.close(true, reason);
		this.client.resetCallbacks();
		for (const cid of Object.keys(this.pendingTrackResolvers)) {
			this.pendingTrackResolvers[cid].reject();
		}
		this.pendingTrackResolvers = {};
	}

	addTrack(req: AddTrackRequest): Promise<TrackInfo> {
		if (this.pendingTrackResolvers[req.cid]) {
			throw new TrackInvalidError('a track with the same ID has already been published');
		}
		return new Promise<TrackInfo>((resolve, reject) => {
			const publicationTimeout = CriticalTimers.setTimeout(() => {
				delete this.pendingTrackResolvers[req.cid];
				reject(ConnectionError.timeout('publication of local track timed out, no response from server'));
			}, 10_000);
			this.pendingTrackResolvers[req.cid] = {
				resolve: (info: TrackInfo) => {
					CriticalTimers.clearTimeout(publicationTimeout);
					resolve(info);
				},
				reject: () => {
					CriticalTimers.clearTimeout(publicationTimeout);
					reject(new Error('Cancelled publication by calling unpublish'));
				},
			};
			this.client.sendAddTrack(req);
		});
	}

	removeTrack(sender: RTCRtpSender): boolean {
		if (sender.track && this.pendingTrackResolvers[sender.track.id]) {
			const {reject} = this.pendingTrackResolvers[sender.track.id];
			if (reject) {
				reject();
			}
			delete this.pendingTrackResolvers[sender.track.id];
		}
		try {
			this.pcManager!.removeTrack(sender);
			return true;
		} catch (e: unknown) {
			this.log.warn('failed to remove track', {error: e});
		}
		return false;
	}

	updateMuteStatus(trackSid: string, muted: boolean) {
		this.client.sendMuteTrack(trackSid, muted);
	}

	get dataSubscriberReadyState(): string | undefined {
		return this.dataChannelForKind(DataChannelKind.RELIABLE, true)?.readyState;
	}

	async getConnectedServerAddress(): Promise<string | undefined> {
		return this.pcManager?.getConnectedAddress();
	}

	setRegionStrategy(strategy: RegionStrategy | undefined) {
		this.regionStrategy = strategy;
	}

	private async configure(joinResponse?: JoinResponse, useSinglePeerConnection?: boolean) {
		if (this.pcManager && this.pcManager.currentState !== PCTransportState.NEW) {
			return;
		}
		if (!joinResponse) {
			const rtcConfig = this.makeRTCConfiguration();
			this.pcManager = new PCTransportManager(
				'publisher-only',
				this.loggerOptions,
				rtcConfig,
				this.options.subscriberVideoCodecExclusions,
			);
		} else {
			this.participantSid = joinResponse.participant?.sid;
			const rtcConfig = this.makeRTCConfiguration(joinResponse);
			this.pcManager = new PCTransportManager(
				useSinglePeerConnection
					? 'publisher-only'
					: joinResponse.subscriberPrimary
						? 'subscriber-primary'
						: 'publisher-primary',
				this.loggerOptions,
				rtcConfig,
				this.options.subscriberVideoCodecExclusions,
			);
		}

		this.emit(EngineEvent.TransportsCreated, this.pcManager.publisher, this.pcManager.subscriber);

		this.pcManager.onIceCandidate = (candidate, target) => {
			this.client.sendIceCandidate(candidate, target);
		};

		this.pcManager.onPublisherOffer = (offer, offerId) => {
			this.client.sendOffer(offer, offerId);
		};

		this.pcManager.onDataChannel = this.handleDataChannel;
		this.pcManager.onStateChange = async (connectionState, publisherState, subscriberState) => {
			this.log.debug(`primary PC state changed ${connectionState}`);

			if (connectionState === PCTransportState.CONNECTING) {
				this.transportConnectingSince = Date.now();
			} else {
				this.transportConnectingSince = undefined;
			}

			if (['closed', 'disconnected', 'failed'].includes(publisherState)) {
				this.publisherConnectionPromise = undefined;
			}
			if (connectionState === PCTransportState.CONNECTED) {
				const shouldEmit = this.pcState === PCState.New;
				this.pcState = PCState.Connected;
				if (shouldEmit) {
					this.emit(EngineEvent.Connected, this.latestJoinResponse!);
				}
			} else if (connectionState === PCTransportState.FAILED) {
				if (this.pcState === PCState.Connected || this.pcState === PCState.Reconnecting) {
					this.pcState = PCState.Disconnected;

					this.handleDisconnect(
						'peerconnection failed',
						subscriberState === 'failed' ? ReconnectReason.RR_SUBSCRIBER_FAILED : ReconnectReason.RR_PUBLISHER_FAILED,
					);
				}
			}

			const isSignalSevered =
				this.client.isDisconnected || this.client.currentState === SignalConnectionState.RECONNECTING;
			const isPCSevered = [PCTransportState.FAILED, PCTransportState.CLOSING, PCTransportState.CLOSED].includes(
				connectionState,
			);
			if (isSignalSevered && isPCSevered && !this._isClosed) {
				this.emit(EngineEvent.Offline);
			}
		};
		this.pcManager.onTrack = (ev: RTCTrackEvent) => {
			if (ev.streams.length === 0) return;
			this.emit(EngineEvent.MediaTrackAdded, ev.track, ev.streams[0], ev.receiver);
		};
	}

	private setupSignalClientCallbacks() {
		this.client.onAnswer = async (sd, offerId, midToTrackId) => {
			if (!this.pcManager) {
				return;
			}
			this.log.debug('received server answer', {
				RTCSdpType: sd.type,
				sdp: sd.sdp,
				midToTrackId,
			});
			if (this.pcManager.mode === 'publisher-only') {
				this.midToTrackId = midToTrackId;
			}
			await this.pcManager.setPublisherAnswer(sd, offerId);
		};

		this.client.onTrickle = (candidate, target) => {
			if (!this.pcManager) {
				return;
			}
			this.log.debug('got ICE candidate from peer', {candidate, target});
			this.pcManager.addIceCandidate(candidate, target);
		};

		this.client.onOffer = async (sd, offerId, midToTrackId) => {
			this.latestRemoteOfferId = offerId;
			if (!this.pcManager) {
				return;
			}
			this.midToTrackId = midToTrackId;
			const answer = await this.pcManager.createSubscriberAnswerFromOffer(sd, offerId);
			if (answer) {
				this.client.sendAnswer(answer, offerId);
			}
		};

		this.client.onLocalTrackPublished = (res: TrackPublishedResponse) => {
			this.log.debug('received trackPublishedResponse', {
				cid: res.cid,
				track: res.track?.sid,
			});
			if (!this.pendingTrackResolvers[res.cid]) {
				this.log.error(`missing track resolver for ${res.cid}`, {cid: res.cid});
				return;
			}
			const {resolve} = this.pendingTrackResolvers[res.cid];
			delete this.pendingTrackResolvers[res.cid];
			resolve(res.track!);
		};

		this.client.onLocalTrackUnpublished = (response: TrackUnpublishedResponse) => {
			this.emit(EngineEvent.LocalTrackUnpublished, response);
		};

		this.client.onLocalTrackSubscribed = (trackSid: string) => {
			this.emit(EngineEvent.LocalTrackSubscribed, trackSid);
		};

		this.client.onTokenRefresh = (token: string) => {
			this.token = token;
			this.emit(EngineEvent.TokenRefreshed, token);
		};

		this.client.onRemoteMuteChanged = (trackSid: string, muted: boolean) => {
			this.emit(EngineEvent.RemoteMute, trackSid, muted);
		};

		this.client.onSubscribedQualityUpdate = (update: SubscribedQualityUpdate) => {
			this.emit(EngineEvent.SubscribedQualityUpdate, update);
		};

		this.client.onRoomMoved = (res: RoomMovedResponse) => {
			this.participantSid = res.participant?.sid;
			if (this.latestJoinResponse) {
				this.latestJoinResponse.room = res.room;
			}
			this.emit(EngineEvent.RoomMoved, res);
		};

		this.client.onMediaSectionsRequirement = (requirement: MediaSectionsRequirement) => {
			this.addMediaSections(requirement.numAudios, requirement.numVideos);
			this.negotiate();
		};

		this.client.onPublishDataTrackResponse = (event: PublishDataTrackResponse) => {
			this.emit(EngineEvent.PublishDataTrackResponse, event);
		};

		this.client.onUnPublishDataTrackResponse = (event: UnpublishDataTrackResponse) => {
			this.emit(EngineEvent.UnPublishDataTrackResponse, event);
		};

		this.client.onDataTrackSubscriberHandles = (event: DataTrackSubscriberHandles) => {
			this.emit(EngineEvent.DataTrackSubscriberHandles, event);
		};

		this.client.onClose = () => {
			this.handleDisconnect('signal', ReconnectReason.RR_SIGNAL_DISCONNECTED);
		};

		this.client.onLeave = (leave: LeaveRequest) => {
			this.log.info(`client leave request received (action=${leave?.action})`, {
				reason: leave?.reason,
			});
			if (leave.regions) {
				this.log.debug('updating regions');
				this.emit(EngineEvent.ServerRegionsReported, leave.regions);
			}
			switch (leave.action) {
				case LeaveRequest_Action.DISCONNECT:
					this.emit(EngineEvent.Disconnected, leave?.reason);
					this.close(`server leave: ${DisconnectReason[leave.reason] ?? leave.reason}`);
					break;
				case LeaveRequest_Action.RECONNECT:
					this.fullReconnectOnNext = true;
					this.handleDisconnect(leaveReconnect);
					break;
				case LeaveRequest_Action.RESUME:
					this.handleDisconnect(leaveReconnect);
					break;
				default:
					break;
			}
		};
	}

	private makeRTCConfiguration(serverResponse?: JoinResponse | ReconnectResponse): RTCConfiguration {
		const rtcConfig = {...this.rtcConfig};
		const needsInsertableStreams =
			this.signalOpts?.e2eeEnabled || (this.frameMetadataWorker && !shouldUseFrameMetadataScriptTransform());
		if (needsInsertableStreams && isInsertableStreamSupported()) {
			this.log.debug('E2EE - setting up transports with insertable streams');
			rtcConfig.encodedInsertableStreams = true;
		}

		rtcConfig.sdpSemantics = 'unified-plan';
		rtcConfig.continualGatheringPolicy = 'gather_continually';

		if (!serverResponse) {
			return rtcConfig;
		}

		if (serverResponse.iceServers && !rtcConfig.iceServers) {
			const rtcIceServers: Array<RTCIceServer> = [];
			serverResponse.iceServers.forEach((iceServer) => {
				const rtcIceServer: RTCIceServer = {
					urls: iceServer.urls,
				};
				if (iceServer.username) rtcIceServer.username = iceServer.username;
				if (iceServer.credential) {
					rtcIceServer.credential = iceServer.credential;
				}
				rtcIceServers.push(rtcIceServer);
			});
			rtcConfig.iceServers = rtcIceServers;
		}

		if (
			serverResponse.clientConfiguration &&
			serverResponse.clientConfiguration.forceRelay === ClientConfigSetting.ENABLED
		) {
			rtcConfig.iceTransportPolicy = 'relay';
		}

		return rtcConfig;
	}

	private applyInitialPublisherLayout() {
		this.createDataChannels();
		if (!isReactNative()) {
			this.addMediaSections(initialMediaSectionsAudio, initialMediaSectionsVideo);
		}
	}

	private addMediaSections(numAudios: number, numVideos: number) {
		const transceiverInit: RTCRtpTransceiverInit = {direction: 'recvonly'};
		for (let i: number = 0; i < numAudios; i++) {
			this.pcManager?.addPublisherTransceiverOfKind('audio', transceiverInit);
		}
		const receivesMedia = this.pcManager?.mode === 'publisher-only';
		for (let i: number = 0; i < numVideos; i++) {
			const transceiver = this.pcManager?.addPublisherTransceiverOfKind('video', transceiverInit);
			if (receivesMedia && transceiver) {
				const negotiated = negotiateDependencyDescriptor(transceiver);

				this.log.debug('dependency descriptor negotiated for received video', {negotiated});
			}
		}
	}

	private createDataChannels() {
		if (!this.pcManager) {
			return;
		}

		this.dataChannels.createPublisherChannels(this.pcManager);
	}

	private handleDataChannel = async ({channel}: RTCDataChannelEvent) => {
		if (!channel) {
			return;
		}
		if (this.dataChannels.adoptSubscriberChannel(channel)) {
			this.log.debug(`on data channel ${channel.id}, ${channel.label}`);
		}
	};

	private async decodeDataMessage(message: MessageEvent): Promise<Uint8Array | undefined> {
		if (message.data instanceof ArrayBuffer) {
			return new Uint8Array(message.data);
		}
		if (message.data instanceof Blob) {
			return new Uint8Array(await message.data.arrayBuffer());
		}
		this.log.error('unsupported data type', {data: message.data});
		return undefined;
	}

	private handleDataMessage = async (message: MessageEvent) => {
		const unlock = await this.dataProcessLock.lock();
		try {
			const bytes = await this.decodeDataMessage(message);
			if (!bytes) {
				return;
			}
			const dp = DataPacket.fromBinary(bytes);

			if (dp.sequence > 0 && dp.participantSid !== '') {
				const lastSeq = this.reliableReceivedState.get(dp.participantSid);
				if (lastSeq && dp.sequence <= lastSeq) {
					return;
				}
				this.reliableReceivedState.set(dp.participantSid, dp.sequence);
			}

			if (dp.value?.case === 'speaker') {
				this.emit(EngineEvent.ActiveSpeakersUpdate, dp.value.value.speakers);
			} else if (dp.value?.case === 'encryptedPacket') {
				if (!this.e2eeManager) {
					this.log.error('Received encrypted packet but E2EE not set up');
					return;
				}
				let decryptedData: Awaited<ReturnType<BaseE2EEManager['handleEncryptedData']>>;
				try {
					decryptedData = await this.e2eeManager.handleEncryptedData(
						dp.value.value.encryptedValue as NonSharedUint8Array,
						dp.value.value.iv as NonSharedUint8Array,
						dp.participantIdentity,
						dp.value.value.keyIndex,
					);
				} catch (err) {
					this.log.debug('failed to decrypt data packet', {
						error: err,
						participantIdentity: dp.participantIdentity,
					});
					return;
				}
				const decryptedPacket = EncryptedPacketPayload.fromBinary(decryptedData.payload);
				const newDp = new DataPacket({
					value: decryptedPacket.value,
					participantIdentity: dp.participantIdentity,
					participantSid: dp.participantSid,
				});
				if (newDp.value?.case === 'user') {
					applyUserDataCompat(newDp, newDp.value.value);
				}
				this.emit(EngineEvent.DataPacketReceived, newDp, dp.value.value.encryptionType);
			} else {
				if (dp.value?.case === 'user') {
					applyUserDataCompat(dp, dp.value.value);
				}
				this.emit(EngineEvent.DataPacketReceived, dp, Encryption_Type.NONE);
			}
		} finally {
			unlock();
		}
	};

	private handleDataTrackMessage = async (message: MessageEvent) => {
		const bytes = await this.decodeDataMessage(message);
		if (!bytes) {
			return;
		}
		this.emit('dataTrackPacketReceived', bytes);
	};

	private handleDataError = (event: Event) => {
		if (this._isClosed) {
			return;
		}

		const channel = event.currentTarget as RTCDataChannel;
		const channelKind = channel.maxRetransmits === 0 ? 'lossy' : 'reliable';

		if (typeof RTCErrorEvent !== 'undefined' && event instanceof RTCErrorEvent && event.error) {
			const {error} = event;
			this.log.error(`DataChannel error on ${channelKind}: ${error.message}`, {
				error,
				errorDetail: error.errorDetail,
				sctpCauseCode: error.sctpCauseCode,
			});
		} else {
			this.log.error(`Unknown DataChannel error on ${channelKind}`, {event});
		}
	};

	private handleDataChannelClose = (kind: DataChannelKind) => () => {
		if (!this._isClosed && this.pcManager?.publisher.getConnectionState() === 'connected') {
			this.log.error(`publisher data channel '${DataChannelKind[kind]}' closed unexpectedly`, this.logContext);
		}
	};

	async createSender(track: LocalTrack, opts: TrackPublishOptions, encodings?: Array<RTCRtpEncodingParameters>) {
		let sender: RTCRtpSender;
		if (supportsTransceiver()) {
			sender = await this.createTransceiverRTCRtpSender(track, opts, encodings);
		} else if (supportsAddTrack()) {
			this.log.warn('using add-track fallback');
			sender = await this.createRTCRtpSender(track.mediaStreamTrack);
		} else {
			throw new UnexpectedConnectionState('Required webRTC APIs not supported on this device');
		}
		this.setupFrameMetadataSender(sender, opts);
		return sender;
	}

	async createSimulcastSender(
		track: LocalVideoTrack,
		simulcastTrack: SimulcastTrackInfo,
		opts: TrackPublishOptions,
		encodings?: Array<RTCRtpEncodingParameters>,
	) {
		let sender: RTCRtpSender | undefined;
		if (supportsTransceiver()) {
			sender = await this.createSimulcastTransceiverSender(track, simulcastTrack, opts, encodings);
		} else if (supportsAddTrack()) {
			this.log.debug('using add-track fallback');
			sender = await this.createRTCRtpSender(track.mediaStreamTrack);
		} else {
			throw new UnexpectedConnectionState('Cannot stream on this device');
		}
		if (sender) {
			this.setupFrameMetadataSender(sender, opts);
		}
		return sender;
	}

	private get frameMetadataWorker(): Worker | undefined {
		return (this.options.frameMetadata ?? this.options.packetTrailer)?.worker;
	}

	private setupFrameMetadataSender(sender: RTCRtpSender, opts: TrackPublishOptions = {}) {
		const worker = this.frameMetadataWorker;
		if (!worker || this.signalOpts?.e2eeEnabled) {
			return;
		}

		const frameMetadata = opts.frameMetadata ?? opts.packetTrailer;
		const hasMetadata = hasFrameMetadataPublishOptions(frameMetadata);

		if (shouldUseFrameMetadataScriptTransform()) {
			if (hasMetadata) {
				sender.transform = new RTCRtpScriptTransform(worker, {
					kind: 'encode',
					packetTrailer: frameMetadata,
				});
			}
			return;
		}

		if (
			!isFrameMetadataSupported(this.options.frameMetadata ?? this.options.packetTrailer) ||
			!('createEncodedStreams' in sender)
		) {
			if (hasMetadata) {
				this.log.warn('frame metadata transform not supported; skipping write', this.logContext);
			}
			return;
		}

		// @ts-expect-error
		const {readable, writable} = sender.createEncodedStreams();
		if (hasMetadata) {
			worker.postMessage(
				{
					kind: 'encode',
					data: {
						readableStream: readable,
						writableStream: writable,
						packetTrailer: frameMetadata,
					},
				},
				[readable, writable],
			);
		} else {
			readable.pipeTo(writable);
		}
	}

	private async createTransceiverRTCRtpSender(
		track: LocalTrack,
		opts: TrackPublishOptions,
		encodings?: Array<RTCRtpEncodingParameters>,
	) {
		if (!this.pcManager) {
			throw new UnexpectedConnectionState('publisher is closed');
		}

		const streams: Array<MediaStream> = [];

		if (track.mediaStream) {
			streams.push(track.mediaStream);
		}

		const isVideo = isVideoTrack(track);
		if (isVideo) {
			track.codec = opts.videoCodec;
		}

		const transceiverInit: RTCRtpTransceiverInit = {direction: 'sendonly', streams};
		if (encodings) {
			transceiverInit.sendEncodings = encodings;
		}
		const transceiver = await this.pcManager.addPublisherTransceiver(track.mediaStreamTrack, transceiverInit);
		if (isVideo) {
			this.setPublisherCodecPreferences(transceiver, opts.videoCodec);
		}

		return transceiver.sender;
	}

	private async createSimulcastTransceiverSender(
		track: LocalVideoTrack,
		simulcastTrack: SimulcastTrackInfo,
		opts: TrackPublishOptions,
		encodings?: Array<RTCRtpEncodingParameters>,
	) {
		if (!this.pcManager) {
			throw new UnexpectedConnectionState('publisher is closed');
		}
		const transceiverInit: RTCRtpTransceiverInit = {direction: 'sendonly'};
		if (encodings) {
			transceiverInit.sendEncodings = encodings;
		}
		const transceiver = await this.pcManager.addPublisherTransceiver(simulcastTrack.mediaStreamTrack, transceiverInit);
		this.setPublisherCodecPreferences(transceiver, opts.videoCodec);
		if (!opts.videoCodec) {
			return;
		}
		await track.setSimulcastTrackSender(opts.videoCodec, transceiver.sender);
		return transceiver.sender;
	}

	private async createRTCRtpSender(track: MediaStreamTrack) {
		if (!this.pcManager) {
			throw new UnexpectedConnectionState('publisher is closed');
		}
		return this.pcManager.addPublisherTrack(track);
	}

	private setPublisherCodecPreferences(transceiver: RTCRtpTransceiver, codec: VideoCodec | undefined): void {
		if (!codec || typeof transceiver.setCodecPreferences !== 'function') return;
		if (transceiver.sender.track?.kind && transceiver.sender.track.kind !== 'video') return;
		if (typeof RTCRtpSender === 'undefined' || typeof RTCRtpSender.getCapabilities !== 'function') return;
		const capabilities = RTCRtpSender.getCapabilities('video');
		if (!capabilities) return;
		const preferences = selectPublisherCodecPreferences(codec, capabilities.codecs);
		if (preferences.length === 0) {
			this.log.warn('sender cannot encode the requested codec, leaving the browser order in place', {
				...this.logContext,
				codec,
			});
			return;
		}
		try {
			transceiver.setCodecPreferences(preferences);
		} catch (error) {
			this.log.warn('failed to set publisher codec preferences', {...this.logContext, codec, error});
		}
	}

	private handleDisconnect = (connection: string, disconnectReason?: ReconnectReason) => {
		if (this._isClosed) {
			return;
		}

		this.log.warn(`${connection} disconnected`);
		if (this.reconnectAttempts === 0) {
			this.reconnectStart = Date.now();
		}

		const disconnect = (duration: number) => {
			this.log.warn(`could not recover connection after ${this.reconnectAttempts} attempts, ${duration}ms. giving up`);
			this.emit(EngineEvent.Disconnected);
			this.close(`gave up reconnecting after ${this.reconnectAttempts} attempts, ${duration}ms`);
		};

		const duration = Date.now() - this.reconnectStart;
		let delay = this.getNextRetryDelay({
			elapsedMs: duration,
			retryCount: this.reconnectAttempts,
		});

		if (delay === null) {
			disconnect(duration);
			return;
		}
		if (connection === leaveReconnect) {
			delay = 0;
		}

		this.log.debug(`reconnecting in ${delay}ms`);

		this.clearReconnectTimeout();
		if (this.token) {
			this.emit(EngineEvent.TokenRefreshed, this.token);
		}
		this.reconnectTimeout = CriticalTimers.setTimeout(
			() => this.attemptReconnect(disconnectReason).finally(() => (this.reconnectTimeout = undefined)),
			delay,
		);
	};

	private handleLocalConnectionQuality(update: ConnectionQualityUpdate) {
		if (!this.participantSid) {
			return;
		}
		const localUpdate = update.updates.find((u) => u.participantSid === this.participantSid);
		if (!localUpdate) {
			return;
		}
		if (localUpdate.quality === ProtoConnectionQuality.LOST) {
			this.scheduleLostQualityReconnect();
		} else {
			this.clearLostQualityTimeout();
		}
	}

	private scheduleLostQualityReconnect() {
		if (this.lostQualityTimeout) {
			return;
		}
		this.lostQualityTimeout = CriticalTimers.setTimeout(() => {
			this.lostQualityTimeout = undefined;
			if (this._isClosed || this.pcState !== PCState.Connected || this.attemptingReconnect) {
				return;
			}
			if (!this.hasActivePublisherSenders()) {
				return;
			}
			this.log.warn('local connection quality lost while publishing, triggering full reconnect', this.logContext);
			this.fullReconnectOnNext = true;
			this.handleDisconnect('connection quality lost', ReconnectReason.RR_PUBLISHER_FAILED);
		}, connectionQualityLostTimeout);
	}

	private clearLostQualityTimeout() {
		if (this.lostQualityTimeout) {
			CriticalTimers.clearTimeout(this.lostQualityTimeout);
			this.lostQualityTimeout = undefined;
		}
	}

	private hasActivePublisherSenders(): boolean {
		return (
			this.pcManager?.publisher.getSenders().some((sender) => !!sender.track && sender.track.readyState === 'live') ??
			false
		);
	}

	reconnect(reason: ReconnectReason = ReconnectReason.RR_UNKNOWN) {
		this.fullReconnectOnNext = true;
		this.handleDisconnect('reconcile', reason);
	}

	private async attemptReconnect(reason?: ReconnectReason) {
		if (this._isClosed) {
			return;
		}
		if (this.attemptingReconnect) {
			this.log.warn('already attempting reconnect, returning early');
			return;
		}

		this.clearLostQualityTimeout();

		if (
			this.clientConfiguration?.resumeConnection === ClientConfigSetting.DISABLED ||
			(this.pcManager?.currentState ?? PCTransportState.NEW) === PCTransportState.NEW
		) {
			this.fullReconnectOnNext = true;
		}

		const fullReconnect = this.fullReconnectOnNext;
		this.fullReconnectOnNext = false;

		let succeeded = false;
		try {
			this.attemptingReconnect = true;
			if (fullReconnect) {
				await this.restartConnection();
			} else {
				await this.resumeConnection(reason);
			}
			this.clearPendingReconnect();
			succeeded = true;
		} catch (e) {
			this.reconnectAttempts += 1;
			let recoverable = true;
			if (e instanceof UnexpectedConnectionState) {
				this.log.debug('received unrecoverable error', {error: e});
				recoverable = false;
			} else if (fullReconnect || !(e instanceof SignalReconnectError)) {
				this.fullReconnectOnNext = true;
			}

			if (recoverable) {
				this.handleDisconnect('reconnect', ReconnectReason.RR_UNKNOWN);
			} else {
				this.log.info(
					`could not recover connection after ${this.reconnectAttempts} attempts, ${
						Date.now() - this.reconnectStart
					}ms. giving up`,
				);
				this.emit(EngineEvent.Disconnected);
				await this.close(
					`gave up reconnecting after ${this.reconnectAttempts} attempts, ${Date.now() - this.reconnectStart}ms`,
				);
			}
		} finally {
			this.attemptingReconnect = false;

			if (succeeded && this.fullReconnectOnNext && !this._isClosed) {
				this.log.debug('full reconnect requested during in-progress attempt, dispatching');
				this.handleDisconnect('reconnect');
			}
		}
	}

	private getNextRetryDelay(context: ReconnectContext) {
		try {
			return this.reconnectPolicy.nextRetryDelayInMs(context);
		} catch (e) {
			this.log.warn('encountered error in reconnect policy', {error: e});
		}

		return null;
	}

	private async restartConnection(regionUrl?: string) {
		try {
			if (!this.url || !this.token) {
				throw new UnexpectedConnectionState('could not reconnect, url or token not saved');
			}

			this.log.info(`reconnecting, attempt: ${this.reconnectAttempts}`);
			this.emit(EngineEvent.Restarting);

			if (!this.client.isDisconnected) {
				await this.client.sendLeave();
			}
			await this.cleanupPeerConnections();
			await this.cleanupClient();

			let joinResponse: JoinResponse;
			try {
				if (!this.signalOpts) {
					this.log.warn('attempted connection restart, without signal options present');
					throw new SignalReconnectError();
				}
				joinResponse = (
					await this.join(
						regionUrl ?? this.url,
						this.token,
						this.signalOpts,
						undefined,
						!this.options.singlePeerConnection,
					)
				).joinResponse;
			} catch (e) {
				if (e instanceof ConnectionError && e.reason === ConnectionErrorReason.NotAllowed) {
					throw new UnexpectedConnectionState('could not reconnect, token might be expired');
				}
				throw new SignalReconnectError();
			}

			if (this.shouldFailNext) {
				this.shouldFailNext = false;
				throw new Error('simulated failure');
			}

			this.client.setReconnected();
			this.emit(EngineEvent.SignalRestarted, joinResponse);

			await this.waitForPCReconnected();

			if (this.client.currentState !== SignalConnectionState.CONNECTED) {
				throw new SignalReconnectError('Signal connection got severed during reconnect');
			}

			this.regionStrategy?.resetAttempts();
			this.emit(EngineEvent.Restarted);
		} catch (error) {
			const nextRegionUrl = await this.regionStrategy?.getNextUrl();
			if (nextRegionUrl) {
				await this.restartConnection(nextRegionUrl);
				return;
			} else {
				this.regionStrategy?.resetAttempts();
				throw error;
			}
		}
	}

	private async resumeConnection(reason?: ReconnectReason): Promise<void> {
		if (!this.url || !this.token) {
			throw new UnexpectedConnectionState('could not reconnect, url or token not saved');
		}
		if (!this.pcManager) {
			throw new UnexpectedConnectionState('publisher and subscriber connections unset');
		}

		this.log.info(`resuming signal connection, attempt ${this.reconnectAttempts}`);
		this.emit(EngineEvent.Resuming);
		let res: ReconnectResponse | undefined;
		try {
			this.setupSignalClientCallbacks();
			res = await this.client.reconnect(this.url, this.token, this.participantSid, reason);
		} catch (error) {
			let message = '';
			if (error instanceof Error) {
				message = error.message;
				this.log.error(error.message, {error});
			}
			if (error instanceof ConnectionError && error.reason === ConnectionErrorReason.NotAllowed) {
				throw new UnexpectedConnectionState('could not reconnect, token might be expired');
			}
			if (error instanceof ConnectionError && error.reason === ConnectionErrorReason.LeaveRequest) {
				throw error;
			}
			throw new SignalReconnectError(message);
		}
		this.emit(EngineEvent.SignalResumed);

		if (res) {
			const rtcConfig = this.makeRTCConfiguration(res);
			this.pcManager.updateConfiguration(rtcConfig);
			if (this.latestJoinResponse) {
				this.latestJoinResponse.serverInfo = res.serverInfo;
			}
		} else {
			this.log.warn('Did not receive reconnect response');
		}

		if (this.shouldFailNext) {
			this.shouldFailNext = false;
			throw new Error('simulated failure');
		}

		await this.pcManager.triggerIceRestart();

		await this.waitForPCReconnected();

		if (this.client.currentState !== SignalConnectionState.CONNECTED) {
			throw new SignalReconnectError('Signal connection got severed during reconnect');
		}

		this.client.setReconnected();

		const reliableDC = this.dataChannelForKind(DataChannelKind.RELIABLE);
		if (reliableDC?.readyState === 'open' && reliableDC.id === null) {
			this.createDataChannels();
		}

		if (res?.lastMessageSeq) {
			this.resendReliableMessagesForResume(res.lastMessageSeq).catch((error) => {
				this.log.warn('failed to resend reliable messages after resume', {
					...this.logContext,
					error,
				});
			});
		}

		this.emit(EngineEvent.Resumed);
	}

	async waitForPCInitialConnection(timeout?: number, abortController?: AbortController) {
		if (!this.pcManager) {
			throw new UnexpectedConnectionState('PC manager is closed');
		}
		await this.pcManager.ensurePCTransportConnection(abortController, timeout);
	}

	private async waitForPCReconnected() {
		this.pcState = PCState.Reconnecting;

		this.log.debug('waiting for peer connection to reconnect');
		try {
			await sleep(minReconnectWait);
			if (!this.pcManager) {
				throw new UnexpectedConnectionState('PC manager is closed');
			}
			await this.pcManager.ensurePCTransportConnection(undefined, this.peerConnectionTimeout);
			this.pcState = PCState.Connected;
		} catch (e: unknown) {
			this.pcState = PCState.Disconnected;
			const message = e instanceof Error ? e.message : String(e);
			throw ConnectionError.internal(`could not establish PC connection, ${message}`);
		}
	}

	waitForRestarted = () => {
		return new Promise<void>((resolve, reject) => {
			if (this.pcState === PCState.Connected) {
				resolve();
			}
			const onRestarted = () => {
				this.off(EngineEvent.Disconnected, onDisconnected);
				resolve();
			};
			const onDisconnected = () => {
				this.off(EngineEvent.Restarted, onRestarted);
				reject();
			};
			this.once(EngineEvent.Restarted, onRestarted);
			this.once(EngineEvent.Disconnected, onDisconnected);
		});
	};

	async publishRpcAck(destinationIdentity: string, requestId: string) {
		const packet = new DataPacket({
			destinationIdentities: [destinationIdentity],
			kind: DataPacket_Kind.RELIABLE,
			value: {
				case: 'rpcAck',
				value: new RpcAck({
					requestId,
				}),
			},
		});
		await this.sendDataPacket(packet, DataChannelKind.RELIABLE);
	}

	async sendDataPacket(packet: DataPacket, kind: Exclude<DataChannelKind, DataChannelKind.DATA_TRACK_LOSSY>) {
		await this.ensurePublisherConnected(kind);

		if (this.e2eeManager?.isDataChannelEncryptionEnabled) {
			const encryptablePacket = asEncryptablePacket(packet);
			if (encryptablePacket) {
				const encryptedData = await this.e2eeManager.encryptData(encryptablePacket.toBinary() as NonSharedUint8Array);
				packet.value = {
					case: 'encryptedPacket',
					value: new EncryptedPacket({
						encryptedValue: encryptedData.payload,
						iv: encryptedData.iv,
						keyIndex: encryptedData.keyIndex,
					}),
				};
			}
		}

		if (kind === DataChannelKind.RELIABLE) {
			packet.sequence = this.reliableChannel.nextSequence();
		}
		const msg = packet.toBinary() as Uint8Array<ArrayBuffer>;
		const maxPublisherMessageSizeBytes = Math.min(
			this.pcManager?.getMaxPublisherMessageSize() ?? DEFAULT_MAX_MESSAGE_SIZE,
			DEFAULT_MAX_MESSAGE_SIZE,
		);
		if (
			typeof maxPublisherMessageSizeBytes !== 'undefined' &&
			maxPublisherMessageSizeBytes !== 0 &&
			msg.byteLength > maxPublisherMessageSizeBytes
		) {
			throw new PublishDataError(
				`cannot publish data packet larger than ${maxPublisherMessageSizeBytes} bytes (got ${msg.byteLength})`,
			);
		}

		if (kind === DataChannelKind.RELIABLE) {
			await this.reliableChannel.send(msg, packet.sequence);
		} else {
			await this.lossyChannel.send(msg);
		}
	}

	async sendDataTrackFrame(bytes: NonSharedUint8Array) {
		await this.ensurePublisherConnected(DataChannelKind.DATA_TRACK_LOSSY);
		await this.dataTrackChannel.send(bytes);
	}

	private async resendReliableMessagesForResume(lastMessageSeq: number) {
		await this.ensurePublisherConnected(DataChannelKind.RELIABLE);
		await this.reliableChannel.replay(lastMessageSeq);
	}
	private flowControlFor(kind: DataChannelKind): FlowControlledDataChannel {
		return this.dataChannels.channelFor(kind);
	}

	async waitForBufferHeadroom(kind: DataChannelKind) {
		return this.flowControlFor(kind).waitForHeadroomWithLock();
	}

	async ensureDataTransportConnected(kind: DataChannelKind, subscriber: boolean = this.subscriberPrimary) {
		if (!this.pcManager) {
			throw new UnexpectedConnectionState('PC manager is closed');
		}
		const transport = subscriber ? this.pcManager.subscriber : this.pcManager.publisher;
		const transportName = subscriber ? 'Subscriber' : 'Publisher';
		if (!transport) {
			throw ConnectionError.internal(`${transportName} connection not set`);
		}

		let needNegotiation = false;
		if (!subscriber && !this.dataChannelForKind(kind, subscriber)) {
			this.createDataChannels();
			needNegotiation = true;
		}

		if (
			!needNegotiation &&
			!subscriber &&
			!this.pcManager.publisher.isICEConnected &&
			this.pcManager.publisher.getICEConnectionState() !== 'checking'
		) {
			needNegotiation = true;
		}
		if (needNegotiation) {
			this.negotiate().catch((err) => {
				this.log.error(err);
			});
		}

		const targetChannel = this.dataChannelForKind(kind, subscriber);
		if (targetChannel?.readyState === 'open') {
			return;
		}

		const endTime = Date.now() + this.peerConnectionTimeout;
		while (Date.now() < endTime) {
			if (transport.isICEConnected && this.dataChannelForKind(kind, subscriber)?.readyState === 'open') {
				return;
			}
			await sleep(50);
		}

		throw ConnectionError.internal(
			`could not establish ${transportName} connection, state: ${transport.getICEConnectionState()}`,
		);
	}

	private async ensurePublisherConnected(kind: DataChannelKind) {
		if (!this.publisherConnectionPromise) {
			this.publisherConnectionPromise = this.ensureDataTransportConnected(kind, false);
		}
		await this.publisherConnectionPromise;
	}

	verifyTransport(): boolean {
		if (!this.pcManager) {
			return false;
		}
		const state = this.pcManager.currentState;
		const allowedConnectionStates: Array<PCTransportState> = [PCTransportState.CONNECTING, PCTransportState.CONNECTED];
		if (!allowedConnectionStates.includes(state)) {
			return false;
		}

		if (!this.client.ws || this.client.ws.readyState === WebSocket.CLOSED) {
			return false;
		}

		if (
			state === PCTransportState.CONNECTING &&
			this.transportConnectingSince !== undefined &&
			Date.now() - this.transportConnectingSince > this.peerConnectionTimeout
		) {
			this.log.warn('transport stuck in connecting state', this.logContext);
			return false;
		}

		return true;
	}

	async negotiate(): Promise<void> {
		return new TypedPromise<void, NegotiationError | Error>(async (resolve, reject) => {
			if (!this.pcManager) {
				reject(new NegotiationError('PC manager is closed'));
				return;
			}

			this.pcManager.requirePublisher();
			if (!this.dataChannels.hasPublisherChannels) {
				this.createDataChannels();
			}

			const abortController = new AbortController();

			const handleClosed = () => {
				abortController.abort();
				this.log.debug('engine disconnected while negotiation was ongoing');
				resolve();
				return;
			};

			if (this.isClosed) {
				reject(new NegotiationError('cannot negotiate on closed engine'));
			}
			this.on(EngineEvent.Closing, handleClosed);
			this.on(EngineEvent.Restarting, handleClosed);
			this.pcManager.publisher.off(PCEvents.RTPVideoPayloadTypes, this.onRtpMapAvailable);
			this.pcManager.publisher.once(PCEvents.RTPVideoPayloadTypes, this.onRtpMapAvailable);

			try {
				await this.pcManager.negotiate(abortController);
				resolve();
			} catch (e: unknown) {
				if (abortController.signal.aborted) {
					resolve();
					return;
				}
				if (e instanceof NegotiationError) {
					this.fullReconnectOnNext = true;
				}
				this.handleDisconnect('negotiation', ReconnectReason.RR_UNKNOWN);
				if (e instanceof Error) {
					reject(e);
				} else {
					reject(new Error(String(e)));
				}
			} finally {
				this.off(EngineEvent.Closing, handleClosed);
				this.off(EngineEvent.Restarting, handleClosed);
			}
		});
	}
	dataChannelForKind(kind: DataChannelKind, sub?: boolean): RTCDataChannel | undefined {
		return this.dataChannels.getHandle(kind, sub);
	}

	sendSyncState(
		remoteTracks: Array<RemoteTrackPublication>,
		localTracks: Array<LocalTrackPublication>,
		localDataTrackInfos: Array<DataTrackInfo>,
	) {
		if (!this.pcManager) {
			this.log.warn('sync state cannot be sent without peer connection setup');
			return;
		}
		const previousPublisherOffer = this.pcManager.publisher.getLocalDescription();
		const previousPublisherAnswer = this.pcManager.publisher.getRemoteDescription();
		const previousSubscriberOffer = this.pcManager.subscriber?.getRemoteDescription();
		const previousSubscriberAnswer = this.pcManager.subscriber?.getLocalDescription();

		const autoSubscribe = this.signalOpts?.autoSubscribe ?? true;
		const trackSids: Array<string> = [];
		const trackSidsDisabled: Array<string> = [];

		remoteTracks.forEach((track) => {
			if (track.isDesired !== autoSubscribe) {
				trackSids.push(track.trackSid);
			}
			if (!track.isEnabled) {
				trackSidsDisabled.push(track.trackSid);
			}
		});

		this.client.sendSyncState(
			new SyncState({
				answer:
					this.pcManager.mode === 'publisher-only'
						? previousPublisherAnswer
							? toProtoSessionDescription({
									sdp: previousPublisherAnswer.sdp,
									type: previousPublisherAnswer.type,
								})
							: undefined
						: previousSubscriberAnswer
							? toProtoSessionDescription({
									sdp: previousSubscriberAnswer.sdp,
									type: previousSubscriberAnswer.type,
								})
							: undefined,
				offer:
					this.pcManager.mode === 'publisher-only'
						? previousPublisherOffer
							? toProtoSessionDescription({
									sdp: previousPublisherOffer.sdp,
									type: previousPublisherOffer.type,
								})
							: undefined
						: previousSubscriberOffer
							? toProtoSessionDescription({
									sdp: previousSubscriberOffer.sdp,
									type: previousSubscriberOffer.type,
								})
							: undefined,
				subscription: new UpdateSubscription({
					trackSids,
					subscribe: !autoSubscribe,
					participantTracks: [],
				}),
				publishTracks: getTrackPublicationInfo(localTracks),
				dataChannels: this.dataChannelsInfo(),
				trackSidsDisabled,
				datachannelReceiveStates: this.reliableReceivedState.map((seq, sid) => {
					return new DataChannelReceiveState({
						publisherSid: sid,
						lastSeq: seq,
					});
				}),
				publishDataTracks: localDataTrackInfos.map((info) => {
					return new PublishDataTrackResponse({info: DataTrackInfo.toProtobuf(info)});
				}),
			}),
		);
	}

	failNext() {
		this.shouldFailNext = true;
	}

	failNextV1Path() {
		this.shouldFailOnV1Path = true;
	}

	private onRtpMapAvailable = (rtpTypes: MediaAttributes['rtp']) => {
		const rtpMap = new Map<number, VideoCodec>();
		rtpTypes.forEach((rtp) => {
			const codec = rtp.codec.toLowerCase();
			if (isVideoCodec(codec)) {
				rtpMap.set(rtp.payload, codec);
			}
		});
		this.emit(EngineEvent.RTPVideoMapUpdate, rtpMap);
	};

	private dataChannelsInfo(): Array<DataChannelInfo> {
		const infos: Array<DataChannelInfo> = [];
		const getInfo = (dc: RTCDataChannel | undefined, target: SignalTarget) => {
			if (dc?.id !== undefined && dc.id !== null) {
				infos.push(
					new DataChannelInfo({
						label: dc.label,
						id: dc.id,
						target,
					}),
				);
			}
		};
		getInfo(this.dataChannelForKind(DataChannelKind.LOSSY), SignalTarget.PUBLISHER);
		getInfo(this.dataChannelForKind(DataChannelKind.RELIABLE), SignalTarget.PUBLISHER);
		getInfo(this.dataChannelForKind(DataChannelKind.LOSSY, true), SignalTarget.SUBSCRIBER);
		getInfo(this.dataChannelForKind(DataChannelKind.RELIABLE, true), SignalTarget.SUBSCRIBER);
		return infos;
	}

	private clearReconnectTimeout() {
		if (this.reconnectTimeout) {
			CriticalTimers.clearTimeout(this.reconnectTimeout);
		}
	}

	private clearPendingReconnect() {
		this.clearReconnectTimeout();
		this.reconnectAttempts = 0;
	}

	private handleBrowserOnLine = async () => {
		if (!this.url) {
			return;
		}
		const hasNetworkConnection = await fetch(toHttpUrl(this.url!), {method: 'HEAD'})
			.then((resp) => resp.ok)
			.catch(() => false);

		if (!hasNetworkConnection) {
			return;
		}
		this.log.info('detected network reconnected');

		if (
			this.client.currentState === SignalConnectionState.RECONNECTING ||
			(this.isWaitingForNetworkReconnect && this.client.currentState === SignalConnectionState.CONNECTED)
		) {
			this.clearReconnectTimeout();
			this.attemptReconnect(ReconnectReason.RR_SIGNAL_DISCONNECTED);
			this.isWaitingForNetworkReconnect = false;
		}
	};

	private handleBrowserOffline = async () => {
		if (!this.url) {
			return;
		}
		try {
			await Promise.race([fetch(toHttpUrl(this.url), {method: 'HEAD'}), sleep(4_000).then(() => Promise.reject())]);
		} catch (_e) {
			if (window.navigator.onLine === false) {
				this.log.info('detected network interruption');
				this.isWaitingForNetworkReconnect = true;
			}
		}
	};

	private registerOnLineListener() {
		if (isWeb()) {
			window.addEventListener('online', this.handleBrowserOnLine);
			window.addEventListener('offline', this.handleBrowserOffline);
		}
	}

	private deregisterOnLineListener() {
		if (isWeb()) {
			window.removeEventListener('online', this.handleBrowserOnLine);
			window.removeEventListener('offline', this.handleBrowserOffline);
		}
	}

	getTrackIdForReceiver(receiver: RTCRtpReceiver): string | undefined {
		const mid = this.pcManager?.getMidForReceiver(receiver);
		if (mid) {
			const match = Object.entries(this.midToTrackId).find(([key]) => key === mid);
			if (match) {
				return match[1];
			}
		}
		return undefined;
	}
}

function getFmtpParameter(sdpFmtpLine: string | undefined, key: string): string | null {
	const lowerKey = key.toLowerCase();
	for (const part of sdpFmtpLine?.split(';') ?? []) {
		const [rawName, ...rawValueParts] = part.split('=');
		if (rawName?.trim().toLowerCase() !== lowerKey) continue;
		const value = rawValueParts.join('=').trim().toLowerCase();
		return value.length > 0 ? value : null;
	}
	return null;
}

function getH264PublisherCodecScore(codec: RtpCodecCapability): number {
	const profileLevelId = getFmtpParameter(codec.sdpFmtpLine, 'profile-level-id');
	const packetizationMode = getFmtpParameter(codec.sdpFmtpLine, 'packetization-mode');
	const packetizationScore = packetizationMode === '1' ? 0 : 1;
	if (profileLevelId && h264PreferredHardwareProfileLevelIds.has(profileLevelId)) return packetizationScore;
	if (profileLevelId === h264OpenH264ProfileLevelId) return 10 + packetizationScore;
	if (profileLevelId) return 20 + packetizationScore;
	return 30 + packetizationScore;
}

function preferHardwareH264Codecs(codecs: ReadonlyArray<RtpCodecCapability>): Array<RtpCodecCapability> {
	return codecs
		.map((codec, index) => ({codec, index, score: getH264PublisherCodecScore(codec)}))
		.sort((a, b) => a.score - b.score || a.index - b.index)
		.map((entry) => entry.codec);
}

export function selectPublisherCodecPreferences(
	codec: VideoCodec,
	codecs: ReadonlyArray<RtpCodecCapability>,
): Array<RtpCodecCapability> {
	const mimeTypes = new Set(videoCodecMimeTypes[codec]);
	const selected = codecs.filter((entry) => mimeTypes.has(entry.mimeType.toLowerCase()));
	if (selected.length === 0) return [];
	const preferred = codec === 'h264' ? preferHardwareH264Codecs(selected) : selected;
	const isH264 = (entry: RtpCodecCapability): boolean => entry.mimeType.toLowerCase() === 'video/h264';
	const remaining = codecs.filter((entry) => !mimeTypes.has(entry.mimeType.toLowerCase()));
	const rankedH264 = preferHardwareH264Codecs(remaining.filter(isH264));
	let nextH264 = 0;
	const rest = remaining.map((entry) => (isH264(entry) ? rankedH264[nextH264++] : entry));
	return [...preferred, ...rest];
}

export type EngineEventCallbacks = {
	connected: (joinResp: JoinResponse) => void;
	disconnected: (reason?: DisconnectReason) => void;
	resuming: () => void;
	resumed: () => void;
	restarting: () => void;
	restarted: () => void;
	signalResumed: () => void;
	signalRestarted: (joinResp: JoinResponse) => void;
	closing: () => void;
	mediaTrackAdded: (track: MediaStreamTrack, streams: MediaStream, receiver: RTCRtpReceiver) => void;
	activeSpeakersUpdate: (speakers: Array<SpeakerInfo>) => void;
	dataPacketReceived: (packet: DataPacket, encryptionType: Encryption_Type) => void;
	transcriptionReceived: (transcription: Transcription) => void;
	transportsCreated: (publisher: PCTransport, subscriber?: PCTransport) => void;
	trackSenderAdded: (track: Track, sender: RTCRtpSender) => void;
	rtpVideoMapUpdate: (rtpMap: Map<number, VideoCodec>) => void;
	dcBufferStatusChanged: (isLow: boolean, kind: DataChannelKind) => void;
	participantUpdate: (infos: Array<ParticipantInfo>) => void;
	roomUpdate: (room: RoomModel) => void;
	roomMoved: (room: RoomMovedResponse) => void;
	connectionQualityUpdate: (update: ConnectionQualityUpdate) => void;
	speakersChanged: (speakerUpdates: Array<SpeakerInfo>) => void;
	streamStateChanged: (update: StreamStateUpdate) => void;
	subscriptionError: (resp: SubscriptionResponse) => void;
	subscriptionPermissionUpdate: (update: SubscriptionPermissionUpdate) => void;
	subscribedQualityUpdate: (update: SubscribedQualityUpdate) => void;
	localTrackUnpublished: (unpublishedResponse: TrackUnpublishedResponse) => void;
	localTrackSubscribed: (trackSid: string) => void;
	remoteMute: (trackSid: string, muted: boolean) => void;
	offline: () => void;
	signalRequestResponse: (response: RequestResponse) => void;
	signalConnected: (joinResp: JoinResponse) => void;
	publishDataTrackResponse: (event: PublishDataTrackResponse) => void;
	unPublishDataTrackResponse: (event: UnpublishDataTrackResponse) => void;
	dataTrackSubscriberHandles: (event: DataTrackSubscriberHandles) => void;
	dataTrackPacketReceived: (packet: Uint8Array) => void;
	joined: (joinResponse: JoinResponse) => void;
	tokenRefreshed: (token: string) => void;
	serverRegionsReported: (regions: RegionSettings) => void;
};

export interface RegionStrategy {
	getNextUrl(abortSignal?: AbortSignal): Promise<string | null>;
	resetAttempts(): void;
}

function applyUserDataCompat(newObj: DataPacket, oldObj: UserPacket) {
	const participantIdentity = newObj.participantIdentity ? newObj.participantIdentity : oldObj.participantIdentity;
	newObj.participantIdentity = participantIdentity;
	oldObj.participantIdentity = participantIdentity;

	const destinationIdentities =
		newObj.destinationIdentities.length !== 0 ? newObj.destinationIdentities : oldObj.destinationIdentities;
	newObj.destinationIdentities = destinationIdentities;
	oldObj.destinationIdentities = destinationIdentities;
}
