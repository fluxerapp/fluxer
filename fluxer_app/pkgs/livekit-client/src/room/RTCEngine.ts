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
	type DisconnectReason,
	EncryptedPacket,
	EncryptedPacketPayload,
	Encryption_Type,
	type JoinResponse,
	type LeaveRequest,
	LeaveRequest_Action,
	type MediaSectionsRequirement,
	type ParticipantInfo,
	ReconnectReason,
	type ReconnectResponse,
	type RequestResponse,
	type Room as RoomModel,
	type RoomMovedResponse,
	RpcAck,
	RpcResponse,
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
	UpdateSubscription,
	type UserPacket,
} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type {MediaAttributes} from 'sdp-transform';
import type TypedEventEmitter from 'typed-emitter';
import type {SignalOptions} from '../api/SignalClient.ts';
import {SignalClient, SignalConnectionState, toProtoSessionDescription} from '../api/SignalClient.ts';
import type {BaseE2EEManager} from '../e2ee/E2eeManager.ts';
import {asEncryptablePacket} from '../e2ee/utils.ts';
import log, {getLogger, LoggerNames} from '../logger.ts';
import type {InternalRoomOptions} from '../options.ts';
import {DataPacketBuffer} from '../utils/dataPacketBuffer.ts';
import TypedPromise from '../utils/TypedPromise.ts';
import {TTLMap} from '../utils/ttlmap.ts';
import {roomConnectOptionDefaults} from './defaults.ts';
import {
	ConnectionError,
	ConnectionErrorReason,
	NegotiationError,
	SignalReconnectError,
	TrackInvalidError,
	UnexpectedConnectionState,
} from './errors.ts';
import {EngineEvent} from './events.ts';
import type PCTransport from './PCTransport.ts';
import {PCEvents} from './PCTransport.ts';
import {PCTransportManager, PCTransportState} from './PCTransportManager.ts';
import type {ReconnectContext, ReconnectPolicy} from './ReconnectPolicy.ts';
import {DEFAULT_MAX_AGE_MS, type RegionUrlProvider} from './RegionUrlProvider.ts';
import type {RpcError} from './rpc.ts';
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
import {isVideoCodec, isVideoTrack, isWeb, sleep, supportsAddTrack, supportsTransceiver, toHttpUrl} from './utils.ts';

const lossyDataChannel = '_lossy';
const reliableDataChannel = '_reliable';
const minReconnectWait = 2 * 1000;
const leaveReconnect = 'leave-reconnect';
const reliabeReceiveStateTTL = 30_000;
const lossyDataChannelBufferThresholdMin = 8 * 1024;
const lossyDataChannelBufferThresholdMax = 256 * 1024;
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

	get pendingReconnect() {
		return !!this.reconnectTimeout;
	}

	private lossyDC?: RTCDataChannel;

	private lossyDCSub?: RTCDataChannel;

	private reliableDC?: RTCDataChannel;

	private dcBufferStatus: Map<DataPacket_Kind, boolean>;

	private reliableDCSub?: RTCDataChannel;

	private subscriberPrimary: boolean = false;

	private pcState: PCState = PCState.New;

	private _isClosed: boolean = true;

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

	private regionUrlProvider?: RegionUrlProvider;

	private log = log;

	private loggerOptions: LoggerOptions;

	private publisherConnectionPromise: Promise<void> | undefined;

	private reliableDataSequence: number = 1;

	private reliableMessageBuffer = new DataPacketBuffer();

	private reliableReceivedState: TTLMap<string, number> = new TTLMap(reliabeReceiveStateTTL);

	private lossyDataStatCurrentBytes: number = 0;

	private lossyDataStatByterate: number = 0;

	private lossyDataStatInterval: TimerHandle | undefined;

	private lossyDataDropCount: number = 0;

	private midToTrackId: {[key: string]: string} = {};

	private isWaitingForNetworkReconnect: boolean = false;

	constructor(private options: InternalRoomOptions) {
		super();
		this.log = getLogger(options.loggerName ?? LoggerNames.Engine);
		this.loggerOptions = {
			loggerName: options.loggerName,
			loggerContextCb: () => this.logContext,
		};
		this.client = new SignalClient(undefined, this.loggerOptions);
		this.client.signalLatency = this.options.expSignalLatency;
		this.reconnectPolicy = this.options.reconnectPolicy;
		this.closingLock = new Mutex();
		this.dataProcessLock = new Mutex();
		this.dcBufferStatus = new Map([
			[DataPacket_Kind.LOSSY, true],
			[DataPacket_Kind.RELIABLE, true],
		]);

		this.client.onParticipantUpdate = (updates) => this.emit(EngineEvent.ParticipantUpdate, updates);
		this.client.onConnectionQuality = (update) => this.emit(EngineEvent.ConnectionQualityUpdate, update);
		this.client.onRoomUpdate = (update) => this.emit(EngineEvent.RoomUpdate, update);
		this.client.onSubscriptionError = (resp) => this.emit(EngineEvent.SubscriptionError, resp);
		this.client.onSubscriptionPermissionUpdate = (update) =>
			this.emit(EngineEvent.SubscriptionPermissionUpdate, update);
		this.client.onSpeakersChanged = (update) => this.emit(EngineEvent.SpeakersChanged, update);
		this.client.onStreamStateUpdate = (update) => this.emit(EngineEvent.StreamStateChanged, update);
		this.client.onRequestResponse = (response) => this.emit(EngineEvent.SignalRequestResponse, response);
	}

	get logContext() {
		return {
			room: this.latestJoinResponse?.room?.name,
			roomID: this.latestJoinResponse?.room?.sid,
			participant: this.latestJoinResponse?.participant?.identity,
			pID: this.participantSid,
		};
	}

	async join(
		url: string,
		token: string,
		opts: SignalOptions,
		abortSignal?: AbortSignal,
		useV0Path: boolean = false,
	): Promise<JoinResponse> {
		this.url = url;
		this.token = token;
		this.signalOpts = opts;
		this.maxJoinAttempts = opts.maxRetries;
		try {
			this.joinAttempts += 1;

			this.setupSignalClientCallbacks();
			const joinResponse = await this.client.join(url, token, opts, abortSignal, useV0Path);
			this._isClosed = false;
			this.latestJoinResponse = joinResponse;

			this.subscriberPrimary = joinResponse.subscriberPrimary;
			if (!this.pcManager) {
				await this.configure(joinResponse, !useV0Path);
			}

			if (!this.subscriberPrimary || joinResponse.fastPublish) {
				this.negotiate().catch((err) => {
					log.error(err, this.logContext);
				});
			}

			this.registerOnLineListener();
			this.clientConfiguration = joinResponse.clientConfiguration;
			this.emit(EngineEvent.SignalConnected, joinResponse);
			return joinResponse;
		} catch (e) {
			if (e instanceof ConnectionError) {
				if (e.reason === ConnectionErrorReason.ServerUnreachable) {
					this.log.warn(
						`Couldn't connect to server, attempt ${this.joinAttempts} of ${this.maxJoinAttempts}`,
						this.logContext,
					);
					if (this.joinAttempts < this.maxJoinAttempts) {
						return this.join(url, token, opts, abortSignal, useV0Path);
					}
				} else if (e.reason === ConnectionErrorReason.ServiceNotFound) {
					this.log.warn(`Initial connection failed: ${e.message} – Retrying`);
					return this.join(url, token, opts, abortSignal, true);
				}
			}
			throw e;
		}
	}

	async close() {
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
			this.cleanupLossyDataStats();
			await this.cleanupPeerConnections();
			await this.cleanupClient();
		} finally {
			unlock();
		}
	}

	async cleanupPeerConnections() {
		await this.pcManager?.close();
		this.pcManager = undefined;

		const dcCleanup = (dc: RTCDataChannel | undefined) => {
			if (!dc) return;
			dc.close();
			dc.onbufferedamountlow = null;
			dc.onclose = null;
			dc.onclosing = null;
			dc.onerror = null;
			dc.onmessage = null;
			dc.onopen = null;
		};
		dcCleanup(this.lossyDC);
		dcCleanup(this.lossyDCSub);
		dcCleanup(this.reliableDC);
		dcCleanup(this.reliableDCSub);

		this.lossyDC = undefined;
		this.lossyDCSub = undefined;
		this.reliableDC = undefined;
		this.reliableDCSub = undefined;
		this.reliableMessageBuffer = new DataPacketBuffer();
		this.reliableDataSequence = 1;
		this.reliableReceivedState.clear();
	}

	cleanupLossyDataStats() {
		this.lossyDataStatByterate = 0;
		this.lossyDataStatCurrentBytes = 0;
		if (this.lossyDataStatInterval) {
			CriticalTimers.clearInterval(this.lossyDataStatInterval);
			this.lossyDataStatInterval = undefined;
		}
		this.lossyDataDropCount = 0;
	}

	async cleanupClient() {
		await this.client.close();
		this.client.resetCallbacks();
	}

	addTrack(req: AddTrackRequest): Promise<TrackInfo> {
		if (this.pendingTrackResolvers[req.cid]) {
			throw new TrackInvalidError('a track with the same ID has already been published');
		}
		return new Promise<TrackInfo>((resolve, reject) => {
			const publicationTimeout = setTimeout(() => {
				delete this.pendingTrackResolvers[req.cid];
				reject(ConnectionError.timeout('publication of local track timed out, no response from server'));
			}, 10_000);
			this.pendingTrackResolvers[req.cid] = {
				resolve: (info: TrackInfo) => {
					clearTimeout(publicationTimeout);
					resolve(info);
				},
				reject: () => {
					clearTimeout(publicationTimeout);
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
			this.log.warn('failed to remove track', {...this.logContext, error: e});
		}
		return false;
	}

	updateMuteStatus(trackSid: string, muted: boolean) {
		this.client.sendMuteTrack(trackSid, muted);
	}

	get dataSubscriberReadyState(): string | undefined {
		return this.reliableDCSub?.readyState;
	}

	async getConnectedServerAddress(): Promise<string | undefined> {
		return this.pcManager?.getConnectedAddress();
	}

	setRegionUrlProvider(provider: RegionUrlProvider) {
		this.regionUrlProvider = provider;
	}

	private async configure(joinResponse: JoinResponse, useSinglePeerConnection: boolean) {
		if (this.pcManager && this.pcManager.currentState !== PCTransportState.NEW) {
			return;
		}

		this.participantSid = joinResponse.participant?.sid;

		const rtcConfig = this.makeRTCConfiguration(joinResponse);

		this.pcManager = new PCTransportManager(
			rtcConfig,
			useSinglePeerConnection
				? 'publisher-only'
				: joinResponse.subscriberPrimary
					? 'subscriber-primary'
					: 'publisher-primary',
			this.loggerOptions,
			this.options.subscriberVideoCodecExclusions,
		);

		this.emit(EngineEvent.TransportsCreated, this.pcManager.publisher, this.pcManager.subscriber);

		this.pcManager.onIceCandidate = (candidate, target) => {
			this.client.sendIceCandidate(candidate, target);
		};

		this.pcManager.onPublisherOffer = (offer, offerId) => {
			this.client.sendOffer(offer, offerId);
		};

		this.pcManager.onDataChannel = this.handleDataChannel;
		this.pcManager.onStateChange = async (connectionState, publisherState, subscriberState) => {
			this.log.debug(`primary PC state changed ${connectionState}`, this.logContext);

			if (['closed', 'disconnected', 'failed'].includes(publisherState)) {
				this.publisherConnectionPromise = undefined;
			}
			if (connectionState === PCTransportState.CONNECTED) {
				const shouldEmit = this.pcState === PCState.New;
				this.pcState = PCState.Connected;
				if (shouldEmit) {
					this.emit(EngineEvent.Connected, joinResponse);
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

		if (!supportOptionalDatachannel(joinResponse.serverInfo?.protocol)) {
			this.createDataChannels();
		}
	}

	private setupSignalClientCallbacks() {
		this.client.onAnswer = async (sd, offerId, midToTrackId) => {
			if (!this.pcManager) {
				return;
			}
			this.log.debug('received server answer', {
				...this.logContext,
				RTCSdpType: sd.type,
				sdp: sd.sdp,
				midToTrackId,
			});
			this.midToTrackId = midToTrackId;
			await this.pcManager.setPublisherAnswer(sd, offerId);
		};

		this.client.onTrickle = (candidate, target) => {
			if (!this.pcManager) {
				return;
			}
			this.log.debug('got ICE candidate from peer', {...this.logContext, candidate, target});
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
				...this.logContext,
				cid: res.cid,
				track: res.track?.sid,
			});
			if (!this.pendingTrackResolvers[res.cid]) {
				this.log.error(`missing track resolver for ${res.cid}`, {
					...this.logContext,
					cid: res.cid,
				});
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
			this.regionUrlProvider?.updateToken(token);
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
			const transceiverInit: RTCRtpTransceiverInit = {direction: 'recvonly'};
			for (let i: number = 0; i < requirement.numAudios; i++) {
				this.pcManager?.addPublisherTransceiverOfKind('audio', transceiverInit);
			}
			for (let i: number = 0; i < requirement.numVideos; i++) {
				this.pcManager?.addPublisherTransceiverOfKind('video', transceiverInit);
			}

			this.negotiate();
		};

		this.client.onClose = () => {
			this.handleDisconnect('signal', ReconnectReason.RR_SIGNAL_DISCONNECTED);
		};

		this.client.onLeave = (leave: LeaveRequest) => {
			this.log.debug('client leave request', {...this.logContext, reason: leave?.reason});
			if (leave.regions && this.regionUrlProvider) {
				this.log.debug('updating regions', this.logContext);
				this.regionUrlProvider.setServerReportedRegions({
					updatedAtInMs: Date.now(),
					maxAgeInMs: DEFAULT_MAX_AGE_MS,
					regionSettings: leave.regions,
				});
			}
			switch (leave.action) {
				case LeaveRequest_Action.DISCONNECT:
					this.emit(EngineEvent.Disconnected, leave?.reason);
					this.close();
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

	private makeRTCConfiguration(serverResponse: JoinResponse | ReconnectResponse): RTCConfiguration {
		const rtcConfig = {...this.rtcConfig};

		if (this.signalOpts?.e2eeEnabled) {
			this.log.debug('E2EE - setting up transports with insertable streams', this.logContext);
			rtcConfig.encodedInsertableStreams = true;
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

		rtcConfig.sdpSemantics = 'unified-plan';
		rtcConfig.continualGatheringPolicy = 'gather_continually';

		return rtcConfig;
	}

	private createDataChannels() {
		if (!this.pcManager) {
			return;
		}

		if (this.lossyDC) {
			this.lossyDC.onmessage = null;
			this.lossyDC.onerror = null;
		}
		if (this.reliableDC) {
			this.reliableDC.onmessage = null;
			this.reliableDC.onerror = null;
		}

		this.lossyDC = this.pcManager.createPublisherDataChannel(lossyDataChannel, {
			ordered: false,
			maxRetransmits: 0,
		});
		this.reliableDC = this.pcManager.createPublisherDataChannel(reliableDataChannel, {
			ordered: true,
		});

		this.lossyDC.onmessage = this.handleDataMessage;
		this.reliableDC.onmessage = this.handleDataMessage;

		this.lossyDC.onerror = this.handleDataError;
		this.reliableDC.onerror = this.handleDataError;

		this.lossyDC.bufferedAmountLowThreshold = 65535;
		this.reliableDC.bufferedAmountLowThreshold = 65535;

		this.lossyDC.onbufferedamountlow = this.handleBufferedAmountLow;
		this.reliableDC.onbufferedamountlow = this.handleBufferedAmountLow;

		this.cleanupLossyDataStats();
		this.lossyDataStatInterval = setInterval(() => {
			this.lossyDataStatByterate = this.lossyDataStatCurrentBytes;
			this.lossyDataStatCurrentBytes = 0;

			const dc = this.dataChannelForKind(DataPacket_Kind.LOSSY);
			if (dc) {
				const threshold = this.lossyDataStatByterate / 10;
				dc.bufferedAmountLowThreshold = Math.min(
					Math.max(threshold, lossyDataChannelBufferThresholdMin),
					lossyDataChannelBufferThresholdMax,
				);
			}
		}, 1000);
	}

	private handleDataChannel = async ({channel}: RTCDataChannelEvent) => {
		if (!channel) {
			return;
		}
		if (channel.label === reliableDataChannel) {
			this.reliableDCSub = channel;
		} else if (channel.label === lossyDataChannel) {
			this.lossyDCSub = channel;
		} else {
			return;
		}
		this.log.debug(`on data channel ${channel.id}, ${channel.label}`, this.logContext);
		channel.onmessage = this.handleDataMessage;
	};

	private handleDataMessage = async (message: MessageEvent) => {
		const unlock = await this.dataProcessLock.lock();
		try {
			let buffer: ArrayBuffer | undefined;
			if (message.data instanceof ArrayBuffer) {
				buffer = message.data;
			} else if (message.data instanceof Blob) {
				buffer = await message.data.arrayBuffer();
			} else {
				this.log.error('unsupported data type', {...this.logContext, data: message.data});
				return;
			}
			const dp = DataPacket.fromBinary(new Uint8Array(buffer));

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
					this.log.error('Received encrypted packet but E2EE not set up', this.logContext);
					return;
				}
				const decryptedData = await this.e2eeManager?.handleEncryptedData(
					dp.value.value.encryptedValue,
					dp.value.value.iv,
					dp.participantIdentity,
					dp.value.value.keyIndex,
				);
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

	private handleDataError = (event: Event) => {
		const channel = event.currentTarget as RTCDataChannel;
		const channelKind = channel.maxRetransmits === 0 ? 'lossy' : 'reliable';

		if (event instanceof ErrorEvent && event.error) {
			const {error} = event.error;
			this.log.error(`DataChannel error on ${channelKind}: ${event.message}`, {
				...this.logContext,
				error,
			});
		} else {
			this.log.error(`Unknown DataChannel error on ${channelKind}`, {...this.logContext, event});
		}
	};

	private handleBufferedAmountLow = (event: Event) => {
		const channel = event.currentTarget as RTCDataChannel;
		const channelKind = channel.maxRetransmits === 0 ? DataPacket_Kind.LOSSY : DataPacket_Kind.RELIABLE;

		this.updateAndEmitDCBufferStatus(channelKind);
	};

	async createSender(track: LocalTrack, opts: TrackPublishOptions, encodings?: Array<RTCRtpEncodingParameters>) {
		if (supportsTransceiver()) {
			const sender = await this.createTransceiverRTCRtpSender(track, opts, encodings);
			return sender;
		}
		if (supportsAddTrack()) {
			this.log.warn('using add-track fallback', this.logContext);
			const sender = await this.createRTCRtpSender(track.mediaStreamTrack);
			return sender;
		}
		throw new UnexpectedConnectionState('Required webRTC APIs not supported on this device');
	}

	async createSimulcastSender(
		track: LocalVideoTrack,
		simulcastTrack: SimulcastTrackInfo,
		opts: TrackPublishOptions,
		encodings?: Array<RTCRtpEncodingParameters>,
	) {
		if (supportsTransceiver()) {
			return this.createSimulcastTransceiverSender(track, simulcastTrack, opts, encodings);
		}
		if (supportsAddTrack()) {
			this.log.debug('using add-track fallback', this.logContext);
			return this.createRTCRtpSender(track.mediaStreamTrack);
		}

		throw new UnexpectedConnectionState('Cannot stream on this device');
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
		track.setSimulcastTrackSender(opts.videoCodec, transceiver.sender);
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
		if (preferences.length === 0) return;
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

		this.log.warn(`${connection} disconnected`, this.logContext);
		if (this.reconnectAttempts === 0) {
			this.reconnectStart = Date.now();
		}

		const disconnect = (duration: number) => {
			this.log.warn(
				`could not recover connection after ${this.reconnectAttempts} attempts, ${duration}ms. giving up`,
				this.logContext,
			);
			this.emit(EngineEvent.Disconnected);
			this.close();
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

		this.log.debug(`reconnecting in ${delay}ms`, this.logContext);

		this.clearReconnectTimeout();
		if (this.token && this.regionUrlProvider) {
			this.regionUrlProvider.updateToken(this.token);
		}
		this.reconnectTimeout = CriticalTimers.setTimeout(
			() => this.attemptReconnect(disconnectReason).finally(() => (this.reconnectTimeout = undefined)),
			delay,
		);
	};

	private async attemptReconnect(reason?: ReconnectReason) {
		if (this._isClosed) {
			return;
		}
		if (this.attemptingReconnect) {
			log.warn('already attempting reconnect, returning early', this.logContext);
			return;
		}
		if (
			this.clientConfiguration?.resumeConnection === ClientConfigSetting.DISABLED ||
			(this.pcManager?.currentState ?? PCTransportState.NEW) === PCTransportState.NEW
		) {
			this.fullReconnectOnNext = true;
		}

		try {
			this.attemptingReconnect = true;
			if (this.fullReconnectOnNext) {
				await this.restartConnection();
			} else {
				await this.resumeConnection(reason);
			}
			this.clearPendingReconnect();
			this.fullReconnectOnNext = false;
		} catch (e) {
			this.reconnectAttempts += 1;
			let recoverable = true;
			if (e instanceof UnexpectedConnectionState) {
				this.log.debug('received unrecoverable error', {...this.logContext, error: e});
				recoverable = false;
			} else if (!(e instanceof SignalReconnectError)) {
				this.fullReconnectOnNext = true;
			}

			if (recoverable) {
				this.handleDisconnect('reconnect', ReconnectReason.RR_UNKNOWN);
			} else {
				this.log.info(
					`could not recover connection after ${this.reconnectAttempts} attempts, ${
						Date.now() - this.reconnectStart
					}ms. giving up`,
					this.logContext,
				);
				this.emit(EngineEvent.Disconnected);
				await this.close();
			}
		} finally {
			this.attemptingReconnect = false;
		}
	}

	private getNextRetryDelay(context: ReconnectContext) {
		try {
			return this.reconnectPolicy.nextRetryDelayInMs(context);
		} catch (e) {
			this.log.warn('encountered error in reconnect policy', {...this.logContext, error: e});
		}

		return null;
	}

	private async restartConnection(regionUrl?: string) {
		try {
			if (!this.url || !this.token) {
				throw new UnexpectedConnectionState('could not reconnect, url or token not saved');
			}

			this.log.info(`reconnecting, attempt: ${this.reconnectAttempts}`, this.logContext);
			this.emit(EngineEvent.Restarting);

			if (!this.client.isDisconnected) {
				await this.client.sendLeave();
			}
			await this.cleanupPeerConnections();
			await this.cleanupClient();

			let joinResponse: JoinResponse;
			try {
				if (!this.signalOpts) {
					this.log.warn('attempted connection restart, without signal options present', this.logContext);
					throw new SignalReconnectError();
				}
				joinResponse = await this.join(
					regionUrl ?? this.url,
					this.token,
					this.signalOpts,
					undefined,
					!this.options.singlePeerConnection,
				);
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

			this.regionUrlProvider?.resetAttempts();
			this.emit(EngineEvent.Restarted);
		} catch (error) {
			const nextRegionUrl = await this.regionUrlProvider?.getNextBestRegionUrl();
			if (nextRegionUrl) {
				await this.restartConnection(nextRegionUrl);
				return;
			} else {
				this.regionUrlProvider?.resetAttempts();
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

		this.log.info(`resuming signal connection, attempt ${this.reconnectAttempts}`, this.logContext);
		this.emit(EngineEvent.Resuming);
		let res: ReconnectResponse | undefined;
		try {
			this.setupSignalClientCallbacks();
			res = await this.client.reconnect(this.url, this.token, this.participantSid, reason);
		} catch (error) {
			let message = '';
			if (error instanceof Error) {
				message = error.message;
				this.log.error(error.message, {...this.logContext, error});
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
			this.log.warn('Did not receive reconnect response', this.logContext);
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

		if (this.reliableDC?.readyState === 'open' && this.reliableDC.id === null) {
			this.createDataChannels();
		}

		if (res?.lastMessageSeq) {
			this.resendReliableMessagesForResume(res.lastMessageSeq);
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

		this.log.debug('waiting for peer connection to reconnect', this.logContext);
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

	async publishRpcResponse(
		destinationIdentity: string,
		requestId: string,
		payload: string | null,
		error: RpcError | null,
	) {
		const packet = new DataPacket({
			destinationIdentities: [destinationIdentity],
			kind: DataPacket_Kind.RELIABLE,
			value: {
				case: 'rpcResponse',
				value: new RpcResponse({
					requestId,
					value: error ? {case: 'error', value: error.toProto()} : {case: 'payload', value: payload ?? ''},
				}),
			},
		});

		await this.sendDataPacket(packet, DataPacket_Kind.RELIABLE);
	}

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
		await this.sendDataPacket(packet, DataPacket_Kind.RELIABLE);
	}

	async sendDataPacket(packet: DataPacket, kind: DataPacket_Kind) {
		await this.ensurePublisherConnected(kind);

		if (this.e2eeManager?.isDataChannelEncryptionEnabled) {
			const encryptablePacket = asEncryptablePacket(packet);
			if (encryptablePacket) {
				const encryptedData = await this.e2eeManager.encryptData(encryptablePacket.toBinary());
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

		if (kind === DataPacket_Kind.RELIABLE) {
			packet.sequence = this.reliableDataSequence;
			this.reliableDataSequence += 1;
		}

		const msg = packet.toBinary();

		const dc = this.dataChannelForKind(kind);
		if (dc) {
			if (kind === DataPacket_Kind.RELIABLE) {
				await this.waitForBufferStatusLow(kind);
				this.reliableMessageBuffer.push({data: msg, sequence: packet.sequence});
			} else {
				if (!this.isBufferStatusLow(kind)) {
					this.lossyDataDropCount += 1;
					if (this.lossyDataDropCount % 100 === 0) {
						this.log.warn(
							`dropping lossy data channel messages, total dropped: ${this.lossyDataDropCount}`,
							this.logContext,
						);
					}
					return;
				}
				this.lossyDataStatCurrentBytes += msg.byteLength;
			}

			if (this.attemptingReconnect) {
				return;
			}

			dc.send(new Uint8Array(msg));
		}

		this.updateAndEmitDCBufferStatus(kind);
	}

	private async resendReliableMessagesForResume(lastMessageSeq: number) {
		await this.ensurePublisherConnected(DataPacket_Kind.RELIABLE);
		const dc = this.dataChannelForKind(DataPacket_Kind.RELIABLE);
		if (dc) {
			this.reliableMessageBuffer.popToSequence(lastMessageSeq);
			this.reliableMessageBuffer.getAll().forEach((msg) => {
				dc.send(new Uint8Array(msg.data));
			});
		}
		this.updateAndEmitDCBufferStatus(DataPacket_Kind.RELIABLE);
	}

	private updateAndEmitDCBufferStatus = (kind: DataPacket_Kind) => {
		if (kind === DataPacket_Kind.RELIABLE) {
			const dc = this.dataChannelForKind(kind);
			if (dc) {
				this.reliableMessageBuffer.alignBufferedAmount(dc.bufferedAmount);
			}
		}

		const status = this.isBufferStatusLow(kind);
		if (typeof status !== 'undefined' && status !== this.dcBufferStatus.get(kind)) {
			this.dcBufferStatus.set(kind, status);
			this.emit(EngineEvent.DCBufferStatusChanged, status, kind);
		}
	};

	private isBufferStatusLow = (kind: DataPacket_Kind): boolean | undefined => {
		const dc = this.dataChannelForKind(kind);
		if (dc) {
			return dc.bufferedAmount <= dc.bufferedAmountLowThreshold;
		}
		return undefined;
	};

	waitForBufferStatusLow(kind: DataPacket_Kind): TypedPromise<void, UnexpectedConnectionState> {
		return new TypedPromise(async (resolve, reject) => {
			if (this.isBufferStatusLow(kind)) {
				resolve();
			} else {
				const onClosing = () => reject(new UnexpectedConnectionState('engine closed'));
				this.once(EngineEvent.Closing, onClosing);
				while (!this.dcBufferStatus.get(kind)) {
					await sleep(10);
				}
				this.off(EngineEvent.Closing, onClosing);
				resolve();
			}
		});
	}

	async ensureDataTransportConnected(kind: DataPacket_Kind, subscriber: boolean = this.subscriberPrimary) {
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
				log.error(err, this.logContext);
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

	private async ensurePublisherConnected(kind: DataPacket_Kind) {
		if (!this.publisherConnectionPromise) {
			this.publisherConnectionPromise = this.ensureDataTransportConnected(kind, false);
		}
		await this.publisherConnectionPromise;
	}

	verifyTransport(): boolean {
		if (!this.pcManager) {
			return false;
		}
		const allowedConnectionStates: Array<PCTransportState> = [PCTransportState.CONNECTING, PCTransportState.CONNECTED];
		if (!allowedConnectionStates.includes(this.pcManager.currentState)) {
			return false;
		}

		if (!this.client.ws || this.client.ws.readyState === WebSocket.CLOSED) {
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
			if (this.pcManager.publisher.getTransceivers().length === 0 && !this.lossyDC && !this.reliableDC) {
				this.createDataChannels();
			}

			const abortController = new AbortController();

			const handleClosed = () => {
				abortController.abort();
				this.log.debug('engine disconnected while negotiation was ongoing', this.logContext);
				resolve();
				return;
			};

			if (this.isClosed) {
				reject(new NegotiationError('cannot negotiate on closed engine'));
			}
			this.on(EngineEvent.Closing, handleClosed);
			this.on(EngineEvent.Restarting, handleClosed);

			this.pcManager.publisher.once(PCEvents.RTPVideoPayloadTypes, (rtpTypes: MediaAttributes['rtp']) => {
				const rtpMap = new Map<number, VideoCodec>();
				rtpTypes.forEach((rtp) => {
					const codec = rtp.codec.toLowerCase();
					if (isVideoCodec(codec)) {
						rtpMap.set(rtp.payload, codec);
					}
				});
				this.emit(EngineEvent.RTPVideoMapUpdate, rtpMap);
			});

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

	dataChannelForKind(kind: DataPacket_Kind, sub?: boolean): RTCDataChannel | undefined {
		if (!sub) {
			if (kind === DataPacket_Kind.LOSSY) {
				return this.lossyDC;
			}
			if (kind === DataPacket_Kind.RELIABLE) {
				return this.reliableDC;
			}
		} else {
			if (kind === DataPacket_Kind.LOSSY) {
				return this.lossyDCSub;
			}
			if (kind === DataPacket_Kind.RELIABLE) {
				return this.reliableDCSub;
			}
		}
		return undefined;
	}

	sendSyncState(remoteTracks: Array<RemoteTrackPublication>, localTracks: Array<LocalTrackPublication>) {
		if (!this.pcManager) {
			this.log.warn('sync state cannot be sent without peer connection setup', this.logContext);
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
			}),
		);
	}

	failNext() {
		this.shouldFailNext = true;
	}

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
		getInfo(this.dataChannelForKind(DataPacket_Kind.LOSSY), SignalTarget.PUBLISHER);
		getInfo(this.dataChannelForKind(DataPacket_Kind.RELIABLE), SignalTarget.PUBLISHER);
		getInfo(this.dataChannelForKind(DataPacket_Kind.LOSSY, true), SignalTarget.SUBSCRIBER);
		getInfo(this.dataChannelForKind(DataPacket_Kind.RELIABLE, true), SignalTarget.SUBSCRIBER);
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
	const rtx = codecs.filter((entry) => entry.mimeType.toLowerCase() === 'video/rtx');
	return [...preferred, ...rtx];
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
	dcBufferStatusChanged: (isLow: boolean, kind: DataPacket_Kind) => void;
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
};

function supportOptionalDatachannel(protocol: number | undefined): boolean {
	return protocol !== undefined && protocol > 13;
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
