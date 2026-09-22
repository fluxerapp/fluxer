// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {
	type AddTrackRequest,
	type AudioTrackFeature,
	type ClientInfo,
	type ClientInfo_Capability,
	type ConnectionQualityUpdate,
	ConnectionSettings,
	type DataTrackSubscriberHandles,
	DisconnectReason,
	Encryption_Type,
	JoinRequest,
	type JoinResponse,
	LeaveRequest,
	LeaveRequest_Action,
	type MediaSectionsRequirement,
	MuteTrackRequest,
	type ParticipantInfo,
	Ping,
	PublishDataTrackRequest,
	type PublishDataTrackResponse,
	protoInt64,
	type ReconnectReason,
	type ReconnectResponse,
	type RequestResponse,
	type Room,
	type RoomMovedResponse,
	SessionDescription,
	SignalRequest,
	type SignalResponse,
	type SignalTarget,
	type SimulateScenario,
	type SpeakerInfo,
	type StreamStateUpdate,
	type SubscribedQualityUpdate,
	SubscriptionPermission,
	type SubscriptionPermissionUpdate,
	type SubscriptionResponse,
	type SyncState,
	type TrackPermission,
	type TrackPublishedResponse,
	type TrackUnpublishedResponse,
	TrickleRequest,
	UnpublishDataTrackRequest,
	type UnpublishDataTrackResponse,
	UpdateDataSubscription,
	UpdateDataSubscription_Update,
	UpdateLocalAudioTrack,
	UpdateParticipantMetadata,
	type UpdateSubscription,
	type UpdateTrackSettings,
	UpdateVideoLayers,
	type VideoLayer,
	WrappedJoinRequest,
	WrappedJoinRequest_Compression,
} from '@livekit/protocol';
import log, {getLogger, LoggerNames} from '../logger.ts';
import type {DataTrackHandle} from '../room/data-track/handle.ts';
import type {DataTrackSid} from '../room/data-track/types.ts';
import {ConnectionError, ConnectionErrorReason} from '../room/errors.ts';
import CriticalTimers, {type TimerHandle} from '../room/timers.ts';
import type {LoggerOptions} from '../room/types.ts';
import {getClientInfo, isCompressionStreamSupported, isReactNative, sleep} from '../room/utils.ts';
import type {NonSharedUint8Array} from '../type-polyfills/non-shared-typed-arrays.ts';
import {AsyncQueue} from '../utils/AsyncQueue.ts';
import {announceMachine} from '../utils/machineInspector.ts';
import {
	createSignalMachine,
	type SignalLifecycleState,
	type SignalMachine,
	type SignalMachineInput,
} from './SignalClientStateMachine.ts';
import {createRtcUrl, createValidateUrl, getAbortReasonAsString, parseSignalResponse} from './utils.ts';
import {type WebSocketConnection, WebSocketStream} from './WebSocketStream.ts';

interface ConnectOpts extends SignalOptions {
	reconnect?: boolean;
	reconnectReason?: number;
	sid?: string;
}

export interface SignalOptions {
	autoSubscribe: boolean;
	adaptiveStream?: boolean;
	clientInfoCapabilities?: Array<ClientInfo_Capability>;
	maxRetries: number;
	e2eeEnabled: boolean;
	websocketTimeout: number;
}

type SignalMessage = SignalRequest['message'];

type SignalKind = NonNullable<SignalMessage>['case'];

const passThroughQueueSignals: Array<SignalKind> = ['syncState', 'trickle', 'offer', 'answer', 'simulate', 'leave'];

function isRecoverableReconnectError(error: unknown): boolean {
	if (error instanceof ConnectionError) {
		return error.reason !== ConnectionErrorReason.LeaveRequest && error.reason !== ConnectionErrorReason.NotAllowed;
	}
	return true;
}

function canPassThroughQueue(req: SignalMessage): boolean {
	return passThroughQueueSignals.indexOf(req!.case) >= 0;
}

export enum SignalConnectionState {
	CONNECTING,
	CONNECTED,
	RECONNECTING,
	DISCONNECTING,
	DISCONNECTED,
}

function lifecycleToConnectionState(lifecycle: SignalLifecycleState): SignalConnectionState {
	switch (lifecycle) {
		case 'connected':
			return SignalConnectionState.CONNECTED;
		case 'connecting':
			return SignalConnectionState.CONNECTING;
		case 'reconnecting':
			return SignalConnectionState.RECONNECTING;
		case 'disconnecting':
			return SignalConnectionState.DISCONNECTING;
		default:
			return SignalConnectionState.DISCONNECTED;
	}
}

const MAX_WS_CLOSE_TIME = 250;

const JOIN_RESPONSE_TIMEOUT = 5_000;

export class SignalClient {
	requestQueue: AsyncQueue;

	queuedRequests: Array<() => Promise<void>>;

	useJSON: boolean;

	rtt: number = 0;

	signalLatency?: number;

	onClose?: (reason: string) => void;

	onAnswer?: (sd: RTCSessionDescriptionInit, offerId: number, midToTrackId: {[key: string]: string}) => void;

	onOffer?: (sd: RTCSessionDescriptionInit, offerId: number, midToTrackId: {[key: string]: string}) => void;

	onTrickle?: (sd: RTCIceCandidateInit, target: SignalTarget) => void;

	onParticipantUpdate?: (updates: Array<ParticipantInfo>) => void;

	onLocalTrackPublished?: (res: TrackPublishedResponse) => void;

	onNegotiateRequested?: () => void;

	onSpeakersChanged?: (res: Array<SpeakerInfo>) => void;

	onRemoteMuteChanged?: (trackSid: string, muted: boolean) => void;

	onRoomUpdate?: (room: Room) => void;

	onConnectionQuality?: (update: ConnectionQualityUpdate) => void;

	onStreamStateUpdate?: (update: StreamStateUpdate) => void;

	onSubscribedQualityUpdate?: (update: SubscribedQualityUpdate) => void;

	onSubscriptionPermissionUpdate?: (update: SubscriptionPermissionUpdate) => void;

	onSubscriptionError?: (update: SubscriptionResponse) => void;

	onLocalTrackUnpublished?: (res: TrackUnpublishedResponse) => void;

	onTokenRefresh?: (token: string) => void;

	onLeave?: (leave: LeaveRequest) => void;

	onRequestResponse?: (response: RequestResponse) => void;

	onLocalTrackSubscribed?: (trackSid: string) => void;

	onRoomMoved?: (res: RoomMovedResponse) => void;

	onMediaSectionsRequirement?: (requirement: MediaSectionsRequirement) => void;

	onPublishDataTrackResponse?: (event: PublishDataTrackResponse) => void;

	onUnPublishDataTrackResponse?: (event: UnpublishDataTrackResponse) => void;

	onDataTrackSubscriberHandles?: (event: DataTrackSubscriberHandles) => void;

	onJoined?: (event: JoinResponse) => void;

	connectOptions?: ConnectOpts;

	ws?: WebSocketStream;

	get currentState() {
		return lifecycleToConnectionState(this.lifecycleState);
	}

	get isDisconnected() {
		const state = this.currentState;
		return state === SignalConnectionState.DISCONNECTING || state === SignalConnectionState.DISCONNECTED;
	}

	private get lifecycleState(): SignalLifecycleState {
		return this.machine.currentState();
	}

	private get attemptId() {
		return this.machine.context.attemptId;
	}

	private sendLifecycleInput(input: SignalMachineInput): boolean {
		const before = this.lifecycleState;
		this.machine.handle(input.type, input);
		return this.lifecycleState !== before;
	}

	private async settleInFlightClose() {
		this.log.debug('waiting for an in-flight close to settle before establishing a session');
		(await this.closingLock.lock())();
	}

	private get isEstablishingConnection() {
		return this.lifecycleState === 'connecting' || this.lifecycleState === 'reconnecting';
	}

	private getNextRequestId() {
		this._requestId += 1;
		return this._requestId;
	}

	private options?: SignalOptions;

	private pingTimeout: TimerHandle | undefined;

	private pingTimeoutDuration: number | undefined;

	private pingIntervalDuration: number | undefined;

	private pingInterval: TimerHandle | undefined;

	private closingLock: Mutex;

	private machine: SignalMachine;

	private connectionLock: Mutex;

	private log = log;

	private loggerContextCb?: LoggerOptions['loggerContextCb'];

	private _requestId = 0;

	private streamWriter: WritableStreamDefaultWriter<ArrayBuffer | string> | undefined;

	private useV0SignalPath = false;

	constructor(useJSON: boolean = false, loggerOptions: LoggerOptions = {}) {
		this.loggerContextCb = loggerOptions.loggerContextCb;
		this.log = getLogger(loggerOptions.loggerName ?? LoggerNames.Signal, () => this.logContext);
		this.useJSON = useJSON;
		this.requestQueue = new AsyncQueue();
		this.queuedRequests = [];
		this.closingLock = new Mutex();
		this.connectionLock = new Mutex();
		this.machine = createSignalMachine();
		this.machine.on('transitioned', ({fromState, toState}) => {
			this.log.debug(`signal lifecycle: ${fromState} -> ${toState}`);
		});
		this.machine.on('nohandler', ({inputName}) => {
			this.log.debug(`ignoring signal lifecycle input ${inputName} in state ${this.lifecycleState}`);
		});
		announceMachine('signal', this.machine);
	}

	private get logContext() {
		return this.loggerContextCb?.() ?? {};
	}

	async join(
		url: string,
		token: string,
		opts: SignalOptions,
		abortSignal?: AbortSignal,
		useV0Path: boolean = false,
		publisherOffer?: SessionDescription,
	): Promise<JoinResponse> {
		if (this.lifecycleState === 'disconnecting') {
			await this.settleInFlightClose();
		}
		if (!this.sendLifecycleInput({type: 'connect'})) {
			throw ConnectionError.internal(
				`cannot establish a signal session from '${this.lifecycleState}', close the current one first`,
			);
		}
		this.options = opts;
		try {
			const res = await this.connect(url, token, opts, abortSignal, useV0Path, publisherOffer);
			return res as JoinResponse;
		} catch (e) {
			this.sendLifecycleInput({type: 'connectFailed', error: e});
			throw e;
		}
	}

	async reconnect(
		url: string,
		token: string,
		sid?: string,
		reason?: ReconnectReason,
	): Promise<ReconnectResponse | undefined> {
		if (!this.options) {
			this.log.warn('attempted to reconnect without signal options being set, ignoring');
			return;
		}
		if (this.lifecycleState === 'disconnecting') {
			await this.settleInFlightClose();
		}
		if (!this.sendLifecycleInput({type: 'reconnect'})) {
			throw ConnectionError.internal(`cannot resume the signal session from '${this.lifecycleState}'`);
		}
		this.clearPingInterval();

		try {
			const res = (await this.connect(
				url,
				token,
				{
					...this.options,
					reconnect: true,
					sid,
					reconnectReason: reason,
				},
				undefined,
				this.useV0SignalPath,
			)) as ReconnectResponse | undefined;
			return res;
		} catch (e) {
			this.sendLifecycleInput({
				type: 'reconnectFailed',
				error: e,
				recoverable: isRecoverableReconnectError(e),
			});
			throw e;
		}
	}

	private async connect(
		url: string,
		token: string,
		opts: ConnectOpts,
		abortSignal?: AbortSignal,
		useV0Path: boolean = false,
		publisherOffer?: SessionDescription,
	): Promise<JoinResponse | ReconnectResponse | undefined> {
		const unlock = await this.connectionLock.lock();

		this.connectOptions = opts;
		this.useV0SignalPath = useV0Path;

		const clientInfo = getClientInfo(opts.clientInfoCapabilities);
		const params = useV0Path
			? createConnectionParams(token, clientInfo, opts)
			: await createJoinRequestConnectionParams(token, clientInfo, opts, publisherOffer);
		const rtcUrl = createRtcUrl(url, params, useV0Path).toString();
		const validateUrl = createValidateUrl(rtcUrl).toString();

		return new Promise<JoinResponse | ReconnectResponse | undefined>((resolve, reject) => {
			void (async () => {
				try {
					let alreadyAborted = false;
					const abortHandler = async (eventOrError: Event | Error) => {
						if (alreadyAborted) {
							return;
						}
						alreadyAborted = true;
						const target = eventOrError instanceof Event ? eventOrError.currentTarget : eventOrError;
						const reason = getAbortReasonAsString(target, 'Abort handler called');
						if (this.streamWriter && !this.isDisconnected) {
							this.sendLeave()
								.then(() => this.close(undefined, reason))
								.catch((e) => {
									this.log.error(e);
									this.close();
								});
						} else {
							this.close();
						}
						cleanupAbortHandlers();
						reject(eventOrError instanceof ConnectionError ? eventOrError : ConnectionError.cancelled(reason));
					};

					abortSignal?.addEventListener('abort', abortHandler);

					const cleanupAbortHandlers = () => {
						clearTimeout(wsTimeout);
						abortSignal?.removeEventListener('abort', abortHandler);
					};

					const wsTimeout = setTimeout(() => {
						abortHandler(ConnectionError.timeout('room connection has timed out (signal)'));
					}, opts.websocketTimeout);

					const redactedUrl = new URL(rtcUrl);
					if (redactedUrl.searchParams.has('access_token')) {
						redactedUrl.searchParams.set('access_token', '<redacted>');
					}

					if (this.ws) {
						const startClose = performance.now();
						await this.teardownTransport('replaced by a new connection attempt');
						this.log.debug(`closed previous ws connection in ${performance.now() - startClose}ms`);
					}

					const attemptId = this.attemptId;

					this.log.info(`signal connecting to ${redactedUrl}`, {
						reconnect: opts.reconnect,
						reconnectReason: opts.reconnectReason,
					});
					this.ws = new WebSocketStream<ArrayBuffer>(rtcUrl);

					let upgradeFailed = false;
					this.ws.opened.catch(() => {
						upgradeFailed = true;
					});

					try {
						this.ws.closed
							.then((closeInfo) => {
								if (this.isEstablishingConnection && !upgradeFailed) {
									reject(
										ConnectionError.internal(
											`Websocket got closed during a (re)connection attempt: ${closeInfo.reason}`,
										),
									);
								}
								this.log.debug('websocket closed', {
									reason: closeInfo.reason,
									code: closeInfo.closeCode,
									attemptId,
									state: this.lifecycleState,
								});
								this.handleOnClose(
									closeInfo.reason ||
										(closeInfo.closeCode === 1000 ? 'server closed the signal connection' : 'Unexpected WS error'),
									attemptId,
								);
								return;
							})
							.catch((reason) => {
								if (this.isEstablishingConnection && !upgradeFailed) {
									reject(ConnectionError.internal(`Websocket error during a (re)connection attempt: ${reason}`));
								}
							});
						const connection = await this.ws.opened.catch(async (reason: unknown) => {
							if (this.lifecycleState !== 'connected') {
								clearTimeout(wsTimeout);
								const error = await this.handleConnectionError(reason, validateUrl);
								reject(error);
								return;
							}
							this.handleWSError(reason);
							reject(reason);
							return;
						});
						clearTimeout(wsTimeout);
						if (!connection) {
							return;
						}
						const signalReader = connection.readable.getReader();
						this.streamWriter = connection.writable.getWriter();

						let firstMessage: ReadableStreamReadResult<string | ArrayBuffer>;
						let firstMessageTimeout: ReturnType<typeof setTimeout> | undefined;
						try {
							firstMessage = await Promise.race([
								signalReader.read(),
								new Promise<never>((_, rejectRead) => {
									firstMessageTimeout = setTimeout(() => {
										rejectRead(
											ConnectionError.timeout('signal connection timed out while waiting for the first message'),
										);
									}, JOIN_RESPONSE_TIMEOUT);
								}),
							]);
						} catch (e) {
							signalReader.releaseLock();
							reject(e);
							this.close();
							return;
						} finally {
							clearTimeout(firstMessageTimeout);
						}
						signalReader.releaseLock();
						if (!firstMessage.value) {
							throw ConnectionError.internal('no message received as first message');
						}

						const firstSignalResponse = parseSignalResponse(firstMessage.value);

						const validation = this.validateFirstMessage(firstSignalResponse, opts.reconnect ?? false);

						if (!validation.isValid) {
							reject(validation.error);
							return;
						}

						if (firstSignalResponse.message?.case === 'join') {
							this.pingTimeoutDuration = firstSignalResponse.message.value.pingTimeout;
							this.pingIntervalDuration = firstSignalResponse.message.value.pingInterval;

							if (this.pingTimeoutDuration && this.pingTimeoutDuration > 0) {
								this.log.debug('ping config', {
									timeout: this.pingTimeoutDuration,
									interval: this.pingIntervalDuration,
								});
							}

							if (this.onJoined) {
								this.onJoined(firstSignalResponse.message.value);
							}
						}

						const firstMessageToProcess = validation.shouldProcessFirstMessage ? firstSignalResponse : undefined;
						this.handleSignalConnected(connection, wsTimeout, attemptId, firstMessageToProcess);
						resolve(validation.response);
					} catch (e) {
						reject(e);
					} finally {
						cleanupAbortHandlers();
					}
				} finally {
					unlock();
				}
			})();
		});
	}

	async startReadingLoop(
		signalReader: ReadableStreamDefaultReader<string | ArrayBuffer>,
		firstMessage?: SignalResponse,
	) {
		if (firstMessage) {
			this.handleSignalResponse(firstMessage);
		}
		const attemptId = this.attemptId;
		while (true) {
			if (this.signalLatency) {
				await sleep(this.signalLatency);
			}
			try {
				const {done, value} = await signalReader.read();
				if (done) {
					break;
				}
				const resp = parseSignalResponse(value);
				this.handleSignalResponse(resp);
			} catch (e) {
				this.log.error(`error reading from signal stream`, {error: e});
				await this.handleOnClose('error in reading loop', attemptId);
				break;
			}
		}
	}

	resetCallbacks = () => {
		this.onAnswer = undefined;
		this.onLeave = undefined;
		this.onLocalTrackPublished = undefined;
		this.onLocalTrackUnpublished = undefined;
		this.onNegotiateRequested = undefined;
		this.onOffer = undefined;
		this.onRemoteMuteChanged = undefined;
		this.onSubscribedQualityUpdate = undefined;
		this.onTokenRefresh = undefined;
		this.onTrickle = undefined;
		this.onClose = undefined;
		this.onMediaSectionsRequirement = undefined;
	};

	async close(updateState: boolean = true, reason = 'Close method called on signal client') {
		const drivesLifecycle = updateState && this.sendLifecycleInput({type: 'close', reason});
		const unlock = await this.closingLock.lock();
		try {
			await this.teardownTransport(reason);
		} finally {
			if (drivesLifecycle) {
				this.sendLifecycleInput({type: 'closeComplete'});
			}
			unlock();
		}
	}

	private async teardownTransport(reason: string) {
		try {
			this.clearPingInterval();
			if (this.ws) {
				this.ws.close({closeCode: 1000, reason});

				const closePromise = this.ws.closed;
				this.ws = undefined;
				this.streamWriter = undefined;
				await Promise.race([closePromise, sleep(MAX_WS_CLOSE_TIME)]);
			}
		} catch (e) {
			this.log.debug('websocket error while closing', {error: e});
		}
	}

	sendOffer(offer: RTCSessionDescriptionInit, offerId: number) {
		this.log.debug('sending offer', {offerSdp: offer.sdp});
		this.sendRequest({
			case: 'offer',
			value: toProtoSessionDescription(offer, offerId),
		});
	}

	sendAnswer(answer: RTCSessionDescriptionInit, offerId: number) {
		this.log.debug('sending answer', {answerSdp: answer.sdp});
		return this.sendRequest({
			case: 'answer',
			value: toProtoSessionDescription(answer, offerId),
		});
	}

	sendIceCandidate(candidate: RTCIceCandidateInit, target: SignalTarget) {
		this.log.debug('sending ice candidate', {candidate});
		return this.sendRequest({
			case: 'trickle',
			value: new TrickleRequest({
				candidateInit: JSON.stringify(candidate),
				target,
			}),
		});
	}

	sendMuteTrack(trackSid: string, muted: boolean) {
		return this.sendRequest({
			case: 'mute',
			value: new MuteTrackRequest({
				sid: trackSid,
				muted,
			}),
		});
	}

	sendAddTrack(req: AddTrackRequest) {
		return this.sendRequest({
			case: 'addTrack',
			value: req,
		});
	}

	async sendUpdateLocalMetadata(metadata: string, name: string, attributes: Record<string, string> = {}) {
		const requestId = this.getNextRequestId();
		await this.sendRequest({
			case: 'updateMetadata',
			value: new UpdateParticipantMetadata({
				requestId,
				metadata,
				name,
				attributes,
			}),
		});
		return requestId;
	}

	sendUpdateTrackSettings(settings: UpdateTrackSettings) {
		this.sendRequest({
			case: 'trackSetting',
			value: settings,
		});
	}

	sendUpdateSubscription(sub: UpdateSubscription) {
		return this.sendRequest({
			case: 'subscription',
			value: sub,
		});
	}

	sendSyncState(sync: SyncState) {
		return this.sendRequest({
			case: 'syncState',
			value: sync,
		});
	}

	sendUpdateVideoLayers(trackSid: string, layers: Array<VideoLayer>) {
		return this.sendRequest({
			case: 'updateLayers',
			value: new UpdateVideoLayers({
				trackSid,
				layers,
			}),
		});
	}

	sendUpdateSubscriptionPermissions(allParticipants: boolean, trackPermissions: Array<TrackPermission>) {
		return this.sendRequest({
			case: 'subscriptionPermission',
			value: new SubscriptionPermission({
				allParticipants,
				trackPermissions,
			}),
		});
	}

	sendSimulateScenario(scenario: SimulateScenario) {
		return this.sendRequest({
			case: 'simulate',
			value: scenario,
		});
	}

	sendPing() {
		return Promise.all([
			this.sendRequest({
				case: 'ping',
				value: protoInt64.parse(Date.now()),
			}),
			this.sendRequest({
				case: 'pingReq',
				value: new Ping({
					timestamp: protoInt64.parse(Date.now()),
					rtt: protoInt64.parse(this.rtt),
				}),
			}),
		]);
	}

	sendUpdateLocalAudioTrack(trackSid: string, features: Array<AudioTrackFeature>) {
		return this.sendRequest({
			case: 'updateAudioTrack',
			value: new UpdateLocalAudioTrack({trackSid, features}),
		});
	}

	sendLeave() {
		return this.sendRequest({
			case: 'leave',
			value: new LeaveRequest({
				reason: DisconnectReason.CLIENT_INITIATED,
				action: LeaveRequest_Action.DISCONNECT,
			}),
		});
	}

	sendPublishDataTrackRequest(handle: DataTrackHandle, name: string, usesE2ee: boolean) {
		return this.sendRequest({
			case: 'publishDataTrackRequest',
			value: new PublishDataTrackRequest({
				pubHandle: handle,
				name: name,
				encryption: usesE2ee ? Encryption_Type.GCM : Encryption_Type.NONE,
			}),
		});
	}

	sendUnPublishDataTrackRequest(handle: DataTrackHandle) {
		return this.sendRequest({
			case: 'unpublishDataTrackRequest',
			value: new UnpublishDataTrackRequest({pubHandle: handle}),
		});
	}

	sendUpdateDataSubscription(sid: DataTrackSid, subscribe: boolean) {
		return this.sendRequest({
			case: 'updateDataSubscription',
			value: new UpdateDataSubscription({
				updates: [
					new UpdateDataSubscription_Update({
						trackSid: sid,
						subscribe,
					}),
				],
			}),
		});
	}

	private async sendRequest(message: SignalMessage, fromQueue: boolean = false) {
		const canQueue = !fromQueue && !canPassThroughQueue(message);
		const isHoldingRequests = this.lifecycleState === 'reconnecting' || this.queuedRequests.length > 0;
		if (canQueue && isHoldingRequests) {
			this.queuedRequests.push(async () => {
				await this.sendRequest(message, true);
			});
			return;
		}
		if (!fromQueue) {
			await this.requestQueue.flush();
		}
		if (this.signalLatency) {
			await sleep(this.signalLatency);
		}
		const isLeaveOnShutdown = message.case === 'leave' && !!this.streamWriter;
		if (this.isDisconnected && !isLeaveOnShutdown) {
			this.log.debug(`skipping signal request (type: ${message.case}) - SignalClient disconnected`);
			return;
		}
		if (!this.streamWriter) {
			this.log.error(`cannot send signal request before connected, type: ${message?.case}`);
			return;
		}
		const req = new SignalRequest({message});

		try {
			if (this.useJSON) {
				await this.streamWriter.write(req.toJsonString());
			} else {
				await this.streamWriter.write((req.toBinary() as NonSharedUint8Array).buffer);
			}
		} catch (e) {
			this.log.error('error sending signal message', {error: e});
		}
	}

	private handleSignalResponse(res: SignalResponse) {
		const msg = res.message;
		if (msg === undefined) {
			this.log.debug('received unsupported message');
			return;
		}

		let pingHandled = false;
		if (msg.case === 'answer') {
			const sd = fromProtoSessionDescription(msg.value);
			if (this.onAnswer) {
				this.onAnswer(sd, msg.value.id, msg.value.midToTrackId);
			}
		} else if (msg.case === 'offer') {
			const sd = fromProtoSessionDescription(msg.value);
			if (this.onOffer) {
				this.onOffer(sd, msg.value.id, msg.value.midToTrackId);
			}
		} else if (msg.case === 'trickle') {
			const candidate: RTCIceCandidateInit = JSON.parse(msg.value.candidateInit!);
			if (this.onTrickle) {
				this.onTrickle(candidate, msg.value.target);
			}
		} else if (msg.case === 'update') {
			if (this.onParticipantUpdate) {
				this.onParticipantUpdate(msg.value.participants ?? []);
			}
		} else if (msg.case === 'trackPublished') {
			if (this.onLocalTrackPublished) {
				this.onLocalTrackPublished(msg.value);
			}
		} else if (msg.case === 'speakersChanged') {
			if (this.onSpeakersChanged) {
				this.onSpeakersChanged(msg.value.speakers ?? []);
			}
		} else if (msg.case === 'leave') {
			if (this.onLeave) {
				this.onLeave(msg.value);
			}
		} else if (msg.case === 'mute') {
			if (this.onRemoteMuteChanged) {
				this.onRemoteMuteChanged(msg.value.sid, msg.value.muted);
			}
		} else if (msg.case === 'roomUpdate') {
			if (this.onRoomUpdate && msg.value.room) {
				this.onRoomUpdate(msg.value.room);
			}
		} else if (msg.case === 'connectionQuality') {
			if (this.onConnectionQuality) {
				this.onConnectionQuality(msg.value);
			}
		} else if (msg.case === 'streamStateUpdate') {
			if (this.onStreamStateUpdate) {
				this.onStreamStateUpdate(msg.value);
			}
		} else if (msg.case === 'subscribedQualityUpdate') {
			if (this.onSubscribedQualityUpdate) {
				this.onSubscribedQualityUpdate(msg.value);
			}
		} else if (msg.case === 'subscriptionPermissionUpdate') {
			if (this.onSubscriptionPermissionUpdate) {
				this.onSubscriptionPermissionUpdate(msg.value);
			}
		} else if (msg.case === 'refreshToken') {
			if (this.onTokenRefresh) {
				this.onTokenRefresh(msg.value);
			}
		} else if (msg.case === 'trackUnpublished') {
			if (this.onLocalTrackUnpublished) {
				this.onLocalTrackUnpublished(msg.value);
			}
		} else if (msg.case === 'subscriptionResponse') {
			if (this.onSubscriptionError) {
				this.onSubscriptionError(msg.value);
			}
		} else if (msg.case === 'pong') {
		} else if (msg.case === 'pongResp') {
			this.rtt = Date.now() - Number.parseInt(msg.value.lastPingTimestamp.toString(), 10);
			this.resetPingTimeout();
			pingHandled = true;
		} else if (msg.case === 'requestResponse') {
			if (this.onRequestResponse) {
				this.onRequestResponse(msg.value);
			}
		} else if (msg.case === 'trackSubscribed') {
			if (this.onLocalTrackSubscribed) {
				this.onLocalTrackSubscribed(msg.value.trackSid);
			}
		} else if (msg.case === 'roomMoved') {
			if (this.onTokenRefresh) {
				this.onTokenRefresh(msg.value.token);
			}
			if (this.onRoomMoved) {
				this.onRoomMoved(msg.value);
			}
		} else if (msg.case === 'mediaSectionsRequirement') {
			if (this.onMediaSectionsRequirement) {
				this.onMediaSectionsRequirement(msg.value);
			}
		} else if (msg.case === 'publishDataTrackResponse') {
			if (this.onPublishDataTrackResponse) {
				this.onPublishDataTrackResponse(msg.value);
			}
		} else if (msg.case === 'unpublishDataTrackResponse') {
			if (this.onUnPublishDataTrackResponse) {
				this.onUnPublishDataTrackResponse(msg.value);
			}
		} else if (msg.case === 'dataTrackSubscriberHandles') {
			if (this.onDataTrackSubscriberHandles) {
				this.onDataTrackSubscriberHandles(msg.value);
			}
		} else {
			this.log.debug('unsupported message', {msgCase: msg.case});
		}

		if (!pingHandled) {
			this.resetPingTimeout();
		}
	}

	setReconnected() {
		while (this.queuedRequests.length > 0) {
			const req = this.queuedRequests.shift();
			if (req) {
				this.requestQueue.run(req);
			}
		}
	}

	async handleOnClose(reason: string, attemptId: number = this.attemptId) {
		const onCloseCallback = this.onClose;
		if (!this.sendLifecycleInput({type: 'transportFailed', attemptId, reason})) {
			this.log.debug(`ignoring transport close in state ${this.lifecycleState}`, {
				reason,
				attemptId,
				currentAttemptId: this.attemptId,
			});
			return;
		}
		await this.teardownTransport(reason);
		this.log.info(`websocket connection closed: ${reason}`, {reason});
		if (onCloseCallback) {
			onCloseCallback(reason);
		}
	}

	private handleWSError(error: unknown) {
		this.log.error('websocket error', {error});
	}

	private resetPingTimeout() {
		this.clearPingTimeout();
		if (!this.pingTimeoutDuration) {
			this.log.warn('ping timeout duration not set');
			return;
		}
		this.pingTimeout = CriticalTimers.setTimeout(() => {
			this.log.warn(
				`ping timeout triggered. last pong received at: ${new Date(
					Date.now() - this.pingTimeoutDuration! * 1000,
				).toUTCString()}`,
			);
			this.handleOnClose('ping timeout');
		}, this.pingTimeoutDuration * 1000);
	}

	private clearPingTimeout() {
		if (this.pingTimeout) {
			CriticalTimers.clearTimeout(this.pingTimeout);
		}
	}

	private startPingInterval() {
		this.clearPingInterval();
		this.resetPingTimeout();
		if (!this.pingIntervalDuration) {
			this.log.warn('ping interval duration not set');
			return;
		}
		this.log.debug('start ping interval');
		this.pingInterval = CriticalTimers.setInterval(() => {
			this.sendPing();
		}, this.pingIntervalDuration * 1000);
	}

	private clearPingInterval() {
		this.log.debug('clearing ping interval');
		this.clearPingTimeout();
		if (this.pingInterval) {
			CriticalTimers.clearInterval(this.pingInterval);
		}
	}

	private handleSignalConnected(
		connection: WebSocketConnection,
		timeoutHandle: ReturnType<typeof setTimeout>,
		attemptId: number,
		firstMessage?: SignalResponse,
	) {
		clearTimeout(timeoutHandle);
		const established = this.sendLifecycleInput(
			this.lifecycleState === 'reconnecting'
				? {type: 'reconnectComplete', attemptId}
				: {type: 'connectComplete', attemptId},
		);
		if (!established) {
			this.log.debug('discarding a connection whose attempt no longer owns the session', {
				attemptId,
				currentAttemptId: this.attemptId,
				state: this.lifecycleState,
			});
			return;
		}
		this.log.info('signal connected');
		this.startPingInterval();
		this.startReadingLoop(connection.readable.getReader(), firstMessage);
	}

	private validateFirstMessage(
		firstSignalResponse: SignalResponse,
		isReconnect: boolean,
	): {
		isValid: boolean;
		response?: JoinResponse | ReconnectResponse;
		error?: ConnectionError;
		shouldProcessFirstMessage?: boolean;
	} {
		if (firstSignalResponse.message?.case === 'join') {
			return {
				isValid: true,
				response: firstSignalResponse.message.value,
			};
		} else if (this.lifecycleState === 'reconnecting' && firstSignalResponse.message?.case !== 'leave') {
			if (firstSignalResponse.message?.case === 'reconnect') {
				return {
					isValid: true,
					response: firstSignalResponse.message.value,
				};
			} else {
				this.log.debug('declaring signal reconnected without reconnect response received');
				return {
					isValid: true,
					response: undefined,
					shouldProcessFirstMessage: true,
				};
			}
		} else if (this.isEstablishingConnection && firstSignalResponse.message?.case === 'leave') {
			return {
				isValid: false,
				error: ConnectionError.leaveRequest(
					'Received leave request while trying to (re)connect',
					firstSignalResponse.message.value.reason,
				),
			};
		} else if (!isReconnect) {
			return {
				isValid: false,
				error: ConnectionError.internal(
					`did not receive join response, got ${firstSignalResponse.message?.case} instead`,
				),
			};
		}

		return {
			isValid: false,
			error: ConnectionError.internal('Unexpected first message'),
		};
	}

	private async handleConnectionError(reason: unknown, validateUrl: string): Promise<ConnectionError> {
		try {
			const resp = await fetch(validateUrl);

			switch (resp.status) {
				case 404: {
					const errorMsg = await resp.text();
					if (errorMsg.includes('requested room does not exist')) {
						return ConnectionError.notAllowed(errorMsg, resp.status);
					}
					return ConnectionError.serviceNotFound(
						'v1 RTC path not found. Consider upgrading your LiveKit server version',
						'v0-rtc',
					);
				}
				case 401:
				case 403: {
					const msg = await resp.text();
					return ConnectionError.notAllowed(msg, resp.status);
				}
				default:
					break;
			}

			if (reason instanceof ConnectionError) {
				return reason;
			} else {
				return ConnectionError.internal(`Encountered unknown websocket error during connection: ${reason}`, {
					status: resp.status,
					statusText: resp.statusText,
				});
			}
		} catch (e) {
			return e instanceof ConnectionError
				? e
				: ConnectionError.serverUnreachable(e instanceof Error ? e.message : 'server was not reachable');
		}
	}
}

function fromProtoSessionDescription(sd: SessionDescription): RTCSessionDescriptionInit {
	const rsd: RTCSessionDescriptionInit = {
		type: 'offer',
		sdp: sd.sdp,
	};
	switch (sd.type) {
		case 'answer':
		case 'offer':
		case 'pranswer':
		case 'rollback':
			rsd.type = sd.type;
			break;
		default:
			break;
	}
	return rsd;
}

export function toProtoSessionDescription(
	rsd: RTCSessionDescription | RTCSessionDescriptionInit,
	id?: number,
): SessionDescription {
	const sd = new SessionDescription({
		sdp: rsd.sdp!,
		type: rsd.type!,
		id,
	});
	return sd;
}

function createConnectionParams(token: string, info: ClientInfo, opts: ConnectOpts): URLSearchParams {
	const params = new URLSearchParams();
	params.set('access_token', token);

	if (opts.reconnect) {
		params.set('reconnect', '1');
		if (opts.sid) {
			params.set('sid', opts.sid);
		}
	}

	params.set('auto_subscribe', opts.autoSubscribe ? '1' : '0');

	params.set('sdk', isReactNative() ? 'reactnative' : 'js');
	params.set('version', info.version!);
	params.set('protocol', info.protocol!.toString());
	params.set('client_protocol', info.clientProtocol!.toString());
	if (info.deviceModel) {
		params.set('device_model', info.deviceModel);
	}
	if (info.os) {
		params.set('os', info.os);
	}
	if (info.osVersion) {
		params.set('os_version', info.osVersion);
	}
	if (info.browser) {
		params.set('browser', info.browser);
	}
	if (info.browserVersion) {
		params.set('browser_version', info.browserVersion);
	}

	if (opts.adaptiveStream) {
		params.set('adaptive_stream', '1');
	}

	if (opts.reconnectReason) {
		params.set('reconnect_reason', opts.reconnectReason.toString());
	}

	// @ts-expect-error
	if (navigator.connection?.type) {
		// @ts-expect-error
		params.set('network', navigator.connection.type);
	}

	return params;
}

async function createJoinRequestConnectionParams(
	token: string,
	info: ClientInfo,
	opts: ConnectOpts,
	publisherOffer?: SessionDescription,
): Promise<URLSearchParams> {
	const params = new URLSearchParams();
	params.set('access_token', token);

	const joinRequest = new JoinRequest({
		clientInfo: info,
		connectionSettings: new ConnectionSettings({
			autoSubscribe: !!opts.autoSubscribe,
			adaptiveStream: !!opts.adaptiveStream,
		}),
		reconnect: !!opts.reconnect,
		participantSid: opts.sid ? opts.sid : undefined,
		publisherOffer: publisherOffer,
	});
	if (opts.reconnectReason) {
		joinRequest.reconnectReason = opts.reconnectReason;
	}
	const joinRequestBytes = joinRequest.toBinary();
	let requestBytes: Uint8Array;
	let compression: WrappedJoinRequest_Compression;
	if (isCompressionStreamSupported()) {
		const stream = new CompressionStream('gzip');
		const writer = stream.writable.getWriter();
		writer.write(new Uint8Array(joinRequestBytes));
		writer.close();
		const chunks: Array<Uint8Array> = [];
		const reader = stream.readable.getReader();
		while (true) {
			const {done, value} = await reader.read();
			if (done) break;
			chunks.push(value);
		}
		const totalLength = chunks.reduce((acc, chunk) => acc + chunk.length, 0);
		const result = new Uint8Array(totalLength);
		let offset = 0;
		for (const chunk of chunks) {
			result.set(chunk, offset);
			offset += chunk.length;
		}
		requestBytes = result;
		compression = WrappedJoinRequest_Compression.GZIP;
	} else {
		requestBytes = joinRequestBytes;
		compression = WrappedJoinRequest_Compression.NONE;
	}
	const wrappedJoinRequest = new WrappedJoinRequest({
		joinRequest: requestBytes,
		compression,
	});
	const wrappedBytes = wrappedJoinRequest.toBinary();
	const bytesToBase64 = (bytes: Uint8Array) => {
		const binString = Array.from(bytes, (byte) => String.fromCodePoint(byte)).join('');
		return btoa(binString);
	};
	params.set('join_request', bytesToBase64(wrappedBytes).replace(/\+/g, '-').replace(/\//g, '_'));

	return params;
}
