// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {SignalTarget} from '@livekit/protocol';
import log, {getLogger, LoggerNames} from '../logger.ts';
import TypedPromise from '../utils/TypedPromise.ts';
import type {Throws} from '../utils/throws.ts';
import {roomConnectOptionDefaults} from './defaults.ts';
import {ConnectionError, NegotiationError} from './errors.ts';
import PCTransport, {PCEvents} from './PCTransport.ts';
import CriticalTimers, {type TimerHandle} from './timers.ts';
import type {VideoCodec} from './track/options.ts';
import type {LoggerOptions} from './types.ts';
import {sleep} from './utils.ts';

export enum PCTransportState {
	NEW,
	CONNECTING,
	CONNECTED,
	FAILED,
	CLOSING,
	CLOSED,
}

type PCMode = 'subscriber-primary' | 'publisher-primary' | 'publisher-only';

const ICE_CANDIDATE_ERROR_KEYS_MAX = 32;

function describeIceCandidateErrorEvent(event: Event) {
	const candidateError = event as Partial<RTCPeerConnectionIceErrorEvent>;
	return {
		errorCode: typeof candidateError.errorCode === 'number' ? candidateError.errorCode : undefined,
		errorText: typeof candidateError.errorText === 'string' ? candidateError.errorText : undefined,
		url: typeof candidateError.url === 'string' ? candidateError.url : undefined,
		address: typeof candidateError.address === 'string' ? candidateError.address : undefined,
		port: typeof candidateError.port === 'number' ? candidateError.port : undefined,
	};
}
export class PCTransportManager {
	public publisher: PCTransport;

	public subscriber?: PCTransport;

	public peerConnectionTimeout: number = roomConnectOptionDefaults.peerConnectionTimeout;

	public get needsPublisher() {
		return this.isPublisherConnectionRequired;
	}

	public get needsSubscriber() {
		return this.isSubscriberConnectionRequired;
	}

	public get currentState() {
		return this.state;
	}

	public onStateChange?: (
		state: PCTransportState,
		pubState: RTCPeerConnectionState,
		subState?: RTCPeerConnectionState,
	) => void;

	public onIceCandidate?: (ev: RTCIceCandidate, target: SignalTarget) => void;

	public onDataChannel?: (ev: RTCDataChannelEvent) => void;

	public onTrack?: (ev: RTCTrackEvent) => void;

	public onPublisherOffer?: (offer: RTCSessionDescriptionInit, offerId: number) => void;

	private isPublisherConnectionRequired: boolean;

	private isSubscriberConnectionRequired: boolean;

	private state: PCTransportState;

	private connectionLock: Mutex;

	private remoteOfferLock: Mutex;

	private log = log;

	private iceLog = log;

	private loggerOptions: LoggerOptions;

	private _mode: PCMode;

	private seenIceCandidateErrorKeys = new Set<string>();

	get mode(): PCMode {
		return this._mode;
	}
	constructor(
		mode: PCMode,
		loggerOptions: LoggerOptions,
		rtcConfig?: RTCConfiguration,
		subscriberVideoCodecExclusions?: Array<VideoCodec>,
	) {
		this.loggerOptions = loggerOptions;
		this.log = getLogger(loggerOptions.loggerName ?? LoggerNames.PCManager, () => this.logContext);
		this.iceLog = getLogger(LoggerNames.ICE, () => this.logContext);

		this.isPublisherConnectionRequired = mode !== 'subscriber-primary';
		this.isSubscriberConnectionRequired = mode === 'subscriber-primary';
		this.publisher = new PCTransport(rtcConfig, loggerOptions);
		this._mode = mode;
		if (mode !== 'publisher-only') {
			this.subscriber = new PCTransport(rtcConfig, loggerOptions);
			this.subscriber.onConnectionStateChange = this.updateState;
			this.subscriber.onIceConnectionStateChange = this.updateState;
			this.subscriber.onSignalingStatechange = this.updateState;
			this.subscriber.onIceCandidateError = (event) => {
				this.handleIceCandidateError('subscriber', this.subscriber, event);
			};
			this.subscriber.onIceCandidate = (candidate) => {
				this.onIceCandidate?.(candidate, SignalTarget.SUBSCRIBER);
			};
			this.subscriber.onDataChannel = (ev) => {
				this.onDataChannel?.(ev);
			};
			this.subscriber.onTrack = (ev) => {
				this.onTrack?.(ev);
			};
		}

		const receivingTransport = this.subscriber ?? this.publisher;
		if (receivingTransport) {
			for (const codec of subscriberVideoCodecExclusions ?? []) {
				receivingTransport.excludedVideoDecoderMimeTypes.add(`video/${codec}`);
			}
		}

		this.publisher.onConnectionStateChange = this.updateState;
		this.publisher.onIceConnectionStateChange = this.updateState;
		this.publisher.onSignalingStatechange = this.updateState;
		this.publisher.onIceCandidateError = (event) => {
			this.handleIceCandidateError('publisher', this.publisher, event);
		};
		this.publisher.onIceCandidate = (candidate) => {
			this.onIceCandidate?.(candidate, SignalTarget.PUBLISHER);
		};
		this.publisher.onTrack = (ev) => {
			this.onTrack?.(ev);
		};

		this.publisher.onOffer = (offer, offerId) => {
			this.onPublisherOffer?.(offer, offerId);
		};

		this.state = PCTransportState.NEW;

		this.connectionLock = new Mutex();
		this.remoteOfferLock = new Mutex();
	}

	private get logContext() {
		return {
			...this.loggerOptions.loggerContextCb?.(),
		};
	}

	private handleIceCandidateError(label: 'publisher' | 'subscriber', transport: PCTransport | undefined, event: Event) {
		const detail = describeIceCandidateErrorEvent(event);
		const context = {
			...this.logContext,
			...detail,
			transportState: this.describeTransport(label, transport),
		};
		if (transport?.isICEConnected) {
			this.log.debug(`${label} ice candidate error after ice connected`, context);
			return;
		}
		const key = `${label}|${detail.errorCode}|${detail.url}|${detail.address}|${detail.port}`;
		if (this.seenIceCandidateErrorKeys.has(key)) {
			this.log.debug(`${label} ice candidate error repeated`, context);
			return;
		}
		if (this.seenIceCandidateErrorKeys.size < ICE_CANDIDATE_ERROR_KEYS_MAX) {
			this.seenIceCandidateErrorKeys.add(key);
		}
		this.log.warn(`${label} ice candidate error`, context);
	}

	private describeTransport(label: string, transport: PCTransport | undefined) {
		if (!transport) return {label, present: false};
		return {
			label,
			present: true,
			connectionState: transport.getConnectionState(),
			iceConnectionState: transport.getICEConnectionState(),
			signalingState: transport.getSignallingState(),
		};
	}

	private describeRequiredTransports() {
		return {
			managerState: PCTransportState[this.state],
			mode: this.mode,
			needsPublisher: this.needsPublisher,
			needsSubscriber: this.needsSubscriber,
			publisher: this.describeTransport('publisher', this.publisher),
			subscriber: this.describeTransport('subscriber', this.subscriber),
		};
	}

	requirePublisher(require = true) {
		this.isPublisherConnectionRequired = require;
		this.updateState();
	}

	createAndSendPublisherOffer(options?: RTCOfferOptions) {
		return this.publisher.createAndSendOffer(options);
	}

	setPublisherAnswer(sd: RTCSessionDescriptionInit, offerId: number) {
		return this.publisher.setRemoteDescription(sd, offerId);
	}

	removeTrack(sender: RTCRtpSender) {
		return this.publisher.removeTrack(sender);
	}

	async close() {
		if (this.publisher && this.publisher.getSignallingState() !== 'closed') {
			const publisher = this.publisher;
			for (const sender of publisher.getSenders()) {
				try {
					if (publisher.canRemoveTrack()) {
						publisher.removeTrack(sender);
					}
				} catch (e) {
					this.log.warn('could not removeTrack', {error: e});
				}
			}
		}
		await Promise.all([this.publisher.close(), this.subscriber?.close()]);
		this.updateState();
	}

	async triggerIceRestart() {
		this.seenIceCandidateErrorKeys.clear();
		this.iceLog.warn('triggering ICE restart');
		if (this.needsPublisher) {
			await this.createAndSendPublisherOffer({iceRestart: true});
		}
	}

	async addIceCandidate(candidate: RTCIceCandidateInit, target: SignalTarget) {
		this.iceLog.debug('adding remote ICE candidate', {target, candidate});
		if (target === SignalTarget.PUBLISHER) {
			await this.publisher.addIceCandidate(candidate);
		} else {
			await this.subscriber?.addIceCandidate(candidate);
		}
	}

	async createSubscriberAnswerFromOffer(sd: RTCSessionDescriptionInit, offerId: number) {
		this.log.debug('received server offer', {
			RTCSdpType: sd.type,
			sdp: sd.sdp,
			signalingState: this.subscriber?.getSignallingState().toString(),
		});
		const unlock = await this.remoteOfferLock.lock();
		try {
			const success = await this.subscriber?.setRemoteDescription(sd, offerId);
			if (!success) {
				return undefined;
			}

			const answer = await this.subscriber?.createAndSetAnswer();
			return answer;
		} finally {
			unlock();
		}
	}

	updateConfiguration(config: RTCConfiguration, iceRestart?: boolean) {
		this.log.debug('updating rtc configuration', {iceRestart});
		this.publisher.setConfiguration(config);
		this.subscriber?.setConfiguration(config);
		if (iceRestart) {
			this.triggerIceRestart();
		}
	}

	async ensurePCTransportConnection(abortController?: AbortController, timeout?: number) {
		const unlock = await this.connectionLock.lock();
		try {
			if (
				this.isPublisherConnectionRequired &&
				this.publisher.getConnectionState() !== 'connected' &&
				this.publisher.getConnectionState() !== 'connecting'
			) {
				this.log.debug('negotiation required, start negotiating');
				this.publisher.negotiate();
			}
			await Promise.all(
				this.requiredTransports?.map((transport) => this.ensureTransportConnected(transport, abortController, timeout)),
			);
		} finally {
			unlock();
		}
	}

	async negotiate(abortController: AbortController): Promise<Throws<void, NegotiationError | Error>> {
		return new TypedPromise<void, NegotiationError | Error>((resolve, reject) => {
			const checkpoint = this.publisher.latestOfferId;

			if (this.publisher.latestAcknowledgedOfferId > checkpoint) {
				this.log.debug(`negotiation already handled in more recent acknowledged offer`, this.logContext);
				resolve();
				return;
			}

			let cleanedUp = false;
			const cleanup = () => {
				if (cleanedUp) return;
				cleanedUp = true;
				clearTimeout(deadlineTimer);
				this.publisher.off(PCEvents.OfferAnswered, onAnswered);
				abortController.signal.removeEventListener('abort', onAbort);
			};

			const onAnswered = (offerId: number) => {
				if (offerId > checkpoint) {
					cleanup();
					resolve();
				}
			};

			const onAbort = () => {
				cleanup();
				reject(new NegotiationError('negotiation aborted'));
			};

			const deadlineTimer = setTimeout(() => {
				cleanup();
				reject(new NegotiationError('negotiation timed out'));
			}, this.peerConnectionTimeout);

			abortController.signal.addEventListener('abort', onAbort);
			this.publisher.on(PCEvents.OfferAnswered, onAnswered);

			this.publisher.negotiate((e) => {
				cleanup();
				if (e instanceof Error) {
					reject(e);
				} else {
					reject(new Error(String(e)));
				}
			});
		});
	}

	addPublisherTransceiver(track: MediaStreamTrack, transceiverInit: RTCRtpTransceiverInit) {
		return this.publisher.addTransceiver(track, transceiverInit);
	}

	addPublisherTransceiverOfKind(kind: 'audio' | 'video', transceiverInit: RTCRtpTransceiverInit) {
		return this.publisher.addTransceiverOfKind(kind, transceiverInit);
	}

	getMidForReceiver(receiver: RTCRtpReceiver): string | null | undefined {
		const transceivers = this.subscriber ? this.subscriber.getTransceivers() : this.publisher.getTransceivers();
		const matchingTransceiver = transceivers.find((transceiver) => transceiver.receiver === receiver);
		return matchingTransceiver?.mid;
	}

	getMaxPublisherMessageSize() {
		return this.publisher.getMaxMessageSize();
	}

	addPublisherTrack(track: MediaStreamTrack) {
		return this.publisher.addTrack(track);
	}

	createPublisherDataChannel(label: string, dataChannelDict: RTCDataChannelInit) {
		return this.publisher.createDataChannel(label, dataChannelDict);
	}

	getConnectedAddress(target?: SignalTarget) {
		if (target === SignalTarget.PUBLISHER) {
			return this.publisher.getConnectedAddress();
		} else if (target === SignalTarget.SUBSCRIBER) {
			return this.publisher.getConnectedAddress();
		}
		return this.requiredTransports[0].getConnectedAddress();
	}

	private get requiredTransports() {
		const transports: Array<PCTransport> = [];
		if (this.isPublisherConnectionRequired) {
			transports.push(this.publisher);
		}
		if (this.isSubscriberConnectionRequired && this.subscriber) {
			transports.push(this.subscriber);
		}
		return transports;
	}

	private updateState = () => {
		const previousState = this.state;

		const connectionStates = this.requiredTransports.map((tr) => tr.getConnectionState());
		if (connectionStates.every((st) => st === 'connected')) {
			this.state = PCTransportState.CONNECTED;
		} else if (connectionStates.some((st) => st === 'failed')) {
			this.state = PCTransportState.FAILED;
		} else if (connectionStates.some((st) => st === 'connecting')) {
			this.state = PCTransportState.CONNECTING;
		} else if (connectionStates.every((st) => st === 'closed')) {
			this.state = PCTransportState.CLOSED;
		} else if (connectionStates.some((st) => st === 'closed')) {
			this.state = PCTransportState.CLOSING;
		} else if (connectionStates.every((st) => st === 'new')) {
			this.state = PCTransportState.NEW;
		}

		if (previousState !== this.state) {
			this.log.debug(`pc state change: from ${PCTransportState[previousState]} to ${PCTransportState[this.state]}`);
			this.onStateChange?.(this.state, this.publisher.getConnectionState(), this.subscriber?.getConnectionState());
		}
	};

	private async ensureTransportConnected(
		pcTransport: PCTransport,
		abortController?: AbortController,
		timeout: number = this.peerConnectionTimeout,
	) {
		const connectionState = pcTransport.getConnectionState();
		if (connectionState === 'connected') {
			return;
		}

		return new Promise<void>((resolve, reject) => {
			let connectTimeout: TimerHandle | undefined;
			let settled = false;
			const cleanup = () => {
				if (connectTimeout !== undefined) {
					CriticalTimers.clearTimeout(connectTimeout);
				}
				abortController?.signal.removeEventListener('abort', abortHandler);
			};
			const rejectOnce = (error: Error) => {
				if (settled) return;
				settled = true;
				cleanup();
				reject(error);
			};
			const abortHandler = () => {
				this.log.warn('abort transport connection');
				rejectOnce(ConnectionError.cancelled('room connection has been cancelled'));
			};
			if (abortController?.signal.aborted) {
				abortHandler();
				return;
			}
			abortController?.signal.addEventListener('abort', abortHandler);

			connectTimeout = CriticalTimers.setTimeout(() => {
				rejectOnce(
					ConnectionError.internal(
						`could not establish pc connection: ${JSON.stringify(this.describeRequiredTransports())}`,
					),
				);
			}, timeout);

			void (async () => {
				while (this.state !== PCTransportState.CONNECTED) {
					await sleep(50);
					if (abortController?.signal.aborted) {
						rejectOnce(ConnectionError.cancelled('room connection has been cancelled'));
						return;
					}
				}
				if (settled) return;
				settled = true;
				cleanup();
				resolve();
			})().catch(rejectOnce);
		});
	}
}
