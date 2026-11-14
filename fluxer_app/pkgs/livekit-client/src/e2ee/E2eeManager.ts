// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Encryption_Type, type TrackInfo} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type TypedEventEmitter from 'typed-emitter';
import type {FrameMetadata} from '../frameMetadata/types.ts';
import {hasFrameMetadataPublishOptions} from '../frameMetadata/utils.ts';
import {getLogger, LoggerNames, type LogLevel, onWorkerLogLevelChanged, workerLogger} from '../logger.ts';
import {DeviceUnsupportedError} from '../room/errors.ts';
import {EngineEvent, ParticipantEvent, RoomEvent} from '../room/events.ts';
import type Room from '../room/Room.ts';
import {ConnectionState} from '../room/Room.ts';
import type RTCEngine from '../room/RTCEngine.ts';
import type {TrackPublishOptions, VideoCodec} from '../room/track/options.ts';
import type RemoteTrack from '../room/track/RemoteTrack.ts';
import RemoteVideoTrack from '../room/track/RemoteVideoTrack.ts';
import type {Track} from '../room/track/Track.ts';
import type {TrackPublication} from '../room/track/TrackPublication.ts';
import {mimeTypeToVideoCodecString} from '../room/track/utils.ts';
import {Future, isLocalTrack, isSafariBased, isScriptTransformSupportedForWorker, isVideoTrack} from '../room/utils.ts';
import type {NonSharedUint8Array} from '../type-polyfills/non-shared-typed-arrays.ts';
import {E2EE_FLAG, E2EE_TRACK_ID} from './constants.ts';
import {CryptorError, CryptorErrorReason} from './errors.ts';
import {type E2EEManagerCallbacks, EncryptionEvent, KeyProviderEvent} from './events.ts';
import type {BaseKeyProvider} from './KeyProvider.ts';
import type {
	DecryptDataRequestMessage,
	DecryptDataResponseMessage,
	E2EEManagerOptions,
	E2EEWorkerMessage,
	EnableMessage,
	EncodeMessage,
	EncryptDataRequestMessage,
	EncryptDataResponseMessage,
	InitMessage,
	KeyInfo,
	RatchetRequestMessage,
	RemoveTransformMessage,
	RTPVideoMapMessage,
	ScriptTransformOptions,
	SetKeyMessage,
	SifTrailerMessage,
	UpdateCodecMessage,
} from './types.ts';
import {isE2EESupported} from './utils.ts';

export interface BaseE2EEManager {
	setup(room: Room): void;
	setupEngine(engine: RTCEngine): void;
	isEnabled: boolean;
	isDataChannelEncryptionEnabled: boolean;
	setParticipantCryptorEnabled(enabled: boolean, participantIdentity: string): void;
	setSifTrailer(trailer: NonSharedUint8Array): void;
	encryptData(data: NonSharedUint8Array): Promise<EncryptDataResponseMessage['data']>;
	handleEncryptedData(
		payload: NonSharedUint8Array,
		iv: NonSharedUint8Array,
		participantIdentity: string,
		keyIndex: number,
	): Promise<DecryptDataResponseMessage['data']>;
	on<E extends keyof E2EEManagerCallbacks>(event: E, listener: E2EEManagerCallbacks[E]): this;
	dispose?(): void;
}

export class E2EEManager
	extends (EventEmitter as new () => TypedEventEmitter<E2EEManagerCallbacks>)
	implements BaseE2EEManager
{
	protected worker: Worker;

	protected room?: Room;

	private encryptionEnabled: boolean;

	private keyProvider: BaseKeyProvider;

	private decryptDataRequests: Map<string, Future<DecryptDataResponseMessage['data'], Error>> = new Map();

	private encryptDataRequests: Map<string, Future<EncryptDataResponseMessage['data'], Error>> = new Map();

	private dataChannelEncryptionEnabled: boolean;

	private unsubscribeLogLevel?: () => void;

	private log = getLogger(LoggerNames.E2EE, () => this.logContext);

	get logContext() {
		return {
			room: this.room?.name,
			participant: this.room?.localParticipant.identity,
		};
	}

	private static disposeRegistry =
		typeof FinalizationRegistry !== 'undefined' &&
		typeof WeakRef !== 'undefined' &&
		new FinalizationRegistry((cleanup: () => void) => {
			cleanup();
		});

	constructor(options: E2EEManagerOptions, dcEncryptionEnabled: boolean) {
		super();
		this.keyProvider = options.keyProvider;
		this.worker = options.worker;
		this.encryptionEnabled = false;
		this.dataChannelEncryptionEnabled = dcEncryptionEnabled;
	}

	get isEnabled(): boolean {
		return this.encryptionEnabled;
	}

	get isDataChannelEncryptionEnabled(): boolean {
		return this.isEnabled && this.dataChannelEncryptionEnabled;
	}

	setup(room: Room) {
		if (!isE2EESupported()) {
			throw new DeviceUnsupportedError('tried to setup end-to-end encryption on an unsupported browser');
		}
		this.log.info('setting up e2ee');
		if (room !== this.room) {
			this.room = room;
			this.setupEventListeners(room, this.keyProvider);
			const msg: InitMessage = {
				kind: 'init',
				data: {
					keyProviderOptions: this.keyProvider.getOptions(),
					loglevel: workerLogger.getLevel() as LogLevel,
				},
			};
			if (this.worker) {
				this.log.info(`initializing worker`, {worker: this.worker});
				this.worker.onmessage = this.onWorkerMessage;
				this.worker.onerror = this.onWorkerError;
				this.worker.postMessage(msg);
				this.subscribeToLogLevelChanges();
			}
		}
	}

	private subscribeToLogLevelChanges() {
		this.unsubscribeLogLevel?.();

		let unsub: (() => void) | undefined;
		if (E2EEManager.disposeRegistry) {
			const workerRef = new WeakRef(this.worker);
			unsub = onWorkerLogLevelChanged((level) => {
				const worker = workerRef.deref();
				if (!worker) {
					unsub?.();
					return;
				}
				worker.postMessage({kind: 'setLogLevel', data: {level}});
			});
			E2EEManager.disposeRegistry.register(this, unsub, this);
		} else {
			const worker = this.worker;
			unsub = onWorkerLogLevelChanged((level) => {
				worker.postMessage({kind: 'setLogLevel', data: {level}});
			});
		}
		this.unsubscribeLogLevel = unsub;
	}

	dispose() {
		this.unsubscribeLogLevel?.();
		this.unsubscribeLogLevel = undefined;
		if (E2EEManager.disposeRegistry) {
			E2EEManager.disposeRegistry.unregister(this);
		}

		const disposalError = new CryptorError('E2EEManager disposed', CryptorErrorReason.InternalError);
		for (const future of [...this.encryptDataRequests.values()]) {
			future.reject?.(disposalError);
		}
		for (const future of [...this.decryptDataRequests.values()]) {
			future.reject?.(disposalError);
		}

		if (this.worker) {
			this.worker.onmessage = null;
			this.worker.onerror = null;
		}
		this.removeAllListeners();
	}

	setParticipantCryptorEnabled(enabled: boolean, participantIdentity: string) {
		this.log.debug(`set e2ee to ${enabled} for participant ${participantIdentity}`);
		this.postEnable(enabled, participantIdentity);
	}

	setSifTrailer(trailer: NonSharedUint8Array) {
		if (!trailer || trailer.length === 0) {
			this.log.warn("ignoring server sent trailer as it's empty");
		} else {
			this.postSifTrailer(trailer);
		}
	}

	private onWorkerMessage = (ev: MessageEvent<E2EEWorkerMessage>) => {
		const {kind, data} = ev.data;
		switch (kind) {
			case 'error':
				if (data.uuid) {
					const decryptFuture = this.decryptDataRequests.get(data.uuid);
					if (decryptFuture?.reject) {
						decryptFuture.reject(data.error);
						break;
					}

					const encryptFuture = this.encryptDataRequests.get(data.uuid);
					if (encryptFuture?.reject) {
						encryptFuture.reject(data.error);
						break;
					}
				}
				this.log.error(data.error.message);
				this.emit(EncryptionEvent.EncryptionError, data.error, data.participantIdentity);
				break;
			case 'initAck':
				if (data.enabled) {
					this.keyProvider.getKeys().forEach((keyInfo) => {
						this.postKey(keyInfo, false);
					});
				}
				break;

			case 'enable':
				if (data.enabled) {
					this.keyProvider.getKeys().forEach((keyInfo) => {
						this.postKey(keyInfo, false);
					});
				}
				if (
					this.encryptionEnabled !== data.enabled &&
					data.participantIdentity === this.room?.localParticipant.identity
				) {
					this.emit(EncryptionEvent.ParticipantEncryptionStatusChanged, data.enabled, this.room!.localParticipant);
					this.encryptionEnabled = data.enabled;
				} else if (data.participantIdentity) {
					const participant = this.room?.getParticipantByIdentity(data.participantIdentity);
					if (!participant) {
						throw TypeError(`couldn't set encryption status, participant not found${data.participantIdentity}`);
					}
					this.emit(EncryptionEvent.ParticipantEncryptionStatusChanged, data.enabled, participant);
				}
				break;
			case 'ratchetKey':
				this.keyProvider.emit(
					KeyProviderEvent.KeyRatcheted,
					data.ratchetResult,
					data.participantIdentity,
					data.keyIndex,
				);
				break;

			case 'decryptDataResponse': {
				const decryptFuture = this.decryptDataRequests.get(data.uuid);
				if (decryptFuture?.resolve) {
					decryptFuture.resolve(data);
				}
				break;
			}
			case 'encryptDataResponse': {
				const encryptFuture = this.encryptDataRequests.get(data.uuid);
				if (encryptFuture?.resolve) {
					encryptFuture.resolve(data as EncryptDataResponseMessage['data']);
				}
				break;
			}
			case 'packetTrailerMetadata':
				this.handleFrameMetadata(data.trackId, data.rtpTimestamp, data.ssrc, data.metadata);
				break;
			case 'log':
				workerLogger[data.level](data.msg, data.context);
				break;
			default:
				break;
		}
	};

	private onWorkerError = (ev: ErrorEvent) => {
		this.log.error('e2ee worker encountered an error:', {error: ev.error});
		this.emit(EncryptionEvent.EncryptionError, ev.error, undefined);
	};

	private handleFrameMetadata(trackId: string, rtpTimestamp: number, ssrc: number, metadata: FrameMetadata) {
		if (!this.room) {
			return;
		}
		for (const participant of [this.room.localParticipant, ...this.room.remoteParticipants.values()]) {
			for (const pub of participant.trackPublications.values()) {
				if (
					pub.track &&
					pub.track.mediaStreamID === trackId &&
					pub.track instanceof RemoteVideoTrack &&
					pub.track.frameMetadataExtractor
				) {
					pub.track.frameMetadataExtractor.storeMetadata(rtpTimestamp, ssrc, metadata);
					return;
				}
			}
		}
	}

	public setupEngine(engine: RTCEngine) {
		engine.on(EngineEvent.RTPVideoMapUpdate, (rtpMap) => {
			this.postRTPMap(rtpMap);
		});
	}

	private setupEventListeners(room: Room, keyProvider: BaseKeyProvider) {
		room.on(RoomEvent.TrackPublished, (pub, participant) =>
			this.setParticipantCryptorEnabledForPublication(pub, participant.identity),
		);
		room
			.on(RoomEvent.ConnectionStateChanged, (state) => {
				if (state === ConnectionState.Connected) {
					room.remoteParticipants.forEach((participant) => {
						participant.trackPublications.forEach((pub) => {
							this.setParticipantCryptorEnabledForPublication(pub, participant.identity);
						});
					});
				}
			})
			.on(RoomEvent.TrackUnsubscribed, (track, _, participant) => {
				const msg: RemoveTransformMessage = {
					kind: 'removeTransform',
					data: {
						participantIdentity: participant.identity,
						trackId: track.mediaStreamID,
					},
				};
				this.worker?.postMessage(msg);
			})
			.on(RoomEvent.TrackSubscribed, (track, pub, participant) => {
				this.setupE2EEReceiver(track, participant.identity, pub.trackInfo);
			})
			.on(RoomEvent.SignalConnected, () => {
				if (!this.room) {
					throw new TypeError(`expected room to be present on signal connect`);
				}
				const latestKeyIndex = keyProvider.getLatestManuallySetKeyIndex();
				keyProvider.getKeys().forEach((keyInfo) => {
					this.postKey(keyInfo, latestKeyIndex === (keyInfo.keyIndex ?? 0));
				});
				this.setParticipantCryptorEnabled(
					this.room.localParticipant.isE2EEEnabled,
					this.room.localParticipant.identity,
				);
			});

		room.localParticipant.on(ParticipantEvent.LocalSenderCreated, async (sender, track, codec, trackId) => {
			this.setupE2EESender(track, sender, codec, trackId);
		});

		room.localParticipant.on(ParticipantEvent.LocalTrackPublished, (publication) => {
			if (!isVideoTrack(publication.track) || !isSafariBased()) {
				return;
			}
			const msg: UpdateCodecMessage = {
				kind: 'updateCodec',
				data: {
					trackId: publication.track!.mediaStreamID,
					codec: mimeTypeToVideoCodecString(publication.trackInfo!.codecs[0].mimeType),
					participantIdentity: this.room!.localParticipant.identity,
					hasPacketTrailer: false,
				},
			};

			this.worker.postMessage(msg);
		});

		keyProvider
			.on(KeyProviderEvent.SetKey, (keyInfo, updateCurrentKeyIndex) =>
				this.postKey(keyInfo, updateCurrentKeyIndex ?? true),
			)
			.on(KeyProviderEvent.RatchetRequest, (participantId, keyIndex) =>
				this.postRatchetRequest(participantId, keyIndex),
			);
	}

	async encryptData(data: NonSharedUint8Array): Promise<EncryptDataResponseMessage['data']> {
		if (!this.worker) {
			throw Error('could not encrypt data, worker is missing');
		}
		const uuid = crypto.randomUUID();
		const msg: EncryptDataRequestMessage = {
			kind: 'encryptDataRequest',
			data: {
				uuid,
				payload: data,
				participantIdentity: this.room!.localParticipant.identity,
			},
		};
		const future = new Future<EncryptDataResponseMessage['data'], Error>();
		future.onFinally = () => {
			this.encryptDataRequests.delete(uuid);
		};
		this.encryptDataRequests.set(uuid, future);
		this.worker.postMessage(msg);
		return future!.promise!;
	}

	handleEncryptedData(
		payload: NonSharedUint8Array,
		iv: NonSharedUint8Array,
		participantIdentity: string,
		keyIndex: number,
	) {
		if (!this.worker) {
			throw Error('could not handle encrypted data, worker is missing');
		}
		const uuid = crypto.randomUUID();
		const msg: DecryptDataRequestMessage = {
			kind: 'decryptDataRequest',
			data: {
				uuid,
				payload,
				iv,
				participantIdentity,
				keyIndex,
			},
		};
		const future = new Future<DecryptDataResponseMessage['data'], Error>();
		future.onFinally = () => {
			this.decryptDataRequests.delete(uuid);
		};
		this.decryptDataRequests.set(uuid, future);
		this.worker.postMessage(msg);
		return future.promise;
	}

	private postRatchetRequest(participantIdentity?: string, keyIndex?: number) {
		if (!this.worker) {
			throw Error('could not ratchet key, worker is missing');
		}
		const msg: RatchetRequestMessage = {
			kind: 'ratchetRequest',
			data: {
				participantIdentity: participantIdentity,
				keyIndex,
			},
		};
		this.worker.postMessage(msg);
	}

	private postKey({key, participantIdentity, keyIndex}: KeyInfo, updateCurrentKeyIndex: boolean) {
		if (!this.worker) {
			throw Error('could not set key, worker is missing');
		}
		const msg: SetKeyMessage = {
			kind: 'setKey',
			data: {
				participantIdentity: participantIdentity,
				isPublisher: participantIdentity === this.room?.localParticipant.identity,
				key,
				keyIndex,
				updateCurrentKeyIndex,
			},
		};
		this.worker.postMessage(msg);
	}

	private postEnable(enabled: boolean, participantIdentity: string) {
		if (this.worker) {
			const enableMsg: EnableMessage = {
				kind: 'enable',
				data: {
					enabled,
					participantIdentity,
				},
			};
			this.worker.postMessage(enableMsg);
		} else {
			throw new ReferenceError('failed to enable e2ee, worker is not ready');
		}
	}

	private postRTPMap(map: Map<number, VideoCodec>) {
		if (!this.worker) {
			throw TypeError('could not post rtp map, worker is missing');
		}
		if (!this.room?.localParticipant.identity) {
			throw TypeError('could not post rtp map, local participant identity is missing');
		}
		const msg: RTPVideoMapMessage = {
			kind: 'setRTPMap',
			data: {
				map,
				participantIdentity: this.room.localParticipant.identity,
			},
		};
		this.worker.postMessage(msg);
	}

	private postSifTrailer(trailer: NonSharedUint8Array) {
		if (!this.worker) {
			throw Error('could not post SIF trailer, worker is missing');
		}
		const msg: SifTrailerMessage = {
			kind: 'setSifTrailer',
			data: {
				trailer,
			},
		};
		this.worker.postMessage(msg);
	}

	private setParticipantCryptorEnabledForPublication(pub: TrackPublication, participantIdentity: string) {
		if (!pub.trackInfo) {
			this.log.warn('skipping e2ee enabled update for publication without trackInfo', {
				trackSid: pub.trackSid,
			});
			return;
		}
		this.setParticipantCryptorEnabled(pub.trackInfo.encryption !== Encryption_Type.NONE, participantIdentity);
	}

	private setupE2EEReceiver(track: RemoteTrack, remoteId: string, trackInfo?: TrackInfo) {
		if (!track.receiver) {
			return;
		}
		if (!trackInfo?.mimeType || trackInfo.mimeType === '') {
			throw new TypeError('MimeType missing from trackInfo, cannot set up E2EE cryptor');
		}
		const hasPacketTrailer =
			track.kind === 'video' && !!trackInfo.packetTrailerFeatures && trackInfo.packetTrailerFeatures.length > 0;
		this.handleReceiver(
			track.receiver,
			track.mediaStreamID,
			remoteId,
			track.kind === 'video' ? mimeTypeToVideoCodecString(trackInfo.mimeType) : undefined,
			hasPacketTrailer,
		);
	}

	private setupE2EESender(track: Track, sender: RTCRtpSender, codec?: VideoCodec, trackId?: string) {
		if (!isLocalTrack(track) || !sender) {
			if (!sender) this.log.warn('early return because sender is not ready');
			return;
		}
		const resolvedCodec =
			track.kind === 'video' ? (codec ?? (this.room?.options?.publishDefaults?.videoCodec as VideoCodec)) : undefined;
		this.handleSender(
			sender,
			trackId ?? track.mediaStreamID,
			resolvedCodec,
			isVideoTrack(track) ? (track.publishOptions?.frameMetadata ?? track.publishOptions?.packetTrailer) : undefined,
		);
	}

	private async handleReceiver(
		receiver: RTCRtpReceiver,
		trackId: string,
		participantIdentity: string,
		codec: VideoCodec | undefined,
		hasPacketTrailer: boolean,
	) {
		if (!this.worker) {
			return;
		}

		if (isScriptTransformSupportedForWorker()) {
			const options: ScriptTransformOptions = {
				kind: 'decode',
				participantIdentity,
				trackId,
				codec,
				hasPacketTrailer,
			};
			receiver.transform = new RTCRtpScriptTransform(this.worker, options);
		} else {
			if (E2EE_FLAG in receiver && E2EE_TRACK_ID in receiver) {
				const msg: UpdateCodecMessage = {
					kind: 'updateCodec',
					data: {
						trackId,
						previousTrackId: receiver[E2EE_TRACK_ID] as string,
						codec,
						participantIdentity,
						hasPacketTrailer,
					},
				};
				this.worker.postMessage(msg);
				receiver[E2EE_TRACK_ID] = trackId;
				return;
			}
			// @ts-expect-error
			let writable: WritableStream = receiver.writableStream;
			// @ts-expect-error
			let readable: ReadableStream = receiver.readableStream;

			if (!writable || !readable) {
				// @ts-expect-error
				const receiverStreams = receiver.createEncodedStreams();
				receiver.writableStream = receiverStreams.writable;
				writable = receiverStreams.writable;
				receiver.readableStream = receiverStreams.readable;
				readable = receiverStreams.readable;
			}

			const msg: EncodeMessage = {
				kind: 'decode',
				data: {
					readableStream: readable,
					writableStream: writable,
					trackId: trackId,
					codec,
					participantIdentity: participantIdentity,
					hasPacketTrailer,
				},
			};
			this.worker.postMessage(msg, [readable, writable]);
		}

		// @ts-expect-error
		receiver[E2EE_FLAG] = true;
		// @ts-expect-error
		receiver[E2EE_TRACK_ID] = trackId;
	}

	private handleSender(
		sender: RTCRtpSender,
		trackId: string,
		codec?: VideoCodec,
		frameMetadata?: TrackPublishOptions['frameMetadata'],
	) {
		if (!this.worker) {
			return;
		}

		if (!this.room?.localParticipant.identity || this.room.localParticipant.identity === '') {
			throw TypeError('local identity needs to be known in order to set up encrypted sender');
		}

		if (E2EE_FLAG in sender) {
			if (E2EE_TRACK_ID in sender) {
				const msg: UpdateCodecMessage = {
					kind: 'updateCodec',
					data: {
						trackId,
						previousTrackId: sender[E2EE_TRACK_ID] as string,
						codec,
						participantIdentity: this.room.localParticipant.identity,
						hasPacketTrailer: hasFrameMetadataPublishOptions(frameMetadata),
					},
				};
				this.worker.postMessage(msg);
				sender[E2EE_TRACK_ID] = trackId;
			}
			return;
		}

		if (isScriptTransformSupportedForWorker()) {
			this.log.info('initialize script transform');
			const options: ScriptTransformOptions = {
				kind: 'encode',
				participantIdentity: this.room.localParticipant.identity,
				trackId,
				codec,
				hasPacketTrailer: hasFrameMetadataPublishOptions(frameMetadata),
				packetTrailer: frameMetadata,
			};
			sender.transform = new RTCRtpScriptTransform(this.worker, options);
		} else {
			this.log.info('initialize encoded streams');
			// @ts-expect-error
			const senderStreams = sender.createEncodedStreams();
			const msg: EncodeMessage = {
				kind: 'encode',
				data: {
					readableStream: senderStreams.readable,
					writableStream: senderStreams.writable,
					codec,
					trackId,
					participantIdentity: this.room.localParticipant.identity,
					hasPacketTrailer: hasFrameMetadataPublishOptions(frameMetadata),
					packetTrailer: frameMetadata,
				},
			};
			this.worker.postMessage(msg, [senderStreams.readable, senderStreams.writable]);
		}

		// @ts-expect-error
		sender[E2EE_FLAG] = true;
		// @ts-expect-error
		sender[E2EE_TRACK_ID] = trackId;
	}
}
