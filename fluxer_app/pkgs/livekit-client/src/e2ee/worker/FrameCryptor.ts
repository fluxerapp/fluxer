// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {EventEmitter} from 'events';
import type TypedEventEmitter from 'typed-emitter';
import {getErrorDescription} from '../../api/utils.ts';
import {appendPacketTrailerToEncodedFrame, processPacketTrailer} from '../../frameMetadata/frameMetadata.ts';
import type {FrameMetadataPublishOptions} from '../../frameMetadata/types.ts';
import {hasFrameMetadataPublishOptions} from '../../frameMetadata/utils.ts';
import {workerLogger} from '../../logger.ts';
import {type VideoCodec, videoCodecs} from '../../room/track/options.ts';
import {mimeTypeToVideoCodecString} from '../../room/track/utils.ts';
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import {ENCRYPTION_ALGORITHM, IV_LENGTH, UNENCRYPTED_BYTES} from '../constants.ts';
import {CryptorError, CryptorErrorReason} from '../errors.ts';
import {type CryptorCallbacks, CryptorEvent} from '../events.ts';
import type {
	DecodeRatchetOptions,
	KeyProviderOptions,
	KeySet,
	PTMetadataFromE2EEMessage,
	RatchetResult,
} from '../types.ts';
import {deriveKeys, isVideoFrame, needsRbspUnescaping, parseRbsp, writeRbsp} from '../utils.ts';
import {
	type Av1E2eeMetadata,
	buildAv1E2eeMetadataObu,
	computeAv1EncryptionLayout,
	extractAv1E2eeMetadataObu,
	GCM_TAG_LENGTH_BYTES,
} from './av1Utils.ts';
import {ErrorRateLimiter} from './ErrorRateLimiter.ts';
import {processNALUsForEncryption} from './naluUtils.ts';
import type {ParticipantKeyHandler} from './ParticipantKeyHandler.ts';
import {identifySifPayload} from './sifPayload.ts';

export const encryptionEnabledMap: Map<string, boolean> = new Map();

const FRAME_TRAILER_LENGTH = 2;

export interface EncryptedFrameLayout {
	frameHeaderLength: number;
	ivLength: number;
	ivStart: number;
	cipherTextStart: number;
	cipherTextLength: number;
	keyIndex: number;
}

export function getFrameHeaderLength(unencryptedBytes: number, frameDataLength: number): number {
	if (!Number.isFinite(unencryptedBytes) || unencryptedBytes <= 0) return 0;
	return Math.min(Math.trunc(unencryptedBytes), frameDataLength);
}

export function getEncryptedFrameLayout(
	frameData: ArrayBufferLike,
	frameHeaderLength: number,
): EncryptedFrameLayout | undefined {
	const dataLength = frameData.byteLength;
	const encryptedOverheadLength = FRAME_TRAILER_LENGTH + IV_LENGTH + GCM_TAG_LENGTH_BYTES;
	const maxHeaderLength = Math.max(0, dataLength - encryptedOverheadLength);
	const safeHeaderLength = Math.min(getFrameHeaderLength(frameHeaderLength, dataLength), maxHeaderLength);
	if (dataLength < safeHeaderLength + encryptedOverheadLength) return undefined;
	const trailerStart = dataLength - FRAME_TRAILER_LENGTH;
	const frameTrailer = new Uint8Array(frameData, trailerStart, FRAME_TRAILER_LENGTH);
	const ivLength = frameTrailer[0];
	const keyIndex = frameTrailer[1];
	if (ivLength !== IV_LENGTH) return undefined;
	const ivStart = trailerStart - ivLength;
	if (ivStart < safeHeaderLength) return undefined;
	const cipherTextStart = safeHeaderLength;
	const cipherTextLength = ivStart - cipherTextStart;
	if (cipherTextLength < GCM_TAG_LENGTH_BYTES) return undefined;
	return {frameHeaderLength: safeHeaderLength, ivLength, ivStart, cipherTextStart, cipherTextLength, keyIndex};
}

export interface FrameCryptorConstructor {
	new (opts?: unknown): BaseFrameCryptor;
}

export interface TransformerInfo {
	readable: ReadableStream;
	writable: WritableStream;
	transformer: TransformStream;
	trackId: string;
	symbol: symbol;
}

type EncodedFrame = RTCEncodedVideoFrame | RTCEncodedAudioFrame;

export class BaseFrameCryptor extends (EventEmitter as new () => TypedEventEmitter<CryptorCallbacks>) {
	protected encodeFunction(_encodedFrame: EncodedFrame, _controller: TransformStreamDefaultController): Promise<void> {
		throw Error('not implemented for subclass');
	}

	protected decodeFunction(_encodedFrame: EncodedFrame, _controller: TransformStreamDefaultController): Promise<void> {
		throw Error('not implemented for subclass');
	}
}

export class FrameCryptor extends BaseFrameCryptor {
	private sendCounts: Map<number, number>;

	private participantIdentity: string | undefined;

	private trackId: string | undefined;

	private keys: ParticipantKeyHandler;

	private videoCodec?: VideoCodec;

	private rtpMap: Map<number, VideoCodec>;

	private keyProviderOptions: KeyProviderOptions;

	private sifTrailer: NonSharedUint8Array;

	private detectedCodec?: VideoCodec;

	private av1LayoutFailureLogged: boolean = false;

	private currentTransform?: TransformerInfo;

	private retainedStreams?: {
		readable: ReadableStream<EncodedFrame>;
		writable: WritableStream<EncodedFrame>;
		operation: 'encode' | 'decode';
	};

	private hasFrameMetadata: boolean = false;

	private frameMetadataOpts?: FrameMetadataPublishOptions;

	private frameMetadataFrameId = 0;

	private errorLimiter = new ErrorRateLimiter();

	private undecryptedTrackTimeout?: ReturnType<typeof setTimeout>;

	private readonly UNDECRYPTED_TRACK_GRACE_MS = 2000;

	private loggedNALUFallbacks: Set<string> = new Set();

	private readonly encodeTrailer: NonSharedUint8Array = new Uint8Array(FRAME_TRAILER_LENGTH);

	constructor(opts: {
		keys: ParticipantKeyHandler;
		participantIdentity: string;
		keyProviderOptions: KeyProviderOptions;
		sifTrailer?: NonSharedUint8Array;
	}) {
		super();
		this.sendCounts = new Map();
		this.keys = opts.keys;
		this.participantIdentity = opts.participantIdentity;
		this.rtpMap = new Map();
		this.keyProviderOptions = opts.keyProviderOptions;
		this.sifTrailer = opts.sifTrailer ?? Uint8Array.from([]);
	}

	private get logContext() {
		return {
			participant: this.participantIdentity,
			mediaTrackId: this.trackId,
			fallbackCodec: this.videoCodec,
		};
	}

	setParticipant(id: string, keys: ParticipantKeyHandler) {
		workerLogger.debug('setting new participant on cryptor', {
			...this.logContext,
			newParticipant: id,
			hadPreviousParticipant: !!this.participantIdentity,
		});

		if (this.participantIdentity && this.participantIdentity !== id) {
			workerLogger.warn('cryptor has already a participant set, cleaning up before switching', {
				oldParticipant: this.participantIdentity,
				newParticipant: id,
				trackId: this.trackId,
			});
			this.unsetParticipant();
		}

		this.participantIdentity = id;
		this.keys = keys;
	}

	unsetParticipant() {
		workerLogger.debug('unsetting participant', this.logContext);

		clearTimeout(this.undecryptedTrackTimeout);
		this.undecryptedTrackTimeout = undefined;
		this.participantIdentity = undefined;
		this.videoCodec = undefined;
		this.resetCodecState();
		this.errorLimiter.reset();
	}

	isEnabled() {
		if (this.participantIdentity) {
			return encryptionEnabledMap.get(this.participantIdentity);
		} else {
			return undefined;
		}
	}

	getParticipantIdentity() {
		return this.participantIdentity;
	}

	getTrackId() {
		return this.trackId;
	}

	setTrackId(trackId: string) {
		if (this.trackId === trackId) {
			return;
		}
		workerLogger.debug('re-pointing cryptor at new trackId', {
			...this.logContext,
			newTrackId: trackId,
		});
		this.trackId = trackId;
		this.resetCodecState();
	}

	hasActiveTransform() {
		return !!this.currentTransform;
	}

	private scheduleUndecryptedTrackWatchdog(operation: 'encode' | 'decode') {
		clearTimeout(this.undecryptedTrackTimeout);
		this.undecryptedTrackTimeout = undefined;
		if (operation !== 'decode') {
			return;
		}
		this.undecryptedTrackTimeout = setTimeout(() => {
			if (this.currentTransform || !this.isEnabled()) {
				return;
			}
			workerLogger.warn('encrypted track has no active decrypt transform', this.logContext);
			this.emitThrottledError(
				new CryptorError(
					`no active decrypt transform for encrypted track ${this.trackId}`,
					CryptorErrorReason.InternalError,
					this.participantIdentity,
				),
			);
		}, this.UNDECRYPTED_TRACK_GRACE_MS);
	}

	ensureTransform() {
		if (this.currentTransform) {
			return true;
		}
		if (!this.retainedStreams || this.trackId === undefined) {
			workerLogger.warn('no streams retained, cannot re-establish transform', this.logContext);
			return false;
		}
		const {readable, writable, operation} = this.retainedStreams;
		workerLogger.info('re-establishing transform', {...this.logContext, operation});
		return this.setupTransform(operation, readable, writable, this.trackId);
	}

	setVideoCodec(codec: VideoCodec) {
		if (this.videoCodec !== codec) {
			this.resetCodecState();
		}
		this.videoCodec = codec;
	}

	setRtpMap(map: Map<number, VideoCodec>) {
		this.rtpMap = map;
	}

	setHasFrameMetadata(hasFrameMetadata: boolean) {
		this.hasFrameMetadata = hasFrameMetadata;
	}

	setFrameMetadataOpts(frameMetadata?: FrameMetadataPublishOptions) {
		this.frameMetadataOpts = frameMetadata;
		this.frameMetadataFrameId = 0;
	}

	setupTransform(
		operation: 'encode' | 'decode',
		readable: ReadableStream<EncodedFrame>,
		writable: WritableStream<EncodedFrame>,
		trackId: string,
		codec?: VideoCodec,
		frameMetadata?: FrameMetadataPublishOptions,
	) {
		const trackChanged = this.trackId !== trackId;
		this.setTrackId(trackId);
		if (codec) {
			workerLogger.info('setting codec on cryptor to', {codec});
			this.setVideoCodec(codec);
		} else if (trackChanged) {
			this.videoCodec = undefined;
		}
		if (operation === 'encode') {
			this.setFrameMetadataOpts(frameMetadata);
		}

		workerLogger.debug('Setting up frame cryptor transform', {
			operation,
			passedTrackId: trackId,
			codec,
			hasCurrentTransform: !!this.currentTransform,
			...this.logContext,
		});

		this.retainedStreams = {readable, writable, operation};

		clearTimeout(this.undecryptedTrackTimeout);

		const symbol = Symbol('transform');

		const transformFn = operation === 'encode' ? this.encodeFunction : this.decodeFunction;
		const transformStream = new TransformStream({
			transform: transformFn.bind(this),
		});

		this.currentTransform = {
			readable,
			writable,
			transformer: transformStream,
			trackId,
			symbol,
		};

		try {
			readable
				.pipeThrough(transformStream)
				.pipeTo(writable)
				.catch((e) => {
					if (e instanceof TypeError && e.message === 'Destination stream closed') {
						workerLogger.debug('destination stream closed');
					} else {
						workerLogger.warn('transform error', {error: e, ...this.logContext});
						this.emit(
							CryptorEvent.Error,
							e instanceof CryptorError ? e : new CryptorError(e.message, undefined, this.participantIdentity),
						);
					}
				})
				.finally(() => {
					if (this.currentTransform?.symbol === symbol) {
						workerLogger.debug('transform completed', {
							...this.logContext,
							trackId,
						});
						this.currentTransform = undefined;
					}
					this.scheduleUndecryptedTrackWatchdog(operation);
				});
		} catch (e: unknown) {
			if (this.currentTransform?.symbol === symbol) {
				this.currentTransform = undefined;
			}
			workerLogger.error('failed to set up transform', {error: e, ...this.logContext});
			this.emit(
				CryptorEvent.Error,
				e instanceof CryptorError
					? e
					: new CryptorError(
							getErrorDescription(e, 'transform'),
							CryptorErrorReason.InternalError,
							this.participantIdentity,
						),
			);
			return false;
		}

		return true;
	}

	setSifTrailer(trailer: NonSharedUint8Array) {
		workerLogger.debug('setting SIF trailer', {...this.logContext, trailer});
		this.sifTrailer = trailer;
	}

	private emitThrottledError(error: CryptorError) {
		const errorKey = `${this.participantIdentity}-${error.reason}-decrypt`;
		const emit = this.errorLimiter.shouldEmit(errorKey, () => {
			workerLogger.warn(`Suppressing further decryption errors for ${this.participantIdentity}`, {
				...this.logContext,
				errorKey,
			});
		});
		if (!emit) return;

		const count = this.errorLimiter.countFor(errorKey);
		if (count > 1) {
			workerLogger.debug(`Decryption error (${count} occurrences in window)`, {
				...this.logContext,
				reason: CryptorErrorReason[error.reason],
			});
		}
		this.emit(CryptorEvent.Error, error);
	}

	private resetCodecState() {
		this.detectedCodec = undefined;
		this.av1LayoutFailureLogged = false;
	}

	protected override async encodeFunction(encodedFrame: EncodedFrame, controller: TransformStreamDefaultController) {
		if (encodedFrame.data.byteLength === 0) {
			controller.enqueue(encodedFrame);
			return;
		}

		const encryptionEnabled = this.isEnabled();
		if (encryptionEnabled === false) {
			this.appendFrameMetadata(encodedFrame);
			controller.enqueue(encodedFrame);
			return;
		}
		if (encryptionEnabled !== true) {
			return;
		}
		const keySet = this.keys.getKeySet();
		if (!keySet) {
			this.emitThrottledError(
				new CryptorError(
					`key set not found for ${this.participantIdentity} at index ${this.keys.getCurrentKeyIndex()}`,
					CryptorErrorReason.MissingKey,
					this.participantIdentity,
				),
			);
			return;
		}
		const {encryptionKey} = keySet;
		const keyIndex = this.keys.getCurrentKeyIndex();

		if (encryptionKey) {
			const iv = this.makeIV(encodedFrame.getMetadata().synchronizationSource ?? -1, encodedFrame.timestamp);
			const frameInfo = this.getUnencryptedBytes(encodedFrame);

			if (isVideoFrame(encodedFrame) && this.detectedCodec === 'av1') {
				await this.encodeAv1Frame(encodedFrame, controller, encryptionKey, iv, keyIndex);
				return;
			}

			const frameHeaderLength = getFrameHeaderLength(frameInfo.unencryptedBytes, encodedFrame.data.byteLength);
			const frameHeader: NonSharedUint8Array = new Uint8Array(encodedFrame.data, 0, frameHeaderLength);
			const frameTrailer = this.encodeTrailer;

			frameTrailer[0] = IV_LENGTH;
			frameTrailer[1] = keyIndex;

			try {
				const cipherText = await crypto.subtle.encrypt(
					{
						name: ENCRYPTION_ALGORITHM,
						iv,
						additionalData: frameHeader,
					},
					encryptionKey,
					new Uint8Array(encodedFrame.data, frameHeaderLength),
				);

				let newDataWithoutHeader: NonSharedUint8Array = new Uint8Array(
					cipherText.byteLength + iv.byteLength + frameTrailer.byteLength,
				);
				newDataWithoutHeader.set(new Uint8Array(cipherText));
				newDataWithoutHeader.set(new Uint8Array(iv), cipherText.byteLength);
				newDataWithoutHeader.set(frameTrailer, cipherText.byteLength + iv.byteLength);

				if (frameInfo.requiresNALUProcessing) {
					newDataWithoutHeader = writeRbsp(newDataWithoutHeader);
				}

				const newData = new Uint8Array(frameHeader.byteLength + newDataWithoutHeader.byteLength);
				newData.set(frameHeader);
				newData.set(newDataWithoutHeader, frameHeader.byteLength);

				encodedFrame.data = newData.buffer;
				this.appendFrameMetadata(encodedFrame);

				controller.enqueue(encodedFrame);
				return;
			} catch (e: unknown) {
				workerLogger.error(`error while encrypting`, {...this.logContext, error: e});
			}
		} else {
			workerLogger.debug('failed to encrypt, emitting error', this.logContext);
			this.emitThrottledError(
				new CryptorError(
					`encryption key missing for encoding`,
					CryptorErrorReason.MissingKey,
					this.participantIdentity,
				),
			);
		}
	}

	private async encodeAv1Frame(
		encodedFrame: RTCEncodedVideoFrame,
		controller: TransformStreamDefaultController,
		encryptionKey: CryptoKey,
		iv: ArrayBuffer,
		keyIndex: number,
	) {
		const plainPayload: NonSharedUint8Array = new Uint8Array(encodedFrame.data);
		const layout = computeAv1EncryptionLayout(plainPayload);
		if (!layout) {
			const debugInfo = this.av1LayoutFailureLogged
				? undefined
				: {
						length: plainPayload.byteLength,
						firstByte: plainPayload[0],
						looksLikeRtpAggregationHeader: (plainPayload[0] & 0x07) === 0,
						looksLikeObuHeader: (plainPayload[0] & 0x80) === 0 && (plainPayload[0] & 0x01) === 0,
					};
			this.av1LayoutFailureLogged = true;
			workerLogger.warn('AV1 E2EE could not determine encryption layout, dropping frame', {
				...this.logContext,
				...(debugInfo ? {debugInfo} : {}),
			});
			this.emitThrottledError(
				new CryptorError(
					`unable to encrypt AV1 frame: layout detection failed`,
					CryptorErrorReason.InternalError,
					this.participantIdentity,
				),
			);
			return;
		}
		try {
			const plainProtected = layout.extractProtected(plainPayload);
			const cipherProtectedWithTag = new Uint8Array(
				await crypto.subtle.encrypt(
					{
						name: ENCRYPTION_ALGORITHM,
						iv,
						additionalData: layout.buildAAD(plainPayload) as BufferSource,
					},
					encryptionKey,
					plainProtected as BufferSource,
				),
			);
			if (cipherProtectedWithTag.byteLength !== plainProtected.byteLength + GCM_TAG_LENGTH_BYTES) {
				throw new Error(
					`Unexpected AES-GCM output length: got ${cipherProtectedWithTag.byteLength}, expected ${
						plainProtected.byteLength + GCM_TAG_LENGTH_BYTES
					}`,
				);
			}
			const cipherProtected = cipherProtectedWithTag.subarray(0, plainProtected.byteLength);
			const tag = cipherProtectedWithTag.subarray(plainProtected.byteLength);
			const metadataObu = buildAv1E2eeMetadataObu({
				keyIndex,
				iv: new Uint8Array(iv),
				tag,
			});
			const newPayload = new Uint8Array(plainPayload.byteLength + metadataObu.byteLength);
			newPayload.set(plainPayload);
			layout.writeProtected(newPayload, cipherProtected);
			newPayload.set(metadataObu, plainPayload.byteLength);
			encodedFrame.data = newPayload.buffer;
			this.appendFrameMetadata(encodedFrame);
			controller.enqueue(encodedFrame);
		} catch (e: unknown) {
			const errorMessage = getErrorDescription(e, 'encryption');
			workerLogger.error('AV1 E2EE encryption failed, dropping frame', {
				error: e,
				errorMessage,
				...this.logContext,
			});
			this.emitThrottledError(
				new CryptorError(
					`unable to encrypt AV1 frame: ${errorMessage}`,
					CryptorErrorReason.InternalError,
					this.participantIdentity,
				),
			);
		}
	}

	private appendFrameMetadata(encodedFrame: EncodedFrame) {
		if (!hasFrameMetadataPublishOptions(this.frameMetadataOpts) || !isVideoFrame(encodedFrame)) {
			return;
		}

		if (this.frameMetadataOpts?.frameId) {
			this.frameMetadataFrameId = this.frameMetadataFrameId === 0xffffffff ? 1 : this.frameMetadataFrameId + 1;
		}
		appendPacketTrailerToEncodedFrame(encodedFrame, this.frameMetadataOpts, this.frameMetadataFrameId);
	}

	protected override async decodeFunction(encodedFrame: EncodedFrame, controller: TransformStreamDefaultController) {
		if (this.hasFrameMetadata && isVideoFrame(encodedFrame)) {
			try {
				const ptResult = processPacketTrailer(encodedFrame, this.trackId);
				if (ptResult.data) {
					encodedFrame.data = ptResult.data;
				}
				if (ptResult.payload && this.participantIdentity) {
					const msg: PTMetadataFromE2EEMessage = {
						kind: 'packetTrailerMetadata',
						data: ptResult.payload,
					};
					postMessage(msg);
				}
			} catch {}
		}

		if (encodedFrame.data.byteLength === 0) {
			controller.enqueue(encodedFrame);
			return;
		}

		const encryptionEnabled = this.isEnabled();

		if (encryptionEnabled === undefined) {
			if (this.participantIdentity === undefined) {
				workerLogger.debug('dropping frame for unassigned cryptor', this.logContext);
				return;
			}
			this.emitThrottledError(
				new CryptorError(
					`encryption state unknown for track ${this.trackId}, dropping frame`,
					CryptorErrorReason.InternalError,
					this.participantIdentity,
				),
			);
			return;
		}

		if (!encryptionEnabled) {
			controller.enqueue(encodedFrame);
			return;
		}

		if (isFrameServerInjected(encodedFrame.data, this.sifTrailer)) {
			encodedFrame.data = encodedFrame.data.slice(0, encodedFrame.data.byteLength - this.sifTrailer.byteLength);
			if (await identifySifPayload(encodedFrame.data)) {
				workerLogger.debug('enqueue SIF', this.logContext);
				controller.enqueue(encodedFrame);
				return;
			} else {
				workerLogger.warn('Unexpected SIF frame payload, dropping frame', this.logContext);
				return;
			}
		}
		const data: NonSharedUint8Array = new Uint8Array(encodedFrame.data);
		const extractedAv1 = isVideoFrame(encodedFrame) ? extractAv1E2eeMetadataObu(data) : undefined;
		let keyIndex: number;
		if (extractedAv1) {
			keyIndex = extractedAv1.meta.keyIndex;
			this.detectedCodec = 'av1';
		} else {
			keyIndex = data[encodedFrame.data.byteLength - 1];
		}

		if (this.keys.hasInvalidKeyAtIndex(keyIndex)) {
			return;
		}

		if (this.keys.getKeySet(keyIndex)) {
			try {
				const decodedFrame = await this.decryptFrame(
					encodedFrame,
					keyIndex,
					undefined,
					{ratchetCount: 0},
					extractedAv1 ? {payload: extractedAv1.payload, meta: extractedAv1.meta} : undefined,
				);
				this.keys.decryptionSuccess(keyIndex);
				if (decodedFrame) {
					controller.enqueue(decodedFrame);
					return;
				}
			} catch (error) {
				if (error instanceof CryptorError && error.reason === CryptorErrorReason.InvalidKey) {
					if (this.keys.hasValidKey) {
						this.emitThrottledError(error);
						this.keys.decryptionFailure(keyIndex);
					}
				} else if (error instanceof CryptorError && error.reason === CryptorErrorReason.InternalError) {
					workerLogger.warn('dropping malformed encrypted frame', {error, ...this.logContext});
					this.emitThrottledError(error);
				} else {
					workerLogger.warn('decoding frame failed', {error});
				}
			}
		} else {
			workerLogger.warn(`skipping decryption due to missing key at index ${keyIndex}`);
			this.emitThrottledError(
				new CryptorError(
					`missing key at index ${keyIndex} for participant ${this.participantIdentity}`,
					CryptorErrorReason.MissingKey,
					this.participantIdentity,
				),
			);
			this.keys.decryptionFailure(keyIndex);
		}
	}

	private async decryptFrame(
		encodedFrame: EncodedFrame,
		keyIndex: number,
		initialMaterial: KeySet | undefined = undefined,
		ratchetOpts: DecodeRatchetOptions = {ratchetCount: 0},
		av1?: {payload: Uint8Array; meta: Av1E2eeMetadata},
	): Promise<EncodedFrame | undefined> {
		const keySet = this.keys.getKeySet(keyIndex);
		if (!ratchetOpts.encryptionKey && !keySet) {
			throw new TypeError(`no encryption key found for decryption of ${this.participantIdentity}`);
		}
		const frameInfo = this.getUnencryptedBytes(encodedFrame);

		try {
			if (isVideoFrame(encodedFrame) && av1) {
				const {payload, meta} = av1;
				if (meta.keyIndex !== keyIndex) {
					throw new Error(`AV1 key index mismatch (meta=${meta.keyIndex}, expected=${keyIndex})`);
				}
				const layout = computeAv1EncryptionLayout(payload);
				if (!layout) {
					throw new Error('AV1 layout detection failed during decryption');
				}
				const cipherProtected = layout.extractProtected(payload);
				const cipherProtectedWithTag = new Uint8Array(cipherProtected.byteLength + meta.tag.byteLength);
				cipherProtectedWithTag.set(cipherProtected);
				cipherProtectedWithTag.set(meta.tag, cipherProtected.byteLength);
				const plainProtected = new Uint8Array(
					await crypto.subtle.decrypt(
						{
							name: ENCRYPTION_ALGORITHM,
							iv: meta.iv as BufferSource,
							additionalData: layout.buildAAD(payload) as BufferSource,
						},
						ratchetOpts.encryptionKey ?? keySet!.encryptionKey,
						cipherProtectedWithTag,
					),
				);
				const newPayload = new Uint8Array(payload.byteLength);
				newPayload.set(payload);
				layout.writeProtected(newPayload, plainProtected);
				encodedFrame.data = newPayload.buffer;
				return encodedFrame;
			}

			const frameHeaderLength = getFrameHeaderLength(frameInfo.unencryptedBytes, encodedFrame.data.byteLength);
			let frameHeader: NonSharedUint8Array = new Uint8Array(encodedFrame.data, 0, frameHeaderLength);
			let encryptedData: NonSharedUint8Array = new Uint8Array(
				encodedFrame.data,
				frameHeader.length,
				encodedFrame.data.byteLength - frameHeader.length,
			);
			if (frameInfo.requiresNALUProcessing && needsRbspUnescaping(encryptedData)) {
				encryptedData = parseRbsp(encryptedData);
				const newUint8 = new Uint8Array(frameHeader.byteLength + encryptedData.byteLength);
				newUint8.set(frameHeader);
				newUint8.set(encryptedData, frameHeader.byteLength);
				encodedFrame.data = newUint8.buffer;
			}

			const encryptedFrameLayout = getEncryptedFrameLayout(encodedFrame.data, frameHeader.byteLength);
			if (!encryptedFrameLayout) {
				throw new CryptorError(
					`invalid encrypted frame layout for participant ${this.participantIdentity}`,
					CryptorErrorReason.InternalError,
					this.participantIdentity,
				);
			}
			frameHeader = new Uint8Array(encodedFrame.data, 0, encryptedFrameLayout.frameHeaderLength);
			const iv = new Uint8Array(encodedFrame.data, encryptedFrameLayout.ivStart, encryptedFrameLayout.ivLength);

			const plainText = await crypto.subtle.decrypt(
				{
					name: ENCRYPTION_ALGORITHM,
					iv,
					additionalData: frameHeader,
				},
				ratchetOpts.encryptionKey ?? keySet!.encryptionKey,
				new Uint8Array(encodedFrame.data, encryptedFrameLayout.cipherTextStart, encryptedFrameLayout.cipherTextLength),
			);

			const newData = new ArrayBuffer(frameHeader.byteLength + plainText.byteLength);
			const newUint8 = new Uint8Array(newData);

			newUint8.set(frameHeader);
			newUint8.set(new Uint8Array(plainText), frameHeader.byteLength);

			encodedFrame.data = newData;

			return encodedFrame;
		} catch (error: unknown) {
			if (error instanceof CryptorError && error.reason === CryptorErrorReason.InternalError) {
				throw error;
			}
			if (this.keyProviderOptions.ratchetWindowSize > 0) {
				if (ratchetOpts.ratchetCount < this.keyProviderOptions.ratchetWindowSize) {
					workerLogger.debug(
						`ratcheting key attempt ${ratchetOpts.ratchetCount} of ${
							this.keyProviderOptions.ratchetWindowSize
						}, for kind ${isVideoFrame(encodedFrame) ? 'video' : 'audio'}`,
					);

					let ratchetedKeySet: KeySet | undefined;
					let ratchetResult: RatchetResult | undefined;
					if ((initialMaterial ?? keySet) === this.keys.getKeySet(keyIndex)) {
						ratchetResult = await this.keys.ratchetKey(keyIndex, false);

						ratchetedKeySet = await deriveKeys(ratchetResult.cryptoKey, this.keyProviderOptions);
					}

					const frame = await this.decryptFrame(
						encodedFrame,
						keyIndex,
						initialMaterial || keySet,
						{
							ratchetCount: ratchetOpts.ratchetCount + 1,
							encryptionKey: ratchetedKeySet?.encryptionKey,
						},
						av1,
					);
					if (frame && ratchetedKeySet) {
						if ((initialMaterial ?? keySet) === this.keys.getKeySet(keyIndex)) {
							this.keys.setKeySet(ratchetedKeySet, keyIndex, ratchetResult);
							this.keys.setCurrentKeyIndex(keyIndex);
						}
					}
					return frame;
				} else {
					workerLogger.warn('maximum ratchet attempts exceeded');
					throw new CryptorError(
						`valid key missing for participant ${this.participantIdentity}`,
						CryptorErrorReason.InvalidKey,
						this.participantIdentity,
					);
				}
			} else {
				throw new CryptorError(
					`Decryption failed: ${getErrorDescription(error, 'decryption')}`,
					CryptorErrorReason.InvalidKey,
					this.participantIdentity,
				);
			}
		}
	}

	private makeIV(synchronizationSource: number, timestamp: number): ArrayBuffer {
		const iv = new ArrayBuffer(IV_LENGTH);
		const ivView = new DataView(iv);

		if (!this.sendCounts.has(synchronizationSource)) {
			this.sendCounts.set(synchronizationSource, Math.floor(Math.random() * 0xffff));
		}

		const sendCount = this.sendCounts.get(synchronizationSource) ?? 0;

		ivView.setUint32(0, synchronizationSource);
		ivView.setUint32(4, timestamp);
		ivView.setUint32(8, timestamp - (sendCount % 0xffff));

		this.sendCounts.set(synchronizationSource, sendCount + 1);

		return iv;
	}

	private static readonly FRAME_INFO_AUDIO = {
		unencryptedBytes: UNENCRYPTED_BYTES.audio,
		requiresNALUProcessing: false,
	} as const;

	private static readonly FRAME_INFO_VP8_KEY = {
		unencryptedBytes: UNENCRYPTED_BYTES.key,
		requiresNALUProcessing: false,
	} as const;

	private static readonly FRAME_INFO_VP8_DELTA = {
		unencryptedBytes: UNENCRYPTED_BYTES.delta,
		requiresNALUProcessing: false,
	} as const;

	private static readonly FRAME_INFO_ZERO = {unencryptedBytes: 0, requiresNALUProcessing: false} as const;

	private getUnencryptedBytes(frame: EncodedFrame): {
		readonly unencryptedBytes: number;
		readonly requiresNALUProcessing: boolean;
	} {
		if (!isVideoFrame(frame)) {
			return FrameCryptor.FRAME_INFO_AUDIO;
		}

		const detectedCodec = this.getVideoCodec(frame) ?? this.videoCodec;
		if (detectedCodec !== this.detectedCodec) {
			workerLogger.debug('detected different codec', {
				detectedCodec,
				oldCodec: this.detectedCodec,
				...this.logContext,
			});
			this.detectedCodec = detectedCodec;
		}

		if (detectedCodec === 'vp8') {
			return frame.type === 'key' ? FrameCryptor.FRAME_INFO_VP8_KEY : FrameCryptor.FRAME_INFO_VP8_DELTA;
		}
		if (detectedCodec === 'vp9' || detectedCodec === 'av1') {
			return FrameCryptor.FRAME_INFO_ZERO;
		}

		const payloadType = frame.getMetadata().payloadType;
		const fallbackKey = `${this.participantIdentity}-${this.trackId}-${payloadType}`;
		try {
			const knownCodec = detectedCodec === 'h264' || detectedCodec === 'h265' ? detectedCodec : undefined;
			const naluResult = processNALUsForEncryption(new Uint8Array(frame.data), knownCodec);

			if (naluResult.requiresNALUProcessing) {
				this.loggedNALUFallbacks.delete(fallbackKey);
				return {
					unencryptedBytes: naluResult.unencryptedBytes,
					requiresNALUProcessing: true,
				};
			}
		} catch (e) {
			this.logNALUFallbackOnce(fallbackKey, payloadType, e);
		}

		return frame.type === 'key' ? FrameCryptor.FRAME_INFO_VP8_KEY : FrameCryptor.FRAME_INFO_VP8_DELTA;
	}

	private logNALUFallbackOnce(fallbackKey: string, payloadType: number | undefined, error: unknown) {
		if (this.loggedNALUFallbacks.has(fallbackKey)) {
			return;
		}
		this.loggedNALUFallbacks.add(fallbackKey);
		workerLogger.warn('NALU processing failed, falling back to VP8 handling', {
			error,
			payloadType,
			...this.logContext,
		});
	}

	private getVideoCodec(frame: RTCEncodedVideoFrame): VideoCodec | undefined {
		const metadata = frame.getMetadata();
		if (metadata.mimeType) {
			const maybeKnownCodec = mimeTypeToVideoCodecString(metadata.mimeType);
			if (videoCodecs.includes(maybeKnownCodec)) {
				return maybeKnownCodec;
			}
		}
		if (this.rtpMap.size === 0) {
			return undefined;
		}
		const payloadType = metadata.payloadType;
		return payloadType !== undefined ? this.rtpMap.get(payloadType) : undefined;
	}
}

export function isFrameServerInjected(frameData: ArrayBufferLike, trailerBytes: NonSharedUint8Array): boolean {
	const trailerLen = trailerBytes.byteLength;
	if (trailerLen === 0) {
		return false;
	}
	if (frameData.byteLength < trailerLen) {
		return false;
	}
	const frameTrailer = new Uint8Array(frameData, frameData.byteLength - trailerLen, trailerLen);
	for (let i = 0; i < trailerLen; i++) {
		if (trailerBytes[i] !== frameTrailer[i]) return false;
	}
	return true;
}
