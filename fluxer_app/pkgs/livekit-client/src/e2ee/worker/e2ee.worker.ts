// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {workerLogger} from '../../logger.ts';
import type {VideoCodec} from '../../room/track/options.ts';
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import {AsyncQueue} from '../../utils/AsyncQueue.ts';
import {KEY_PROVIDER_DEFAULTS} from '../constants.ts';
import {CryptorErrorReason} from '../errors.ts';
import {CryptorEvent, KeyHandlerEvent} from '../events.ts';
import type {
	DecryptDataResponseMessage,
	E2EEWorkerMessage,
	EncryptDataResponseMessage,
	ErrorMessage,
	InitAck,
	KeyProviderOptions,
	RatchetMessage,
	RatchetRequestMessage,
	RatchetResult,
	ScriptTransformOptions,
} from '../types.ts';
import {DataCryptor} from './DataCryptor.ts';
import {ErrorRateLimiter} from './ErrorRateLimiter.ts';
import {encryptionEnabledMap, FrameCryptor} from './FrameCryptor.ts';
import {ParticipantKeyHandler} from './ParticipantKeyHandler.ts';

const participantCryptors: Array<FrameCryptor> = [];
const participantKeys: Map<string, ParticipantKeyHandler> = new Map();
let sharedKeyHandler: ParticipantKeyHandler | undefined;
const messageQueue = new AsyncQueue();

const isEncryptionEnabled: boolean = false;

let useSharedKey: boolean = false;

let sifTrailer: NonSharedUint8Array | undefined;

let keyProviderOptions: KeyProviderOptions = KEY_PROVIDER_DEFAULTS;

let rtpMap: Map<number, VideoCodec> = new Map();

const dataDecryptErrorLimiter = new ErrorRateLimiter();

workerLogger.setDefaultLevel('info');
workerLogger.methodFactory = (methodName) => (msg, context) => {
	postMessage({
		kind: 'log',
		data: {level: methodName, msg, context},
	});
};
workerLogger.setLevel(workerLogger.getLevel());

self.addEventListener('message', (ev) => {
	messageQueue.run(async () => {
		const {kind, data}: E2EEWorkerMessage = ev.data;

		switch (kind) {
			case 'init': {
				workerLogger.setLevel(data.loglevel);
				workerLogger.info('e2ee worker initialized');
				keyProviderOptions = data.keyProviderOptions;
				useSharedKey = !!data.keyProviderOptions.sharedKey;
				const ackMsg: InitAck = {
					kind: 'initAck',
					data: {enabled: isEncryptionEnabled},
				};
				postMessage(ackMsg);
				break;
			}
			case 'setLogLevel':
				workerLogger.setLevel(data.level);
				break;
			case 'enable':
				setEncryptionEnabled(data.enabled, data.participantIdentity);
				workerLogger.info(`updated e2ee enabled status for ${data.participantIdentity} to ${data.enabled}`);
				postMessage(ev.data);
				break;
			case 'decode': {
				const cryptor = getTrackCryptor(data.participantIdentity, data.trackId);
				cryptor.setHasFrameMetadata(data.hasPacketTrailer);
				cryptor.setupTransform(kind, data.readableStream, data.writableStream, data.trackId, data.codec);
				break;
			}
			case 'encode': {
				const pubCryptor = getTrackCryptor(data.participantIdentity, data.trackId);
				pubCryptor.setHasFrameMetadata(data.hasPacketTrailer);
				pubCryptor.setupTransform(
					kind,
					data.readableStream,
					data.writableStream,
					data.trackId,
					data.codec,
					data.packetTrailer,
				);
				break;
			}

			case 'encryptDataRequest': {
				const {
					payload: encryptedPayload,
					iv,
					keyIndex,
				} = await DataCryptor.encrypt(data.payload, getParticipantKeyHandler(data.participantIdentity));
				postMessage({
					kind: 'encryptDataResponse',
					data: {
						payload: encryptedPayload,
						iv,
						keyIndex,
						uuid: data.uuid,
					},
				} satisfies EncryptDataResponseMessage);
				break;
			}

			case 'decryptDataRequest':
				try {
					const {payload: decryptedPayload} = await DataCryptor.decrypt(
						data.payload,
						data.iv,
						getParticipantKeyHandler(data.participantIdentity),
						data.keyIndex,
					);
					postMessage({
						kind: 'decryptDataResponse',
						data: {payload: decryptedPayload, uuid: data.uuid},
					} satisfies DecryptDataResponseMessage);
				} catch (error) {
					const errorKey = `${data.participantIdentity}-datadecrypt`;
					const shouldLog = dataDecryptErrorLimiter.shouldEmit(errorKey, () => {
						workerLogger.warn(`Suppressing further data decryption errors for ${data.participantIdentity}`, {errorKey});
					});
					if (shouldLog) {
						workerLogger.error('DataCryptor decryption failed', {
							error,
							participantIdentity: data.participantIdentity,
							uuid: data.uuid,
						});
					}
					postMessage({
						kind: 'error',
						data: {
							error: error instanceof Error ? error : new Error(String(error)),
							uuid: data.uuid,
						},
					} satisfies ErrorMessage);
				}
				break;

			case 'setKey':
				if (useSharedKey) {
					await setSharedKey(data.key, data.keyIndex, data.updateCurrentKeyIndex);
				} else if (data.participantIdentity) {
					workerLogger.info(`set participant sender key ${data.participantIdentity} index ${data.keyIndex}`);
					await getParticipantKeyHandler(data.participantIdentity).setKey(
						data.key,
						data.keyIndex,
						data.updateCurrentKeyIndex,
					);
				} else {
					workerLogger.error('no participant Id was provided and shared key usage is disabled');
				}
				break;
			case 'removeTransform':
				unsetCryptorParticipant(data.trackId, data.participantIdentity);
				break;
			case 'updateCodec': {
				const trackCryptor = getTrackCryptor(data.participantIdentity, data.trackId, data.previousTrackId);
				if (data.codec) {
					trackCryptor.setVideoCodec(data.codec);
				}
				trackCryptor.setHasFrameMetadata(data.hasPacketTrailer);
				workerLogger.info('updated codec', {
					participantIdentity: data.participantIdentity,
					trackId: data.trackId,
					previousTrackId: data.previousTrackId,
					codec: data.codec,
					hasPacketTrailer: data.hasPacketTrailer,
				});
				if (data.previousTrackId !== undefined && !trackCryptor.ensureTransform()) {
					workerLogger.error('could not re-establish transform for reused sender or receiver', {
						participantIdentity: data.participantIdentity,
						trackId: data.trackId,
						previousTrackId: data.previousTrackId,
					});
				}
				break;
			}
			case 'setRTPMap':
				rtpMap = data.map;
				participantCryptors.forEach((cr) => {
					if (cr.getParticipantIdentity() === data.participantIdentity) {
						cr.setRtpMap(data.map);
					}
				});
				break;
			case 'ratchetRequest':
				handleRatchetRequest(data);
				break;
			case 'setSifTrailer':
				handleSifTrailer(data.trailer);
				break;
			default:
				break;
		}
	});
});

async function handleRatchetRequest(data: RatchetRequestMessage['data']) {
	if (useSharedKey) {
		const keyHandler = getSharedKeyHandler();
		await keyHandler.ratchetKey(data.keyIndex);
		keyHandler.resetKeyStatus();
	} else if (data.participantIdentity) {
		const keyHandler = getParticipantKeyHandler(data.participantIdentity);
		await keyHandler.ratchetKey(data.keyIndex);
		keyHandler.resetKeyStatus();
	} else {
		workerLogger.error('no participant Id was provided for ratchet request and shared key usage is disabled');
	}
}

function getTrackCryptor(participantIdentity: string, trackId: string, previousTrackId?: string) {
	let cryptors = participantCryptors.filter((c) => c.getTrackId() === trackId);

	if (cryptors.length === 0 && previousTrackId !== undefined && previousTrackId !== trackId) {
		const previous = participantCryptors.filter((c) => c.getTrackId() === previousTrackId);
		if (previous.length > 0) {
			workerLogger.info('reusing cryptor from previous trackId', {
				participantIdentity,
				trackId,
				previousTrackId,
			});
			previous[0].setTrackId(trackId);
			cryptors = previous;
		}
	}

	if (cryptors.length > 1) {
		const debugInfo = cryptors
			.map((c) => {
				return {participant: c.getParticipantIdentity()};
			})
			.join(',');
		workerLogger.error(
			`Found multiple cryptors for the same trackID ${trackId}. target participant: ${participantIdentity} `,
			{participants: debugInfo},
		);
	}
	let cryptor = cryptors[0];
	if (!cryptor) {
		workerLogger.info('creating new cryptor for', {participantIdentity, trackId});
		if (!keyProviderOptions) {
			throw Error('Missing keyProvider options');
		}
		cryptor = new FrameCryptor({
			participantIdentity,
			keys: getParticipantKeyHandler(participantIdentity),
			keyProviderOptions,
			sifTrailer,
		});
		cryptor.setRtpMap(rtpMap);
		setupCryptorErrorEvents(cryptor);
		participantCryptors.push(cryptor);
	} else if (participantIdentity !== cryptor.getParticipantIdentity()) {
		cryptor.setParticipant(participantIdentity, getParticipantKeyHandler(participantIdentity));
	}

	return cryptor;
}

function getParticipantKeyHandler(participantIdentity: string) {
	if (useSharedKey) {
		return getSharedKeyHandler();
	}
	let keys = participantKeys.get(participantIdentity);
	if (!keys) {
		keys = new ParticipantKeyHandler(participantIdentity, keyProviderOptions);
		keys.on(KeyHandlerEvent.KeyRatcheted, emitRatchetedKeys);
		participantKeys.set(participantIdentity, keys);
	}
	return keys;
}

function getSharedKeyHandler() {
	if (!sharedKeyHandler) {
		workerLogger.debug('creating new shared key handler');
		sharedKeyHandler = new ParticipantKeyHandler('shared-key', keyProviderOptions);
	}
	return sharedKeyHandler;
}

function unsetCryptorParticipant(trackId: string, participantIdentity: string) {
	const cryptors = participantCryptors.filter(
		(c) => c.getParticipantIdentity() === participantIdentity && c.getTrackId() === trackId,
	);
	if (cryptors.length > 1) {
		workerLogger.error('Found multiple cryptors for the same participant and trackID combination', {
			trackId,
			participantIdentity,
		});
	}
	const cryptor = cryptors[0];
	if (!cryptor) {
		workerLogger.warn('Could not unset participant on cryptor', {trackId, participantIdentity});
	} else {
		cryptor.unsetParticipant();
	}
}

function setEncryptionEnabled(enable: boolean, participantIdentity: string) {
	workerLogger.debug(`setting encryption enabled for all tracks of ${participantIdentity}`, {
		enable,
	});
	encryptionEnabledMap.set(participantIdentity, enable);
}

async function setSharedKey(key: CryptoKey, index?: number, updateCurrentKeyIndex?: boolean) {
	workerLogger.info('set shared key', {index});
	await getSharedKeyHandler().setKey(key, index, updateCurrentKeyIndex);
}

function setupCryptorErrorEvents(cryptor: FrameCryptor) {
	cryptor.on(CryptorEvent.Error, (error) => {
		const msg: ErrorMessage = {
			kind: 'error',
			data: {
				error: new Error(`${CryptorErrorReason[error.reason]}: ${error.message}`),
				participantIdentity: error.participantIdentity,
			},
		};
		postMessage(msg);
	});
}

function emitRatchetedKeys(ratchetResult: RatchetResult, participantIdentity: string, keyIndex?: number) {
	const msg: RatchetMessage = {
		kind: `ratchetKey`,
		data: {
			participantIdentity,
			keyIndex,
			ratchetResult,
		},
	};
	postMessage(msg);
}

function handleSifTrailer(trailer: NonSharedUint8Array) {
	sifTrailer = trailer;
	participantCryptors.forEach((c) => {
		c.setSifTrailer(trailer);
	});
}
if (self.RTCTransformEvent) {
	self.onrtctransform = (event: RTCTransformEvent) => {
		const transformer = event.transformer;
		const options = transformer.options as ScriptTransformOptions;
		const {kind, participantIdentity, trackId, codec, hasPacketTrailer} = options;
		messageQueue.run(async () => {
			const cryptor = getTrackCryptor(participantIdentity, trackId);
			cryptor.setHasFrameMetadata(hasPacketTrailer);
			workerLogger.debug('onrtctransform setup', {participantIdentity, trackId, codec});
			cryptor.setupTransform(
				kind,
				transformer.readable,
				transformer.writable,
				trackId,
				codec,
				kind === 'encode' ? options.packetTrailer : undefined,
			);
		});
	};
}
