// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {getErrorDescription} from '../../api/utils.ts';
import {workerLogger} from '../../logger.ts';
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import {ENCRYPTION_ALGORITHM} from '../constants.ts';
import {CryptorError, CryptorErrorReason} from '../errors.ts';
import type {DecodeRatchetOptions, KeySet, RatchetResult} from '../types.ts';
import {deriveKeys} from '../utils.ts';
import type {ParticipantKeyHandler} from './ParticipantKeyHandler.ts';

// biome-ignore lint/complexity/noStaticOnlyClass: Public worker API is intentionally grouped under DataCryptor.
export class DataCryptor {
	private static sendCount = 0;

	private static makeIV(timestamp: number) {
		const iv = new ArrayBuffer(12);
		const ivView = new DataView(iv);
		const randomBytes = crypto.getRandomValues(new Uint32Array(1));
		ivView.setUint32(0, randomBytes[0]);
		ivView.setUint32(4, timestamp);
		ivView.setUint32(8, timestamp - (DataCryptor.sendCount % 0xffff));
		DataCryptor.sendCount++;

		return iv;
	}

	static async encrypt(
		data: NonSharedUint8Array,
		keys: ParticipantKeyHandler,
	): Promise<{
		payload: NonSharedUint8Array;
		iv: NonSharedUint8Array;
		keyIndex: number;
	}> {
		const iv = DataCryptor.makeIV(performance.now());
		const keySet = await keys.getKeySet();
		if (!keySet) {
			throw new Error('No key set found');
		}

		const cipherText = await crypto.subtle.encrypt(
			{
				name: ENCRYPTION_ALGORITHM,
				iv,
			},
			keySet.encryptionKey,
			new Uint8Array(data),
		);

		return {
			payload: new Uint8Array(cipherText),
			iv: new Uint8Array(iv),
			keyIndex: keys.getCurrentKeyIndex(),
		};
	}

	static async decrypt(
		data: NonSharedUint8Array,
		iv: NonSharedUint8Array,
		keys: ParticipantKeyHandler,
		keyIndex: number = 0,
		initialMaterial?: KeySet,
		ratchetOpts: DecodeRatchetOptions = {ratchetCount: 0},
	): Promise<{
		payload: NonSharedUint8Array;
	}> {
		const keySet = await keys.getKeySet(keyIndex);
		if (!keySet) {
			throw new Error('No key set found');
		}

		try {
			const plainText = await crypto.subtle.decrypt(
				{
					name: ENCRYPTION_ALGORITHM,
					iv: iv as BufferSource,
				},
				keySet.encryptionKey,
				new Uint8Array(data),
			);
			return {
				payload: new Uint8Array(plainText),
			};
		} catch (error: unknown) {
			if (keys.keyProviderOptions.ratchetWindowSize > 0) {
				if (ratchetOpts.ratchetCount < keys.keyProviderOptions.ratchetWindowSize) {
					workerLogger.debug(
						`DataCryptor: ratcheting key attempt ${ratchetOpts.ratchetCount} of ${
							keys.keyProviderOptions.ratchetWindowSize
						}, for data packet`,
					);

					let ratchetedKeySet: KeySet | undefined;
					let ratchetResult: RatchetResult | undefined;
					if ((initialMaterial ?? keySet) === keys.getKeySet(keyIndex)) {
						ratchetResult = await keys.ratchetKey(keyIndex, false);

						ratchetedKeySet = await deriveKeys(ratchetResult.cryptoKey, keys.keyProviderOptions);
					}

					const decryptedData = await DataCryptor.decrypt(data, iv, keys, keyIndex, initialMaterial, {
						ratchetCount: ratchetOpts.ratchetCount + 1,
						encryptionKey: ratchetedKeySet?.encryptionKey,
					});

					if (decryptedData && ratchetedKeySet) {
						if ((initialMaterial ?? keySet) === keys.getKeySet(keyIndex)) {
							keys.setKeySet(ratchetedKeySet, keyIndex, ratchetResult);
							keys.setCurrentKeyIndex(keyIndex);
						}
					}
					return decryptedData;
				} else {
					workerLogger.warn('DataCryptor: maximum ratchet attempts exceeded');
					throw new CryptorError(
						`DataCryptor: valid key missing for participant ${keys.participantIdentity}`,
						CryptorErrorReason.InvalidKey,
						keys.participantIdentity,
					);
				}
			} else {
				throw new CryptorError(
					`DataCryptor: Decryption failed: ${getErrorDescription(error, 'decryption')}`,
					CryptorErrorReason.InvalidKey,
					keys.participantIdentity,
				);
			}
		}
	}
}
