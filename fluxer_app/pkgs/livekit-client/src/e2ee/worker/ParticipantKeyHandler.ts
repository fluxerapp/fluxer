// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {EventEmitter} from 'events';
import type TypedEventEmitter from 'typed-emitter';
import {workerLogger} from '../../logger.ts';
import {KeyHandlerEvent, type ParticipantKeyHandlerCallbacks} from '../events.ts';
import type {KeyProviderOptions, KeySet, RatchetResult} from '../types.ts';
import {deriveKeys, importKey, ratchet} from '../utils.ts';

export class ParticipantKeyHandler extends (EventEmitter as new () => TypedEventEmitter<ParticipantKeyHandlerCallbacks>) {
	private currentKeyIndex: number;

	private cryptoKeyRing: Array<KeySet | undefined>;

	private decryptionFailureCounts: Array<number>;

	private ratchetPromiseMap: Map<number, Promise<RatchetResult>>;

	readonly participantIdentity: string;

	readonly keyProviderOptions: KeyProviderOptions;

	get hasValidKey(): boolean {
		return !this.hasInvalidKeyAtIndex(this.currentKeyIndex);
	}

	constructor(participantIdentity: string, keyProviderOptions: KeyProviderOptions) {
		super();
		this.currentKeyIndex = 0;
		if (keyProviderOptions.keyringSize < 1 || keyProviderOptions.keyringSize > 256) {
			throw new TypeError('Keyring size needs to be between 1 and 256');
		}
		this.cryptoKeyRing = new Array(keyProviderOptions.keyringSize).fill(undefined);
		this.decryptionFailureCounts = new Array(keyProviderOptions.keyringSize).fill(0);
		this.keyProviderOptions = keyProviderOptions;
		this.ratchetPromiseMap = new Map();
		this.participantIdentity = participantIdentity;
	}

	hasInvalidKeyAtIndex(keyIndex: number): boolean {
		return (
			this.keyProviderOptions.failureTolerance >= 0 &&
			this.decryptionFailureCounts[keyIndex] > this.keyProviderOptions.failureTolerance
		);
	}

	decryptionFailure(keyIndex: number = this.currentKeyIndex): void {
		if (this.keyProviderOptions.failureTolerance < 0) {
			return;
		}

		this.decryptionFailureCounts[keyIndex] += 1;

		if (this.decryptionFailureCounts[keyIndex] > this.keyProviderOptions.failureTolerance) {
			workerLogger.warn(`key for ${this.participantIdentity} at index ${keyIndex} is being marked as invalid`);
		}
	}

	decryptionSuccess(keyIndex: number = this.currentKeyIndex): void {
		this.resetKeyStatus(keyIndex);
	}

	resetKeyStatus(keyIndex?: number): void {
		if (keyIndex === undefined) {
			this.decryptionFailureCounts.fill(0);
		} else {
			this.decryptionFailureCounts[keyIndex] = 0;
		}
	}

	ratchetKey(keyIndex?: number, setKey = true): Promise<RatchetResult> {
		const currentKeyIndex = keyIndex ?? this.getCurrentKeyIndex();

		const existingPromise = this.ratchetPromiseMap.get(currentKeyIndex);
		if (typeof existingPromise !== 'undefined') {
			return existingPromise;
		}
		const ratchetPromise = (async (): Promise<RatchetResult> => {
			try {
				const keySet = this.getKeySet(currentKeyIndex);
				if (!keySet) {
					throw new TypeError(`Cannot ratchet key without a valid keyset of participant ${this.participantIdentity}`);
				}
				const currentMaterial = keySet.material;
				const chainKey = await ratchet(currentMaterial, this.keyProviderOptions.ratchetSalt);
				const newMaterial = await importKey(chainKey, currentMaterial.algorithm.name, 'derive');
				const ratchetResult: RatchetResult = {
					chainKey,
					cryptoKey: newMaterial,
				};
				if (setKey) {
					await this.setKeyFromMaterial(newMaterial, currentKeyIndex, ratchetResult);
				}
				return ratchetResult;
			} finally {
				this.ratchetPromiseMap.delete(currentKeyIndex);
			}
		})();
		this.ratchetPromiseMap.set(currentKeyIndex, ratchetPromise);
		return ratchetPromise;
	}

	async setKey(material: CryptoKey, keyIndex = 0, updateCurrentKeyIndex = true) {
		await this.setKeyFromMaterial(material, keyIndex, null, updateCurrentKeyIndex);
		if (updateCurrentKeyIndex) {
			this.resetKeyStatus(keyIndex);
		}
	}

	async setKeyFromMaterial(
		material: CryptoKey,
		keyIndex: number,
		ratchetedResult: RatchetResult | null = null,
		updateCurrentKeyIndex = true,
	) {
		const keySet = await deriveKeys(material, this.keyProviderOptions);
		const newIndex = keyIndex >= 0 ? keyIndex % this.cryptoKeyRing.length : this.currentKeyIndex;
		workerLogger.debug(`setting new key with index ${keyIndex}`, {
			usage: material.usages,
			algorithm: material.algorithm,
			ratchetSalt: this.keyProviderOptions.ratchetSalt,
		});
		this.setKeySet(keySet, newIndex, ratchetedResult);
		if (newIndex >= 0 && updateCurrentKeyIndex) this.currentKeyIndex = newIndex;
	}

	setKeySet(keySet: KeySet, keyIndex: number, ratchetedResult: RatchetResult | null = null) {
		this.cryptoKeyRing[keyIndex % this.cryptoKeyRing.length] = keySet;

		if (ratchetedResult) {
			this.emit(KeyHandlerEvent.KeyRatcheted, ratchetedResult, this.participantIdentity, keyIndex);
		}
	}

	async setCurrentKeyIndex(index: number) {
		this.currentKeyIndex = index % this.cryptoKeyRing.length;
		this.resetKeyStatus(index);
	}

	getCurrentKeyIndex() {
		return this.currentKeyIndex;
	}

	getKeySet(keyIndex?: number) {
		return this.cryptoKeyRing[keyIndex ?? this.currentKeyIndex];
	}
}
