// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {type DataPacket, EncryptedPacketPayload} from '@livekit/protocol';
import type {NonSharedUint8Array} from '../type-polyfills/non-shared-typed-arrays.ts';
import {ENCRYPTION_ALGORITHM} from './constants.ts';
import type {KeyProviderOptions} from './types.ts';

export function isE2EESupported() {
	return isInsertableStreamSupported() || isScriptTransformSupported();
}

export function isScriptTransformSupported() {
	return typeof window !== 'undefined' && typeof window.RTCRtpScriptTransform !== 'undefined';
}

export function isInsertableStreamSupported() {
	return (
		typeof window !== 'undefined' &&
		typeof window.RTCRtpSender !== 'undefined' &&
		typeof window.RTCRtpSender.prototype.createEncodedStreams !== 'undefined'
	);
}

export function isVideoFrame(frame: RTCEncodedAudioFrame | RTCEncodedVideoFrame): frame is RTCEncodedVideoFrame {
	return 'type' in frame;
}

export async function importKey(
	keyBytes: NonSharedUint8Array | ArrayBuffer,
	algorithm: string | {name: string} = {name: ENCRYPTION_ALGORITHM},
	usage: 'derive' | 'encrypt' = 'encrypt',
) {
	return crypto.subtle.importKey(
		'raw',
		keyBytes,
		algorithm,
		false,
		usage === 'derive' ? ['deriveBits', 'deriveKey'] : ['encrypt', 'decrypt'],
	);
}

export async function createKeyMaterialFromString(password: string) {
	const enc = new TextEncoder();

	const keyMaterial = await crypto.subtle.importKey(
		'raw',
		enc.encode(password),
		{
			name: 'PBKDF2',
		},
		false,
		['deriveBits', 'deriveKey'],
	);

	return keyMaterial;
}

export async function createKeyMaterialFromBuffer(cryptoBuffer: ArrayBuffer) {
	const keyMaterial = await crypto.subtle.importKey('raw', cryptoBuffer, 'HKDF', false, ['deriveBits', 'deriveKey']);

	return keyMaterial;
}

function getAlgoOptions(algorithmName: string, salt: string) {
	const textEncoder = new TextEncoder();
	const encodedSalt = textEncoder.encode(salt);
	switch (algorithmName) {
		case 'HKDF':
			return {
				name: 'HKDF',
				salt: encodedSalt,
				hash: 'SHA-256',
				info: new ArrayBuffer(128),
			};
		case 'PBKDF2': {
			return {
				name: 'PBKDF2',
				salt: encodedSalt,
				hash: 'SHA-256',
				iterations: 100000,
			};
		}
		default:
			throw new Error(`algorithm ${algorithmName} is currently unsupported`);
	}
}

export async function deriveKeys(material: CryptoKey, options: KeyProviderOptions) {
	const algorithmOptions = getAlgoOptions(material.algorithm.name, options.ratchetSalt);

	const encryptionKey = await crypto.subtle.deriveKey(
		algorithmOptions,
		material,
		{
			name: ENCRYPTION_ALGORITHM,
			length: options.keySize,
		},
		false,
		['encrypt', 'decrypt'],
	);

	return {material, encryptionKey};
}

export function createE2EEKey(): NonSharedUint8Array {
	return window.crypto.getRandomValues(new Uint8Array(32));
}

export async function ratchet(material: CryptoKey, salt: string): Promise<ArrayBuffer> {
	const algorithmOptions = getAlgoOptions(material.algorithm.name, salt);

	return crypto.subtle.deriveBits(algorithmOptions, material, 256);
}

export function needsRbspUnescaping(frameData: NonSharedUint8Array) {
	for (let i = 0; i < frameData.length - 3; i++) {
		if (frameData[i] === 0 && frameData[i + 1] === 0 && frameData[i + 2] === 3) return true;
	}
	return false;
}
export function parseRbsp(stream: NonSharedUint8Array): NonSharedUint8Array {
	const dataOut = new Uint8Array(stream.length);
	let writePos = 0;
	const length = stream.length;
	for (let i = 0; i < length; ) {
		if (length - i >= 3 && !stream[i] && !stream[i + 1] && stream[i + 2] === 3) {
			dataOut[writePos++] = stream[i++];
			dataOut[writePos++] = stream[i++];
			i++;
		} else {
			dataOut[writePos++] = stream[i++];
		}
	}
	return dataOut.subarray(0, writePos);
}

const kZerosInStartSequence = 2;
const kEmulationByte = 3;
export function writeRbsp(data_in: NonSharedUint8Array): NonSharedUint8Array {
	const dataOut = new Uint8Array(data_in.length * 2);
	let writePos = 0;
	let numConsecutiveZeros = 0;
	for (let i = 0; i < data_in.length; ++i) {
		const byte = data_in[i];
		if (byte <= kEmulationByte && numConsecutiveZeros >= kZerosInStartSequence) {
			dataOut[writePos++] = kEmulationByte;
			numConsecutiveZeros = 0;
		}
		dataOut[writePos++] = byte;
		if (byte === 0) {
			++numConsecutiveZeros;
		} else {
			numConsecutiveZeros = 0;
		}
	}
	return dataOut.slice(0, writePos);
}

export function asEncryptablePacket(packet: DataPacket): EncryptedPacketPayload | undefined {
	if (
		packet.value?.case !== 'sipDtmf' &&
		packet.value?.case !== 'metrics' &&
		packet.value?.case !== 'speaker' &&
		packet.value?.case !== 'transcription' &&
		packet.value?.case !== 'encryptedPacket'
	) {
		return new EncryptedPacketPayload({
			value: packet.value,
		});
	}
	return undefined;
}
