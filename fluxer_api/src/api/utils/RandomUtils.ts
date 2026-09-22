// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import crypto from 'node:crypto';

const RANDOM_STRING_ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
const RANDOM_DIGIT_ALPHABET = '0123456789';

function randomFromAlphabet(length: number, alphabet: string): string {
	assert(Number.isSafeInteger(length) && length >= 0, 'Random string length must be a nonnegative safe integer');
	const alphabetLength = alphabet.length;
	const rangeSize = 256 - (256 % alphabetLength);
	const randomBytes = new Uint8Array(length * 2);
	crypto.getRandomValues(randomBytes);
	let result = '';
	let byteIndex = 0;
	while (result.length < length) {
		if (byteIndex >= randomBytes.length) {
			crypto.getRandomValues(randomBytes);
			byteIndex = 0;
		}
		const randomByte = randomBytes[byteIndex++]!;
		if (randomByte >= rangeSize) continue;
		result += alphabet.charAt(randomByte % alphabetLength);
	}
	return result;
}

export function randomNumericCode(length: number): string {
	return randomFromAlphabet(length, RANDOM_DIGIT_ALPHABET);
}

export function randomString(length: number): string {
	return randomFromAlphabet(length, RANDOM_STRING_ALPHABET);
}
