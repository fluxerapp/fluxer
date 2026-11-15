// SPDX-License-Identifier: AGPL-3.0-or-later

import {pbkdf2} from '@noble/hashes/pbkdf2.js';
import {sha256, sha384, sha512} from '@noble/hashes/sha2.js';
import {deriveKey as deriveKeyWithWebCrypto} from 'altcha-lib/algorithms/web/pbkdf2';
import type {DeriveKeyFunction} from 'altcha-lib/types';

const PBKDF2_HASHES = {
	'PBKDF2/SHA-256': sha256,
	'PBKDF2/SHA-384': sha384,
	'PBKDF2/SHA-512': sha512,
} as const;

export const deriveKeyInJavaScript: DeriveKeyFunction = async (parameters, salt, password) => {
	const {algorithm, cost, keyLength = 32} = parameters;
	const hash = PBKDF2_HASHES[algorithm as keyof typeof PBKDF2_HASHES];
	if (hash === undefined) {
		throw new Error(`Unsupported algorithm: ${String(algorithm)}`);
	}
	return {parameters: {}, derivedKey: pbkdf2(hash, password, salt, {c: cost, dkLen: keyLength})};
};

export const deriveKey: DeriveKeyFunction =
	globalThis.crypto?.subtle === undefined ? deriveKeyInJavaScript : deriveKeyWithWebCrypto;
