// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';

const TOTP_ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
const getRandomBytes = (length = 20): Uint8Array => {
	const bytes = new Uint8Array(length);
	crypto.getRandomValues(bytes);
	return bytes;
};
const encodeTotpKey = (bytes: Uint8Array): string => {
	let bits = 0;
	let value = 0;
	let output = '';
	for (const byte of bytes) {
		value = (value << 8) | byte;
		bits += 8;
		while (bits >= 5) {
			output += TOTP_ALPHABET[(value >>> (bits - 5)) & 31];
			bits -= 5;
		}
	}
	if (bits > 0) {
		output += TOTP_ALPHABET[(value << (5 - bits)) & 31];
	}
	return output;
};

export function generateTotpSecret() {
	return encodeTotpKey(getRandomBytes());
}

export function encodeTotpSecret(secret: string) {
	return secret.replace(/[\s._-]+/g, '').toUpperCase();
}

export function encodeTotpSecretAsURL(accountName: string, secret: string, issuer = RuntimeConfig.productName) {
	const url = new URL('otpauth://totp');
	url.pathname = `/${encodeURIComponent(issuer)}:${encodeURIComponent(accountName)}`;
	url.searchParams.set('secret', encodeTotpSecret(secret));
	url.searchParams.set('issuer', issuer);
	return url.toString();
}
