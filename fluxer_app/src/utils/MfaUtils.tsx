/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

const getRandomBytes = (size = 10) => crypto.getRandomValues(new Uint8Array(size));

const encodeTotpKey = (bin: Uint8Array) => {
	const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
	let bits = '';
	for (const byte of bin) {
		bits += byte.toString(2).padStart(8, '0');
	}

	let base32 = '';
	for (let i = 0; i < bits.length; i += 5) {
		const chunk = bits.substring(i, i + 5).padEnd(5, '0');
		base32 += alphabet[Number.parseInt(chunk, 2)];
	}

	return base32
		.toLowerCase()
		.replace(/(.{4})/g, '$1 ')
		.trim();
};

export const generateTotpSecret = () => encodeTotpKey(getRandomBytes());

export const encodeTotpSecret = (secret: string) => secret.replace(/[\s._-]+/g, '').toUpperCase();

export const encodeTotpSecretAsURL = (accountName: string, secret: string, issuer = 'Fluxer') =>
	`otpauth://totp/${encodeURIComponent(issuer)}:${encodeURIComponent(accountName)}\
?secret=${encodeTotpSecret(secret)}\
&issuer=${encodeURIComponent(issuer)}`;
