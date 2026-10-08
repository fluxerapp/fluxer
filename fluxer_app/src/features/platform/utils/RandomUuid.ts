// SPDX-License-Identifier: AGPL-3.0-or-later

const HEX = Array.from({length: 256}, (_, byte) => byte.toString(16).padStart(2, '0'));

function fillRandomBytes(bytes: Uint8Array<ArrayBuffer>): void {
	const cryptoApi = globalThis.crypto as Partial<Crypto> | undefined;
	if (typeof cryptoApi?.getRandomValues === 'function') {
		cryptoApi.getRandomValues(bytes);
		return;
	}
	for (let index = 0; index < bytes.length; index++) {
		bytes[index] = Math.floor(Math.random() * 256);
	}
}

export function randomUuid(): string {
	const cryptoApi = globalThis.crypto as Partial<Crypto> | undefined;
	if (typeof cryptoApi?.randomUUID === 'function') {
		return cryptoApi.randomUUID();
	}
	const bytes = new Uint8Array(16);
	fillRandomBytes(bytes);
	bytes[6] = (bytes[6]! & 0x0f) | 0x40;
	bytes[8] = (bytes[8]! & 0x3f) | 0x80;
	let uuid = '';
	for (let index = 0; index < 16; index++) {
		if (index === 4 || index === 6 || index === 8 || index === 10) uuid += '-';
		uuid += HEX[bytes[index]!];
	}
	return uuid;
}
