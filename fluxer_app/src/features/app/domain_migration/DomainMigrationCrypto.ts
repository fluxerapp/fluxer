// SPDX-License-Identifier: AGPL-3.0-or-later

const IV_BYTES = 12;
const KEY_BYTES = 32;
const GZIP_MAGIC_FIRST = 0x1f;
const GZIP_MAGIC_SECOND = 0x8b;
const BASE64_CHUNK = 0x8000;

export function bytesToBase64Url(bytes: Uint8Array): string {
	let binary = '';
	for (let offset = 0; offset < bytes.length; offset += BASE64_CHUNK) {
		binary += String.fromCharCode(...bytes.subarray(offset, offset + BASE64_CHUNK));
	}
	return btoa(binary).replace(/\+/gu, '-').replace(/\//gu, '_').replace(/=+$/u, '');
}

export function base64UrlToBytes(value: string): Uint8Array<ArrayBuffer> {
	const base64 = value.replace(/-/gu, '+').replace(/_/gu, '/');
	const binary = atob(base64.padEnd(Math.ceil(base64.length / 4) * 4, '='));
	const bytes = new Uint8Array(binary.length);
	for (let index = 0; index < binary.length; index++) {
		bytes[index] = binary.charCodeAt(index);
	}
	return bytes;
}

export function randomBase64Url(byteLength: number): string {
	return bytesToBase64Url(crypto.getRandomValues(new Uint8Array(byteLength)));
}

export async function sha256Hex(value: string): Promise<string> {
	const digest = new Uint8Array(await crypto.subtle.digest('SHA-256', new TextEncoder().encode(value)));
	return Array.from(digest, (byte) => byte.toString(16).padStart(2, '0')).join('');
}

async function pipeBytes(
	bytes: Uint8Array<ArrayBuffer>,
	transform: GenericTransformStream,
): Promise<Uint8Array<ArrayBuffer>> {
	const stream = new Blob([bytes]).stream().pipeThrough(transform as TransformStream<Uint8Array, Uint8Array>);
	return new Uint8Array(await new Response(stream).arrayBuffer());
}

async function compress(bytes: Uint8Array<ArrayBuffer>): Promise<Uint8Array<ArrayBuffer>> {
	if (typeof CompressionStream === 'undefined') {
		return bytes;
	}
	return pipeBytes(bytes, new CompressionStream('gzip'));
}

async function decompress(bytes: Uint8Array<ArrayBuffer>): Promise<Uint8Array<ArrayBuffer>> {
	if (bytes[0] !== GZIP_MAGIC_FIRST || bytes[1] !== GZIP_MAGIC_SECOND) {
		return bytes;
	}
	if (typeof DecompressionStream === 'undefined') {
		throw new Error('DecompressionStream unavailable');
	}
	return pipeBytes(bytes, new DecompressionStream('gzip'));
}

export async function encryptDomainMigrationPayload(value: unknown): Promise<{payload: string; key: string}> {
	const plaintext = await compress(new TextEncoder().encode(JSON.stringify(value)));
	const rawKey = crypto.getRandomValues(new Uint8Array(KEY_BYTES));
	const iv = crypto.getRandomValues(new Uint8Array(IV_BYTES));
	const key = await crypto.subtle.importKey('raw', rawKey, 'AES-GCM', false, ['encrypt']);
	const ciphertext = new Uint8Array(await crypto.subtle.encrypt({name: 'AES-GCM', iv}, key, plaintext));
	const sealed = new Uint8Array(IV_BYTES + ciphertext.length);
	sealed.set(iv, 0);
	sealed.set(ciphertext, IV_BYTES);
	return {payload: bytesToBase64Url(sealed), key: bytesToBase64Url(rawKey)};
}

export async function decryptDomainMigrationPayload(payload: string, encodedKey: string): Promise<unknown> {
	const sealed = base64UrlToBytes(payload);
	const rawKey = base64UrlToBytes(encodedKey);
	if (rawKey.length !== KEY_BYTES || sealed.length <= IV_BYTES) {
		throw new Error('Malformed handoff payload');
	}
	const key = await crypto.subtle.importKey('raw', rawKey, 'AES-GCM', false, ['decrypt']);
	const plaintext = new Uint8Array(
		await crypto.subtle.decrypt({name: 'AES-GCM', iv: sealed.subarray(0, IV_BYTES)}, key, sealed.subarray(IV_BYTES)),
	);
	return JSON.parse(new TextDecoder().decode(await decompress(plaintext)));
}
