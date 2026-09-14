// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createHash,
	createHmac,
	createPrivateKey,
	createPublicKey,
	createSign,
	generateKeyPairSync,
	randomBytes,
} from 'node:crypto';
import {decode as base32Decode, encode as base32Encode} from 'hi-base32';

export interface WebAuthnDevice {
	privateKey: string;
	publicKey: string;
	credentialId: Buffer;
	userHandle: Buffer;
	rpId: string;
	origin: string;
	signCount: number;
}

export interface WebAuthnRegistrationOptions {
	challenge: string;
	rp: {
		id: string;
		name: string;
	};
	user: {
		id: string;
		name: string;
		displayName: string;
	};
	authenticatorSelection?: {
		residentKey?: string;
		requireResidentKey?: boolean;
		userVerification?: string;
	};
}

export interface WebAuthnAuthenticationOptions {
	challenge: string;
	rpId: string;
	allowCredentials?: Array<{
		id: string;
		type: string;
	}>;
	userVerification: string;
}

export interface WebAuthnCredentialMetadata {
	id: string;
	name: string;
}

interface AuthenticatorAttestationResponse {
	clientDataJSON: string;
	attestationObject: string;
	transports: Array<string>;
}

interface AuthenticatorAssertionResponse {
	clientDataJSON: string;
	authenticatorData: string;
	signature: string;
	userHandle: string;
}

interface WebAuthnRegistrationResponse {
	id: string;
	rawId: string;
	type: string;
	clientExtensionResults: Record<string, unknown>;
	response: AuthenticatorAttestationResponse;
}

interface WebAuthnAuthenticationResponse {
	id: string;
	rawId: string;
	type: string;
	clientExtensionResults: Record<string, unknown>;
	response: AuthenticatorAssertionResponse;
}

export function createTotpSecret(): string {
	const buf = randomBytes(20);
	return base32Encode(buf).replace(/=/g, '');
}

export function generateTotpCode(secret: string, time = Date.now()): string {
	const key = Buffer.from(base32Decode.asBytes(secret.toUpperCase()));
	const epoch = Math.floor(time / 1000);
	const counter = Math.floor(epoch / 30);
	const counterBuf = Buffer.alloc(8);
	counterBuf.writeBigUInt64BE(BigInt(counter));
	const hmac = createHmac('sha1', key);
	hmac.update(counterBuf);
	const hash = hmac.digest();
	const offset = hash[hash.length - 1] & 0x0f;
	const binary =
		((hash[offset] & 0x7f) << 24) |
		((hash[offset + 1] & 0xff) << 16) |
		((hash[offset + 2] & 0xff) << 8) |
		(hash[offset + 3] & 0xff);
	const otp = binary % 1000000;
	return otp.toString().padStart(6, '0');
}

function resolveWebAuthnOrigin(): {
	rpId: string;
	origin: string;
} {
	const origin = process.env.FLUXER_WEBAPP_ORIGIN || 'http://localhost:8088';
	try {
		const url = new URL(origin);
		return {rpId: url.hostname, origin};
	} catch {
		return {rpId: 'localhost', origin};
	}
}

export function createWebAuthnDevice(): WebAuthnDevice {
	const {privateKey, publicKey} = generateKeyPairSync('ec', {
		namedCurve: 'P-256',
	});
	const credentialId = randomBytes(32);
	const {rpId, origin} = resolveWebAuthnOrigin();
	return {
		privateKey: privateKey.export({type: 'pkcs8', format: 'der'}).toString('base64'),
		publicKey: publicKey.export({type: 'spki', format: 'der'}).toString('base64'),
		credentialId,
		userHandle: Buffer.alloc(0),
		rpId,
		origin,
		signCount: 0,
	};
}

function encodeBase64URL(data: Buffer): string {
	return data.toString('base64url');
}

export function decodeBase64URL(value: string): Buffer {
	try {
		return Buffer.from(value, 'base64url');
	} catch {
		return Buffer.from(value, 'base64');
	}
}

function padCoordinate(bytes: Buffer): Buffer {
	if (bytes.length === 32) return bytes;
	const padded = Buffer.alloc(32);
	const offset = 32 - bytes.length;
	bytes.copy(padded, offset);
	return padded;
}

function encodeCBOR(value: unknown): Uint8Array {
	if (value instanceof Map) {
		const items: Array<Uint8Array> = [];
		for (const [k, v] of value.entries()) {
			items.push(encodeCBOR(k));
			items.push(encodeCBOR(v));
		}
		return encodeCBORMap(items.length, Buffer.concat(items));
	}
	if (typeof value === 'number') {
		if (value >= 0 && value <= 23) {
			return new Uint8Array([value]);
		}
		if (value >= 24 && value <= 255) {
			return new Uint8Array([0x18, value]);
		}
		if (value >= 256 && value <= 65535) {
			const buf = Buffer.alloc(2);
			buf.writeUInt16BE(value);
			return new Uint8Array([0x19, ...buf]);
		}
		if (value < 0 && value >= -24) {
			return new Uint8Array([0x20 + Math.abs(value) - 1]);
		}
	}
	if (typeof value === 'string') {
		const buf = Buffer.from(value);
		if (buf.length <= 23) {
			return new Uint8Array([0x60 + buf.length, ...buf]);
		}
		if (buf.length <= 255) {
			return new Uint8Array([0x78, buf.length, ...buf]);
		}
	}
	if (Array.isArray(value)) {
		const items: Array<Uint8Array> = value.map(encodeCBOR);
		return encodeCBORArray(items.length, Buffer.concat(items));
	}
	if (Buffer.isBuffer(value)) {
		if (value.length <= 23) {
			return new Uint8Array([0x40 + value.length, ...value]);
		}
		if (value.length <= 255) {
			return new Uint8Array([0x58, value.length, ...value]);
		}
	}
	if (value === null) {
		return new Uint8Array([0xf6]);
	}
	if (typeof value === 'object' && value !== null) {
		return new Uint8Array([0xa0]);
	}
	throw new Error(`Unsupported CBOR value: ${typeof value}`);
}

function encodeCBORMap(length: number, data: Buffer): Uint8Array {
	if (length <= 23) {
		return new Uint8Array([0xa0 + length, ...data]);
	}
	if (length <= 255) {
		return new Uint8Array([0xb8, length, ...data]);
	}
	throw new Error('Map too large');
}

function encodeCBORArray(length: number, data: Buffer): Uint8Array {
	if (length <= 23) {
		return new Uint8Array([0x80 + length, ...data]);
	}
	if (length <= 255) {
		return new Uint8Array([0x98, length, ...data]);
	}
	throw new Error('Array too large');
}

function buildAttestationObject(authData: Buffer): Buffer {
	const payload = new Map<number, unknown>([
		[3, 'none'],
		[2, new Map()],
		[1, authData],
	]);
	const cbor = encodeCBOR(payload);
	return Buffer.from(cbor);
}

function buildRegistrationAuthData(device: WebAuthnDevice): Buffer {
	const rpHash = createHash('sha256').update(device.rpId).digest();
	const flags = 0x01 | 0x04 | 0x40;
	const privateKeyObj = createPrivateKey({
		key: Buffer.from(device.privateKey, 'base64'),
		format: 'der',
		type: 'pkcs8',
	});
	const publicKeyObj = createPublicKey(privateKeyObj);
	const pubKeyDer = publicKeyObj.export({type: 'spki', format: 'der'});
	const pubKeyBuf = Buffer.from(pubKeyDer);
	let x: Buffer, y: Buffer;
	const asn1Offset = pubKeyBuf.indexOf(Buffer.from([0x30, 0x59, 0x30, 0x13]));
	if (asn1Offset > 0 && pubKeyBuf.length >= asn1Offset + 68) {
		x = pubKeyBuf.subarray(asn1Offset + 4 + 3, asn1Offset + 4 + 35);
		y = pubKeyBuf.subarray(asn1Offset + 4 + 36, asn1Offset + 4 + 68);
	} else {
		x = randomBytes(32);
		y = randomBytes(32);
	}
	const paddedX = padCoordinate(x);
	const paddedY = padCoordinate(y);
	const key = new Map<number, unknown>([
		[1, 2],
		[3, -7],
		[-1, 1],
		[-2, Array.from(paddedX)],
		[-3, Array.from(paddedY)],
	]);
	const coseKey = encodeCBOR(key);
	const buf = Buffer.concat([
		rpHash,
		Buffer.from([flags]),
		Buffer.from([
			(device.signCount >> 24) & 0xff,
			(device.signCount >> 16) & 0xff,
			(device.signCount >> 8) & 0xff,
			device.signCount & 0xff,
		]),
		Buffer.alloc(16),
		Buffer.from([(device.credentialId.length >> 8) & 0xff, device.credentialId.length & 0xff]),
		device.credentialId,
		Buffer.from(coseKey),
	]);
	return buf;
}

function buildAssertionAuthData(device: WebAuthnDevice, includeUV = true): Buffer {
	const rpHash = createHash('sha256').update(device.rpId).digest();
	const flags = includeUV ? 0x01 | 0x04 : 0x01;
	device.signCount++;
	const buf = Buffer.concat([
		rpHash,
		Buffer.from([flags]),
		Buffer.from([
			(device.signCount >> 24) & 0xff,
			(device.signCount >> 16) & 0xff,
			(device.signCount >> 8) & 0xff,
			device.signCount & 0xff,
		]),
	]);
	return buf;
}

function signWithPrivateKey(device: WebAuthnDevice, data: Buffer): Buffer {
	const sign = createSign('SHA256');
	sign.update(data);
	sign.end();
	const privateKeyObj = createPrivateKey({
		key: Buffer.from(device.privateKey, 'base64'),
		format: 'der',
		type: 'pkcs8',
	});
	return Buffer.from(sign.sign(privateKeyObj));
}

export function createRegistrationResponse(
	device: WebAuthnDevice,
	options: WebAuthnRegistrationOptions,
	_name: string,
): WebAuthnRegistrationResponse {
	const challenge = decodeBase64URL(options.challenge);
	device.userHandle = decodeBase64URL(options.user.id);
	if (options.rp.id) {
		device.rpId = options.rp.id;
	}
	const clientData = {
		type: 'webauthn.create',
		challenge: encodeBase64URL(challenge),
		origin: device.origin,
		crossOrigin: false,
	};
	const clientDataJSON = Buffer.from(JSON.stringify(clientData));
	const authData = buildRegistrationAuthData(device);
	const attestationObject = buildAttestationObject(authData);
	return {
		id: encodeBase64URL(device.credentialId),
		rawId: encodeBase64URL(device.credentialId),
		type: 'public-key',
		clientExtensionResults: {},
		response: {
			clientDataJSON: encodeBase64URL(clientDataJSON),
			attestationObject: encodeBase64URL(attestationObject),
			transports: ['internal'],
		},
	};
}

export function createAuthenticationResponse(
	device: WebAuthnDevice,
	options: WebAuthnAuthenticationOptions,
): WebAuthnAuthenticationResponse {
	const challenge = decodeBase64URL(options.challenge);
	if (options.rpId) {
		device.rpId = options.rpId;
	}
	const clientData = {
		type: 'webauthn.get',
		challenge: encodeBase64URL(challenge),
		origin: device.origin,
		crossOrigin: false,
	};
	const clientDataJSON = Buffer.from(JSON.stringify(clientData));
	const authData = buildAssertionAuthData(device, true);
	const clientDataHash = createHash('sha256').update(clientDataJSON).digest();
	const sigInput = Buffer.concat([authData, clientDataHash]);
	const signature = signWithPrivateKey(device, sigInput);
	return {
		id: encodeBase64URL(device.credentialId),
		rawId: encodeBase64URL(device.credentialId),
		type: 'public-key',
		clientExtensionResults: {},
		response: {
			clientDataJSON: encodeBase64URL(clientDataJSON),
			authenticatorData: encodeBase64URL(authData),
			signature: encodeBase64URL(signature),
			userHandle: encodeBase64URL(device.userHandle),
		},
	};
}

export function createAuthenticationResponseWithoutUV(
	device: WebAuthnDevice,
	options: WebAuthnAuthenticationOptions,
): WebAuthnAuthenticationResponse {
	const challenge = decodeBase64URL(options.challenge);
	if (options.rpId) {
		device.rpId = options.rpId;
	}
	const clientData = {
		type: 'webauthn.get',
		challenge: encodeBase64URL(challenge),
		origin: device.origin,
		crossOrigin: false,
	};
	const clientDataJSON = Buffer.from(JSON.stringify(clientData));
	const authData = buildAssertionAuthData(device, false);
	const clientDataHash = createHash('sha256').update(clientDataJSON).digest();
	const sigInput = Buffer.concat([authData, clientDataHash]);
	const signature = signWithPrivateKey(device, sigInput);
	return {
		id: encodeBase64URL(device.credentialId),
		rawId: encodeBase64URL(device.credentialId),
		type: 'public-key',
		clientExtensionResults: {},
		response: {
			clientDataJSON: encodeBase64URL(clientDataJSON),
			authenticatorData: encodeBase64URL(authData),
			signature: encodeBase64URL(signature),
			userHandle: encodeBase64URL(device.userHandle),
		},
	};
}
