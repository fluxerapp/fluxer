// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createHash,
	createPrivateKey,
	createPublicKey,
	generateKeyPairSync,
	type KeyObject,
	sign,
	verify,
} from 'node:crypto';

const ED25519_SPKI_PREFIX = Buffer.from('302a300506032b6570032100', 'hex');
const ED25519_PKCS8_PREFIX = Buffer.from('302e020100300506032b657004220420', 'hex');
const ED25519_PUBLIC_KEY_BYTES = 32;
const ED25519_PRIVATE_KEY_BYTES = 32;
const ED25519_SIGNATURE_PREFIX = 'ed25519:';
const BASE64URL_PATTERN = /^[A-Za-z0-9_-]+={0,2}$/;

export interface FederationPublicKeyExport {
	pem: string;
	base64Url: string;
}

export interface FederationPrivateKeyExport {
	pem: string;
	base64Url: string;
}

export interface FederationKeyPair {
	publicKey: KeyObject;
	privateKey: KeyObject;
	publicKeyPem: string;
	privateKeyPem: string;
	keyId: string;
}

function assertEd25519PublicKey(key: KeyObject): void {
	if (key.type !== 'public' || key.asymmetricKeyType !== 'ed25519') {
		throw new Error('Federation public key must be an Ed25519 public key');
	}
}

function assertEd25519PrivateKey(key: KeyObject): void {
	if (key.type !== 'private' || key.asymmetricKeyType !== 'ed25519') {
		throw new Error('Federation private key must be an Ed25519 private key');
	}
}

function decodeBase64Url(value: string): Buffer | null {
	if (!BASE64URL_PATTERN.test(value)) {
		return null;
	}
	const decoded = Buffer.from(value, 'base64url');
	return decoded.length > 0 ? decoded : null;
}

function jwkString(key: KeyObject, field: 'x' | 'd'): string {
	const jwk = key.export({format: 'jwk'}) as Record<string, unknown>;
	const value = jwk[field];
	if (typeof value !== 'string' || value.length === 0) {
		throw new Error(`Federation Ed25519 key is missing the ${field} JWK field`);
	}
	return value;
}

export function generateFederationKeyPair(): FederationKeyPair {
	const {publicKey, privateKey} = generateKeyPairSync('ed25519');
	const publicExport = exportFederationPublicKey(publicKey);
	const privateExport = exportFederationPrivateKey(privateKey);
	return {
		publicKey,
		privateKey,
		publicKeyPem: publicExport.pem,
		privateKeyPem: privateExport.pem,
		keyId: deriveFederationKeyId(publicKey),
	};
}

export function exportFederationPublicKey(publicKey: KeyObject): FederationPublicKeyExport {
	assertEd25519PublicKey(publicKey);
	return {
		pem: publicKey.export({type: 'spki', format: 'pem'}).toString(),
		base64Url: jwkString(publicKey, 'x'),
	};
}

export function exportFederationPrivateKey(privateKey: KeyObject): FederationPrivateKeyExport {
	assertEd25519PrivateKey(privateKey);
	return {
		pem: privateKey.export({type: 'pkcs8', format: 'pem'}).toString(),
		base64Url: jwkString(privateKey, 'd'),
	};
}

export function importFederationPublicKey(value: string): KeyObject {
	const trimmed = value.trim();
	const isPem = trimmed.includes('-----BEGIN');
	const key = isPem
		? createPublicKey(trimmed)
		: (() => {
				const raw = decodeBase64Url(trimmed);
				if (!raw || raw.length !== ED25519_PUBLIC_KEY_BYTES) {
					throw new Error('Federation public key must be a PEM document or a 32-byte base64url value');
				}
				return createPublicKey({
					key: Buffer.concat([ED25519_SPKI_PREFIX, raw]),
					format: 'der',
					type: 'spki',
				});
			})();
	assertEd25519PublicKey(key);
	return key;
}

export function importFederationPrivateKey(value: string): KeyObject {
	const trimmed = value.trim();
	const isPem = trimmed.includes('-----BEGIN');
	const key = isPem
		? createPrivateKey(trimmed)
		: (() => {
				const raw = decodeBase64Url(trimmed);
				if (!raw || raw.length !== ED25519_PRIVATE_KEY_BYTES) {
					throw new Error('Federation private key must be a PEM document or a 32-byte base64url seed');
				}
				return createPrivateKey({
					key: Buffer.concat([ED25519_PKCS8_PREFIX, raw]),
					format: 'der',
					type: 'pkcs8',
				});
			})();
	assertEd25519PrivateKey(key);
	return key;
}

export function deriveFederationKeyId(publicKey: KeyObject): string {
	const raw = Buffer.from(exportFederationPublicKey(publicKey).base64Url, 'base64url');
	const digest = createHash('sha256').update(raw).digest('hex');
	return `ed25519-${digest.slice(0, 16)}`;
}

export function signFederationBytes(bytes: Uint8Array, privateKey: KeyObject): string {
	assertEd25519PrivateKey(privateKey);
	const signature = sign(null, Buffer.from(bytes), privateKey);
	return `${ED25519_SIGNATURE_PREFIX}${signature.toString('base64url')}`;
}

export function verifyFederationBytes(bytes: Uint8Array, signature: string, publicKey: KeyObject): boolean {
	try {
		assertEd25519PublicKey(publicKey);
		if (!signature.startsWith(ED25519_SIGNATURE_PREFIX)) {
			return false;
		}
		const raw = decodeBase64Url(signature.slice(ED25519_SIGNATURE_PREFIX.length));
		if (raw?.length !== 64) {
			return false;
		}
		return verify(null, Buffer.from(bytes), publicKey, raw);
	} catch {
		return false;
	}
}

export function sha256Hex(value: string | Uint8Array): string {
	return createHash('sha256').update(value).digest('hex');
}

export function sha256Tagged(value: string | Uint8Array): string {
	return `sha256:${sha256Hex(value)}`;
}
