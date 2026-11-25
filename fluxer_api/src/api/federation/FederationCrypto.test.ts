// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateKeyPairSync} from 'node:crypto';
import {
	deriveFederationKeyId,
	exportFederationPrivateKey,
	exportFederationPublicKey,
	generateFederationKeyPair,
	importFederationPrivateKey,
	importFederationPublicKey,
	sha256Hex,
	sha256Tagged,
	signFederationBytes,
	verifyFederationBytes,
} from '@app/api/federation/FederationCrypto';
import {describe, expect, it} from 'vitest';

const MESSAGE = Buffer.from('federated message bytes', 'utf8');

describe('FederationCrypto', () => {
	it('signs and verifies bytes with Ed25519', () => {
		const instance = generateFederationKeyPair();
		const signature = signFederationBytes(MESSAGE, instance.privateKey);

		expect(signature.startsWith('ed25519:')).toBe(true);
		expect(verifyFederationBytes(MESSAGE, signature, instance.publicKey)).toBe(true);
		expect(verifyFederationBytes(Buffer.from('federated message bytez', 'utf8'), signature, instance.publicKey)).toBe(
			false,
		);
	});

	it('rejects signatures from another instance', () => {
		const instanceA = generateFederationKeyPair();
		const instanceB = generateFederationKeyPair();
		const signature = signFederationBytes(MESSAGE, instanceA.privateKey);

		expect(verifyFederationBytes(MESSAGE, signature, instanceB.publicKey)).toBe(false);
	});

	it('rejects malformed and truncated signatures', () => {
		const instance = generateFederationKeyPair();

		expect(verifyFederationBytes(MESSAGE, 'not-a-signature', instance.publicKey)).toBe(false);
		expect(verifyFederationBytes(MESSAGE, 'ed25519:', instance.publicKey)).toBe(false);
		expect(verifyFederationBytes(MESSAGE, 'ed25519:AAAA', instance.publicKey)).toBe(false);
	});

	it('round-trips public keys as PEM and raw base64url', () => {
		const instance = generateFederationKeyPair();
		const exported = exportFederationPublicKey(instance.publicKey);

		const fromPem = importFederationPublicKey(exported.pem);
		const fromRaw = importFederationPublicKey(exported.base64Url);
		const signature = signFederationBytes(MESSAGE, instance.privateKey);

		expect(exportFederationPublicKey(fromPem).base64Url).toBe(exported.base64Url);
		expect(exportFederationPublicKey(fromRaw).base64Url).toBe(exported.base64Url);
		expect(verifyFederationBytes(MESSAGE, signature, fromPem)).toBe(true);
		expect(verifyFederationBytes(MESSAGE, signature, fromRaw)).toBe(true);
	});

	it('round-trips private keys as PEM and raw base64url seeds', () => {
		const instance = generateFederationKeyPair();
		const exported = exportFederationPrivateKey(instance.privateKey);

		const fromPem = importFederationPrivateKey(exported.pem);
		const fromRaw = importFederationPrivateKey(exported.base64Url);
		const signature = signFederationBytes(MESSAGE, fromPem);

		expect(exportFederationPrivateKey(fromPem).base64Url).toBe(exported.base64Url);
		expect(exportFederationPrivateKey(fromRaw).base64Url).toBe(exported.base64Url);
		expect(verifyFederationBytes(MESSAGE, signature, instance.publicKey)).toBe(true);
	});

	it('derives stable and distinct key ids', () => {
		const instance = generateFederationKeyPair();
		const other = generateFederationKeyPair();

		expect(deriveFederationKeyId(instance.publicKey)).toBe(instance.keyId);
		expect(deriveFederationKeyId(instance.publicKey)).toBe(deriveFederationKeyId(instance.publicKey));
		expect(deriveFederationKeyId(other.publicKey)).not.toBe(instance.keyId);
		expect(instance.keyId.startsWith('ed25519-')).toBe(true);
	});

	it('rejects keys that are not Ed25519', () => {
		const {publicKey, privateKey} = generateKeyPairSync('rsa', {modulusLength: 2048});

		expect(() => exportFederationPublicKey(publicKey)).toThrow(/Ed25519/);
		expect(() => exportFederationPrivateKey(privateKey)).toThrow(/Ed25519/);
		expect(() => importFederationPublicKey('AAAA')).toThrow(/32-byte base64url/);
	});

	it('hashes deterministically', () => {
		expect(sha256Hex('abc')).toBe('ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
		expect(sha256Tagged('abc')).toBe('sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
		expect(sha256Hex('abc')).toBe(sha256Hex(Buffer.from('abc', 'utf8')));
	});
});
