// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, createHmac, randomBytes, randomInt, timingSafeEqual} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';

const CODE_HMAC_LABEL = 'fluxer-account-code-v1';
const DEVICE_PROOF_LABEL = 'fluxer-device-proof-v1';
const DEVICE_ID_BYTES = 16;
const MAC_BYTES = 32;

const CODE_LENGTH = 6;

function deriveKey(label: string): Buffer {
	return createHmac('sha256', Config.auth.sudoModeSecret).update(label).digest();
}

export function sha256Hex(value: string): string {
	return createHash('sha256').update(value).digest('hex');
}

export function normalizeEmailKey(email: string): string {
	return sha256Hex(email.trim().toLowerCase());
}

export function generateCode(): string {
	return randomInt(0, 10 ** CODE_LENGTH)
		.toString()
		.padStart(CODE_LENGTH, '0');
}

export function generateOpaqueToken(bytes = 32): string {
	return randomBytes(bytes).toString('base64url');
}

export function hashCode(code: string, scope: string): string {
	return createHmac('sha256', deriveKey(CODE_HMAC_LABEL)).update(`${scope}:${code}`).digest('hex');
}

export function codeMatches(storedHash: string, code: string, scope: string): boolean {
	const expected = Buffer.from(storedHash, 'hex');
	const actual = Buffer.from(hashCode(code, scope), 'hex');
	return expected.length === actual.length && timingSafeEqual(expected, actual);
}

function deviceMac(userId: UserID, deviceId: Buffer): Buffer {
	return createHmac('sha256', deriveKey(DEVICE_PROOF_LABEL))
		.update(userId.toString())
		.update(':')
		.update(deviceId)
		.digest();
}

export function issueDeviceProof(userId: UserID): string {
	const deviceId = randomBytes(DEVICE_ID_BYTES);
	return Buffer.concat([deviceId, deviceMac(userId, deviceId)]).toString('base64url');
}

export function deviceProofMatches(proof: string | null | undefined, userId: UserID): boolean {
	if (!proof) return false;
	const decoded = Buffer.from(proof, 'base64url');
	if (decoded.length !== DEVICE_ID_BYTES + MAC_BYTES) return false;
	const deviceId = decoded.subarray(0, DEVICE_ID_BYTES);
	const mac = decoded.subarray(DEVICE_ID_BYTES);
	return timingSafeEqual(mac, deviceMac(userId, deviceId));
}
