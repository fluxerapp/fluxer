// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, timingSafeEqual} from 'node:crypto';
import {isJsonRecord, parseJsonWithGuard} from '../../utils/JsonBoundaryUtils';

const KEY_DERIVATION_LABEL = 'fluxer.harvest-download.v1';

interface HarvestDownloadTokenPayload {
	userId: string;
	harvestId: string;
	storageKey: string;
	expiresAt: number;
}

function deriveKey(secret: string): Buffer {
	return createHmac('sha256', secret).update(KEY_DERIVATION_LABEL).digest();
}

function computeSignature(payloadBase64: string, secret: string): Buffer {
	return createHmac('sha256', deriveKey(secret)).update(payloadBase64).digest();
}

function isHarvestDownloadTokenPayload(value: unknown): value is HarvestDownloadTokenPayload {
	if (!isJsonRecord(value)) return false;
	return (
		typeof value['userId'] === 'string' &&
		typeof value['harvestId'] === 'string' &&
		typeof value['storageKey'] === 'string' &&
		typeof value['expiresAt'] === 'number' &&
		Number.isFinite(value['expiresAt'])
	);
}

export function signHarvestDownloadToken(payload: HarvestDownloadTokenPayload, secret: string): string {
	const payloadBase64 = Buffer.from(JSON.stringify(payload)).toString('base64url');
	const signature = computeSignature(payloadBase64, secret).toString('base64url');
	return `${payloadBase64}.${signature}`;
}

export function verifyHarvestDownloadToken(token: string, secret: string): HarvestDownloadTokenPayload | null {
	const dotIndex = token.indexOf('.');
	if (dotIndex === -1) {
		return null;
	}
	const payloadBase64 = token.slice(0, dotIndex);
	const signatureBase64 = token.slice(dotIndex + 1);
	const expectedSignature = computeSignature(payloadBase64, secret);
	let providedSignature: Buffer;
	try {
		providedSignature = Buffer.from(signatureBase64, 'base64url');
	} catch {
		return null;
	}
	if (expectedSignature.length !== providedSignature.length) {
		return null;
	}
	if (!timingSafeEqual(expectedSignature, providedSignature)) {
		return null;
	}
	let payload: HarvestDownloadTokenPayload | null;
	try {
		const payloadJson = Buffer.from(payloadBase64, 'base64url').toString('utf-8');
		payload = parseJsonWithGuard(payloadJson, isHarvestDownloadTokenPayload);
	} catch {
		return null;
	}
	if (!payload || Date.now() > payload.expiresAt) {
		return null;
	}
	return payload;
}
