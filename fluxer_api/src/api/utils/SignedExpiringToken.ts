// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, timingSafeEqual} from 'node:crypto';
import {parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';

interface ExpiringPayload {
	expiresAt: number;
}

function computeSignature(payloadBase64: string, key: string | Buffer): Buffer {
	return createHmac('sha256', key).update(payloadBase64).digest();
}

export function signExpiringToken<T extends ExpiringPayload>(payload: T, key: string | Buffer): string {
	const payloadBase64 = Buffer.from(JSON.stringify(payload)).toString('base64url');
	const signature = computeSignature(payloadBase64, key).toString('base64url');
	return `${payloadBase64}.${signature}`;
}

export function verifyExpiringToken<T extends ExpiringPayload>(
	token: string,
	key: string | Buffer,
	guard: (value: unknown) => value is T,
): T | null {
	const dotIndex = token.indexOf('.');
	if (dotIndex === -1) return null;
	const payloadBase64 = token.slice(0, dotIndex);
	const signatureBase64 = token.slice(dotIndex + 1);
	const expectedSignature = computeSignature(payloadBase64, key);
	let providedSignature: Buffer;
	try {
		providedSignature = Buffer.from(signatureBase64, 'base64url');
	} catch {
		return null;
	}
	if (expectedSignature.length !== providedSignature.length) return null;
	if (!timingSafeEqual(expectedSignature, providedSignature)) return null;
	let payload: T | null;
	try {
		const payloadJson = Buffer.from(payloadBase64, 'base64url').toString('utf-8');
		payload = parseJsonWithGuard(payloadJson, guard);
	} catch {
		return null;
	}
	if (!payload || Date.now() > payload.expiresAt) return null;
	return payload;
}
