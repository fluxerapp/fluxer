// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, timingSafeEqual} from 'node:crypto';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {type ConnectionType, ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';

interface ConnectionInitiationTokenPayload {
	userId: string;
	type: ConnectionType;
	identifier: string;
	verificationCode: string;
	expiresAt: number;
}

function computeSignature(payloadBase64: string, secret: string): Buffer {
	return createHmac('sha256', secret).update(payloadBase64).digest();
}

function isConnectionType(value: unknown): value is ConnectionType {
	return value === ConnectionTypes.BLUESKY || value === ConnectionTypes.DOMAIN;
}

function isConnectionInitiationTokenPayload(value: unknown): value is ConnectionInitiationTokenPayload {
	if (!isJsonRecord(value)) return false;
	return (
		typeof value['userId'] === 'string' &&
		isConnectionType(value['type']) &&
		typeof value['identifier'] === 'string' &&
		typeof value['verificationCode'] === 'string' &&
		typeof value['expiresAt'] === 'number' &&
		Number.isFinite(value['expiresAt'])
	);
}

export function signInitiationToken(payload: ConnectionInitiationTokenPayload, secret: string): string {
	const payloadJson = JSON.stringify(payload);
	const payloadBase64 = Buffer.from(payloadJson).toString('base64url');
	const signature = computeSignature(payloadBase64, secret).toString('base64url');
	return `${payloadBase64}.${signature}`;
}

export function verifyInitiationToken(token: string, secret: string): ConnectionInitiationTokenPayload | null {
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
	let payload: ConnectionInitiationTokenPayload | null;
	try {
		const payloadJson = Buffer.from(payloadBase64, 'base64url').toString('utf-8');
		payload = parseJsonWithGuard(payloadJson, isConnectionInitiationTokenPayload);
	} catch {
		return null;
	}
	if (!payload || Date.now() > payload.expiresAt) {
		return null;
	}
	return payload;
}
