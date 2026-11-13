// SPDX-License-Identifier: AGPL-3.0-or-later

import {createCipheriv, createDecipheriv, createHmac, randomBytes} from 'node:crypto';
import {SNOWFLAKE_REGEX} from '@app/api/attachment/AttachmentPublicUrl';
import {Config} from '@app/api/Config';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';

const KEY_DERIVATION_LABEL = 'fluxer.attachment-media.v2';
const CIPHER = 'aes-256-gcm';
const IV_BYTES = 12;
const TAG_BYTES = 16;
const MAX_FEATURES = 32;
export const ATTACHMENT_MEDIA_TOKEN_TTL_MS = 5 * 60 * 1000;

export interface AttachmentMediaTokenPayload {
	u: string;
	s: string;
	f: Array<string>;
	c: string;
	a: string;
	expiresAt: number;
}

function deriveKey(): Buffer {
	return createHmac('sha256', Config.auth.connectionInitiationSecret).update(KEY_DERIVATION_LABEL).digest();
}

function isSnowflake(value: unknown): boolean {
	return typeof value === 'string' && SNOWFLAKE_REGEX.test(value);
}

function isAttachmentMediaTokenPayload(value: unknown): value is AttachmentMediaTokenPayload {
	if (!isJsonRecord(value)) return false;
	const features = value['f'];
	return (
		isSnowflake(value['u']) &&
		typeof value['s'] === 'string' &&
		value['s'].length > 0 &&
		Array.isArray(features) &&
		features.length <= MAX_FEATURES &&
		features.every((feature) => typeof feature === 'string') &&
		isSnowflake(value['c']) &&
		isSnowflake(value['a']) &&
		typeof value['expiresAt'] === 'number' &&
		Number.isFinite(value['expiresAt'])
	);
}

export function sealAttachmentMediaToken(payload: AttachmentMediaTokenPayload): string {
	const iv = randomBytes(IV_BYTES);
	const cipher = createCipheriv(CIPHER, deriveKey(), iv);
	const sealed = Buffer.concat([cipher.update(JSON.stringify(payload), 'utf8'), cipher.final()]);
	return Buffer.concat([iv, sealed, cipher.getAuthTag()]).toString('base64url');
}

export function openAttachmentMediaToken(token: string): AttachmentMediaTokenPayload | null {
	const bytes = Buffer.from(token, 'base64url');
	if (bytes.length <= IV_BYTES + TAG_BYTES || bytes.toString('base64url') !== token) return null;
	let payload: AttachmentMediaTokenPayload | null;
	try {
		const decipher = createDecipheriv(CIPHER, deriveKey(), bytes.subarray(0, IV_BYTES));
		decipher.setAuthTag(bytes.subarray(bytes.length - TAG_BYTES));
		const opened = Buffer.concat([
			decipher.update(bytes.subarray(IV_BYTES, bytes.length - TAG_BYTES)),
			decipher.final(),
		]);
		payload = parseJsonWithGuard(opened.toString('utf8'), isAttachmentMediaTokenPayload);
	} catch {
		return null;
	}
	if (!payload || Date.now() > payload.expiresAt) return null;
	return payload;
}
