// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import {SNOWFLAKE_REGEX} from '@app/api/attachment/AttachmentPublicUrl';
import {Config} from '@app/api/Config';
import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {signExpiringToken, verifyExpiringToken} from '@app/api/utils/SignedExpiringToken';

const KEY_DERIVATION_LABEL = 'fluxer.attachment-media.v1';
const MAX_FEATURES = 32;
export const ATTACHMENT_MEDIA_TOKEN_TTL_MS = 4 * 60 * 60 * 1000;
export const SCOPED_ATTACHMENT_MEDIA_TOKEN_TTL_MS = 10 * 60 * 1000;

export interface AttachmentMediaTokenPayload {
	u: string;
	s: string;
	f: Array<string>;
	c?: string;
	a?: string;
	expiresAt: number;
}

function deriveKey(): Buffer {
	return createHmac('sha256', Config.auth.connectionInitiationSecret).update(KEY_DERIVATION_LABEL).digest();
}

function isOptionalSnowflake(value: unknown): boolean {
	return value === undefined || (typeof value === 'string' && SNOWFLAKE_REGEX.test(value));
}

function isAttachmentMediaTokenPayload(value: unknown): value is AttachmentMediaTokenPayload {
	if (!isJsonRecord(value)) return false;
	const features = value['f'];
	return (
		typeof value['u'] === 'string' &&
		SNOWFLAKE_REGEX.test(value['u']) &&
		typeof value['s'] === 'string' &&
		value['s'].length > 0 &&
		Array.isArray(features) &&
		features.length <= MAX_FEATURES &&
		features.every((feature) => typeof feature === 'string') &&
		isOptionalSnowflake(value['c']) &&
		isOptionalSnowflake(value['a']) &&
		(value['c'] === undefined) === (value['a'] === undefined) &&
		typeof value['expiresAt'] === 'number' &&
		Number.isFinite(value['expiresAt'])
	);
}

export function signAttachmentMediaToken(payload: AttachmentMediaTokenPayload): string {
	return signExpiringToken(payload, deriveKey());
}

export function verifyAttachmentMediaToken(token: string): AttachmentMediaTokenPayload | null {
	return verifyExpiringToken(token, deriveKey(), isAttachmentMediaTokenPayload);
}
