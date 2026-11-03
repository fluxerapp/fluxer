// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, createHmac, randomBytes, timingSafeEqual} from 'node:crypto';
import {Config} from '@app/api/Config';
import {
	deriveOperatorPassSecret,
	OPERATOR_PASS_FOLLOWUP_SECRET_LABEL,
	OPERATOR_PASS_UNSUBSCRIBE_SECRET_LABEL,
} from '@app/api/operator_pass/OperatorPassSecrets';
import {ms} from 'itty-time';

export const OPERATOR_PASS_WAITLIST_VERIFY_EXPIRY_MS = ms('48 hours');
const OPERATOR_PASS_WAITLIST_FOLLOWUP_EXPIRY_MS = ms('30 minutes');

const ENTRY_ID_PATTERN = /^[0-9a-f]{32}$/u;
const VERIFY_SECRET_PATTERN = /^[0-9a-f]{64}$/u;
const SIGNATURE_PATTERN = /^[A-Za-z0-9_-]{43}$/u;
const FOLLOWUP_EXPIRY_PATTERN = /^[0-9]{13}$/u;

export interface OperatorPassWaitlistVerifyToken {
	token: string;
	secretHash: string;
}

export interface ParsedOperatorPassWaitlistVerifyToken {
	entryId: string;
	secret: string;
}

export function createOperatorPassWaitlistEntryId(): string {
	return randomBytes(16).toString('hex');
}

function hashOperatorPassWaitlistSecret(secret: string): string {
	return createHash('sha256').update(secret).digest('hex');
}

export function createOperatorPassWaitlistVerifyToken(entryId: string): OperatorPassWaitlistVerifyToken {
	const secret = randomBytes(32).toString('hex');
	return {token: `${entryId}.${secret}`, secretHash: hashOperatorPassWaitlistSecret(secret)};
}

function splitToken(token: string): [string, string] | null {
	const parts = token.split('.');
	if (parts.length !== 2) return null;
	return [parts[0], parts[1]];
}

export function parseOperatorPassWaitlistVerifyToken(token: string): ParsedOperatorPassWaitlistVerifyToken | null {
	const parts = splitToken(token);
	if (!parts) return null;
	const [entryId, secret] = parts;
	if (!ENTRY_ID_PATTERN.test(entryId) || !VERIFY_SECRET_PATTERN.test(secret)) return null;
	return {entryId, secret};
}

export function operatorPassWaitlistSecretMatches(secret: string, storedHash: string): boolean {
	const expected = Buffer.from(storedHash, 'utf8');
	const actual = Buffer.from(hashOperatorPassWaitlistSecret(secret), 'utf8');
	if (expected.length !== actual.length) return false;
	return timingSafeEqual(expected, actual);
}

function signEntryId(entryId: string, secret: string): string {
	return createHmac('sha256', secret).update(entryId).digest('base64url');
}

function getUnsubscribeSecret(): string {
	return deriveOperatorPassSecret(OPERATOR_PASS_UNSUBSCRIBE_SECRET_LABEL);
}

function createOperatorPassUnsubscribeToken(entryId: string, secret: string = getUnsubscribeSecret()): string {
	return `${entryId}.${signEntryId(entryId, secret)}`;
}

export function verifyOperatorPassUnsubscribeToken(
	token: string,
	secret: string = getUnsubscribeSecret(),
): string | null {
	const parts = splitToken(token);
	if (!parts) return null;
	const [entryId, signature] = parts;
	if (!ENTRY_ID_PATTERN.test(entryId) || !SIGNATURE_PATTERN.test(signature)) return null;
	const expected = Buffer.from(signEntryId(entryId, secret), 'utf8');
	const actual = Buffer.from(signature, 'utf8');
	if (expected.length !== actual.length || !timingSafeEqual(expected, actual)) return null;
	return entryId;
}

function getFollowupSecret(): string {
	return deriveOperatorPassSecret(OPERATOR_PASS_FOLLOWUP_SECRET_LABEL);
}

function signFollowup(entryId: string, expiresAtMs: string, secret: string): string {
	return createHmac('sha256', secret).update(`${entryId}.${expiresAtMs}`).digest('base64url');
}

export function createOperatorPassWaitlistFollowupToken(
	entryId: string,
	now: Date = new Date(),
	secret: string = getFollowupSecret(),
): string {
	const expiresAtMs = String(now.getTime() + OPERATOR_PASS_WAITLIST_FOLLOWUP_EXPIRY_MS).padStart(13, '0');
	return `${entryId}.${expiresAtMs}.${signFollowup(entryId, expiresAtMs, secret)}`;
}

export function verifyOperatorPassWaitlistFollowupToken(
	token: string,
	now: Date = new Date(),
	secret: string = getFollowupSecret(),
): string | null {
	const parts = token.split('.');
	if (parts.length !== 3) return null;
	const [entryId, expiresAtMs, signature] = parts;
	if (
		!ENTRY_ID_PATTERN.test(entryId) ||
		!FOLLOWUP_EXPIRY_PATTERN.test(expiresAtMs) ||
		!SIGNATURE_PATTERN.test(signature)
	) {
		return null;
	}
	const expected = Buffer.from(signFollowup(entryId, expiresAtMs, secret), 'utf8');
	const actual = Buffer.from(signature, 'utf8');
	if (expected.length !== actual.length || !timingSafeEqual(expected, actual)) return null;
	if (Number(expiresAtMs) <= now.getTime()) return null;
	return entryId;
}

export function buildOperatorPassWaitlistConfirmUrl(token: string): string {
	return `${Config.endpoints.marketing}/pass/confirm?token=${encodeURIComponent(token)}`;
}

export function buildOperatorPassUnsubscribeUrls(entryId: string): {
	unsubscribeUrl: string;
	unsubscribePageUrl: string;
} {
	const token = encodeURIComponent(createOperatorPassUnsubscribeToken(entryId));
	return {
		unsubscribeUrl: `${Config.endpoints.apiPublic}/operator-pass/waitlist/unsubscribe?token=${token}`,
		unsubscribePageUrl: `${Config.endpoints.marketing}/pass/unsubscribe?token=${token}`,
	};
}
