// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, timingSafeEqual} from 'node:crypto';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {
	deriveOperatorPassSecret,
	OPERATOR_PASS_UNSUBSCRIBE_SECRET_LABEL,
} from '@app/api/operator_pass/OperatorPassSecrets';

const ENTRY_ID_PATTERN = /^[0-9a-f]{32}$/u;
const SIGNATURE_PATTERN = /^[A-Za-z0-9_-]{43}$/u;
const DELETE_ATTEMPTS = 3;

function signEntryId(entryId: string, secret: string): string {
	return createHmac('sha256', secret).update(entryId).digest('base64url');
}

export function verifyOperatorPassUnsubscribeToken(
	token: string,
	secret: string = deriveOperatorPassSecret(OPERATOR_PASS_UNSUBSCRIBE_SECRET_LABEL),
): string | null {
	const parts = token.split('.');
	if (parts.length !== 2) return null;
	const [entryId, signature] = parts;
	if (!ENTRY_ID_PATTERN.test(entryId) || !SIGNATURE_PATTERN.test(signature)) return null;
	const expected = Buffer.from(signEntryId(entryId, secret), 'utf8');
	const actual = Buffer.from(signature, 'utf8');
	if (expected.length !== actual.length || !timingSafeEqual(expected, actual)) return null;
	return entryId;
}

export async function deleteOperatorPassWaitlistEntry(
	repository: OperatorPassRepository,
	entryId: string,
): Promise<void> {
	for (let attempt = 0; attempt < DELETE_ATTEMPTS; attempt += 1) {
		const entry = await repository.findWaitlistEntryByEntryId(entryId);
		if (!entry || (await repository.deleteWaitlistEntry(entry))) return;
	}
	throw new Error('Could not delete the Operator Pass waitlist entry');
}
