// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface BouncedEmailRequestNewResponse {
	ticket: string;
	new_email: string;
	new_code_expires_at: string;
	resend_available_at: string | null;
}

interface EmailChangeStartResponse {
	ticket: string;
	require_original: boolean;
	original_proof?: string | null;
}

interface UserPrivateResponse {
	email: string | null;
	verified: boolean;
	email_bounced?: boolean;
	required_actions: Array<string> | null;
}

async function markEmailAsBounced(harness: ApiTestHarness, account: TestAccount): Promise<void> {
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${account.userId}/security-flags`)
		.body({
			suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL'],
			email_bounced: true,
			email_verified: false,
		})
		.expect(200)
		.execute();
}

describe('Bounced email recovery flow', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('allows bounced users to replace email without original-email verification', async () => {
		const account = await createTestAccount(harness);
		await markEmailAsBounced(harness, account);
		const initialMe = await createBuilder<UserPrivateResponse>(harness, account.token)
			.get('/users/@me')
			.expect(200)
			.execute();
		expect(initialMe.required_actions).toContain('REQUIRE_REVERIFIED_EMAIL');
		const startResponse = await createBuilder<EmailChangeStartResponse>(harness, account.token)
			.post('/users/@me/email-change/start')
			.body({})
			.expect(200)
			.execute();
		expect(startResponse.require_original).toBe(false);
		expect(startResponse.original_proof).toBeDefined();
		const replacementEmail = `replacement-${Date.now()}@example.com`;
		const requestNewResponse = await createBuilder<BouncedEmailRequestNewResponse>(harness, account.token)
			.post('/users/@me/email-change/bounced/request-new')
			.body({new_email: replacementEmail})
			.execute();
		expect(requestNewResponse.new_email).toBe(replacementEmail);
		const originalEmailMessages = await listTestEmails(harness, {recipient: account.email});
		expect(findLastTestEmail(originalEmailMessages, 'email_change_original')).toBeNull();
		const replacementEmailMessages = await listTestEmails(harness, {recipient: replacementEmail});
		const replacementVerificationEmail = findLastTestEmail(replacementEmailMessages, 'email_change_new');
		expect(replacementVerificationEmail?.metadata?.code).toBeDefined();
		const updatedUser = await createBuilder<UserPrivateResponse>(harness, account.token)
			.post('/users/@me/email-change/bounced/verify-new')
			.body({
				ticket: requestNewResponse.ticket,
				code: replacementVerificationEmail!.metadata!.code!,
			})
			.execute();
		expect(updatedUser.email).toBe(replacementEmail);
		expect(updatedUser.verified).toBe(true);
		expect(updatedUser.email_bounced).toBe(false);
		expect(updatedUser.required_actions).toEqual([]);
		const finalMe = await createBuilder<UserPrivateResponse>(harness, account.token).get('/users/@me').execute();
		expect(finalMe.email).toBe(replacementEmail);
		expect(finalMe.email_bounced).toBe(false);
	});
	it('rejects bounced-email recovery for accounts that are not marked as bounced', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/email-change/bounced/request-new')
			.body({new_email: `replacement-${Date.now()}@example.com`})
			.expect(403, 'ACCESS_DENIED')
			.execute();
	});
});
