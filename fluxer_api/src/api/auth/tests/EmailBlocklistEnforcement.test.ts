// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	findLastTestEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
	unclaimAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_CREDENTIALS, TEST_USER_DATA} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ValidationErrorBody {
	code: string;
	errors: Array<{path: string; code: string}>;
}

interface EmailChangeStartResponse {
	ticket: string;
	original_proof?: string;
}

describe('Email blocklist at signup and email change', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'ban:email:add',
			'ban:email:check',
		]);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function blocklist(email: string): Promise<void> {
		await createBuilder(harness, admin.token)
			.post('/admin/blocklists/email/entries')
			.body({email})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	}

	function expectGenericEmailError(json: ValidationErrorBody, path: string): void {
		expect(json.errors.map((entry) => ({path: entry.path, code: entry.code}))).toEqual([
			{path, code: 'INVALID_EMAIL_ADDRESS'},
		]);
	}

	it('refuses registration with a blocklisted address in any letter case', async () => {
		const email = createUniqueEmail('blocked-signup');
		await blocklist(email);
		const {json} = await createBuilder<ValidationErrorBody>(harness, '')
			.post('/auth/register')
			.body({
				email: email.toUpperCase(),
				username: createUniqueUsername('blocked'),
				global_name: TEST_USER_DATA.DEFAULT_GLOBAL_NAME,
				password: TEST_CREDENTIALS.STRONG_PASSWORD,
				date_of_birth: TEST_USER_DATA.DEFAULT_DATE_OF_BIRTH,
				consent: true,
			})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expectGenericEmailError(json, 'email');
	});

	function register(email: string) {
		return createBuilder<ValidationErrorBody>(harness, '')
			.post('/auth/register')
			.body({
				email,
				username: createUniqueUsername('domain'),
				global_name: TEST_USER_DATA.DEFAULT_GLOBAL_NAME,
				password: TEST_CREDENTIALS.STRONG_PASSWORD,
				date_of_birth: TEST_USER_DATA.DEFAULT_DATE_OF_BIRTH,
				consent: true,
			});
	}

	it('refuses registration at a blocklisted domain and its subdomains', async () => {
		const domain = `${randomUUID()}.test`;
		await blocklist(`@${domain.toUpperCase()}`);
		for (const email of [`someone@${domain}`, `SOMEONE@MAIL.${domain.toUpperCase()}`]) {
			const {json} = await register(email).expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY').executeWithResponse();
			expectGenericEmailError(json, 'email');
		}
	});

	it('does not extend a domain entry to unrelated domains', async () => {
		const domain = `${randomUUID()}.test`;
		await blocklist(`@${domain}`);
		await register(`someone@not${domain}`).execute();
		await register(`someone@${domain}.example`).execute();
	});

	it('reports a domain entry through the blocklist check', async () => {
		const domain = `${randomUUID()}.test`;
		await blocklist(`@${domain}`);
		const {banned} = await createBuilder<{banned: boolean}>(harness, admin.token)
			.get(`/admin/blocklists/email/entries/${encodeURIComponent(`@${domain}`)}`)
			.execute();
		expect(banned).toBe(true);
	});

	it('refuses an email change to a blocklisted address before sending a code', async () => {
		const account = await createTestAccount(harness);
		const blocked = createUniqueEmail('blocked-change');
		await blocklist(blocked);
		const start = await createBuilder<EmailChangeStartResponse>(harness, account.token)
			.post('/users/@me/email-change/start')
			.body({password: account.password})
			.execute();
		const originalCode = findLastTestEmail(
			await listTestEmails(harness, {recipient: account.email}),
			'email_change_original',
		)!.metadata.code!;
		const {original_proof: originalProof} = await createBuilder<{original_proof: string}>(harness, account.token)
			.post('/users/@me/email-change/verify-original')
			.body({ticket: start.ticket, code: originalCode, password: account.password})
			.execute();
		const {json} = await createBuilder<ValidationErrorBody>(harness, account.token)
			.post('/users/@me/email-change/request-new')
			.body({
				ticket: start.ticket,
				new_email: blocked,
				original_proof: originalProof,
				password: account.password,
			})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expectGenericEmailError(json, 'new_email');
		expect(await listTestEmails(harness, {recipient: blocked})).toHaveLength(0);
	});

	it('refuses claiming an unclaimed account with a blocklisted address', async () => {
		const account = await createTestAccount(harness);
		await unclaimAccount(harness, account.userId);
		const blocked = createUniqueEmail('blocked-claim');
		await blocklist(blocked);
		const start = await createBuilder<EmailChangeStartResponse>(harness, account.token)
			.post('/users/@me/email-change/start')
			.body({})
			.execute();
		const {json} = await createBuilder<ValidationErrorBody>(harness, account.token)
			.post('/users/@me/email-change/request-new')
			.body({ticket: start.ticket, new_email: blocked, original_proof: start.original_proof})
			.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
			.executeWithResponse();
		expectGenericEmailError(json, 'new_email');
	});
});
