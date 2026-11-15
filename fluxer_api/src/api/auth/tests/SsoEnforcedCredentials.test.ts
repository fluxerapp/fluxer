// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SsoStartResponse {
	state: string;
	client_binding: string;
}

interface SsoCompleteResponse {
	token: string;
	user_id: string;
}

interface ErrorBody {
	code: string;
	message: string;
}

const CREDENTIAL_ROUTES: ReadonlyArray<{method: 'post' | 'patch'; path: string; body: Record<string, unknown>}> = [
	{method: 'post', path: '/users/@me/email-change/start', body: {}},
	{method: 'post', path: '/users/@me/email-change/resend-original', body: {ticket: 'ticket'}},
	{method: 'post', path: '/users/@me/email-change/verify-original', body: {ticket: 'ticket', code: 'code'}},
	{
		method: 'post',
		path: '/users/@me/email-change/request-new',
		body: {ticket: 'ticket', new_email: 'new@example.com', original_proof: 'proof'},
	},
	{method: 'post', path: '/users/@me/email-change/resend-new', body: {ticket: 'ticket'}},
	{
		method: 'post',
		path: '/users/@me/email-change/verify-new',
		body: {ticket: 'ticket', code: 'code', original_proof: 'proof'},
	},
	{method: 'post', path: '/users/@me/email-change/apply', body: {email_token: 'token'}},
	{method: 'post', path: '/users/@me/email-change/bounced/request-new', body: {new_email: 'new@example.com'}},
	{method: 'post', path: '/users/@me/email-change/bounced/resend-new', body: {ticket: 'ticket'}},
	{method: 'post', path: '/users/@me/email-change/bounced/verify-new', body: {ticket: 'ticket', code: 'code'}},
	{method: 'post', path: '/users/@me/password-change/start', body: {}},
	{method: 'post', path: '/users/@me/password-change/resend', body: {ticket: 'ticket'}},
	{method: 'post', path: '/users/@me/password-change/verify', body: {ticket: 'ticket', code: 'code'}},
	{
		method: 'post',
		path: '/users/@me/password-change/complete',
		body: {ticket: 'ticket', verification_proof: 'proof', new_password: 'a-new-password-123'},
	},
	{method: 'patch', path: '/users/@me', body: {new_password: 'a-new-password-123'}},
	{method: 'patch', path: '/users/@me', body: {email_token: 'token'}},
];

describe('Credentials of SSO accounts while SSO is enforced', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'instance:config:update',
			'instance:config:view',
		]);
	});
	afterEach(async () => {
		await disableSso(harness, admin.token);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function signInWithSso(): Promise<SsoCompleteResponse> {
		const start = await createBuilderWithoutAuth<SsoStartResponse>(harness).post('/auth/sso/start').body({}).execute();
		const login = await createBuilderWithoutAuth<SsoCompleteResponse>(harness)
			.post('/auth/sso/complete')
			.body({code: createUniqueEmail('sso-credentials'), state: start.state, client_binding: start.client_binding})
			.execute();
		await createBuilder(harness, login.token)
			.put('/users/@me/date-of-birth')
			.body({date_of_birth: '1990-01-01'})
			.execute();
		return login;
	}

	for (const route of CREDENTIAL_ROUTES) {
		it(`refuses ${route.method.toUpperCase()} ${route.path} ${Object.keys(route.body).join(',')}`, async () => {
			await enableSso(harness, admin.token);
			const login = await signInWithSso();
			await createBuilder(harness, login.token)
				[route.method](route.path)
				.body(route.body)
				.expect(403, 'SSO_REQUIRED')
				.execute();
		});
	}

	it('says the SSO provider manages the credentials of an SSO account', async () => {
		await enableSso(harness, admin.token);
		const login = await signInWithSso();
		const {json} = await createBuilder<ErrorBody>(harness, login.token)
			.post('/users/@me/password-change/start')
			.body({})
			.expect(403, 'SSO_REQUIRED')
			.executeWithResponse();
		expect(json.message).toBe(
			'This instance requires single sign-on, so your SSO provider manages your email and password.',
		);
	});

	it('points a password sign-in at the SSO button while SSO is enforced', async () => {
		const local = await createTestAccount(harness);
		await enableSso(harness, admin.token);
		const {json} = await createBuilderWithoutAuth<ErrorBody>(harness)
			.post('/auth/login')
			.body({email: local.email, password: local.password})
			.expect(403, 'SSO_REQUIRED')
			.executeWithResponse();
		expect(json.message).toBe(
			'This instance requires single sign-on. Sign in with the SSO button on the sign-in page.',
		);
	});

	it('still lets an SSO account edit its profile while SSO is enforced', async () => {
		await enableSso(harness, admin.token);
		const login = await signInWithSso();
		const updated = await createBuilder<{global_name: string | null}>(harness, login.token)
			.patch('/users/@me')
			.body({global_name: 'Renamed'})
			.execute();
		expect(updated.global_name).toBe('Renamed');
	});

	it('lets an SSO account start an email change when SSO is optional', async () => {
		await enableSso(harness, admin.token, {enforced: false});
		const login = await signInWithSso();
		const started = await createBuilder<{ticket: string}>(harness, login.token)
			.post('/users/@me/email-change/start')
			.body({})
			.execute();
		expect(started.ticket).toBeTruthy();
	});

	it('leaves accounts without an SSO link alone while SSO is enforced', async () => {
		const local = await createTestAccount(harness);
		await enableSso(harness, admin.token);
		const started = await createBuilder<{ticket: string}>(harness, local.token)
			.post('/users/@me/email-change/start')
			.body({})
			.execute();
		expect(started.ticket).toBeTruthy();
	});
});
