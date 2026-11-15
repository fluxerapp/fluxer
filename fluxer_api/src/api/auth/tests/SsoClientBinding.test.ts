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
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {SSO_MOBILE_CALLBACK_URI} from '@fluxer/constants/src/SsoConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SsoStartResponse {
	state: string;
	client_binding: string;
}

interface SsoCompleteResponse {
	token: string;
}

interface ValidationErrorBody {
	errors?: Array<{path: string; code: string}>;
}

describe('SSO client binding', () => {
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
		await enableSso(harness, admin.token);
	});
	afterEach(async () => {
		await disableSso(harness, admin.token);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function start(body: Record<string, unknown> = {}): Promise<SsoStartResponse> {
		return createBuilderWithoutAuth<SsoStartResponse>(harness).post('/auth/sso/start').body(body).execute();
	}

	async function completeError(body: Record<string, unknown>): Promise<string | undefined> {
		const {json} = await createBuilderWithoutAuth<ValidationErrorBody>(harness)
			.post('/auth/sso/complete')
			.body({code: createUniqueEmail('sso-binding'), ...body})
			.expect(400)
			.executeWithResponse();
		return json.errors?.find((error) => error.path === 'state')?.code;
	}

	function complete(body: Record<string, unknown>): Promise<SsoCompleteResponse> {
		return createBuilderWithoutAuth<SsoCompleteResponse>(harness)
			.post('/auth/sso/complete')
			.body({code: createUniqueEmail('sso-binding'), ...body})
			.execute();
	}

	it('returns a different client binding on each start', async () => {
		const first = await start();
		const second = await start();
		expect(first.client_binding).toMatch(/^[A-Za-z0-9_-]{43}$/);
		expect(first.client_binding).not.toBe(second.client_binding);
	});

	it('signs in when the client sends the binding it was given', async () => {
		const {state, client_binding} = await start();
		expect((await complete({state, client_binding})).token).toBeTruthy();
	});

	it('rejects a web sign-in without the client binding', async () => {
		const {state} = await start();
		expect(await completeError({state})).toBe(ValidationErrorCodes.INVALID_OR_EXPIRED_SSO_STATE);
	});

	it('rejects a web sign-in with the client binding of another start', async () => {
		const {state} = await start();
		const other = await start();
		expect(await completeError({state, client_binding: other.client_binding})).toBe(
			ValidationErrorCodes.INVALID_OR_EXPIRED_SSO_STATE,
		);
	});

	it('burns the state after a wrong client binding', async () => {
		const {state, client_binding} = await start();
		const other = await start();
		await completeError({state, client_binding: other.client_binding});
		expect(await completeError({state, client_binding})).toBe(ValidationErrorCodes.INVALID_OR_EXPIRED_SSO_STATE);
	});

	it('still lets mobile sign in without the client binding', async () => {
		const {state} = await start({redirect_uri: SSO_MOBILE_CALLBACK_URI});
		expect((await complete({state})).token).toBeTruthy();
	});

	it('rejects a mobile sign-in with the wrong client binding', async () => {
		const {state} = await start({redirect_uri: SSO_MOBILE_CALLBACK_URI});
		const other = await start({redirect_uri: SSO_MOBILE_CALLBACK_URI});
		expect(await completeError({state, client_binding: other.client_binding})).toBe(
			ValidationErrorCodes.INVALID_OR_EXPIRED_SSO_STATE,
		);
	});
});
