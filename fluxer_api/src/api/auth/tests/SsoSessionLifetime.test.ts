// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	enableTotp,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;

describe('SSO session lifetime', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
		]);
		await enableSso(harness, admin.token, {enforced: false});
	});
	afterEach(async () => {
		vi.useRealTimers();
		await disableSso(harness, admin.token);
		getInstanceConfigRepository().clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function signInWithSso(): Promise<{token: string; user_id: string}> {
		const claims = {sub: `itest-lifetime-${Date.now()}-${Math.random()}`, email: createUniqueEmail('sso-lifetime')};
		const {state, client_binding} = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
			.post('/auth/sso/start')
			.body({})
			.execute();
		return createBuilderWithoutAuth<{token: string; user_id: string}>(harness)
			.post('/auth/sso/complete')
			.body({code: JSON.stringify(claims), state, client_binding})
			.expect(200)
			.execute();
	}

	async function handOff({token, user_id}: {token: string; user_id: string}): Promise<string> {
		const {code, poll_secret} = await createBuilderWithoutAuth<{code: string; poll_secret: string}>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		await createBuilderWithoutAuth(harness).get(`/auth/handoff/${code}/info`).execute();
		await createBuilder(harness, token).post('/auth/handoff/complete').body({code, user_id}).expect(204).execute();
		const status = await createBuilderWithoutAuth<{token: string}>(harness)
			.post(`/auth/handoff/${code}/status`)
			.body({poll_secret})
			.execute();
		return status.token;
	}

	function travelDays(days: number): void {
		vi.useFakeTimers({toFake: ['Date']});
		vi.setSystemTime(Date.now() + days * DAY_MS);
	}

	it('keeps an SSO session working for 30 days', async () => {
		const {token} = await signInWithSso();
		travelDays(29);
		await createBuilder(harness, token).get('/users/@me').expect(200).execute();
	});

	it('ends an SSO session after 30 days', async () => {
		const {token} = await signInWithSso();
		travelDays(31);
		await createBuilder(harness, token).get('/users/@me').expect(401).execute();
	});

	it('ends an SSO session that asked for a second factor after 30 days', async () => {
		const account = await createTestAccount(harness);
		const [backupCode] = await enableTotp(harness, account);
		const {state, client_binding} = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
			.post('/auth/sso/start')
			.body({})
			.execute();
		const {ticket} = await createBuilderWithoutAuth<{ticket: string}>(harness)
			.post('/auth/sso/complete')
			.body({code: account.email, state, client_binding})
			.execute();
		const {token} = await createBuilderWithoutAuth<{token: string}>(harness)
			.post('/auth/login/mfa/totp')
			.body({ticket, code: backupCode})
			.execute();
		travelDays(31);
		await createBuilder(harness, token).get('/users/@me').expect(401).execute();
	});

	it('ends a session handed to the desktop app at the same time as the SSO session it came from', async () => {
		const login = await signInWithSso();
		travelDays(20);
		const handedToken = await handOff(login);
		travelDays(11);
		await createBuilder(harness, handedToken).get('/users/@me').expect(401).execute();
	});

	it('leaves an SSO session out of the session list after 30 days', async () => {
		const account = await createTestAccount(harness);
		const {state, client_binding} = await createBuilderWithoutAuth<{state: string; client_binding: string}>(harness)
			.post('/auth/sso/start')
			.body({})
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/sso/complete')
			.body({code: account.email, state, client_binding})
			.expect(200)
			.execute();
		const listed = () => createBuilder<Array<unknown>>(harness, account.token).get('/auth/sessions').execute();
		expect(await listed()).toHaveLength(2);
		travelDays(31);
		expect(await listed()).toHaveLength(1);
	});

	it('leaves password sessions alone', async () => {
		const account = await createTestAccount(harness);
		travelDays(31);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
	});
});
