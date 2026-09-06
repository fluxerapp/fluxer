// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import {
	createAuthHarness,
	createFakeAuthToken,
	createTestAccount,
	loginAccount,
	setUserACLs,
	type TestAccount,
} from './AuthTestUtils';

function authSessionCacheKey(token: string): string {
	return `auth:session:${createHash('sha256').update(token).digest('base64url')}`;
}

async function readCachedSession(harness: ApiTestHarness, token: string): Promise<string | null> {
	return harness.kvProvider.get(authSessionCacheKey(token));
}

describe('Auth session cache invalidation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('caches the session on the token hash and never on the raw token', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
		const cached = await readCachedSession(harness, account.token);
		expect(cached).not.toBeNull();
		expect(cached).not.toContain(account.token);
		await expect(harness.kvProvider.get(`auth:session:${account.token}`)).resolves.toBeNull();
	});
	it('stops serving a session revoked by logout', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
		expect(await readCachedSession(harness, account.token)).not.toBeNull();
		await createBuilder(harness, account.token).post('/auth/logout').expect(204).execute();
		expect(await readCachedSession(harness, account.token)).toBeNull();
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
	});
	it('stops serving a session revoked from another session list', async () => {
		const account = await createTestAccount(harness);
		const second = await loginAccount(harness, account);
		await createBuilder(harness, second.token).get('/users/@me').expect(200).execute();
		expect(await readCachedSession(harness, second.token)).not.toBeNull();
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		const secondSessionIdHash = createHash('sha256').update(second.token).digest('base64url');
		expect(sessions.some((session) => session.id_hash === secondSessionIdHash)).toBe(true);
		await createBuilder(harness, account.token)
			.post('/auth/sessions/logout')
			.body({session_id_hashes: [secondSessionIdHash], password: account.password})
			.expect(204)
			.execute();
		expect(await readCachedSession(harness, second.token)).toBeNull();
		await createBuilder(harness, second.token).get('/users/@me').expect(401).execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
	});
	it('stops serving every session of a user after a password change', async () => {
		const account = await createTestAccount(harness);
		const second = await loginAccount(harness, account);
		const third = await loginAccount(harness, account);
		for (const token of [account.token, second.token, third.token]) {
			await createBuilder(harness, token).get('/users/@me').expect(200).execute();
			expect(await readCachedSession(harness, token)).not.toBeNull();
		}
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({password: account.password, new_password: `cache-rotation-${Date.now()}`})
			.execute();
		for (const token of [account.token, second.token, third.token]) {
			expect(await readCachedSession(harness, token)).toBeNull();
			await createBuilder(harness, token).get('/users/@me').expect(401).execute();
		}
	});
	it('stops serving every session of a user after an admin temp ban', async () => {
		const target = await createTestAccount(harness);
		const targetSecond = await loginAccount(harness, target);
		let admin: TestAccount = await createTestAccount(harness);
		admin = await setUserACLs(harness, admin, ['admin:authenticate', 'user:temp_ban']);
		for (const token of [target.token, targetSecond.token]) {
			await createBuilder(harness, token).get('/users/@me').expect(200).execute();
			expect(await readCachedSession(harness, token)).not.toBeNull();
		}
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${target.userId}/ban`)
			.body({duration_hours: 24, reason: 'cache invalidation coverage'})
			.execute();
		for (const token of [target.token, targetSecond.token]) {
			expect(await readCachedSession(harness, token)).toBeNull();
			await createBuilder(harness, token).get('/users/@me').expect(401).execute();
		}
	});
	it('stops serving sessions after the account disables itself', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
		expect(await readCachedSession(harness, account.token)).not.toBeNull();
		await createBuilder(harness, account.token)
			.post('/users/@me/disable')
			.body({password: account.password})
			.expect(204)
			.execute();
		expect(await readCachedSession(harness, account.token)).toBeNull();
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
	});
	it('caches an unknown token hash without stranding sessions created afterwards', async () => {
		const unknownToken = createFakeAuthToken();
		await createBuilder(harness, unknownToken).get('/users/@me').expect(401).execute();
		await expect(readCachedSession(harness, unknownToken)).resolves.toBe('null');
		await createBuilder(harness, unknownToken).get('/users/@me').expect(401).execute();
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
		const rotated = await loginAccount(harness, account);
		await createBuilder(harness, rotated.token).get('/users/@me').expect(200).execute();
	});
});
