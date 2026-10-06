// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth concurrent sessions', () => {
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
	it('same user can have multiple concurrent sessions', async () => {
		const account = await createTestAccount(harness);
		const session1Token = account.token;
		const account2 = await loginAccount(harness, account);
		const session2Token = account2.token;
		if (session1Token === session2Token) {
			console.warn('warning: multiple logins returned the same token, may indicate single-session behavior');
		}
		await createBuilder(harness, session1Token).get('/users/@me').expect(200).execute();
		await createBuilder(harness, session2Token).get('/users/@me').expect(200).execute();
	});
	it('logging out one session does not affect other sessions', async () => {
		const account = await createTestAccount(harness);
		const session1Token = account.token;
		const account2 = await loginAccount(harness, account);
		const session2Token = account2.token;
		const account3 = await loginAccount(harness, account);
		const session3Token = account3.token;
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, session1Token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThanOrEqual(3);
		await createBuilder(harness, session2Token).post('/auth/logout').expect(204).execute();
		await createBuilder(harness, session1Token).get('/users/@me').expect(200).execute();
		await createBuilder(harness, session3Token).get('/users/@me').expect(200).execute();
		await createBuilder(harness, session2Token).get('/users/@me').expect(401).execute();
	});
	it('can list all active sessions', async () => {
		const account = await createTestAccount(harness);
		await loginAccount(harness, account);
		await loginAccount(harness, account);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		for (const session of sessions) {
			expect(session.id_hash).toBeTruthy();
			expect(session.id_hash.length).toBeGreaterThan(0);
			expect(session).toHaveProperty('masked_ip');
			if (session.masked_ip) {
				expect(session.masked_ip).toContain('x');
			}
			expect(session.client_info?.location).toEqual({
				city: 'Stockholm',
				region: 'Stockholm County',
				country: 'Sweden',
			});
		}
	});
	it('can log out specific session by ID', async () => {
		let account = await createTestAccount(harness);
		await loginAccount(harness, account);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThanOrEqual(2);
		const targetSessionID = sessions[0]!.id_hash;
		await createBuilder(harness, account.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [targetSessionID],
				password: account.password,
			})
			.expect(204)
			.execute();
		account = await loginAccount(harness, account);
		const sessions2 = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions2.find((s) => s.id_hash === targetSessionID)).toBeUndefined();
	});
});
