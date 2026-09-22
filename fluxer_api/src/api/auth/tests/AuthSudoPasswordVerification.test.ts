// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth sudo password verification', () => {
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
	it('logs out session with correct password and returns sudo token', async () => {
		const account = await createTestAccount(harness);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		await createBuilder(harness, account.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
				password: account.password,
			})
			.expect(204)
			.execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
	});
	it('rejects logout with wrong password and preserves token', async () => {
		const account: TestAccount = await createTestAccount(harness);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		await createBuilder(harness, account.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
				password: 'wrong-password',
			})
			.expect(400)
			.execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(200).execute();
	});
	it('rejects logout without password with 403', async () => {
		const account = await createTestAccount(harness);
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThan(0);
		await createBuilder(harness, account.token)
			.post('/auth/sessions/logout')
			.body({
				session_id_hashes: [sessions[0]!.id_hash],
			})
			.expect(403)
			.execute();
	});
});
