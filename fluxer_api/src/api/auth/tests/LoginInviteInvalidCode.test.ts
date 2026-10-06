// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginUser} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth login with invalid invite code', () => {
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
	it('login with invalid invite code succeeds but does not add guilds', async () => {
		const member = await createTestAccount(harness);
		const login = await loginUser(harness, {
			email: member.email,
			password: member.password,
			invite_code: 'invalidcode123',
		});
		expect('mfa' in login).toBe(false);
		if (!('mfa' in login)) {
			const nonMfaLogin = login as {
				user_id: string;
				token: string;
			};
			expect(nonMfaLogin.token).toBeTruthy();
		}
		if (!('mfa' in login)) {
			const nonMfaLogin = login as {
				user_id: string;
				token: string;
			};
			const guilds = await createBuilder<Array<GuildResponse>>(harness, nonMfaLogin.token)
				.get('/users/@me/guilds')
				.execute();
			expect(guilds.length).toBe(0);
		}
	});
});
