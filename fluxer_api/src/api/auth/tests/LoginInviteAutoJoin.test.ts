// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginAccount, loginUser} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth login with invite code auto-join', () => {
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
	it('auto-joins a guild when logging in with invite_code', async () => {
		let owner = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${owner.userId}/acls`)
			.body({
				acls: ['*'],
			})
			.expect(200)
			.execute();
		owner = await loginAccount(harness, owner);
		const guildName = `InviteGuild-${Date.now()}`;
		const guild = await createBuilder<GuildResponse>(harness, owner.token)
			.post('/guilds')
			.body({
				name: guildName,
			})
			.execute();
		if (!guild.system_channel_id) {
			throw new Error('Guild creation did not return a system_channel_id');
		}
		const invite = await createBuilder<{
			code: string;
		}>(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/invites`)
			.body({
				max_uses: 0,
				max_age: 0,
				unique: false,
				temporary: false,
			})
			.execute();
		const member = await createTestAccount(harness);
		const login = await loginUser(harness, {
			email: member.email,
			password: member.password,
			invite_code: invite.code,
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
			const foundGuild = guilds.find((g) => g.id === guild.id);
			expect(foundGuild).toBeDefined();
			expect(foundGuild?.id).toBe(guild.id);
		}
	});
});
