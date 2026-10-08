// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	createUniqueUsername,
	loginAccount,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

async function createGuildInvite(harness: ApiTestHarness): Promise<{
	guildId: string;
	inviteCode: string;
	ownerToken: string;
}> {
	let owner = await createTestAccount(harness);
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${owner.userId}/acls`)
		.body({
			acls: ['*'],
		})
		.expect(200)
		.execute();
	owner = await loginAccount(harness, owner);
	const guild = await createBuilder<GuildResponse>(harness, owner.token)
		.post('/guilds')
		.body({
			name: `InviteGuild-${Date.now()}`,
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
	return {guildId: guild.id, inviteCode: invite.code, ownerToken: owner.token};
}

describe('Auth registration invite auto-join', () => {
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
	it('joins the invite guild on registration', async () => {
		const {guildId, inviteCode, ownerToken} = await createGuildInvite(harness);
		const registration = await registerUser(harness, {
			email: createUniqueEmail('invite-join'),
			username: createUniqueUsername('invite_join'),
			global_name: 'Invite Join',
			password: 'StrongPassword!123',
			date_of_birth: '2000-01-01',
			consent: true,
			invite_code: inviteCode,
		});
		const memberLookup = await createBuilder(harness, ownerToken)
			.get(`/guilds/${guildId}/members/${registration.user_id}`)
			.expect(200)
			.executeWithResponse();
		expect(memberLookup.response.status).toBe(200);
	});
});
