// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount, totpCodeNow} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {
	addMemberRole,
	createGuild,
	createRole,
	getChannel,
	setupTestGuildWithMembers,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures, GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const TOTP_SECRET = 'JBSWY3DPEHPK3PXP';

async function enableTotp(harness: ApiTestHarness, account: TestAccount): Promise<void> {
	await createBuilder(harness, account.token)
		.post('/users/@me/mfa/totp/enable')
		.body({secret: TOTP_SECRET, code: totpCodeNow(TOTP_SECRET), password: account.password})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function loginWithTotp(harness: ApiTestHarness, account: TestAccount): Promise<TestAccount> {
	const loginResp = await createBuilderWithoutAuth<{
		mfa: true;
		ticket: string;
	}>(harness)
		.post('/auth/login')
		.body({email: account.email, password: account.password})
		.expect(HTTP_STATUS.OK)
		.execute();
	const mfaResp = await createBuilderWithoutAuth<{
		token: string;
	}>(harness)
		.post('/auth/login/mfa/totp')
		.body({code: totpCodeNow(TOTP_SECRET), ticket: loginResp.ticket})
		.expect(HTTP_STATUS.OK)
		.execute();
	return {...account, token: mfaResp.token};
}

describe('Guild MFA level', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('rejects enabling mfa_level when owner has no 2FA', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'MFA Test Guild');
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.ELEVATED, password: owner.password})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('allows a no-op mfa_level disable when owner has no 2FA', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'MFA Test Guild');
		await createBuilder(harness, owner.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.NONE, password: owner.password})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	it('requires sudo mode when changing mfa_level', async () => {
		const owner = await createTestAccount(harness);
		await enableTotp(harness, owner);
		const loggedIn = await loginWithTotp(harness, owner);
		const guild = await createGuild(harness, loggedIn.token, 'MFA Test Guild');
		await createBuilder(harness, loggedIn.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.ELEVATED})
			.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
			.execute();
	});
	it('allows enabling mfa_level with sudo verification via TOTP', async () => {
		const owner = await createTestAccount(harness);
		await enableTotp(harness, owner);
		const loggedIn = await loginWithTotp(harness, owner);
		const guild = await createGuild(harness, loggedIn.token, 'MFA Test Guild');
		const updated = await createBuilder<GuildResponse>(harness, loggedIn.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.ELEVATED, mfa_method: 'totp', mfa_code: totpCodeNow(TOTP_SECRET)})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.mfa_level).toBe(GuildMFALevel.ELEVATED);
	});
	it('allows disabling mfa_level with sudo verification via TOTP', async () => {
		const owner = await createTestAccount(harness);
		await enableTotp(harness, owner);
		const loggedIn = await loginWithTotp(harness, owner);
		const guild = await createGuild(harness, loggedIn.token, 'MFA Test Guild');
		await createBuilder<GuildResponse>(harness, loggedIn.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.ELEVATED, mfa_method: 'totp', mfa_code: totpCodeNow(TOTP_SECRET)})
			.expect(HTTP_STATUS.OK)
			.execute();
		const updated = await createBuilder<GuildResponse>(harness, loggedIn.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.NONE, mfa_method: 'totp', mfa_code: totpCodeNow(TOTP_SECRET)})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(updated.mfa_level).toBe(GuildMFALevel.NONE);
	});
	it('rejects mfa_level change from non-owner', async () => {
		const {members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.ELEVATED, password: member.password})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	it('requires 2FA for channel permission overwrite edits in an elevated guild', async () => {
		const {owner, members, guild, channels} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const channel = channels[0]!;
		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Managers',
			permissions: (Permissions.MANAGE_ROLES | Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
		});
		const targetRole = await createRole(harness, owner.token, guild.id, {name: 'Target'});
		await addMemberRole(harness, owner.token, guild.id, member.userId, managerRole.id);
		await enableTotp(harness, owner);
		const loggedInOwner = await loginWithTotp(harness, owner);
		await createBuilder<GuildResponse>(harness, loggedInOwner.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.ELEVATED, mfa_method: 'totp', mfa_code: totpCodeNow(TOTP_SECRET)})
			.expect(HTTP_STATUS.OK)
			.execute();
		const overwrite = {type: 0, allow: Permissions.SEND_MESSAGES.toString(), deny: '0'};
		await createBuilder(harness, member.token)
			.put(`/channels/${channel.id}/permissions/${targetRole.id}`)
			.body(overwrite)
			.expect(HTTP_STATUS.BAD_REQUEST, 'TWO_FACTOR_REQUIRED')
			.execute();
		await createBuilder(harness, loggedInOwner.token)
			.put(`/channels/${channel.id}/permissions/${targetRole.id}`)
			.body(overwrite)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, member.token)
			.delete(`/channels/${channel.id}/permissions/${targetRole.id}`)
			.expect(HTTP_STATUS.BAD_REQUEST, 'TWO_FACTOR_REQUIRED')
			.execute();
		const stored = await getChannel(harness, loggedInOwner.token, channel.id);
		expect(stored.permission_overwrites?.find((entry) => entry.id === targetRole.id)?.allow).toBe(
			Permissions.SEND_MESSAGES.toString(),
		);
	});
	const setupModeratorInStatusCommunity = async (feature: string) => {
		const {owner, members, guild, channels} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const channel = channels[0]!;
		const managerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Managers',
			permissions: (Permissions.MANAGE_ROLES | Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
		});
		const targetRole = await createRole(harness, owner.token, guild.id, {name: 'Target'});
		await addMemberRole(harness, owner.token, guild.id, member.userId, managerRole.id);
		await createBuilder(harness, '')
			.post(`/test/guilds/${guild.id}/features`)
			.body({add_features: [feature]})
			.expect(HTTP_STATUS.OK)
			.execute();
		return {owner, member, guild, channel, targetRole};
	};
	const withSelfHosted = async (selfHosted: boolean, run: () => Promise<void>) => {
		const previous = Config.instance.selfHosted;
		Config.instance.selfHosted = selfHosted;
		try {
			await run();
		} finally {
			Config.instance.selfHosted = previous;
		}
	};
	for (const feature of [GuildFeatures.VERIFIED, GuildFeatures.PARTNERED]) {
		it(`always requires 2FA for moderators in a ${feature} community`, async () => {
			const {owner, member, guild, channel, targetRole} = await setupModeratorInStatusCommunity(feature);
			await withSelfHosted(false, async () => {
				const fetched = await createBuilder<GuildResponse>(harness, owner.token)
					.get(`/guilds/${guild.id}`)
					.expect(HTTP_STATUS.OK)
					.execute();
				expect(fetched.mfa_level).toBe(GuildMFALevel.ELEVATED);
				await createBuilder(harness, member.token)
					.put(`/channels/${channel.id}/permissions/${targetRole.id}`)
					.body({type: 0, allow: Permissions.SEND_MESSAGES.toString(), deny: '0'})
					.expect(HTTP_STATUS.BAD_REQUEST, 'TWO_FACTOR_REQUIRED')
					.execute();
			});
		});
	}
	it('leaves moderator 2FA to the community on self-hosted instances', async () => {
		const {owner, member, guild, channel, targetRole} = await setupModeratorInStatusCommunity(GuildFeatures.VERIFIED);
		await withSelfHosted(true, async () => {
			const fetched = await createBuilder<GuildResponse>(harness, owner.token)
				.get(`/guilds/${guild.id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(fetched.mfa_level).toBe(GuildMFALevel.NONE);
			await createBuilder(harness, member.token)
				.put(`/channels/${channel.id}/permissions/${targetRole.id}`)
				.body({type: 0, allow: Permissions.SEND_MESSAGES.toString(), deny: '0'})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
	});
});
