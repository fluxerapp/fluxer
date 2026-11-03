// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount, totpCodeNow} from '@app/api/auth/tests/AuthTestUtils';
import {
	addMemberRole,
	createGuild,
	createRole,
	getGuild,
	setupTestGuildWithMembers,
} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryCategories, type DiscoveryCategory} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures, GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import type {
	DiscoveryAdminPendingApplicationResponse,
	DiscoveryApplicationResponse,
	DiscoveryStatusResponse,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const TOTP_SECRET = 'JBSWY3DPEHPK3PXP';

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.execute();
}

async function applyForDiscovery(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	description = 'A great community for testing discovery features',
	categoryId: DiscoveryCategory = DiscoveryCategories.GAMING,
): Promise<DiscoveryApplicationResponse> {
	return createBuilder<DiscoveryApplicationResponse>(harness, token)
		.post(`/guilds/${guildId}/discovery`)
		.body({description, category_type: categoryId})
		.expect(HTTP_STATUS.OK)
		.execute();
}

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

async function adminAddFeature(harness: ApiTestHarness, adminToken: string, guildId: string, feature: string) {
	await createBuilder(harness, adminToken)
		.patch(`/admin/guilds/${guildId}`)
		.body({add_features: [feature]})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function getDiscoveryStatus(harness: ApiTestHarness, token: string, guildId: string) {
	return createBuilder<DiscoveryStatusResponse>(harness, token)
		.get(`/guilds/${guildId}/discovery`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function listQueueGuildIds(harness: ApiTestHarness, adminToken: string, path: string): Promise<Array<string>> {
	const rows = await createBuilder<Array<DiscoveryAdminPendingApplicationResponse>>(harness, adminToken)
		.get(path)
		.expect(HTTP_STATUS.OK)
		.execute();
	return rows.map((row) => row.guild_id);
}

describe('Discovery Application Lifecycle', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('keeps a pending application in review when a newly verified guild is below the member minimum', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'discovery:review',
			'guild:update:features',
		]);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Pending Small Verified Guild');
		await setGuildMemberCount(harness, guild.id, 1);
		await applyForDiscovery(harness, owner.token, guild.id);
		await setGuildMemberCount(harness, guild.id, 0);
		await adminAddFeature(harness, admin.token, guild.id, GuildFeatures.VERIFIED);
		const status = await getDiscoveryStatus(harness, owner.token, guild.id);
		expect(status.application?.status).toBe('pending');
		expect((await getGuild(harness, owner.token, guild.id)).features).not.toContain(GuildFeatures.DISCOVERABLE);
		expect(await listQueueGuildIds(harness, admin.token, '/admin/discovery/applications')).toContain(guild.id);
	});
	test('still rejects a second application from an unverified guild with one pending', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Pending Reapply Guild');
		await setGuildMemberCount(harness, guild.id, 1);
		await applyForDiscovery(harness, owner.token, guild.id);
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guild.id}/discovery`)
			.body({
				description: 'A great community for testing discovery features',
				category_type: DiscoveryCategories.GAMING,
			})
			.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.DISCOVERY_ALREADY_APPLIED)
			.execute();
	});
	test('rejects apply with TWO_FACTOR_REQUIRED for a non-owner without MFA in an elevated guild', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const manageGuildRole = await createRole(harness, owner.token, guild.id, {
			name: 'Manage Guild',
			permissions: Permissions.MANAGE_GUILD.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, manageGuildRole.id);
		await enableTotp(harness, owner);
		const elevatedOwner = await loginWithTotp(harness, owner);
		await createBuilder(harness, elevatedOwner.token)
			.patch(`/guilds/${guild.id}`)
			.body({mfa_level: GuildMFALevel.ELEVATED, mfa_method: 'totp', mfa_code: totpCodeNow(TOTP_SECRET)})
			.expect(HTTP_STATUS.OK)
			.execute();
		await setGuildMemberCount(harness, guild.id, 1);
		await createBuilder(harness, member.token)
			.post(`/guilds/${guild.id}/discovery`)
			.body({description: 'Moderator without an authenticator', category_type: DiscoveryCategories.GAMING})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.TWO_FACTOR_REQUIRED)
			.execute();
	});
});
