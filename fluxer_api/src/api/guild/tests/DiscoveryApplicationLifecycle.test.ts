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
import type {DiscoveryApplicationResponse} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const TOTP_SECRET = 'JBSWY3DPEHPK3PXP';

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.execute();
}

async function addGuildFeatureForTesting(harness: ApiTestHarness, guildId: string, feature: string): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/features`)
		.body({add_features: [feature]})
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

async function adminApprove(
	harness: ApiTestHarness,
	adminToken: string,
	guildId: string,
	reason?: string,
): Promise<DiscoveryApplicationResponse> {
	return createBuilder<DiscoveryApplicationResponse>(harness, `${adminToken}`)
		.patch(`/admin/discovery/applications/${guildId}`)
		.body({status: 'approved', reason})
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

async function createAdminAccount(harness: ApiTestHarness) {
	const admin = await createTestAccount(harness);
	return setUserACLs(harness, admin, ['admin:authenticate', 'discovery:review', 'discovery:remove']);
}

describe('Discovery Application Lifecycle', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should auto-approve applications for verified and partnered guilds', async () => {
		const autoApprovedFeatures = [GuildFeatures.VERIFIED, GuildFeatures.PARTNERED];
		for (const feature of autoApprovedFeatures) {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, `Auto-approved ${feature} Guild`);
			await setGuildMemberCount(harness, guild.id, 1);
			await addGuildFeatureForTesting(harness, guild.id, feature);
			const application = await applyForDiscovery(harness, owner.token, guild.id);
			expect(application.status).toBe('approved');
			expect(application.reviewed_at).toBeTruthy();
			expect(application.review_reason).toBeNull();
			const guildData = await getGuild(harness, owner.token, guild.id);
			expect(guildData.features).toContain(GuildFeatures.DISCOVERABLE);
		}
	});
	test('should complete full lifecycle: apply → approve → verify feature → withdraw', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Full Lifecycle Guild');
		await setGuildMemberCount(harness, guild.id, 1);
		const admin = await createAdminAccount(harness);
		const application = await applyForDiscovery(harness, owner.token, guild.id);
		expect(application.status).toBe('pending');
		const approved = await adminApprove(harness, admin.token, guild.id, 'Meets all criteria');
		expect(approved.status).toBe('approved');
		expect(approved.reviewed_at).toBeTruthy();
		expect(approved.review_reason).toBe('Meets all criteria');
		const guildData = await getGuild(harness, owner.token, guild.id);
		expect(guildData.features).toContain(GuildFeatures.DISCOVERABLE);
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/discovery`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const guildAfterWithdraw = await getGuild(harness, owner.token, guild.id);
		expect(guildAfterWithdraw.features).not.toContain(GuildFeatures.DISCOVERABLE);
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
