// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount, totpCodeNow} from '@app/api/auth/tests/AuthTestUtils';
import {createRole, setupTestGuildWithMembers, updateMember} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, wait} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import type {GuildMemberSearchResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSearchSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const TOTP_SECRET = 'JBSWY3DPEHPK3PXP';

async function elevateGuildMfaLevel(harness: ApiTestHarness, owner: TestAccount, guildId: string): Promise<void> {
	await createBuilder(harness, owner.token)
		.post('/users/@me/mfa/totp/enable')
		.body({secret: TOTP_SECRET, code: totpCodeNow(TOTP_SECRET), password: owner.password})
		.expect(HTTP_STATUS.OK)
		.execute();
	const loginResp = await createBuilderWithoutAuth<{
		mfa: true;
		ticket: string;
	}>(harness)
		.post('/auth/login')
		.body({email: owner.email, password: owner.password})
		.expect(HTTP_STATUS.OK)
		.execute();
	const mfaResp = await createBuilderWithoutAuth<{
		token: string;
	}>(harness)
		.post('/auth/login/mfa/totp')
		.body({code: totpCodeNow(TOTP_SECRET), ticket: loginResp.ticket})
		.expect(HTTP_STATUS.OK)
		.execute();
	await createBuilder(harness, mfaResp.token)
		.patch(`/guilds/${guildId}`)
		.body({mfa_level: GuildMFALevel.ELEVATED, mfa_method: 'totp', mfa_code: totpCodeNow(TOTP_SECRET)})
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function indexGuildMembers(harness: ApiTestHarness, guildId: string): Promise<void> {
	await createBuilder<{
		guild_id: string;
		indexed_at: string;
		members_indexed: number;
	}>(harness, '')
		.post(`/test/guilds/${guildId}/mark-members-indexed`)
		.body({})
		.execute();
	await wait(50);
}

describe('Guild Member Search Endpoint', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness.shutdown();
	});
	describe('Permissions', () => {
		test('rejects member without member management permissions (403)', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const regularMember = members[0]!;
			await indexGuildMembers(harness, guild.id);
			await createBuilder(harness, regularMember.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
		test('allows guild owner to search', async () => {
			const {owner, guild} = await setupTestGuildWithMembers(harness, 1);
			await indexGuildMembers(harness, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(0);
		});
		test('rejects an elevated-permission holder without MFA in an elevated guild (400)', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const moderator = members[0]!;
			const banRole = await createRole(harness, owner.token, guild.id, {
				name: 'Ban Members',
				permissions: Permissions.BAN_MEMBERS.toString(),
			});
			await updateMember(harness, owner.token, guild.id, moderator.userId, {roles: [banRole.id]});
			await indexGuildMembers(harness, guild.id);
			await elevateGuildMfaLevel(harness, owner, guild.id);
			await createBuilder(harness, moderator.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'TWO_FACTOR_REQUIRED')
				.execute();
		});
		test('allows a MANAGE_NICKNAMES holder without MFA in an elevated guild', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const nicknameManager = members[0]!;
			const nicknameRole = await createRole(harness, owner.token, guild.id, {
				name: 'Manage Nicknames',
				permissions: Permissions.MANAGE_NICKNAMES.toString(),
			});
			await updateMember(harness, owner.token, guild.id, nicknameManager.userId, {roles: [nicknameRole.id]});
			await indexGuildMembers(harness, guild.id);
			await elevateGuildMfaLevel(harness, owner, guild.id);
			const result = await createBuilder<GuildMemberSearchResponse>(harness, nicknameManager.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.members.length).toBeGreaterThan(0);
		});
		test('returns 404 for non-guild-member', async () => {
			const {guild} = await setupTestGuildWithMembers(harness, 1);
			const outsider = await createTestAccount(harness);
			await indexGuildMembers(harness, guild.id);
			await createBuilder(harness, outsider.token)
				.post(`/guilds/${guild.id}/members-search`)
				.body({})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
				.execute();
		});
	});
});
