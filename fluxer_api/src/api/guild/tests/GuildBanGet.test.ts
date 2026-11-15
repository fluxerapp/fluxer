// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {addMemberRole, createRole, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {GuildBanResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Get guild ban', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	test('returns the ban with its reason, matching the list entry', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${member.userId}`)
			.body({reason: 'Advertising'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const ban = await createBuilder<GuildBanResponse>(harness, owner.token)
			.get(`/guilds/${guild.id}/bans/${member.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(ban.user.id).toBe(member.userId);
		expect(ban.reason).toBe('Advertising');
		expect(ban.moderator_id).toBe(owner.userId);
		expect(ban.expires_at).toBeNull();
		const bans = await createBuilder<Array<GuildBanResponse>>(harness, owner.token)
			.get(`/guilds/${guild.id}/bans`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(bans.find((entry) => entry.user.id === member.userId)).toEqual(ban);
	});

	test('returns the expiry of a timed ban', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		const before = Date.now();
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${member.userId}`)
			.body({ban_duration_seconds: 3600})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const ban = await createBuilder<GuildBanResponse>(harness, owner.token)
			.get(`/guilds/${guild.id}/bans/${member.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(ban.expires_at).not.toBeNull();
		const expiresAt = new Date(ban.expires_at!).getTime();
		expect(expiresAt).toBeGreaterThanOrEqual(before + 3600 * 1000);
		expect(expiresAt).toBeLessThanOrEqual(Date.now() + 3600 * 1000);
	});

	test('returns 404 UNKNOWN_BAN for an account that is not banned', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/bans/${member.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_BAN)
			.execute();
	});

	test('returns 404 UNKNOWN_BAN after the ban is removed', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${member.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, owner.token)
			.delete(`/guilds/${guild.id}/bans/${member.userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/bans/${member.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_BAN)
			.execute();
	});

	test('requires BAN_MEMBERS', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [moderator, target] = members;
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${target.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, moderator.token)
			.get(`/guilds/${guild.id}/bans/${target.userId}`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		const role = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: Permissions.BAN_MEMBERS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, moderator.userId, role.id);
		const ban = await createBuilder<GuildBanResponse>(harness, moderator.token)
			.get(`/guilds/${guild.id}/bans/${target.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(ban.user.id).toBe(target.userId);
	});

	test('answers an outsider and an unknown guild like the ban list does', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		const outsider = await createTestAccount(harness);
		const unknownGuildId = '1234567890123456789';
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${member.userId}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		for (const path of [`/guilds/${guild.id}/bans`, `/guilds/${guild.id}/bans/${member.userId}`]) {
			await createBuilder(harness, outsider.token)
				.get(path)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		}
		for (const path of [`/guilds/${unknownGuildId}/bans`, `/guilds/${unknownGuildId}/bans/${member.userId}`]) {
			await createBuilder(harness, owner.token)
				.get(path)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		}
	});

	test('a bot with BAN_MEMBERS can get a ban', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		const bot = await createTestBotAccount(harness);
		await authorizeBot(harness, owner.token, bot.appId, ['bot'], guild.id, Permissions.BAN_MEMBERS.toString());
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${member.userId}`)
			.body({reason: 'Raid'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const ban = await createBuilder<GuildBanResponse>(harness, `Bot ${bot.botToken}`)
			.get(`/guilds/${guild.id}/bans/${member.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(ban.user.id).toBe(member.userId);
		expect(ban.reason).toBe('Raid');
	});
});
