// SPDX-License-Identifier: AGPL-3.0-or-later

import {collectKnownUserIps} from '@app/api/admin/services/KnownUserIps';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {assertNotBannedFromGuild} from '@app/api/guild/services/GuildBanChecks';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getGuildRepository, getUserActivityBuffer, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {banUser} from '@app/api/moderation/tests/ModerationTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const BOT_HOST_IP = '203.0.113.40';

async function recordBotActivity(harness: ApiTestHarness, botToken: string, ip: string): Promise<void> {
	await createBuilder(harness, `Bot ${botToken}`)
		.get('/users/@me')
		.header('x-forwarded-for', ip)
		.expect(HTTP_STATUS.OK)
		.execute();
	await getUserActivityBuffer().drainAndFlush();
}

describe('Bot activity', () => {
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

	test('a bot request records its last active time and IP', async () => {
		const bot = await createTestBotAccount(harness);
		const botUserId = createUserID(BigInt(bot.botUserId));
		await recordBotActivity(harness, bot.botToken, BOT_HOST_IP);
		const tracking = await getUserRepository().getActivityTracking(botUserId);
		expect(tracking?.last_active_ip).toBe(BOT_HOST_IP);
		expect(tracking?.last_active_at).toBeInstanceOf(Date);
	});

	test('a bot is left out of the IP lookups used to link accounts', async () => {
		const bot = await createTestBotAccount(harness);
		await recordBotActivity(harness, bot.botToken, BOT_HOST_IP);
		const userRepository = getUserRepository();
		const indexed = await userRepository.listUserIdsByLastActiveIp(BOT_HOST_IP, 10, 0);
		expect(indexed.userIds.map((id) => id.toString())).not.toContain(bot.botUserId);
		const botUser = await userRepository.findUnique(createUserID(BigInt(bot.botUserId)));
		expect(botUser).not.toBeNull();
		expect(await collectKnownUserIps(userRepository, botUser!)).not.toContain(BOT_HOST_IP);
	});

	test('banning a bot bans no IP', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Bot Ban Guild');
		const bot = await createTestBotAccount(harness);
		await authorizeBot(harness, owner.token, bot.appId, ['bot'], guild.id, Permissions.ADMINISTRATOR.toString());
		await recordBotActivity(harness, bot.botToken, BOT_HOST_IP);
		await banUser(harness, owner.token, guild.id, bot.botUserId);
		const ban = await getGuildRepository().getBan(createGuildID(BigInt(guild.id)), createUserID(BigInt(bot.botUserId)));
		expect(ban).not.toBeNull();
		expect(ban?.ipAddress).toBeNull();
	});

	test('an IP ban on the host IP of a bot does not apply to the bot', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Shared Host Guild');
		const person = await createTestAccount(harness, {ipAddress: BOT_HOST_IP});
		await banUser(harness, owner.token, guild.id, person.userId);
		const guildId = createGuildID(BigInt(guild.id));
		const personBan = await getGuildRepository().getBan(guildId, createUserID(BigInt(person.userId)));
		expect(personBan?.ipAddress).toBe(BOT_HOST_IP);
		const bot = await createTestBotAccount(harness);
		await recordBotActivity(harness, bot.botToken, BOT_HOST_IP);
		await expect(
			assertNotBannedFromGuild(getGuildRepository(), getUserRepository(), {
				userId: createUserID(BigInt(bot.botUserId)),
				guildId,
				ip: null,
			}),
		).resolves.toBeUndefined();
	});
});
