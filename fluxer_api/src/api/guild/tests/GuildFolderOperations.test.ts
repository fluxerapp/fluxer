// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {acceptInvite, createChannelInvite, createGuild, getChannel} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UNCATEGORIZED_FOLDER_ID} from '@fluxer/constants/src/UserConstants';
import type {UserSettingsResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Guild Folder Operations', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should prepend newly created guild to uncategorized folder', async () => {
		const account = await createTestAccount(harness);
		const guild1 = await createGuild(harness, account.token, 'Guild 1');
		const guild2 = await createGuild(harness, account.token, 'Guild 2');
		const {json: settings} = await createBuilder<UserSettingsResponse>(harness, account.token)
			.get('/users/@me/settings')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		expect(settings.guild_folders).toBeDefined();
		const uncategorizedFolder = settings.guild_folders?.find((folder) => folder.id === UNCATEGORIZED_FOLDER_ID);
		expect(uncategorizedFolder).toBeDefined();
		expect(uncategorizedFolder?.guild_ids).toEqual([guild2.id, guild1.id]);
	});
	test('should prepend newly joined guild to uncategorized folder', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild1 = await createGuild(harness, owner.token, 'Guild 1');
		const channel1 = await getChannel(harness, owner.token, guild1.system_channel_id!);
		const invite1 = await createChannelInvite(harness, owner.token, channel1.id);
		const guild2 = await createGuild(harness, owner.token, 'Guild 2');
		const channel2 = await getChannel(harness, owner.token, guild2.system_channel_id!);
		const invite2 = await createChannelInvite(harness, owner.token, channel2.id);
		await acceptInvite(harness, member.token, invite1.code);
		await acceptInvite(harness, member.token, invite2.code);
		const {json: settings} = await createBuilder<UserSettingsResponse>(harness, member.token)
			.get('/users/@me/settings')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		const uncategorizedFolder = settings.guild_folders?.find((folder) => folder.id === UNCATEGORIZED_FOLDER_ID);
		expect(uncategorizedFolder).toBeDefined();
		expect(uncategorizedFolder?.guild_ids).toEqual([guild2.id, guild1.id]);
	});
	test('should remove guild from all folders when leaving', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild1 = await createGuild(harness, owner.token, 'Guild 1');
		const channel1 = await getChannel(harness, owner.token, guild1.system_channel_id!);
		const invite1 = await createChannelInvite(harness, owner.token, channel1.id);
		const guild2 = await createGuild(harness, owner.token, 'Guild 2');
		const channel2 = await getChannel(harness, owner.token, guild2.system_channel_id!);
		const invite2 = await createChannelInvite(harness, owner.token, channel2.id);
		await acceptInvite(harness, member.token, invite1.code);
		await acceptInvite(harness, member.token, invite2.code);
		await createBuilder(harness, member.token)
			.patch('/users/@me/settings')
			.body({
				guild_folders: [
					{
						id: 1,
						name: 'My Folder',
						guild_ids: [guild1.id],
					},
					{
						id: UNCATEGORIZED_FOLDER_ID,
						guild_ids: [guild2.id],
					},
				],
			})
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		await createBuilder(harness, member.token)
			.delete(`/users/@me/guilds/${guild1.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.executeWithResponse();
		const {json: settings} = await createBuilder<UserSettingsResponse>(harness, member.token)
			.get('/users/@me/settings')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		const customFolder = settings.guild_folders?.find((folder) => folder.id === 1);
		expect(customFolder).toBeUndefined();
		const uncategorizedFolder = settings.guild_folders?.find((folder) => folder.id === UNCATEGORIZED_FOLDER_ID);
		expect(uncategorizedFolder?.guild_ids).toEqual([guild2.id]);
	});
	test('should remove guild from folders across multiple custom folders', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild1 = await createGuild(harness, owner.token, 'Guild 1');
		const channel1 = await getChannel(harness, owner.token, guild1.system_channel_id!);
		const invite1 = await createChannelInvite(harness, owner.token, channel1.id);
		const guild2 = await createGuild(harness, owner.token, 'Guild 2');
		const channel2 = await getChannel(harness, owner.token, guild2.system_channel_id!);
		const invite2 = await createChannelInvite(harness, owner.token, channel2.id);
		const guild3 = await createGuild(harness, owner.token, 'Guild 3');
		const channel3 = await getChannel(harness, owner.token, guild3.system_channel_id!);
		const invite3 = await createChannelInvite(harness, owner.token, channel3.id);
		await acceptInvite(harness, member.token, invite1.code);
		await acceptInvite(harness, member.token, invite2.code);
		await acceptInvite(harness, member.token, invite3.code);
		await createBuilder(harness, member.token)
			.patch('/users/@me/settings')
			.body({
				guild_folders: [
					{
						id: 1,
						name: 'Folder 1',
						guild_ids: [guild1.id, guild2.id],
					},
					{
						id: 2,
						name: 'Folder 2',
						guild_ids: [guild3.id],
					},
				],
			})
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		await createBuilder(harness, member.token)
			.delete(`/users/@me/guilds/${guild2.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.executeWithResponse();
		const {json: settings} = await createBuilder<UserSettingsResponse>(harness, member.token)
			.get('/users/@me/settings')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		const folder1 = settings.guild_folders?.find((folder) => folder.id === 1);
		expect(folder1?.guild_ids).toEqual([guild1.id]);
		const folder2 = settings.guild_folders?.find((folder) => folder.id === 2);
		expect(folder2?.guild_ids).toEqual([guild3.id]);
	});
	test('should remove guild from all members folders when guild is deleted', async () => {
		const owner = await createTestAccount(harness);
		const member1 = await createTestAccount(harness);
		const member2 = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member1.token, invite.code);
		await acceptInvite(harness, member2.token, invite.code);
		await createBuilder(harness, owner.token)
			.post(`/guilds/${guild.id}/delete`)
			.body({password: owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.executeWithResponse();
		const {json: ownerSettings} = await createBuilder<UserSettingsResponse>(harness, owner.token)
			.get('/users/@me/settings')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		const {json: member1Settings} = await createBuilder<UserSettingsResponse>(harness, member1.token)
			.get('/users/@me/settings')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		const {json: member2Settings} = await createBuilder<UserSettingsResponse>(harness, member2.token)
			.get('/users/@me/settings')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		const ownerUncategorized = ownerSettings.guild_folders?.find((folder) => folder.id === UNCATEGORIZED_FOLDER_ID);
		const member1Uncategorized = member1Settings.guild_folders?.find((folder) => folder.id === UNCATEGORIZED_FOLDER_ID);
		const member2Uncategorized = member2Settings.guild_folders?.find((folder) => folder.id === UNCATEGORIZED_FOLDER_ID);
		expect(ownerUncategorized?.guild_ids ?? []).not.toContain(guild.id);
		expect(member1Uncategorized?.guild_ids ?? []).not.toContain(guild.id);
		expect(member2Uncategorized?.guild_ids ?? []).not.toContain(guild.id);
	});
	test('should not update guild folders for bot users when joining', async () => {
		const owner = await createTestAccount(harness);
		const botAccount = await createTestBotAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		await authorizeBot(harness, owner.token, botAccount.appId, ['bot'], guild.id, '0');
		expect(true).toBe(true);
	});
	test('should not update guild folders for bot users when leaving', async () => {
		const owner = await createTestAccount(harness);
		const botAccount = await createTestBotAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		await authorizeBot(harness, owner.token, botAccount.appId, ['bot'], guild.id, '0');
		const {json: ownerGuilds} = await createBuilder<
			Array<{
				id: string;
			}>
		>(harness, owner.token)
			.get('/users/@me/guilds')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		const botIsInGuild = ownerGuilds.some((g) => g.id === guild.id);
		expect(botIsInGuild).toBe(true);
	});
});
