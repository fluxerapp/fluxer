// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getGuildChannels, getRoles} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {SystemChannelFlags} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Guild Template Import', () => {
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
	test('imports @everyone role from template using role name instead of only ID=0', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Template Guild',
				template: {
					name: 'Template Source',
					description: null,
					verification_level: 1,
					default_message_notifications: 1,
					explicit_content_filter: 1,
					system_channel_id: 2001,
					afk_timeout: 300,
					system_channel_flags: 0,
					roles: [
						{
							id: 987654321,
							name: '@everyone',
							permissions: Permissions.VIEW_CHANNEL.toString(),
						},
						{
							id: 2222,
							name: 'Moderators',
							permissions: Permissions.MANAGE_MESSAGES.toString(),
							hoist: true,
							mentionable: true,
						},
					],
					channels: [
						{
							id: 2001,
							type: ChannelTypes.GUILD_TEXT,
							name: 'general',
							position: 0,
						},
					],
				},
			})
			.execute();
		const roles = await getRoles(harness, account.token, guild.id);
		const everyoneRole = roles.find((role) => role.id === guild.id);
		const moderatorsRole = roles.find((role) => role.name === 'Moderators');
		expect(everyoneRole).toBeDefined();
		expect(everyoneRole?.name).toBe('@everyone');
		expect(everyoneRole?.position).toBe(0);
		expect(BigInt(everyoneRole?.permissions ?? '0')).toBe(Permissions.VIEW_CHANNEL);
		expect(moderatorsRole).toBeDefined();
		expect(moderatorsRole?.position).toBe(1);
	});
	test('sanitises template-level guild settings during import', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Sanitised Guild',
				template: {
					name: 'Template Source',
					description: null,
					verification_level: 999,
					default_message_notifications: 42,
					explicit_content_filter: 42,
					system_channel_id: 3001,
					afk_timeout: 5,
					system_channel_flags: SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS | (1 << 12),
					roles: [
						{
							id: 0,
							name: '@everyone',
							permissions: DEFAULT_EVERYONE_PERMISSIONS,
						},
					],
					channels: [
						{
							id: 3001,
							type: ChannelTypes.GUILD_TEXT,
							name: 'general',
							position: 0,
						},
					],
				},
			})
			.execute();
		expect(guild.verification_level).toBe(4);
		expect(guild.default_message_notifications).toBe(1);
		expect(guild.explicit_content_filter).toBe(2);
		expect(guild.system_channel_flags).toBe(SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS);
		expect(guild.afk_timeout).toBe(60);
	});
	test('maps supported competitor-only channel types to Fluxer equivalents and ignores unsupported ones', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Compatibility Guild',
				template: {
					name: 'Template Source',
					description: null,
					verification_level: 0,
					default_message_notifications: 0,
					explicit_content_filter: 0,
					system_channel_id: '4102',
					afk_timeout: 300,
					system_channel_flags: 0,
					roles: [
						{
							id: '0',
							name: '@everyone',
							permissions: DEFAULT_EVERYONE_PERMISSIONS,
						},
					],
					channels: [
						{
							id: '4101',
							type: ChannelTypes.GUILD_CATEGORY,
							name: 'Main',
							position: 0,
						},
						{
							id: '4102',
							type: 5,
							name: 'announcements',
							parent_id: '4101',
							position: 1,
						},
						{
							id: '4103',
							type: 13,
							name: 'town-hall',
							parent_id: '4101',
							position: 2,
						},
						{
							id: '4104',
							type: 16,
							name: 'media-feed',
							parent_id: '4101',
							position: 3,
						},
					],
				},
			})
			.execute();
		const channels = await getGuildChannels(harness, account.token, guild.id);
		const categoryChannel = channels.find((channel) => channel.name === 'Main');
		const announcementsChannel = channels.find((channel) => channel.name === 'announcements');
		const stageChannel = channels.find((channel) => channel.name === 'town-hall');
		const mediaChannel = channels.find((channel) => channel.name === 'media-feed');
		expect(categoryChannel).toBeDefined();
		expect(announcementsChannel).toBeDefined();
		expect(announcementsChannel?.type).toBe(ChannelTypes.GUILD_TEXT);
		expect(announcementsChannel?.parent_id).toBe(categoryChannel?.id ?? null);
		expect(stageChannel).toBeDefined();
		expect(stageChannel?.type).toBe(ChannelTypes.GUILD_VOICE);
		expect(stageChannel?.parent_id).toBe(categoryChannel?.id ?? null);
		expect(mediaChannel).toBeUndefined();
		expect(guild.system_channel_id).toBe(announcementsChannel?.id ?? null);
	});
	test('accepts template roles and channels with empty names', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Empty Names Guild',
				template: {
					name: 'Template Source',
					description: null,
					verification_level: 0,
					default_message_notifications: 0,
					explicit_content_filter: 0,
					system_channel_id: 5001,
					afk_timeout: 300,
					system_channel_flags: 0,
					roles: [
						{id: 0, name: '@everyone', permissions: DEFAULT_EVERYONE_PERMISSIONS},
						{id: 5100, name: '', permissions: DEFAULT_EVERYONE_PERMISSIONS},
					],
					channels: [
						{id: 5001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0},
						{id: 5002, type: ChannelTypes.GUILD_TEXT, name: '', position: 1},
					],
				},
			})
			.execute();
		const roles = await getRoles(harness, account.token, guild.id);
		const channels = await getGuildChannels(harness, account.token, guild.id);
		expect(roles.some((role) => role.name === '')).toBe(true);
		expect(channels.some((channel) => channel.name === '')).toBe(true);
	});
});

const DEFAULT_EVERYONE_PERMISSIONS = Permissions.VIEW_CHANNEL.toString();
