// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID} from '@app/api/BrandedTypes';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import {getGuildChannels, getRoles} from '@app/api/guild/tests/GuildTestUtils';
import {ChannelsByGuild, ThreadOnlyChannelsByGuild} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {SystemChannelFlags} from '@fluxer/constants/src/GuildConstants';
import {CHANNEL_TOPIC_MAX_LENGTH, VOICE_CHANNEL_USER_LIMIT_MAX} from '@fluxer/constants/src/LimitConstants';
import {ChannelFlags, ForumLayoutTypes, ForumSortOrderTypes} from '@fluxer/constants/src/ThreadConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
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
		expect(guild.verification_level).toBe(3);
		expect(guild.default_message_notifications).toBe(1);
		expect(guild.explicit_content_filter).toBe(2);
		expect(guild.system_channel_flags).toBe(SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS);
		expect(guild.afk_timeout).toBe(60);
	});
	test('imports announcement, stage, forum and media channels and ignores unsupported types', async () => {
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
							type: ChannelTypes.GUILD_ANNOUNCEMENT,
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
							type: ChannelTypes.GUILD_MEDIA,
							name: 'media-feed',
							parent_id: '4101',
							position: 3,
						},
						{
							id: '4105',
							type: ChannelTypes.GUILD_FORUM,
							name: 'help',
							parent_id: '4101',
							position: 4,
							rate_limit_per_user: 120,
						},
						{
							id: '4106',
							type: 14,
							name: 'directory',
							parent_id: '4101',
							position: 5,
						},
					],
				},
			})
			.execute();
		const channels = await listThreadAwareChannels(account.token, guild.id);
		const categoryChannel = channels.find((channel) => channel.name === 'Main');
		const announcementsChannel = channels.find((channel) => channel.name === 'announcements');
		const stageChannel = channels.find((channel) => channel.name === 'town-hall');
		const mediaChannel = channels.find((channel) => channel.name === 'media-feed');
		expect(categoryChannel).toBeDefined();
		expect(announcementsChannel).toBeDefined();
		expect(announcementsChannel?.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(announcementsChannel?.parent_id).toBe(categoryChannel?.id ?? null);
		expect(stageChannel).toBeDefined();
		expect(stageChannel?.type).toBe(ChannelTypes.GUILD_VOICE);
		expect(stageChannel?.parent_id).toBe(categoryChannel?.id ?? null);
		const forumChannel = channels.find((channel) => channel.name === 'help');
		expect(mediaChannel?.type).toBe(ChannelTypes.GUILD_MEDIA);
		expect(mediaChannel?.parent_id).toBe(categoryChannel?.id ?? null);
		expect(mediaChannel?.position).toBe(3);
		expect(forumChannel?.type).toBe(ChannelTypes.GUILD_FORUM);
		expect(forumChannel?.parent_id).toBe(categoryChannel?.id ?? null);
		expect(forumChannel?.rate_limit_per_user).toBe(120);
		expect(channels.find((channel) => channel.name === 'directory')).toBeUndefined();
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
	test.each([
		['a negative slowmode', {rate_limit_per_user: -1}],
		['a slowmode above the channel maximum', {rate_limit_per_user: 1_000_000_000}],
		['a fractional position', {position: 0.5}],
		['a negative position', {position: -3}],
		['a topic above the template maximum', {topic: 'x'.repeat(4097)}],
		['a name above the channel maximum', {name: 'x'.repeat(101)}],
		['a negative user limit', {type: ChannelTypes.GUILD_VOICE, user_limit: -1}],
		['a voice connection limit above the maximum', {type: ChannelTypes.GUILD_VOICE, voice_connection_limit: 100_000}],
		['a negative voice connection limit', {type: ChannelTypes.GUILD_VOICE, voice_connection_limit: -5}],
	])('rejects a template channel with %s', async (_label, overrides) => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Bounded Guild',
				template: buildMinimalTemplate({
					channels: [{id: 6001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0, ...overrides}],
				}),
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
	test.each([
		['a negative colour', {color: -1}],
		['a colour above 0xffffff', {color: 0x1000000}],
		['a name above the role maximum', {name: 'x'.repeat(101)}],
	])('rejects a template role with %s', async (_label, overrides) => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Bounded Guild',
				template: buildMinimalTemplate({
					roles: [
						{id: 0, name: '@everyone', permissions: DEFAULT_EVERYONE_PERMISSIONS},
						{id: 6100, name: 'Role', permissions: '0', ...overrides},
					],
				}),
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
	test('clamps imported voice user limits to the channel maximum and keeps channels readable', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Stage Guild',
				template: buildMinimalTemplate({
					channels: [
						{id: 6001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0, rate_limit_per_user: 30},
						{id: 6002, type: 13, name: 'town-hall', position: 1, user_limit: 10_000},
						{id: 6003, type: ChannelTypes.GUILD_VOICE, name: 'lounge', position: 2, voice_connection_limit: 100},
					],
				}),
			})
			.execute();
		const channels = await getGuildChannels(harness, account.token, guild.id);
		expect(channels.find((channel) => channel.name === 'general')?.rate_limit_per_user).toBe(30);
		expect(channels.find((channel) => channel.name === 'town-hall')?.user_limit).toBe(VOICE_CHANNEL_USER_LIMIT_MAX);
		expect(channels.find((channel) => channel.name === 'lounge')?.voice_connection_limit).toBe(100);
	});
	test('accepts forum-length topics and shortens them only for non-forum channels', async () => {
		const account = await createTestAccount(harness);
		const longTopic = `${'a'.repeat(CHANNEL_TOPIC_MAX_LENGTH - 1)}\u{1F600}${'b'.repeat(300)}`;
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Forum Guild',
				template: buildMinimalTemplate({
					channels: [
						{id: 6001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0, topic: longTopic},
						{id: 6002, type: 15, name: 'projects', position: 1, topic: 'c'.repeat(1356)},
					],
				}),
			})
			.execute();
		const channels = await listThreadAwareChannels(account.token, guild.id);
		const general = channels.find((channel) => channel.name === 'general');
		expect(general?.topic).toBe('a'.repeat(CHANNEL_TOPIC_MAX_LENGTH - 1));
		expect(channels.find((channel) => channel.name === 'projects')?.topic).toBe('c'.repeat(1356));
	});
	test('imports forum settings and drops values that cannot carry over', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Forum Settings Guild',
				template: buildMinimalTemplate({
					channels: [
						{id: 6001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0},
						{
							id: 6002,
							type: ChannelTypes.GUILD_FORUM,
							name: 'support',
							position: 1,
							topic: 'Be kind',
							available_tags: [
								{id: 111, name: 'bug', moderated: false, emoji_id: null, emoji_name: '\u{1F41B}'},
								{id: 112, name: 'custom', moderated: false, emoji_id: '123456789', emoji_name: null},
								{id: 113, name: 'bug', moderated: true, emoji_id: null, emoji_name: null},
								{id: 114, name: 'staff', moderated: true, emoji_id: null, emoji_name: 'not-an-emoji'},
							],
							default_reaction_emoji: {emoji_id: '987654321', emoji_name: null},
							default_sort_order: ForumSortOrderTypes.CREATION_TIME,
							default_forum_layout: ForumLayoutTypes.GRID,
							default_tag_setting: 'match_all',
							default_thread_rate_limit_per_user: 60,
							default_auto_archive_duration: 1440,
							flags: ChannelFlags.REQUIRE_TAG | ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS,
						},
						{
							id: 6003,
							type: ChannelTypes.GUILD_MEDIA,
							name: 'gallery',
							position: 2,
							available_tags: Array.from({length: 25}, (_, index) => ({name: `tag-${index}`})),
							default_reaction_emoji: {emoji_id: null, emoji_name: '\u{1F525}'},
							default_forum_layout: ForumLayoutTypes.LIST,
							default_sort_order: 7,
							flags: ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS,
						},
						{
							id: 6004,
							type: ChannelTypes.GUILD_FORUM,
							name: 'announcements-forum',
							position: 3,
							available_tags: [{name: 'staff', moderated: true}],
							flags: ChannelFlags.REQUIRE_TAG,
						},
					],
				}),
			})
			.execute();
		const channels = await listThreadAwareChannels(account.token, guild.id);
		const forum = await getThreadAwareChannel(account.token, findChannelId(channels, 'support'));
		expect(forum.type).toBe(ChannelTypes.GUILD_FORUM);
		expect(forum.topic).toBe('Be kind');
		expect(
			forum.available_tags?.map(({name, moderated, emoji_id, emoji_name}) => ({name, moderated, emoji_id, emoji_name})),
		).toEqual([
			{name: 'bug', moderated: false, emoji_id: null, emoji_name: '\u{1F41B}'},
			{name: 'custom', moderated: false, emoji_id: null, emoji_name: null},
			{name: 'staff', moderated: true, emoji_id: null, emoji_name: null},
		]);
		expect(new Set(forum.available_tags?.map((tag) => tag.id)).size).toBe(3);
		expect(forum.available_tags?.some((tag) => ['111', '112', '113', '114'].includes(tag.id))).toBe(false);
		expect(forum.default_reaction_emoji).toBeNull();
		expect(forum.default_sort_order).toBe(ForumSortOrderTypes.CREATION_TIME);
		expect(forum.default_forum_layout).toBe(ForumLayoutTypes.GRID);
		expect(forum.default_tag_setting).toBe('match_all');
		expect(forum.default_thread_rate_limit_per_user).toBe(60);
		expect(forum.default_auto_archive_duration).toBe(1440);
		expect(forum.flags).toBe(ChannelFlags.REQUIRE_TAG);
		const media = await getThreadAwareChannel(account.token, findChannelId(channels, 'gallery'));
		expect(media.type).toBe(ChannelTypes.GUILD_MEDIA);
		expect(media.available_tags).toHaveLength(20);
		expect(media.default_reaction_emoji).toEqual({emoji_id: null, emoji_name: '\u{1F525}'});
		expect(media.default_forum_layout).toBeUndefined();
		expect(media.default_sort_order).toBeNull();
		expect(media.flags).toBe(ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS);
		const moderatedOnly = await getThreadAwareChannel(account.token, findChannelId(channels, 'announcements-forum'));
		expect(moderatedOnly.flags).toBe(0);
	});
	test('indexes imported forum and media channels as thread-only channels', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Index Guild',
				template: buildMinimalTemplate({
					channels: [
						{id: 6001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0},
						{id: 6002, type: ChannelTypes.GUILD_FORUM, name: 'forum', position: 1},
						{id: 6003, type: ChannelTypes.GUILD_MEDIA, name: 'media', position: 2},
					],
				}),
			})
			.execute();
		const channels = await listThreadAwareChannels(account.token, guild.id);
		const threadOnlyIds = [findChannelId(channels, 'forum'), findChannelId(channels, 'media')];
		const guildId = createGuildID(BigInt(guild.id));
		const threadOnlyRows = await fetchMany<{channel_id: bigint}>(
			ThreadOnlyChannelsByGuild.selectCql({where: ThreadOnlyChannelsByGuild.where.eq('guild_id')}),
			{guild_id: guildId},
		);
		const regularRows = await fetchMany<{channel_id: bigint}>(
			ChannelsByGuild.selectCql({where: ChannelsByGuild.where.eq('guild_id')}),
			{guild_id: guildId},
		);
		expect(threadOnlyRows.map((row) => row.channel_id.toString()).sort()).toEqual([...threadOnlyIds].sort());
		expect(regularRows.some((row) => threadOnlyIds.includes(row.channel_id.toString()))).toBe(false);
		expect(regularRows.some((row) => row.channel_id.toString() === findChannelId(channels, 'general'))).toBe(true);
	});
	test('accepts channels shaped like a full template export', async () => {
		const account = await createTestAccount(harness);
		const exported = {
			bitrate: 64000,
			user_limit: 0,
			nsfw: false,
			rate_limit_per_user: 0,
			topic: null,
			default_auto_archive_duration: null,
			permission_overwrites: [{id: 0, type: 0, allow: '0', deny: Permissions.SEND_MESSAGES.toString()}],
			available_tags: [],
			template: '',
			default_reaction_emoji: null,
			default_thread_rate_limit_per_user: 0,
			default_sort_order: null,
			default_forum_layout: 0,
			default_tag_setting: null,
			icon_emoji: null,
			theme_color: null,
		};
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Exported Guild',
				template: {
					...buildMinimalTemplate({
						channels: [
							{...exported, id: 1, type: ChannelTypes.GUILD_CATEGORY, name: 'Info', position: 0, parent_id: null},
							{...exported, id: 6001, type: ChannelTypes.GUILD_TEXT, name: 'rules', position: 0, parent_id: 1},
							{...exported, id: 3, type: ChannelTypes.GUILD_CATEGORY, name: 'Nested', position: 1, parent_id: 1},
							{
								...exported,
								id: 4,
								type: ChannelTypes.GUILD_FORUM,
								name: 'ideas',
								position: 1,
								parent_id: 1,
								nsfw: null,
							},
						],
					}),
					afk_channel_id: null,
					preferred_locale: 'en-US',
					region: 'us-west',
				},
			})
			.execute();
		const channels = await listThreadAwareChannels(account.token, guild.id);
		const category = channels.find((channel) => channel.name === 'Info');
		const rules = channels.find((channel) => channel.name === 'rules');
		expect(rules?.parent_id).toBe(category?.id ?? null);
		expect(rules?.permission_overwrites?.find((overwrite) => overwrite.id === guild.id)?.deny).toBe(
			Permissions.SEND_MESSAGES.toString(),
		);
		expect(channels.find((channel) => channel.name === 'Nested')?.parent_id ?? null).toBeNull();
		expect(channels.find((channel) => channel.name === 'ideas')?.parent_id).toBe(category?.id ?? null);
		expect(guild.system_channel_id).toBe(rules?.id ?? null);
	});
	test('hides imported forum channels from viewers outside the threads experiment', async () => {
		const account = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, account.token)
			.post('/guilds')
			.body({
				name: 'Hidden Forum Guild',
				template: buildMinimalTemplate({
					channels: [
						{id: 6001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0},
						{id: 6002, type: ChannelTypes.GUILD_FORUM, name: 'forum', position: 1},
					],
				}),
			})
			.execute();
		const legacy = await getGuildChannels(harness, account.token, guild.id);
		expect(legacy.some((channel) => channel.name === 'forum')).toBe(false);
		expect(legacy.some((channel) => channel.name === 'general')).toBe(true);
	});

	function listThreadAwareChannels(token: string, guildId: string): Promise<Array<ChannelResponse>> {
		return createBuilder<Array<ChannelResponse>>(harness, token)
			.get(`/guilds/${guildId}/channels`)
			.header(THREAD_FEATURES_HEADER, THREAD_FEATURES_CAPABLE)
			.execute();
	}

	function getThreadAwareChannel(token: string, channelId: string): Promise<ChannelResponse> {
		return createBuilder<ChannelResponse>(harness, token)
			.get(`/channels/${channelId}`)
			.header(THREAD_FEATURES_HEADER, THREAD_FEATURES_CAPABLE)
			.execute();
	}
});

const THREAD_FEATURES_HEADER = 'X-Fluxer-Features';
const THREAD_FEATURES_CAPABLE = 'channel_threads';

function findChannelId(channels: Array<ChannelResponse>, name: string): string {
	const channel = channels.find((candidate) => candidate.name === name);
	if (!channel) throw new Error(`Missing channel ${name}`);
	return channel.id;
}

const DEFAULT_EVERYONE_PERMISSIONS = Permissions.VIEW_CHANNEL.toString();

function buildMinimalTemplate(overrides: {channels?: Array<object>; roles?: Array<object>}) {
	return {
		name: 'Template Source',
		description: null,
		verification_level: 0,
		default_message_notifications: 0,
		explicit_content_filter: 0,
		system_channel_id: 6001,
		afk_timeout: 300,
		system_channel_flags: 0,
		roles: overrides.roles ?? [{id: 0, name: '@everyone', permissions: DEFAULT_EVERYONE_PERMISSIONS}],
		channels: overrides.channels ?? [{id: 6001, type: ChannelTypes.GUILD_TEXT, name: 'general', position: 0}],
	};
}
