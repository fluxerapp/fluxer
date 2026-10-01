// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	getChannel,
	getGuild,
	sendChannelMessage,
	setupTestGuildWithMembers,
	updateChannel,
	updateGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {getGuildChannels, updateChannelPositions} from '@app/api/guild/tests/GuildTestUtils';
import {deleteMessage, ensureSessionStarted, getMessages, pinMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, executeWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {sortChannelsForOrdering} from '@fluxer/schema/src/domains/channel/GuildChannelOrdering';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Announcement channel type', () => {
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

	test('creates a type 5 channel with the text name rules', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Announcement Guild');
		const text = await createChannel(harness, owner.token, guild.id, 'Big News', ChannelTypes.GUILD_TEXT);
		const announcement = await createChannel(
			harness,
			owner.token,
			guild.id,
			'Big News',
			ChannelTypes.GUILD_ANNOUNCEMENT,
		);
		expect(announcement.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(announcement.name).toBe(text.name);
		expect(announcement.name).not.toBe('Big News');
		const fetched = await getChannel(harness, owner.token, announcement.id);
		expect(fetched.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
	});

	test('keeps the name as typed when flexible names are enabled', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Flexible Guild');
		await updateGuild(harness, owner.token, guild.id, {
			features: [...guild.features, GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES],
		});
		const announcement = await createChannel(
			harness,
			owner.token,
			guild.id,
			'Big News',
			ChannelTypes.GUILD_ANNOUNCEMENT,
		);
		expect(announcement.name).toBe('Big News');
		const renamed = await updateChannel(harness, owner.token, announcement.id, {name: 'Even Bigger News'});
		expect(renamed.name).toBe('Even Bigger News');
		expect(renamed.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
	});

	test('normalises a rename like a text channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Rename Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const renamed = await updateChannel(harness, owner.token, announcement.id, {name: 'Release Notes'});
		expect(renamed.name).not.toBe('Release Notes');
		expect(renamed.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
	});

	test('lists the channel with text before voice in a category', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Ordering Guild');
		const category = await createChannel(harness, owner.token, guild.id, 'Mixed', ChannelTypes.GUILD_CATEGORY);
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const voice = await createChannel(harness, owner.token, guild.id, 'voice', ChannelTypes.GUILD_VOICE);
		await updateChannelPositions(harness, owner.token, guild.id, [
			{id: voice.id, parent_id: category.id},
			{id: announcement.id, parent_id: category.id},
		]);
		const channels = await getGuildChannels(harness, owner.token, guild.id);
		const listed = channels.find((channel) => channel.id === announcement.id);
		expect(listed?.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		const ordered = sortChannelsForOrdering(
			channels.map((channel) => ({
				id: channel.id,
				parentId: channel.parent_id ?? null,
				type: channel.type,
				position: channel.position,
			})),
		)
			.filter((channel) => channel.parentId === category.id)
			.map((channel) => channel.id);
		expect(ordered.indexOf(announcement.id)).toBeLessThan(ordered.indexOf(voice.id));
		const response = await createBuilder<{code: string; errors: Array<{path: string; code: string}>}>(
			harness,
			owner.token,
		)
			.patch(`/guilds/${guild.id}/channels`)
			.body([{id: announcement.id, parent_id: category.id, preceding_sibling_id: voice.id}])
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.VOICE_CHANNELS_CANNOT_BE_ABOVE_TEXT_CHANNELS);
	});

	test('supports sending, editing, reacting, pinning, uploading and deleting', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Messaging Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		await ensureSessionStarted(harness, owner.token);
		const message = await sendChannelMessage(harness, owner.token, announcement.id, 'first announcement');
		expect(message.channel_id).toBe(announcement.id);
		const edited = await createBuilder<MessageResponse>(harness, owner.token)
			.patch(`/channels/${announcement.id}/messages/${message.id}`)
			.body({content: 'edited announcement'})
			.execute();
		expect(edited.content).toBe('edited announcement');
		await createBuilder(harness, owner.token)
			.put(`/channels/${announcement.id}/messages/${message.id}/reactions/%F0%9F%91%8D/@me`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await pinMessage(harness, owner.token, announcement.id, message.id);
		const {response: uploadResponse, json: uploaded} = await sendMessageWithAttachments(
			harness,
			owner.token,
			announcement.id,
			{content: 'with a file', attachments: [{id: 0, filename: 'notes.txt'}]},
			[{index: 0, filename: 'notes.txt', data: Buffer.from('release notes')}],
		);
		expect(uploadResponse.status).toBe(HTTP_STATUS.OK);
		expect(uploaded.attachments).toHaveLength(1);
		const messages = await getMessages(harness, owner.token, announcement.id);
		const reacted = messages.find((entry) => entry.id === message.id);
		expect(reacted?.reactions?.[0]?.count).toBe(1);
		expect(reacted?.pinned).toBe(true);
		await deleteMessage(harness, owner.token, announcement.id, uploaded.id);
		const remaining = await getMessages(harness, owner.token, announcement.id);
		expect(remaining.some((entry) => entry.id === uploaded.id)).toBe(false);
	});

	test('enforces slowmode', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const updated = await updateChannel(harness, owner.token, announcement.id, {rate_limit_per_user: 30});
		expect(updated.rate_limit_per_user).toBe(30);
		await sendChannelMessage(harness, member.token, announcement.id, 'first');
		await createBuilder(harness, member.token)
			.post(`/channels/${announcement.id}/messages`)
			.body({content: 'second'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.SLOWMODE_RATE_LIMITED)
			.execute();
	});

	test('creates and executes an incoming webhook', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const webhook = await createWebhook(harness, announcement.id, owner.token, 'Release Bot');
		expect(webhook.channel_id).toBe(announcement.id);
		const {json} = await executeWebhook(harness, webhook.id, webhook.token, {content: 'shipped', wait: true}, 200);
		expect(json?.channel_id).toBe(announcement.id);
		expect(json?.webhook_id).toBe(webhook.id);
	});

	test('applies the age gate to an age-restricted announcement channel', async () => {
		const owner = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
		const minor = await createTestAccount(harness, {dateOfBirth: '2012-01-01'});
		const guild = await createGuild(harness, owner.token, 'Age Gate Guild');
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await acceptInvite(harness, minor.token, invite.code);
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		await sendChannelMessage(harness, owner.token, announcement.id, 'before the gate');
		const minorView = await getMessages(harness, minor.token, announcement.id);
		expect(minorView).toHaveLength(1);
		const restricted = await updateChannel(harness, owner.token, announcement.id, {nsfw: true});
		expect(restricted.nsfw).toBe(true);
		await createBuilder(harness, minor.token)
			.get(`/channels/${announcement.id}/messages`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.NSFW_CONTENT_AGE_RESTRICTED)
			.execute();
		const adultView = await getMessages(harness, owner.token, announcement.id);
		expect(adultView).toHaveLength(1);
	});

	test('can be the system channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'System Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const updated = await updateGuild(harness, owner.token, guild.id, {system_channel_id: announcement.id});
		expect(updated.system_channel_id).toBe(announcement.id);
		const fetched = await getGuild(harness, owner.token, guild.id);
		expect(fetched.system_channel_id).toBe(announcement.id);
	});

	test('still rejects a voice channel as the system channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Voice System Guild');
		const voice = await createChannel(harness, owner.token, guild.id, 'voice', ChannelTypes.GUILD_VOICE);
		const response = await createBuilder<{errors: Array<{path: string; code: string}>}>(harness, owner.token)
			.patch(`/guilds/${guild.id}`)
			.body({system_channel_id: voice.id})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.SYSTEM_CHANNEL_MUST_BE_TEXT);
	});

	test('imports a template announcement channel as type 5 and keeps it as the system channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<{id: string; system_channel_id: string | null}>(harness, owner.token)
			.post('/guilds')
			.body({
				name: 'Template Guild',
				template: {
					name: 'Template Source',
					description: null,
					verification_level: 0,
					default_message_notifications: 0,
					explicit_content_filter: 0,
					system_channel_id: '7002',
					afk_timeout: 300,
					system_channel_flags: 0,
					roles: [{id: '0', name: '@everyone', permissions: '0'}],
					channels: [
						{id: '7001', type: ChannelTypes.GUILD_VOICE, name: 'lounge', position: 0},
						{id: '7002', type: ChannelTypes.GUILD_ANNOUNCEMENT, name: 'news', position: 1},
					],
				},
			})
			.execute();
		const channels = await getGuildChannels(harness, owner.token, guild.id);
		const news = channels.find((channel: ChannelResponse) => channel.name === 'news');
		expect(news?.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(guild.system_channel_id).toBe(news?.id);
	});

	test('uses a template announcement channel as the fallback system channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<{id: string; system_channel_id: string | null}>(harness, owner.token)
			.post('/guilds')
			.body({
				name: 'Fallback Guild',
				template: {
					name: 'Template Source',
					description: null,
					verification_level: 0,
					default_message_notifications: 0,
					explicit_content_filter: 0,
					system_channel_id: null,
					afk_timeout: 300,
					system_channel_flags: 0,
					roles: [{id: '0', name: '@everyone', permissions: '0'}],
					channels: [{id: '8001', type: ChannelTypes.GUILD_ANNOUNCEMENT, name: 'news', position: 0}],
				},
			})
			.execute();
		const channels = await getGuildChannels(harness, owner.token, guild.id);
		expect(channels).toHaveLength(1);
		expect(channels[0]?.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(guild.system_channel_id).toBe(channels[0]?.id);
	});
});
