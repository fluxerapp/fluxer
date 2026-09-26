// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {
	createWebhook,
	deleteWebhook,
	deleteWebhookByToken,
	getChannelWebhooks,
	getGuildWebhooks,
	getWebhook,
	getWebhookByToken,
	updateWebhook,
	updateWebhookByToken,
} from '@app/api/webhook/tests/WebhookTestUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {WebhookResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('Webhook Operations', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Create webhook', () => {
		it('creates a webhook with name', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
			expect(webhook.id).toBeTruthy();
			expect(webhook.name).toBe('Test Webhook');
			expect(webhook.channel_id).toBe(channelId);
			expect(webhook.guild_id).toBe(guild.id);
			expect(webhook.token).toBeTruthy();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('creates webhook in different channel', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channel = await createChannel(harness, owner.token, guild.id, 'webhook-channel');
			const webhook = await createWebhook(harness, channel.id, owner.token, 'Channel Webhook');
			expect(webhook.channel_id).toBe(channel.id);
			expect(webhook.guild_id).toBe(guild.id);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('creates webhook in voice channel text chat', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Voice Guild');
			const channel = await createChannel(
				harness,
				owner.token,
				guild.id,
				'voice-webhook-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const webhook = await createWebhook(harness, channel.id, owner.token, 'Voice Channel Webhook');
			expect(webhook.channel_id).toBe(channel.id);
			expect(webhook.guild_id).toBe(guild.id);
			const channelWebhooks = await getChannelWebhooks(harness, channel.id, owner.token);
			expect(channelWebhooks.some((candidate) => candidate.id === webhook.id)).toBe(true);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('creates multiple webhooks in same channel', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const webhook1 = await createWebhook(harness, channelId, owner.token, 'First Webhook');
			const webhook2 = await createWebhook(harness, channelId, owner.token, 'Second Webhook');
			expect(webhook1.id).not.toBe(webhook2.id);
			expect(webhook1.token).not.toBe(webhook2.token);
			await deleteWebhook(harness, webhook1.id, owner.token);
			await deleteWebhook(harness, webhook2.id, owner.token);
		});
	});
	describe('Get webhook', () => {
		it('gets webhook by id with user auth', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const created = await createWebhook(harness, channelId, owner.token, 'Get Test Webhook');
			const fetched = await getWebhook(harness, created.id, owner.token);
			expect(fetched.id).toBe(created.id);
			expect(fetched.name).toBe(created.name);
			expect(fetched.channel_id).toBe(created.channel_id);
			await deleteWebhook(harness, created.id, owner.token);
		});
		it('gets webhook by id and token without auth', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const created = await createWebhook(harness, channelId, owner.token, 'Token Get Webhook');
			const fetched = await getWebhookByToken(harness, created.id, created.token);
			expect(fetched.id).toBe(created.id);
			expect(fetched.name).toBe(created.name);
			await deleteWebhook(harness, created.id, owner.token);
		});
	});
	describe('Update webhook', () => {
		it('updates webhook name with user auth', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Original Name');
			const updated = await updateWebhook(harness, webhook.id, owner.token, {name: 'Updated Name'});
			expect(updated.name).toBe('Updated Name');
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('updates webhook name by token without auth', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Original Name');
			const updated = await updateWebhookByToken(harness, webhook.id, webhook.token, {name: 'Token Updated Name'});
			expect(updated.name).toBe('Token Updated Name');
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('updates webhook channel with user auth', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const newChannel = await createChannel(harness, owner.token, guild.id, 'new-channel');
			const webhook = await createWebhook(harness, channelId, owner.token, 'Move Webhook');
			const updated = await createBuilder<WebhookResponse>(harness, owner.token)
				.patch(`/webhooks/${webhook.id}`)
				.body({channel_id: newChannel.id})
				.execute();
			expect(updated.channel_id).toBe(newChannel.id);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('moves webhook to voice channel text chat with user auth', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Voice Move Guild');
			const channelId = guild.system_channel_id!;
			const newChannel = await createChannel(
				harness,
				owner.token,
				guild.id,
				'voice-webhook-target',
				ChannelTypes.GUILD_VOICE,
			);
			const webhook = await createWebhook(harness, channelId, owner.token, 'Voice Move Webhook');
			const updated = await createBuilder<WebhookResponse>(harness, owner.token)
				.patch(`/webhooks/${webhook.id}`)
				.body({channel_id: newChannel.id})
				.execute();
			expect(updated.channel_id).toBe(newChannel.id);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('rejects updating webhook channel by token', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const newChannel = await createChannel(harness, owner.token, guild.id, 'token-new-channel');
			const webhook = await createWebhook(harness, channelId, owner.token, 'Token Move Webhook');
			await createBuilderWithoutAuth<WebhookResponse>(harness)
				.patch(`/webhooks/${webhook.id}/${webhook.token}`)
				.body({channel_id: newChannel.id})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
	describe('Delete webhook', () => {
		it('deletes webhook with user auth', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Delete Me');
			await deleteWebhook(harness, webhook.id, owner.token);
			await createBuilder(harness, owner.token).get(`/webhooks/${webhook.id}`).expect(HTTP_STATUS.NOT_FOUND).execute();
		});
		it('deletes webhook by token without auth', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Operations Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Token Delete Me');
			await deleteWebhookByToken(harness, webhook.id, webhook.token);
			await createBuilder(harness, owner.token).get(`/webhooks/${webhook.id}`).expect(HTTP_STATUS.NOT_FOUND).execute();
		});
	});
	describe('List webhooks', () => {
		it('lists webhooks by guild', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook List Guild');
			const channelId = guild.system_channel_id!;
			const webhook1 = await createWebhook(harness, channelId, owner.token, 'Guild Webhook 1');
			const webhook2 = await createWebhook(harness, channelId, owner.token, 'Guild Webhook 2');
			const webhooks = await getGuildWebhooks(harness, guild.id, owner.token);
			expect(webhooks.length).toBe(2);
			expect(webhooks.some((w) => w.id === webhook1.id)).toBe(true);
			expect(webhooks.some((w) => w.id === webhook2.id)).toBe(true);
			await deleteWebhook(harness, webhook1.id, owner.token);
			await deleteWebhook(harness, webhook2.id, owner.token);
		});
		it('lists webhooks by channel', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook List Guild');
			const channel1 = guild.system_channel_id!;
			const channel2 = (await createChannel(harness, owner.token, guild.id, 'second-channel')).id;
			const webhook1 = await createWebhook(harness, channel1, owner.token, 'Channel 1 Webhook');
			const webhook2 = await createWebhook(harness, channel2, owner.token, 'Channel 2 Webhook');
			const channel1Webhooks = await getChannelWebhooks(harness, channel1, owner.token);
			const channel2Webhooks = await getChannelWebhooks(harness, channel2, owner.token);
			expect(channel1Webhooks.length).toBe(1);
			expect(channel1Webhooks[0]?.id).toBe(webhook1.id);
			expect(channel2Webhooks.length).toBe(1);
			expect(channel2Webhooks[0]?.id).toBe(webhook2.id);
			await deleteWebhook(harness, webhook1.id, owner.token);
			await deleteWebhook(harness, webhook2.id, owner.token);
		});
		it('returns empty array when no webhooks exist', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Empty Webhook Guild');
			const webhooks = await getGuildWebhooks(harness, guild.id, owner.token);
			expect(webhooks).toEqual([]);
		});
	});
});
