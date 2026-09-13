// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('Webhook execution advanced', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Message content', () => {
		it('edits webhook messages with suppress embeds flags', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Edit Flags Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Edit Flags Webhook');
			const createdMessage = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
				.body({
					content: 'Message with embed',
					embeds: [{title: 'Embed to suppress'}],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const updatedMessage = await createBuilderWithoutAuth<MessageResponse>(harness)
				.patch(`/webhooks/${webhook.id}/${webhook.token}/messages/${createdMessage.id}`)
				.body({flags: MessageFlags.SUPPRESS_EMBEDS})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(updatedMessage.flags & MessageFlags.SUPPRESS_EMBEDS).not.toBe(0);
			const fetchedMessage = await createBuilder<MessageResponse>(harness, owner.token)
				.get(`/channels/${channelId}/messages/${createdMessage.id}`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(fetchedMessage.flags & MessageFlags.SUPPRESS_EMBEDS).not.toBe(0);
			expect(fetchedMessage.embeds).toHaveLength(0);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('executes webhook with embeds', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Embed Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Embed Webhook');
			const embed = {
				title: 'Test Embed',
				description: 'This is a test embed from webhook',
				color: 0x00ff00,
			};
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}`)
				.body({embeds: [embed]})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('executes webhook with content and embeds', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Content Embed Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Content Embed Webhook');
			const result = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
				.body({
					content: 'Message with embed',
					embeds: [{title: 'Accompanying Embed'}],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.content).toBe('Message with embed');
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('executes webhook with an empty embed title', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Empty Embed Title Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Empty Embed Title Webhook');
			const result = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
				.body({
					embeds: [{title: '', description: 'Description with intentionally empty title'}],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.embeds).toHaveLength(1);
			expect(result.embeds?.[0]?.title).toBe('');
			expect(result.embeds?.[0]?.description).toBe('Description with intentionally empty title');
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('executes webhook with an empty embed description', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Empty Embed Description Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Empty Embed Description Webhook');
			const result = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
				.body({
					embeds: [{title: 'Workflow run', description: ''}],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.embeds).toHaveLength(1);
			expect(result.embeds?.[0]?.title).toBe('Workflow run');
			expect(result.embeds?.[0]?.description).toBeNull();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('executes webhook with custom username', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Username Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Username Webhook');
			const result = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
				.body({
					content: 'Custom username message',
					username: 'Custom Bot Name',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.content).toBe('Custom username message');
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
	describe('Validation', () => {
		it('rejects empty message', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Validation Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Empty Message Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_EMPTY_MESSAGE')
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('rejects empty content string', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Validation Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Empty Content Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}`)
				.body({content: ''})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_EMPTY_MESSAGE')
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('rejects whitespace only content', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Validation Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Whitespace Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}`)
				.body({content: '   '})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_EMPTY_MESSAGE')
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('allows empty content with valid embed', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Validation Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Embed Only Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}`)
				.body({
					embeds: [{title: 'Embed only message'}],
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
	describe('Wait parameter', () => {
		it('returns 204 without wait parameter', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Wait Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'No Wait Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}`)
				.body({content: 'No wait message'})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('returns message with wait=true', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Wait Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Wait True Webhook');
			const result = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
				.body({content: 'Wait message'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.id).toBeTruthy();
			expect(result.content).toBe('Wait message');
			expect(result.webhook_id).toBe(webhook.id);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('returns 204 with wait=false', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Wait Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Wait False Webhook');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=false`)
				.body({content: 'Wait false message'})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
	describe('Slack compatible endpoint', () => {
		it('executes slack compatible webhook', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Slack Webhook Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Slack Compatible Webhook');
			const {response, text} = await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/slack`)
				.body({text: 'Hello from Slack format'})
				.expect(HTTP_STATUS.OK)
				.executeRaw();
			expect(response.status).toBe(HTTP_STATUS.OK);
			expect(text).toBe('ok');
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('rejects slack webhook with invalid token', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Slack Webhook Guild');
			const channelId = guild.system_channel_id!;
			const webhook = await createWebhook(harness, channelId, owner.token, 'Slack Invalid Token');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/invalid_token/slack`)
				.body({text: 'Should fail'})
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
});
