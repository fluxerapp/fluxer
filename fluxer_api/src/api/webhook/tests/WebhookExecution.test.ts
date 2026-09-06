// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {
	createWebhook,
	deleteWebhook,
	deleteWebhookMessageByToken,
	executeWebhook,
	sendChannelMessage,
} from './WebhookTestUtils';

describe('Webhook execution', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('executes webhook without wait returns 204', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Exec Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		const result = await executeWebhook(harness, webhook.id, webhook.token, {
			content: 'Hello from webhook!',
		});
		expect(result.response.status).toBe(204);
		expect(result.json).toBeNull();
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('executes webhook with wait=true returns message', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Exec Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		const result = await executeWebhook(
			harness,
			webhook.id,
			webhook.token,
			{
				content: 'Custom user',
				username: 'Custom Bot',
				wait: true,
			},
			200,
		);
		expect(result.response.status).toBe(200);
		expect(result.json).not.toBeNull();
		expect(result.json!.content).toBe('Custom user');
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('allows webhook message content up to 4000 characters', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Exec Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		const content = 'w'.repeat(MAX_MESSAGE_LENGTH_PREMIUM);
		const result = await executeWebhook(
			harness,
			webhook.id,
			webhook.token,
			{
				content,
				wait: true,
			},
			200,
		);
		expect(result.json!.content).toBe(content);
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('executes webhook in voice channel text chat', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Voice Exec Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'voice-webhook-exec', ChannelTypes.GUILD_VOICE);
		const webhook = await createWebhook(harness, channel.id, owner.token, 'Voice Exec Webhook');
		const result = await executeWebhook(
			harness,
			webhook.id,
			webhook.token,
			{
				content: 'Hello from voice text chat',
				wait: true,
			},
			200,
		);
		expect(result.response.status).toBe(200);
		expect(result.json).not.toBeNull();
		expect(result.json!.channel_id).toBe(channel.id);
		expect(result.json!.content).toBe('Hello from voice text chat');
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('deletes a webhook message by token', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Exec Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		const result = await executeWebhook(
			harness,
			webhook.id,
			webhook.token,
			{
				content: 'Delete me',
				wait: true,
			},
			200,
		);
		const messageId = result.json!.id;
		await deleteWebhookMessageByToken(harness, webhook.id, webhook.token, messageId);
		await createBuilderWithoutAuth(harness)
			.get(`/webhooks/${webhook.id}/${webhook.token}/messages/${messageId}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('rejects deleting a non-webhook message by token', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Exec Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		const message = await sendChannelMessage(harness, owner.token, channelId, 'Not a webhook message');
		await createBuilderWithoutAuth(harness)
			.delete(`/webhooks/${webhook.id}/${webhook.token}/messages/${message.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('rejects webhook execution without content', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Exec Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		await createBuilderWithoutAuth(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}`)
			.body({content: ''})
			.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_EMPTY_MESSAGE')
			.execute();
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('returns the original message when a webhook execution repeats a nonce', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Exec Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		const nonce = '4815162342';
		const first = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
			.body({content: 'Retried webhook message', nonce})
			.expect(HTTP_STATUS.OK)
			.execute();
		const second = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
			.body({content: 'Retried webhook message', nonce})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(second.id).toBe(first.id);
		const messages = await createBuilder<Array<MessageResponse>>(harness, owner.token)
			.get(`/channels/${channelId}/messages`)
			.execute();
		expect(messages.filter((message) => message.webhook_id === webhook.id)).toHaveLength(1);
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('rejects webhook execution with invalid token', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Exec Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		await createBuilderWithoutAuth(harness)
			.post(`/webhooks/${webhook.id}/invalid_token`)
			.body({content: 'Test'})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
		await deleteWebhook(harness, webhook.id, owner.token);
	});
});
