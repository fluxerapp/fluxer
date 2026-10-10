// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {
	createWebhook,
	deleteWebhook,
	executeWebhook,
	sendChannelMessage,
} from '@app/api/webhook/tests/WebhookTestUtils';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

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
});
