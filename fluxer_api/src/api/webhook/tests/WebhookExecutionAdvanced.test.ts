// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
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
	});
});
