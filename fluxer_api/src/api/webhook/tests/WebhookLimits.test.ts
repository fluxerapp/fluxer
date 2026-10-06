// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {MAX_WEBHOOKS_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import type {WebhookResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('Webhook limits', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Channel webhook limits', () => {
		it('enforces max webhooks per channel limit', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Limit Guild');
			const channelId = guild.system_channel_id!;
			const createdWebhooks: Array<WebhookResponse> = [];
			for (let i = 0; i < MAX_WEBHOOKS_PER_CHANNEL; i++) {
				const webhook = await createWebhook(harness, channelId, owner.token, `Webhook ${i + 1}`);
				createdWebhooks.push(webhook);
			}
			await createBuilder(harness, owner.token)
				.post(`/channels/${channelId}/webhooks`)
				.body({name: 'One Too Many'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'MAX_WEBHOOKS_PER_CHANNEL')
				.execute();
			for (const webhook of createdWebhooks) {
				await deleteWebhook(harness, webhook.id, owner.token);
			}
		});
		it('allows creating webhook after deleting one at limit', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Limit Guild');
			const channelId = guild.system_channel_id!;
			const createdWebhooks: Array<WebhookResponse> = [];
			for (let i = 0; i < MAX_WEBHOOKS_PER_CHANNEL; i++) {
				const webhook = await createWebhook(harness, channelId, owner.token, `Webhook ${i + 1}`);
				createdWebhooks.push(webhook);
			}
			await deleteWebhook(harness, createdWebhooks[0]!.id, owner.token);
			const newWebhook = await createWebhook(harness, channelId, owner.token, 'Replacement Webhook');
			expect(newWebhook.id).toBeTruthy();
			for (let i = 1; i < createdWebhooks.length; i++) {
				await deleteWebhook(harness, createdWebhooks[i]!.id, owner.token);
			}
			await deleteWebhook(harness, newWebhook.id, owner.token);
		});
		it('channel limits are independent per channel', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Limit Guild');
			const channel1 = guild.system_channel_id!;
			const channel2 = (await createChannel(harness, owner.token, guild.id, 'second-channel')).id;
			const channel1Webhooks: Array<WebhookResponse> = [];
			for (let i = 0; i < MAX_WEBHOOKS_PER_CHANNEL; i++) {
				const webhook = await createWebhook(harness, channel1, owner.token, `Ch1 Webhook ${i + 1}`);
				channel1Webhooks.push(webhook);
			}
			const channel2Webhook = await createWebhook(harness, channel2, owner.token, 'Channel 2 Webhook');
			expect(channel2Webhook.id).toBeTruthy();
			for (const webhook of channel1Webhooks) {
				await deleteWebhook(harness, webhook.id, owner.token);
			}
			await deleteWebhook(harness, channel2Webhook.id, owner.token);
		});
	});
	describe('Channel move limits', () => {
		it('rejects moving webhook to channel at limit', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Move Limit Guild');
			const channel1 = guild.system_channel_id!;
			const channel2 = (await createChannel(harness, owner.token, guild.id, 'full-channel')).id;
			const channel2Webhooks: Array<WebhookResponse> = [];
			for (let i = 0; i < MAX_WEBHOOKS_PER_CHANNEL; i++) {
				const webhook = await createWebhook(harness, channel2, owner.token, `Ch2 Webhook ${i + 1}`);
				channel2Webhooks.push(webhook);
			}
			const movingWebhook = await createWebhook(harness, channel1, owner.token, 'Moving Webhook');
			await createBuilder(harness, owner.token)
				.patch(`/webhooks/${movingWebhook.id}`)
				.body({channel_id: channel2})
				.expect(HTTP_STATUS.BAD_REQUEST, 'MAX_WEBHOOKS_PER_CHANNEL')
				.execute();
			await deleteWebhook(harness, movingWebhook.id, owner.token);
			for (const webhook of channel2Webhooks) {
				await deleteWebhook(harness, webhook.id, owner.token);
			}
		});
		it('allows moving webhook to channel under limit', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Move Guild');
			const channel1 = guild.system_channel_id!;
			const channel2 = (await createChannel(harness, owner.token, guild.id, 'target-channel')).id;
			const webhook = await createWebhook(harness, channel1, owner.token, 'Moving Webhook');
			const updated = await createBuilder<WebhookResponse>(harness, owner.token)
				.patch(`/webhooks/${webhook.id}`)
				.body({channel_id: channel2})
				.execute();
			expect(updated.channel_id).toBe(channel2);
			await deleteWebhook(harness, webhook.id, owner.token);
		});
		it('rejects moving webhook by token', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Webhook Token Move Limit Guild');
			const channel1 = guild.system_channel_id!;
			const channel2 = (await createChannel(harness, owner.token, guild.id, 'full-channel-token')).id;
			const movingWebhook = await createWebhook(harness, channel1, owner.token, 'Token Moving Webhook');
			await createBuilder(harness, '')
				.patch(`/webhooks/${movingWebhook.id}/${movingWebhook.token}`)
				.body({channel_id: channel2})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			await deleteWebhook(harness, movingWebhook.id, owner.token);
		});
	});
});
