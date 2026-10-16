// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {MAX_WEBHOOKS_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import type {WebhookResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {afterEach, beforeEach, describe, it} from 'vitest';

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
	});
});
