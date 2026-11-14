// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {afterEach, beforeEach, describe, it} from 'vitest';

describe('Webhook channel validation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Create webhook channel validation', () => {
		it('rejects creating webhook in DM channel', async () => {
			const owner = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			await createFriendship(harness, owner, target);
			const dmChannel = await createBuilder<{
				id: string;
			}>(harness, owner.token)
				.post('/users/@me/channels')
				.body({recipient_id: target.userId})
				.execute();
			await createBuilder(harness, owner.token)
				.post(`/channels/${dmChannel.id}/webhooks`)
				.body({name: 'DM Webhook'})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
		});
	});
	describe('Move webhook channel validation', () => {
		it('rejects moving webhook to channel in different guild', async () => {
			const owner = await createTestAccount(harness);
			const guild1 = await createGuild(harness, owner.token, 'Guild 1');
			const guild2 = await createGuild(harness, owner.token, 'Guild 2');
			const channel1 = guild1.system_channel_id!;
			const channel2 = guild2.system_channel_id!;
			const webhook = await createWebhook(harness, channel1, owner.token, 'Cross Guild Webhook');
			await createBuilder(harness, owner.token)
				.patch(`/webhooks/${webhook.id}`)
				.body({channel_id: channel2})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
});
