// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {beforeAll, beforeEach, describe, it} from 'vitest';

describe('Webhook stickers', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('rejects unknown sticker_ids', async () => {
		const user = await createTestAccount(harness);
		const guild = await createGuild(harness, user.token, 'Webhook Sticker Test Guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, user.token, 'Sticker Test Webhook');
		await createBuilderWithoutAuth(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}`)
			.body({
				content: 'Webhook sticker test',
				sticker_ids: ['999999999999999999'],
			})
			.expect(400)
			.execute();
		await deleteWebhook(harness, webhook.id, user.token);
	});
});
