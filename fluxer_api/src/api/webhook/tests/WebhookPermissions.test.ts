// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeAll, beforeEach, describe, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {acceptInvite, createGuild} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createChannelInvite, createWebhook, deleteWebhook} from './WebhookTestUtils';

describe('Webhook permissions', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('requires permissions to manage webhooks', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, `Webhook Security ${Date.now()}`);
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, member.token, invite.code);
		const webhook = await createWebhook(harness, channelId, owner.token, 'Test Webhook');
		await createBuilder(harness, member.token)
			.post(`/channels/${channelId}/webhooks`)
			.body({name: 'Test Webhook'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, member.token)
			.get(`/channels/${channelId}/webhooks`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, member.token)
			.patch(`/webhooks/${webhook.id}`)
			.body({name: 'Hacked'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, member.token)
			.delete(`/webhooks/${webhook.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await deleteWebhook(harness, webhook.id, owner.token);
	});
});
