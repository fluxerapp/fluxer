// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, it} from 'vitest';

describe('Webhook validation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	it('rejects webhook creation without name', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Validation Guild');
		const channelId = guild.system_channel_id ?? (await createChannel(harness, owner.token, guild.id, 'general')).id;
		await createBuilder(harness, owner.token)
			.post(`/channels/${channelId}/webhooks`)
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects webhook creation with invalid avatar', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook Validation Guild');
		const channelId = guild.system_channel_id ?? (await createChannel(harness, owner.token, guild.id, 'general')).id;
		await createBuilder(harness, owner.token)
			.post(`/channels/${channelId}/webhooks`)
			.body({
				name: 'Test',
				avatar: 'invalid-base64',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	it('rejects getting nonexistent webhook', async () => {
		const owner = await createTestAccount(harness);
		await createGuild(harness, owner.token, 'Webhook Validation Guild');
		await createBuilder(harness, owner.token)
			.get(`/webhooks/${TEST_IDS.NONEXISTENT_WEBHOOK}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	it('rejects updating nonexistent webhook', async () => {
		const owner = await createTestAccount(harness);
		await createGuild(harness, owner.token, 'Webhook Validation Guild');
		await createBuilder(harness, owner.token)
			.patch(`/webhooks/${TEST_IDS.NONEXISTENT_WEBHOOK}`)
			.body({name: 'Updated'})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	it('rejects deleting nonexistent webhook', async () => {
		const owner = await createTestAccount(harness);
		await createGuild(harness, owner.token, 'Webhook Validation Guild');
		await createBuilder(harness, owner.token)
			.delete(`/webhooks/${TEST_IDS.NONEXISTENT_WEBHOOK}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
});
