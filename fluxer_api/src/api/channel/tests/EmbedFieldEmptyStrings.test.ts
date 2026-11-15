// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const EMPTY_FIELDS = [
	{name: 'Status', value: ''},
	{name: '', value: 'Online'},
	{name: '', value: ''},
];

function fieldPairs(message: MessageResponse): Array<[string, string]> {
	return (message.embeds?.[0]?.fields ?? []).map((field) => [field.name, field.value]);
}

const EXPECTED_PAIRS: Array<[string, string]> = [
	['Status', ''],
	['', 'Online'],
	['', ''],
];

describe('Embed fields with empty names and values', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	it('accepts empty field names and values on webhook create and edit', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Empty embed fields');
		const webhook = await createWebhook(harness, guild.system_channel_id!, owner.token, 'Empty fields');
		const base = `/webhooks/${webhook.id}/${webhook.token}`;

		const created = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`${base}?wait=true`)
			.body({embeds: [{fields: EMPTY_FIELDS}]})
			.execute();
		expect(fieldPairs(created)).toEqual(EXPECTED_PAIRS);

		const plain = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`${base}?wait=true`)
			.body({embeds: [{fields: [{name: 'a', value: 'b'}]}]})
			.execute();
		const edited = await createBuilderWithoutAuth<MessageResponse>(harness)
			.patch(`${base}/messages/${plain.id}`)
			.body({embeds: [{fields: EMPTY_FIELDS}]})
			.execute();
		expect(fieldPairs(edited)).toEqual(EXPECTED_PAIRS);
	});

	it('accepts empty field names and values on message create and edit', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Empty embed fields');
		const channelId = guild.system_channel_id!;
		await ensureSessionStarted(harness, account.token);

		const created = await createBuilder<MessageResponse>(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({embeds: [{fields: EMPTY_FIELDS}]})
			.execute();
		expect(fieldPairs(created)).toEqual(EXPECTED_PAIRS);

		const plain = await createBuilder<MessageResponse>(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({content: 'plain'})
			.execute();
		const edited = await createBuilder<MessageResponse>(harness, account.token)
			.patch(`/channels/${channelId}/messages/${plain.id}`)
			.body({embeds: [{fields: EMPTY_FIELDS}]})
			.execute();
		expect(fieldPairs(edited)).toEqual(EXPECTED_PAIRS);
	});

	it('still rejects a missing field value with the exact field path', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Missing embed field value');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Missing value');
		await ensureSessionStarted(harness, owner.token);
		const body = {embeds: [{fields: [{name: 'Status', value: null}]}]};

		const webhookResult = await createBuilderWithoutAuth(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
			.body(body)
			.executeRaw();
		const messageResult = await createBuilder(harness, owner.token)
			.post(`/channels/${channelId}/messages`)
			.body(body)
			.executeRaw();

		for (const result of [webhookResult, messageResult]) {
			expect(result.response.status).toBe(400);
			expect(result.text).toContain('"path":"embeds.0.fields.0.value"');
		}
	});
});
