// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DELETED_USER_ID, DELETED_USER_USERNAME} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createUserID, createWebhookID} from '../../BrandedTypes';
import {createGuild} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {WebhookRepository} from '../WebhookRepository';
import {createWebhook, executeWebhook, executeWebhookWithAttachments, getChannelWebhooks} from './WebhookTestUtils';

const VANISHED_CREATOR_ID = createUserID(999999999999999997n);

function parseErrorCode(text: string): string | undefined {
	return (JSON.parse(text) as {code?: string}).code;
}

describe('Webhook whose creating account cannot be resolved', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	it('lists a webhook with no creator id as the deleted user', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Null creator webhook guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Null Creator Webhook');
		await new WebhookRepository().update(createWebhookID(BigInt(webhook.id)), {creatorId: null});
		const webhooks = await getChannelWebhooks(harness, channelId, owner.token);
		const listed = webhooks.find((entry) => entry.id === webhook.id);
		expect(listed).toBeDefined();
		expect(listed?.user.id).toBe(DELETED_USER_ID.toString());
		expect(listed?.user.username).toBe(DELETED_USER_USERNAME);
	});
	it('lists a webhook whose creator row is gone as the deleted user', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Vanished creator webhook guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Vanished Creator Webhook');
		await new WebhookRepository().update(createWebhookID(BigInt(webhook.id)), {creatorId: VANISHED_CREATOR_ID});
		const webhooks = await getChannelWebhooks(harness, channelId, owner.token);
		const listed = webhooks.find((entry) => entry.id === webhook.id);
		expect(listed).toBeDefined();
		expect(listed?.user.id).toBe(VANISHED_CREATOR_ID.toString());
		expect(listed?.user.username).toBe(DELETED_USER_USERNAME);
	});
	it('answers a multipart execution for a webhook with no creator id with an access decision', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Null creator multipart guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Null Creator Multipart Webhook');
		await new WebhookRepository().update(createWebhookID(BigInt(webhook.id)), {creatorId: null});
		const {response, text} = await executeWebhookWithAttachments(harness, {
			webhookId: webhook.id,
			webhookToken: webhook.token,
			payload: {
				attachments: [{id: 0, filename: 'orphaned.txt'}],
			},
			files: [{index: 0, filename: 'orphaned.txt', data: Buffer.from('uploaded by an orphaned webhook')}],
		});
		expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
		expect(parseErrorCode(text)).toBe(APIErrorCodes.ACCESS_DENIED);
	});
	it('answers a multipart execution for a webhook whose creator row is gone with an access decision', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Vanished creator multipart guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Vanished Creator Multipart Webhook');
		await new WebhookRepository().update(createWebhookID(BigInt(webhook.id)), {creatorId: VANISHED_CREATOR_ID});
		const {response, text} = await executeWebhookWithAttachments(harness, {
			webhookId: webhook.id,
			webhookToken: webhook.token,
			payload: {
				attachments: [{id: 0, filename: 'vanished.txt'}],
			},
			files: [{index: 0, filename: 'vanished.txt', data: Buffer.from('uploaded by a vanished creator')}],
		});
		expect(response.status).toBe(HTTP_STATUS.FORBIDDEN);
		expect(parseErrorCode(text)).toBe(APIErrorCodes.ACCESS_DENIED);
	});
	it('executes a json payload for a webhook with no creator id', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Null creator json guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Null Creator Json Webhook');
		await new WebhookRepository().update(createWebhookID(BigInt(webhook.id)), {creatorId: null});
		const {response, json} = await executeWebhook(
			harness,
			webhook.id,
			webhook.token,
			{content: 'sent by an orphaned webhook', wait: true},
			200,
		);
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(json?.content).toBe('sent by an orphaned webhook');
	});
});
