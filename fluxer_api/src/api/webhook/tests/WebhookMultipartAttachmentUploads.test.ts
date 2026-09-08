// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {loadFixture} from '../../channel/tests/AttachmentTestUtils';
import {createPermissionOverwrite} from '../../channel/tests/ChannelTestUtils';
import {acceptInvite, addMemberRole, createGuild, createRole} from '../../guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createChannelInvite, createWebhook, deleteWebhook, executeWebhookWithAttachments} from './WebhookTestUtils';

describe('Webhook multipart attachment uploads', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	it('executes webhook with attachment-only multipart payload', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook multipart upload guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Multipart Upload Webhook');
		const {response, json} = await executeWebhookWithAttachments(harness, {
			webhookId: webhook.id,
			webhookToken: webhook.token,
			payload: {
				attachments: [{id: 0, filename: 'webhook_upload.png'}],
			},
			files: [{index: 0, filename: 'webhook_upload.png', data: loadFixture('yeah.png')}],
		});
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(json).not.toBeNull();
		expect(json?.webhook_id).toBe(webhook.id);
		expect(json?.attachments).toBeDefined();
		expect(json?.attachments?.length).toBe(1);
		expect(json?.attachments?.[0].filename).toBe('webhook_upload.png');
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('executes webhook multipart payload with content and username overrides', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook multipart override guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Multipart Override Webhook');
		const {response, json} = await executeWebhookWithAttachments(harness, {
			webhookId: webhook.id,
			webhookToken: webhook.token,
			payload: {
				content: 'Webhook with uploaded file',
				username: 'Upload Bot',
				attachments: [{id: 0, filename: 'upload.txt'}],
			},
			files: [{index: 0, filename: 'upload.txt', data: Buffer.from('uploaded through webhook')}],
		});
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(json).not.toBeNull();
		expect(json?.content).toBe('Webhook with uploaded file');
		expect(json?.attachments).toBeDefined();
		expect(json?.attachments?.length).toBe(1);
		expect(json?.attachments?.[0].filename).toBe('upload.txt');
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('executes multipart webhook requests with files but no attachment metadata', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook multipart metadata required guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Metadata Required Webhook');
		const {response} = await executeWebhookWithAttachments(harness, {
			webhookId: webhook.id,
			webhookToken: webhook.token,
			payload: {
				content: 'missing metadata',
			},
			files: [{index: 0, filename: 'missing-metadata.png', data: loadFixture('yeah.png')}],
		});
		expect(response.status).toBe(HTTP_STATUS.OK);
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('rejects multipart webhook requests with attachment metadata but no file upload', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook multipart missing file guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'Missing File Webhook');
		const {response} = await executeWebhookWithAttachments(harness, {
			webhookId: webhook.id,
			webhookToken: webhook.token,
			payload: {
				attachments: [{id: 0, filename: 'missing.png'}],
			},
			files: [],
		});
		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('executes multipart webhook requests when the creator is denied attach files', async () => {
		const owner = await createTestAccount(harness);
		const creator = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook creator denied attach files guild');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, creator.token, invite.code);
		const webhookManagerRole = await createRole(harness, owner.token, guild.id, {
			name: 'Webhook Manager',
			permissions: Permissions.MANAGE_WEBHOOKS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, creator.userId, webhookManagerRole.id);
		const webhook = await createWebhook(harness, channelId, creator.token, 'Denied Creator Webhook');
		await createPermissionOverwrite(harness, owner.token, channelId, creator.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.ATTACH_FILES.toString(),
		});
		const {response, json} = await executeWebhookWithAttachments(harness, {
			webhookId: webhook.id,
			webhookToken: webhook.token,
			payload: {
				attachments: [{id: 0, filename: 'denied_creator.png'}],
			},
			files: [{index: 0, filename: 'denied_creator.png', data: loadFixture('yeah.png')}],
		});
		expect(response.status).toBe(HTTP_STATUS.OK);
		expect(json?.attachments?.length).toBe(1);
		await deleteWebhook(harness, webhook.id, owner.token);
	});
	it('rejects multipart webhook requests when file indices do not match metadata IDs', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Webhook multipart id mismatch guild');
		const channelId = guild.system_channel_id!;
		const webhook = await createWebhook(harness, channelId, owner.token, 'ID Mismatch Webhook');
		const {response} = await executeWebhookWithAttachments(harness, {
			webhookId: webhook.id,
			webhookToken: webhook.token,
			payload: {
				attachments: [{id: 2, filename: 'mismatch.png'}],
			},
			files: [{index: 0, filename: 'mismatch.png', data: loadFixture('yeah.png')}],
		});
		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		await deleteWebhook(harness, webhook.id, owner.token);
	});
});
