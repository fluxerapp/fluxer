// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getMessages} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook, deleteWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const CONVERSATION_UUID = '3f1c2a7e-8b4d-4e6a-9c1f-5d2e7b8a9c01';

function createContactMessage(textContent: string) {
	return {
		id: 311,
		created_at: '2026-10-15T09:12:30.101552Z',
		updated_at: '2026-10-15T09:12:30.101552Z',
		uuid: 'c8a1f2e3-4b5d-4c6e-8f7a-9b0c1d2e3f40',
		type: 'incoming',
		status: 'received',
		conversation_id: 42,
		conversation_uuid: CONVERSATION_UUID,
		content: `<p>${textContent}</p>`,
		text_content: textContent,
		content_type: 'html',
		private: false,
		sender_id: 17,
		sender_type: 'contact',
		meta: {from: ['contact@example.com'], to: ['support@example.com'], subject: 'Cannot sign in'},
		attachments: [],
		author: {
			id: 17,
			first_name: 'Example contact',
			last_name: '',
			email: 'contact@example.com',
			avatar_url: null,
			availability_status: 'offline',
			type: 'contact',
			last_active_at: null,
		},
	};
}

async function setup(harness: ApiTestHarness, name: string) {
	const owner = await createTestAccount(harness);
	const guild = await createGuild(harness, owner.token, `${name} Guild`);
	const channelId = guild.system_channel_id!;
	const webhook = await createWebhook(harness, channelId, owner.token, `${name} Webhook`);
	return {owner, channelId, webhook};
}

async function webhookMessages(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	webhookId: string,
): Promise<Array<MessageResponse>> {
	const messages = await getMessages(harness, token, channelId);
	return messages.filter((message) => message.webhook_id === webhookId);
}

describe('Webhook Libredesk integration', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('POST /webhooks/:webhook_id/:token/libredesk', () => {
		it('posts a test delivery under the Libredesk name and ignores the signature header', async () => {
			const {owner, channelId, webhook} = await setup(harness, 'Libredesk Test Delivery');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/libredesk`)
				.header('User-Agent', 'Libredesk-Webhook/unknown-version')
				.header('X-Libredesk-Signature', 'sha256=0000000000000000000000000000000000000000000000000000000000000000')
				.body({event: 'webhook.test', payload: {id: 3, name: 'Support feed'}, timestamp: '2026-10-15T09:00:00Z'})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const messages = await webhookMessages(harness, owner.token, channelId, webhook.id);
			expect(messages).toHaveLength(1);
			expect(messages[0]!.author.username).toBe('Libredesk');
			expect(messages[0]!.embeds?.[0]?.title).toBe('Test delivery received');
			expect(messages[0]!.embeds?.[0]?.description).toBe('Support feed');
			await deleteWebhook(harness, webhook.id, owner.token);
		});

		it('posts a contact message without resolving any mention', async () => {
			const {owner, channelId, webhook} = await setup(harness, 'Libredesk Mentions');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/libredesk`)
				.body({
					event: 'message.created',
					payload: createContactMessage(`@everyone @here <@${owner.userId}> please help`),
					timestamp: '2026-10-15T09:12:30Z',
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const messages = await webhookMessages(harness, owner.token, channelId, webhook.id);
			expect(messages).toHaveLength(1);
			const message = messages[0]!;
			expect(message.content).toBe('');
			expect(message.mention_everyone).toBe(false);
			expect(message.mentions ?? []).toHaveLength(0);
			expect(message.mention_roles ?? []).toHaveLength(0);
			expect(message.embeds?.[0]?.title).toBe('New message');
			expect(message.embeds?.[0]?.description).toBe(`\\@everyone \\@here \\<\\@${owner.userId}> please help`);
			await deleteWebhook(harness, webhook.id, owner.token);
		});

		it('posts every delivery because Libredesk sends no delivery identifier', async () => {
			const {owner, channelId, webhook} = await setup(harness, 'Libredesk Repeat');
			const body = {
				event: 'message.created',
				payload: createContactMessage('Still cannot sign in.'),
				timestamp: '2026-10-15T09:12:30Z',
			};
			for (let attempt = 0; attempt < 2; attempt++) {
				await createBuilderWithoutAuth(harness)
					.post(`/webhooks/${webhook.id}/${webhook.token}/libredesk`)
					.body(body)
					.expect(HTTP_STATUS.NO_CONTENT)
					.execute();
			}
			expect(await webhookMessages(harness, owner.token, channelId, webhook.id)).toHaveLength(2);
			await deleteWebhook(harness, webhook.id, owner.token);
		});

		it('acknowledges unknown events and activity lines without posting', async () => {
			const {owner, channelId, webhook} = await setup(harness, 'Libredesk Ignored');
			const bodies = [
				{event: 'inbox.renamed', payload: {id: 1, name: 'Support'}},
				{
					event: 'message.created',
					payload: {
						...createContactMessage('Support agent self-assigned this conversation'),
						type: 'activity',
						private: true,
						sender_type: 'agent',
						meta: {activity_type: 'self_assign'},
					},
				},
				{event: 'message.updated', payload: {...createContactMessage('Sent'), status: 'sent'}},
			];
			for (const body of bodies) {
				await createBuilderWithoutAuth(harness)
					.post(`/webhooks/${webhook.id}/${webhook.token}/libredesk`)
					.body(body)
					.expect(HTTP_STATUS.NO_CONTENT)
					.execute();
			}
			expect(await webhookMessages(harness, owner.token, channelId, webhook.id)).toHaveLength(0);
			await deleteWebhook(harness, webhook.id, owner.token);
		});

		it('posts an automation event with a name over 256 characters', async () => {
			const {owner, channelId, webhook} = await setup(harness, 'Libredesk Long Event');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/libredesk`)
				.body({
					event: 'x'.repeat(300),
					payload: {
						actor_id: 1,
						conversation: {
							uuid: CONVERSATION_UUID,
							reference_number: '1042',
							subject: 'Cannot sign in',
							inbox_name: 'Support',
						},
					},
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const messages = await webhookMessages(harness, owner.token, channelId, webhook.id);
			expect(messages).toHaveLength(1);
			expect(messages[0]!.embeds?.[0]?.title).toBe(`\\[Support\\] ${'x'.repeat(29)}...: #1042 Cannot sign in`);
			await deleteWebhook(harness, webhook.id, owner.token);
		});

		it('rejects a body without an event', async () => {
			const {owner, webhook} = await setup(harness, 'Libredesk Invalid Body');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}/libredesk`)
				.body({payload: {id: 3, name: 'Support feed'}})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});

		it('rejects a libredesk webhook with an invalid token', async () => {
			const {owner, webhook} = await setup(harness, 'Libredesk Invalid Token');
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/invalid_token/libredesk`)
				.body({event: 'webhook.test', payload: {id: 3, name: 'Support feed'}})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_WEBHOOK)
				.execute();
			await deleteWebhook(harness, webhook.id, owner.token);
		});
	});
});
