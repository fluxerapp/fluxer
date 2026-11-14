// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMultipartFormData} from '@app/api/channel/tests/AttachmentTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {WebhookCreateResponse, WebhookResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';

export async function createWebhook(
	harness: ApiTestHarness,
	channelId: string,
	token: string,
	name: string,
): Promise<WebhookCreateResponse> {
	return createBuilder<WebhookCreateResponse>(harness, token)
		.post(`/channels/${channelId}/webhooks`)
		.body({name})
		.execute();
}

export async function deleteWebhook(harness: ApiTestHarness, webhookId: string, token: string): Promise<void> {
	return createBuilder<void>(harness, token).delete(`/webhooks/${webhookId}`).expect(204).execute();
}

export async function deleteWebhookMessageByToken(
	harness: ApiTestHarness,
	webhookId: string,
	webhookToken: string,
	messageId: string,
): Promise<void> {
	return createBuilderWithoutAuth<void>(harness)
		.delete(`/webhooks/${webhookId}/${webhookToken}/messages/${messageId}`)
		.expect(204)
		.execute();
}

export async function executeWebhook(
	harness: ApiTestHarness,
	webhookId: string,
	webhookToken: string,
	data: {
		content?: string;
		username?: string;
		wait?: boolean;
	},
	expectedStatus: 200 | 204 = 204,
): Promise<{
	response: Response;
	json: MessageResponse | null;
}> {
	const waitParam = data.wait ? '?wait=true' : '';
	const {response, json} = await createBuilderWithoutAuth<MessageResponse | null>(harness)
		.post(`/webhooks/${webhookId}/${webhookToken}${waitParam}`)
		.body({
			content: data.content,
			username: data.username,
		})
		.expect(expectedStatus)
		.executeWithResponse();
	return {
		response,
		json: response.status === 200 ? json : null,
	};
}

export async function executeWebhookWithAttachments(
	harness: ApiTestHarness,
	params: {
		webhookId: string;
		webhookToken: string;
		payload: Record<string, unknown>;
		files: Array<{
			index: number;
			filename: string;
			data: Buffer;
		}>;
		wait?: boolean;
	},
): Promise<{
	response: Response;
	text: string;
	json: MessageResponse | null;
}> {
	const {webhookId, webhookToken, payload, files} = params;
	const wait = params.wait ?? true;
	const waitQuery = wait ? '?wait=true' : '';
	const {body, contentType} = createMultipartFormData(payload, files);
	const headers = new Headers();
	headers.set('Content-Type', contentType);
	headers.set('x-forwarded-for', '127.0.0.1');
	const response = await harness.app.request(`/webhooks/${webhookId}/${webhookToken}${waitQuery}`, {
		method: 'POST',
		headers,
		body,
	});
	const text = await response.text();
	let json: MessageResponse | null = null;
	try {
		json = text.length > 0 ? (JSON.parse(text) as MessageResponse) : null;
	} catch {
		json = null;
	}
	return {response, text, json};
}

export async function getGuildWebhooks(
	harness: ApiTestHarness,
	guildId: string,
	token: string,
): Promise<Array<WebhookResponse>> {
	return createBuilder<Array<WebhookResponse>>(harness, token).get(`/guilds/${guildId}/webhooks`).execute();
}

export async function getChannelWebhooks(
	harness: ApiTestHarness,
	channelId: string,
	token: string,
): Promise<Array<WebhookResponse>> {
	return createBuilder<Array<WebhookResponse>>(harness, token).get(`/channels/${channelId}/webhooks`).execute();
}

export async function sendChannelMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	content: string,
): Promise<MessageResponse> {
	return createBuilder<MessageResponse>(harness, token)
		.post(`/channels/${channelId}/messages`)
		.body({content})
		.execute();
}

export async function createChannelInvite(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<{
	code: string;
}> {
	return createBuilder<{
		code: string;
	}>(harness, token)
		.post(`/channels/${channelId}/invites`)
		.body({
			max_age: 86400,
		})
		.execute();
}
