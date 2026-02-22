/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createMultipartFormData} from '@fluxer/api/src/channel/tests/AttachmentTestUtils';
import {getPngDataUrl, VALID_PNG_BASE64} from '@fluxer/api/src/emoji/tests/EmojiTestUtils';
import type {ApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import type {GuildEmojiWithUserResponse} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {WebhookResponse, WebhookTokenResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';

export async function createWebhook(
	harness: ApiTestHarness,
	channelId: string,
	token: string,
	name: string,
): Promise<WebhookResponse> {
	return createBuilder<WebhookResponse>(harness, token).post(`/channels/${channelId}/webhooks`).body({name}).execute();
}

export async function getWebhook(harness: ApiTestHarness, webhookId: string, token: string): Promise<WebhookResponse> {
	return createBuilder<WebhookResponse>(harness, token).get(`/webhooks/${webhookId}`).execute();
}

export async function getWebhookByToken(
	harness: ApiTestHarness,
	webhookId: string,
	token: string,
): Promise<WebhookTokenResponse> {
	return createBuilderWithoutAuth<WebhookTokenResponse>(harness).get(`/webhooks/${webhookId}/${token}`).execute();
}

export async function updateWebhook(
	harness: ApiTestHarness,
	webhookId: string,
	token: string,
	updates: {name?: string},
): Promise<WebhookResponse> {
	return createBuilder<WebhookResponse>(harness, token).patch(`/webhooks/${webhookId}`).body(updates).execute();
}

export async function updateWebhookByToken(
	harness: ApiTestHarness,
	webhookId: string,
	webhookToken: string,
	updates: {name?: string},
): Promise<WebhookTokenResponse> {
	return createBuilderWithoutAuth<WebhookTokenResponse>(harness)
		.patch(`/webhooks/${webhookId}/${webhookToken}`)
		.body(updates)
		.execute();
}

export async function deleteWebhook(harness: ApiTestHarness, webhookId: string, token: string): Promise<void> {
	return createBuilder<void>(harness, token).delete(`/webhooks/${webhookId}`).expect(204).execute();
}

export async function deleteWebhookByToken(
	harness: ApiTestHarness,
	webhookId: string,
	webhookToken: string,
): Promise<void> {
	return createBuilderWithoutAuth<void>(harness).delete(`/webhooks/${webhookId}/${webhookToken}`).expect(204).execute();
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
): Promise<{response: Response; json: MessageResponse | null}> {
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
		files: Array<{index: number; filename: string; data: Buffer}>;
		wait?: boolean;
	},
): Promise<{response: Response; text: string; json: MessageResponse | null}> {
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

export async function createGuildEmoji(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	name: string,
	imageData?: string,
): Promise<GuildEmojiWithUserResponse> {
	const image = imageData ?? getPngDataUrl(VALID_PNG_BASE64);
	const {response, json} = await createBuilder<GuildEmojiWithUserResponse>(harness, token)
		.post(`/guilds/${guildId}/emojis`)
		.body({
			name,
			image,
		})
		.executeWithResponse();

	if (response.status !== 200) {
		const errorBody = JSON.stringify(json);
		throw new Error(`Failed to create emoji: ${response.status} - ${errorBody}`);
	}

	return json as GuildEmojiWithUserResponse;
}

export async function createGuildEmojiWithFile(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	name: string,
	_filename: string,
	_mimeType: string,
): Promise<GuildEmojiWithUserResponse> {
	return createGuildEmoji(harness, token, guildId, name);
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

export async function getChannelMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
): Promise<MessageResponse> {
	return createBuilder<MessageResponse>(harness, token).get(`/channels/${channelId}/messages/${messageId}`).execute();
}

export async function grantStaffAccess(harness: ApiTestHarness, userId: string): Promise<void> {
	await createBuilderWithoutAuth(harness).patch(`/test/users/${userId}/flags`).body({flags: 1}).execute();
}

const CREATE_EXPRESSIONS = 0x08000000n;

export async function grantCreateExpressionsPermission(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
): Promise<void> {
	const everyoneRoleId = guildId;

	const roles = await createBuilder<Array<{id: string; permissions: string}>>(harness, token)
		.get(`/guilds/${guildId}/roles`)
		.execute();

	const everyoneRole = roles.find((r) => r.id === everyoneRoleId);

	if (!everyoneRole) {
		throw new Error('Could not find @everyone role');
	}

	const currentPerms = BigInt(everyoneRole.permissions);
	const newPerms = currentPerms | CREATE_EXPRESSIONS;

	await createBuilder<unknown>(harness, token)
		.patch(`/guilds/${guildId}/roles/${everyoneRoleId}`)
		.body({
			permissions: newPerms.toString(),
		})
		.execute();
}

export async function createChannelInvite(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<{code: string}> {
	return createBuilder<{code: string}>(harness, token)
		.post(`/channels/${channelId}/invites`)
		.body({
			max_age: 86400,
		})
		.execute();
}
