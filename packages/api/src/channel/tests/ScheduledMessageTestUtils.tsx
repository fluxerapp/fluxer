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

import {ensureSessionStarted} from '@fluxer/api/src/message/tests/MessageTestUtils';
import type {ApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {ScheduledMessageResponseSchema} from '@fluxer/schema/src/domains/message/ScheduledMessageSchemas';
import type {z} from 'zod';

export type ScheduledMessageResponse = z.infer<typeof ScheduledMessageResponseSchema>;

export async function createGuildChannel(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	name: string,
): Promise<{id: string}> {
	const {response, json} = await createBuilder<{id: string}>(harness, token)
		.post(`/guilds/${guildId}/channels`)
		.body({name, type: 0})
		.executeWithResponse();

	if (response.status !== 200 && response.status !== 201) {
		throw new Error(`Failed to create guild channel: ${response.status}`);
	}

	return json as {id: string};
}

export async function grantStaffAccess(harness: ApiTestHarness, userId: string): Promise<void> {
	await createBuilderWithoutAuth(harness).patch(`/test/users/${userId}/flags`).body({flags: 1}).execute();
}

export async function scheduleMessage(
	harness: ApiTestHarness,
	channelId: string,
	token: string,
	content: string,
	scheduledAt?: Date,
): Promise<ScheduledMessageResponse> {
	const scheduledTime = scheduledAt ?? new Date(Date.now() + 60 * 1000);

	await ensureSessionStarted(harness, token);

	const {json} = await createBuilder<ScheduledMessageResponse>(harness, token)
		.post(`/channels/${channelId}/messages/schedule`)
		.body({
			content,
			scheduled_local_at: scheduledTime.toISOString(),
			timezone: 'UTC',
		})
		.expect(201)
		.executeWithResponse();

	return json as ScheduledMessageResponse;
}

export async function updateScheduledMessage(
	harness: ApiTestHarness,
	scheduledMessageId: string,
	token: string,
	updates: {
		content?: string;
		scheduled_local_at?: string;
		timezone?: string;
	},
): Promise<ScheduledMessageResponse> {
	const {json} = await createBuilder<ScheduledMessageResponse>(harness, token)
		.patch(`/users/@me/scheduled-messages/${scheduledMessageId}`)
		.body(updates)
		.expect(200)
		.executeWithResponse();

	return json as ScheduledMessageResponse;
}

export async function getScheduledMessages(
	harness: ApiTestHarness,
	token: string,
): Promise<Array<ScheduledMessageResponse>> {
	const {json} = await createBuilder<Array<ScheduledMessageResponse>>(harness, token)
		.get('/users/@me/scheduled-messages')
		.expect(200)
		.executeWithResponse();

	return json;
}

export async function getScheduledMessage(
	harness: ApiTestHarness,
	scheduledMessageId: string,
	token: string,
	expectedStatus: 200 | 404 = 200,
): Promise<ScheduledMessageResponse | null> {
	const {response, json} = await createBuilder<ScheduledMessageResponse>(harness, token)
		.get(`/users/@me/scheduled-messages/${scheduledMessageId}`)
		.expect(expectedStatus)
		.executeWithResponse();

	if (response.status === 404) {
		return null;
	}

	return json as ScheduledMessageResponse;
}

export async function cancelScheduledMessage(
	harness: ApiTestHarness,
	scheduledMessageId: string,
	token: string,
): Promise<void> {
	await createBuilder<void>(harness, token)
		.delete(`/users/@me/scheduled-messages/${scheduledMessageId}`)
		.expect(204)
		.execute();
}

export async function triggerScheduledMessageWorker(
	harness: ApiTestHarness,
	userId: string,
	scheduledMessageId: string,
): Promise<void> {
	await createBuilder<void>(harness, '')
		.post(`/test/worker/send-scheduled-message/${userId}/${scheduledMessageId}`)
		.expect(200)
		.execute();
}

export async function getChannelMessages(
	harness: ApiTestHarness,
	channelId: string,
	token: string,
	limit = 20,
): Promise<Array<MessageResponse>> {
	const {json} = await createBuilder<Array<MessageResponse>>(harness, token)
		.get(`/channels/${channelId}/messages?limit=${limit}`)
		.expect(200)
		.executeWithResponse();

	return json;
}

export function messageFromAuthorContains(
	messages: Array<MessageResponse>,
	authorId: string,
	content: string,
): boolean {
	return messages.some((msg) => msg.author.id === authorId && msg.content === content);
}

export function containsMessageContent(messages: Array<MessageResponse>, content: string): boolean {
	return messages.some((msg) => msg.content === content);
}

export async function createChannelInvite(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<{code: string}> {
	const {json} = await createBuilder<{code: string}>(harness, token)
		.post(`/channels/${channelId}/invites`)
		.body({
			max_age: 86400,
		})
		.expect(200)
		.executeWithResponse();

	return json;
}

export async function joinGuild(harness: ApiTestHarness, token: string, inviteCode: string): Promise<void> {
	await createBuilder<void>(harness, token).post(`/invites/${inviteCode}`).body({}).expect(200).execute();
}

export async function removeGuildMember(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	userId: string,
): Promise<void> {
	await createBuilder<void>(harness, token).delete(`/guilds/${guildId}/members/${userId}`).expect(204).execute();
}
