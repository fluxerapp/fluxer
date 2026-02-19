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

import {readFileSync} from 'node:fs';
import {join} from 'node:path';
import {createTestAccount, type TestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {ensureSessionStarted} from '@fluxer/api/src/message/tests/MessageTestUtils';
import type {ApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export interface AttachmentMetadata {
	id: number;
	filename: string;
	title?: string;
	description?: string;
	flags?: number;
}

export function createMultipartFormData(
	payload: Record<string, unknown>,
	files: Array<{index: number; filename: string; data: Buffer}>,
): {body: Buffer; contentType: string} {
	const boundary = `----FormBoundary${Math.random().toString(16).slice(2)}`;

	const chunks: Array<Buffer> = [];

	const payloadJson = JSON.stringify(payload);
	chunks.push(Buffer.from(`--${boundary}\r\n`));
	chunks.push(Buffer.from(`Content-Disposition: form-data; name="payload_json"\r\n`));
	chunks.push(Buffer.from(`Content-Type: application/json\r\n\r\n`));
	chunks.push(Buffer.from(`${payloadJson}\r\n`));

	for (const file of files) {
		chunks.push(Buffer.from(`--${boundary}\r\n`));
		chunks.push(
			Buffer.from(`Content-Disposition: form-data; name="files[${file.index}]"; filename="${file.filename}"\r\n`),
		);
		chunks.push(Buffer.from(`Content-Type: application/octet-stream\r\n\r\n`));
		chunks.push(file.data);
		chunks.push(Buffer.from(`\r\n`));
	}

	chunks.push(Buffer.from(`--${boundary}--\r\n`));

	return {
		body: Buffer.concat(chunks),
		contentType: `multipart/form-data; boundary=${boundary}`,
	};
}

export function loadFixture(filename: string): Buffer {
	const fixturesPath = join(import.meta.dirname, '..', '..', 'test', 'fixtures', filename);
	return readFileSync(fixturesPath);
}

export async function createGuild(harness: ApiTestHarness, token: string, name: string): Promise<GuildResponse> {
	return createBuilder<GuildResponse>(harness, token).post('/guilds').body({name}).execute();
}

export async function createChannel(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	name: string,
	type = 0,
): Promise<ChannelResponse> {
	return createBuilder<ChannelResponse>(harness, token)
		.post(`/guilds/${guildId}/channels`)
		.body({name, type})
		.execute();
}

export async function sendMessageWithAttachments(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	payload: Record<string, unknown>,
	files: Array<{index: number; filename: string; data: Buffer}>,
): Promise<{response: Response; text: string; json: MessageResponse}> {
	await ensureSessionStarted(harness, token);

	const {body, contentType} = createMultipartFormData(payload, files);

	const mergedHeaders = new Headers();
	mergedHeaders.set('Content-Type', contentType);
	mergedHeaders.set('Authorization', token);
	if (!mergedHeaders.has('x-forwarded-for')) {
		mergedHeaders.set('x-forwarded-for', '127.0.0.1');
	}

	const response = await harness.app.request(`/channels/${channelId}/messages`, {
		method: 'POST',
		headers: mergedHeaders,
		body,
	});

	const text = await response.text();
	let json: unknown = null;
	try {
		json = text.length > 0 ? (JSON.parse(text) as unknown) : null;
	} catch {
		json = null;
	}

	return {response, text, json: json as MessageResponse};
}

export async function getMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
): Promise<{response: Response; json: MessageResponse}> {
	return createBuilder<MessageResponse>(harness, token)
		.get(`/channels/${channelId}/messages/${messageId}`)
		.executeWithResponse();
}

export async function setupTestGuildAndChannel(
	harness: ApiTestHarness,
	account?: TestAccount,
): Promise<{account: TestAccount; guild: GuildResponse; channel: ChannelResponse}> {
	const testAccount = account ?? (await createTestAccount(harness));

	const guild = await createGuild(harness, testAccount.token, 'Test Guild');

	const channel = await createChannel(harness, testAccount.token, guild.id, 'test-channel');

	return {account: testAccount, guild, channel};
}

export async function createTestAccountForAttachmentTests(harness: ApiTestHarness): Promise<TestAccount> {
	const account = await createTestAccount(harness);
	await ensureSessionStarted(harness, account.token);
	return account;
}
