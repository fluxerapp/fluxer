// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {readFileSync} from 'node:fs';
import {join} from 'node:path';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

interface MultipartFileInput {
	index: number;
	filename: string;
	data: Buffer;
	contentType?: string;
}

interface CreateMultipartFormDataOptions {
	includePayloadJson?: boolean;
	fields?: Record<string, string | number | boolean | Array<string | number | boolean>>;
	fileFieldName?: (file: MultipartFileInput) => string;
}

export function createMultipartFormData(
	payload: Record<string, unknown>,
	files: Array<MultipartFileInput>,
	options?: CreateMultipartFormDataOptions,
): {
	body: Buffer;
	contentType: string;
} {
	const boundary = `----FormBoundary${randomUUID()}`;
	const chunks: Array<Buffer> = [];
	if (options?.includePayloadJson !== false) {
		const payloadJson = JSON.stringify(payload);
		chunks.push(Buffer.from(`--${boundary}\r\n`));
		chunks.push(Buffer.from(`Content-Disposition: form-data; name="payload_json"\r\n`));
		chunks.push(Buffer.from(`Content-Type: application/json\r\n\r\n`));
		chunks.push(Buffer.from(`${payloadJson}\r\n`));
	}
	for (const [fieldName, fieldValue] of Object.entries(options?.fields ?? {})) {
		const values = Array.isArray(fieldValue) ? fieldValue : [fieldValue];
		for (const value of values) {
			chunks.push(Buffer.from(`--${boundary}\r\n`));
			chunks.push(Buffer.from(`Content-Disposition: form-data; name="${fieldName}"\r\n\r\n`));
			chunks.push(Buffer.from(`${value.toString()}\r\n`));
		}
	}
	for (const file of files) {
		const fieldName = options?.fileFieldName?.(file) ?? `files[${file.index}]`;
		chunks.push(Buffer.from(`--${boundary}\r\n`));
		chunks.push(Buffer.from(`Content-Disposition: form-data; name="${fieldName}"; filename="${file.filename}"\r\n`));
		chunks.push(Buffer.from(`Content-Type: ${file.contentType ?? 'application/octet-stream'}\r\n\r\n`));
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
	files: Array<MultipartFileInput>,
	options?: CreateMultipartFormDataOptions,
): Promise<{
	response: Response;
	text: string;
	json: MessageResponse;
}> {
	await ensureSessionStarted(harness, token);
	const {body, contentType} = createMultipartFormData(payload, files, options);
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

export async function setupTestGuildAndChannel(
	harness: ApiTestHarness,
	account?: TestAccount,
): Promise<{
	account: TestAccount;
	guild: GuildResponse;
	channel: ChannelResponse;
}> {
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
