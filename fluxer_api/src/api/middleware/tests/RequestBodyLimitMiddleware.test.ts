// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createMultipartFormData, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import {THREADS_FEATURE, THREADS_FEATURE_HEADER} from '@app/api/channel/tests/ThreadTestUtils';
import {VALID_PNG_BASE64} from '@app/api/emoji/tests/EmojiTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {
	DEFAULT_REQUEST_BODY_LIMIT,
	isMultipartFormData,
	jsonAssetBodyLimit,
} from '@app/api/middleware/RequestBodyLimitMiddleware';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {AVATAR_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

function maxSizeAvatarDataUrl(): string {
	const image = Buffer.from(VALID_PNG_BASE64, 'base64');
	return `data:image/png;base64,${Buffer.concat([image, Buffer.alloc(AVATAR_MAX_SIZE - image.length)]).toString('base64')}`;
}

function jsonBodyOfLength(length: number): string {
	const prefix = '{"name":"Sized","pad":"';
	const suffix = '"}';
	return `${prefix}${'a'.repeat(length - prefix.length - suffix.length)}${suffix}`;
}

const MIB = 1024 * 1024;

describe('isMultipartFormData', () => {
	it('matches only the multipart/form-data media type', () => {
		expect(isMultipartFormData('multipart/form-data; boundary=x')).toBe(true);
		expect(isMultipartFormData(' Multipart/Form-Data ;boundary=x')).toBe(true);
		expect(isMultipartFormData('application/json; x=multipart/form-data')).toBe(false);
		expect(isMultipartFormData('multipart/form-data-ish')).toBe(false);
		expect(isMultipartFormData('multipart/mixed')).toBe(false);
		expect(isMultipartFormData(undefined)).toBe(false);
	});
});

describe('request body size limit', () => {
	let harness: ApiTestHarness;
	let owner: TestAccount;
	let guildId: string;
	let channelId: string;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'Body Limit Guild');
		guildId = guild.id;
		channelId = guild.system_channel_id!;
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	function send(path: string, method: string, body: string, headers: Record<string, string> = {}) {
		return harness.requestJson({path, method, body, headers: {Authorization: owner.token, ...headers}});
	}

	it('answers an unknown route with a JSON body with 404', async () => {
		const response = await send('/v1/this-route-does-not-exist', 'POST', JSON.stringify({hello: 'world'}), {
			'Content-Type': 'application/json',
		});

		expect(response.status).toBe(404);
	});

	it('answers an unknown route with a body and no content type with 404', async () => {
		const response = await send('/this-route-does-not-exist', 'POST', 'plain body');

		expect(response.status).toBe(404);
	});

	it('accepts a maximum-size avatar under the default limit', async () => {
		const body = JSON.stringify({name: 'Avatar Hook', avatar: maxSizeAvatarDataUrl()});
		expect(body.length).toBeLessThanOrEqual(DEFAULT_REQUEST_BODY_LIMIT);

		const response = await send(`/channels/${channelId}/webhooks`, 'POST', body);

		expect(response.status).toBe(200);
		expect(((await response.json()) as {avatar: string | null}).avatar).toBeTruthy();
	});

	it('rejects a streamed body one byte over the default limit', async () => {
		const response = await send(
			`/channels/${channelId}/webhooks`,
			'POST',
			jsonBodyOfLength(DEFAULT_REQUEST_BODY_LIMIT + 1),
		);

		expect(response.status).toBe(413);
		expect(await response.json()).toMatchObject({code: 'FILE_SIZE_TOO_LARGE'});
	});

	it('rejects a declared length one byte over the default limit', async () => {
		const body = jsonBodyOfLength(DEFAULT_REQUEST_BODY_LIMIT + 1);

		const response = await send(`/channels/${channelId}/webhooks`, 'POST', body, {
			'content-length': String(body.length),
		});

		expect(response.status).toBe(413);
	});

	it('gives the guild update route room for every guild image at once', async () => {
		const routeLimit = jsonAssetBodyLimit({guild_icon: 1, banner: 1, splash: 1, embed_splash: 1});
		const path = `/guilds/${guildId}`;

		expect((await send(path, 'PATCH', jsonBodyOfLength(DEFAULT_REQUEST_BODY_LIMIT + 1))).status).not.toBe(413);
		expect((await send(path, 'PATCH', jsonBodyOfLength(routeLimit + 1))).status).toBe(413);
	});

	it('gives the GitHub webhook route a 25 MiB limit', async () => {
		const path = '/webhooks/1/token/github';

		expect((await send(path, 'POST', jsonBodyOfLength(DEFAULT_REQUEST_BODY_LIMIT + 1))).status).not.toBe(413);
		expect((await send(path, 'POST', jsonBodyOfLength(25 * 1024 * 1024 + 1))).status).toBe(413);
	});

	it('applies the default limit to multipart bodies on routes that do not take uploads', async () => {
		const {body, contentType} = createMultipartFormData({}, [], {
			includePayloadJson: false,
			fields: {grant_type: 'authorization_code', code: 'a'.repeat(DEFAULT_REQUEST_BODY_LIMIT)},
		});
		expect(body.length).toBeGreaterThan(DEFAULT_REQUEST_BODY_LIMIT);

		const response = await harness.app.request('/oauth2/token', {
			method: 'POST',
			headers: {'Content-Type': contentType, 'x-forwarded-for': '127.0.0.1'},
			body,
		});

		expect(response.status).toBe(413);
		expect(await response.json()).toMatchObject({code: 'FILE_SIZE_TOO_LARGE'});
	});

	it('does not treat a content type that only mentions multipart/form-data as multipart', async () => {
		const response = await harness.app.request('/webhooks/livekit', {
			method: 'POST',
			headers: {'Content-Type': 'application/json; x=multipart/form-data', 'x-forwarded-for': '127.0.0.1'},
			body: Buffer.alloc(20 * MIB),
		});

		expect(response.status).toBe(413);
	});

	it('applies the default limit to multipart bodies when starting a thread from a message', async () => {
		const {body, contentType} = createMultipartFormData({name: 'thread'}, [
			{index: 0, filename: 'big.bin', data: Buffer.alloc(20 * MIB)},
		]);

		const response = await harness.app.request(`/channels/${channelId}/messages/1/threads`, {
			method: 'POST',
			headers: {
				Authorization: owner.token,
				'Content-Type': contentType,
				[THREADS_FEATURE_HEADER]: THREADS_FEATURE,
				'x-forwarded-for': '127.0.0.1',
			},
			body,
		});

		expect(response.status).toBe(413);
	});

	it('keeps the default limit for JSON bodies on upload routes', async () => {
		const response = await send(
			`/channels/${channelId}/messages`,
			'POST',
			jsonBodyOfLength(DEFAULT_REQUEST_BODY_LIMIT + 1),
		);

		expect(response.status).toBe(413);
	});

	it('lets multipart uploads above the default limit through on upload routes', async () => {
		const {response} = await sendMessageWithAttachments(
			harness,
			owner.token,
			channelId,
			{content: 'upload', attachments: [{id: 0, filename: 'big.bin'}]},
			[{index: 0, filename: 'big.bin', data: Buffer.alloc(DEFAULT_REQUEST_BODY_LIMIT + MIB)}],
		);

		expect(response.status).toBe(200);
	});
});
