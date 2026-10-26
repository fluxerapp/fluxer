// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {VALID_PNG_BASE64} from '@app/api/emoji/tests/EmojiTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {DEFAULT_REQUEST_BODY_LIMIT, jsonAssetBodyLimit} from '@app/api/middleware/RequestBodyLimitMiddleware';
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
});
