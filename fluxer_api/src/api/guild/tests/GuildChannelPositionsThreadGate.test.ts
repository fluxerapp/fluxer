// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface ValidationResponse {
	code: string;
	errors: Array<{path: string; code: string}>;
}

describe('guild channel positions with thread-only channels', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup() {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'positions');
		const category = await createChannel(harness, owner.token, guild.id, 'cat', ChannelTypes.GUILD_CATEGORY);
		const text = await createChannel(harness, owner.token, guild.id, 'text');
		const forum = await threadsRequest<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'forum', type: ChannelTypes.GUILD_FORUM})
			.execute();
		return {owner, guildId: guild.id, categoryId: category.id, textId: text.id, forumId: forum.id};
	}

	async function rejected(token: string, guildId: string, body: Array<Record<string, unknown>>) {
		const response = await createBuilder<ValidationResponse>(harness, token)
			.patch(`/guilds/${guildId}/channels`)
			.body(body)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		return response.errors[0];
	}

	test('a non-viewer naming a forum id gets the same errors as an unknown id', async () => {
		const s = await setup();
		const unknownId = '1';
		const asId = await rejected(s.owner.token, s.guildId, [
			{id: s.forumId, parent_id: s.categoryId, lock_permissions: true},
		]);
		const unknownAsId = await rejected(s.owner.token, s.guildId, [
			{id: unknownId, parent_id: s.categoryId, lock_permissions: true},
		]);
		expect(asId).toEqual(unknownAsId);
		expect(asId?.code).toBe(ValidationErrorCodes.CHANNEL_NOT_FOUND);

		const asParent = await rejected(s.owner.token, s.guildId, [{id: s.textId, parent_id: s.forumId}]);
		expect(asParent).toEqual(await rejected(s.owner.token, s.guildId, [{id: s.textId, parent_id: unknownId}]));
		expect(asParent?.code).toBe(ValidationErrorCodes.INVALID_PARENT_CHANNEL);

		const asSibling = await rejected(s.owner.token, s.guildId, [{id: s.textId, preceding_sibling_id: s.forumId}]);
		expect(asSibling?.path).toBe('preceding_sibling_id');
		expect(asSibling?.code).toBe(ValidationErrorCodes.INVALID_CHANNEL_ID);

		const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token).get(`/channels/${s.forumId}`).execute();
		expect(forum.parent_id ?? null).toBeNull();
	});

	test('a viewer can still move a forum', async () => {
		const s = await setup();
		await threadsRequest(harness, s.owner.token)
			.patch(`/guilds/${s.guildId}/channels`)
			.body([{id: s.forumId, parent_id: s.categoryId}])
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token).get(`/channels/${s.forumId}`).execute();
		expect(forum.parent_id).toBe(s.categoryId);
	});
});
