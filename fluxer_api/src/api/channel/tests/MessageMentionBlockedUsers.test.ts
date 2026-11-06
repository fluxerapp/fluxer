// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {blockUser, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Message mentions of users who blocked the author', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('drops only the users who blocked the author', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 3);
		const [first, second, third] = members;
		assert(first && second && third, 'Expected three guild members');
		await blockUser(harness, second, owner.userId);
		await blockUser(harness, owner, third.userId);
		const message = await createBuilder<MessageResponse>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: `<@${first.userId}> <@${second.userId}> <@${third.userId}> <@${owner.userId}>`})
			.execute();
		expect(new Set(message.mentions.map((user) => user.id))).toEqual(
			new Set([first.userId, third.userId, owner.userId]),
		);
	});
});
