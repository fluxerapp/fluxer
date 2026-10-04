// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	sendChannelMessage,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('DM everyone mention', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('does not mention everyone in a one-to-one DM', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		const dm = await createDmChannel(harness, user1.token, user2.userId);
		const everyone = await sendChannelMessage(harness, user1.token, dm.id, '@everyone test');
		const here = await sendChannelMessage(harness, user1.token, dm.id, '@here test');
		expect(everyone.mention_everyone).toBe(false);
		expect(here.mention_everyone).toBe(false);
	});
	it('still mentions everyone in a group DM', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const user3 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		await createFriendship(harness, user1, user3);
		const groupDm = await createGroupDmChannel(harness, user1.token, [user2.userId, user3.userId]);
		const message = await sendChannelMessage(harness, user1.token, groupDm.id, '@everyone test');
		expect(message.mention_everyone).toBe(true);
	});
});
