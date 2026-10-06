// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship, createGroupDmChannel, getChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

describe('Group DM name clear', () => {
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
	it.each([
		['an empty string', ''],
		['null', null],
	])('sends a null name to every recipient when cleared with %s', async (_label, clearedName) => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const user3 = await createTestAccount(harness);
		await ensureSessionStarted(harness, user1.token);
		await ensureSessionStarted(harness, user2.token);
		await ensureSessionStarted(harness, user3.token);
		await createFriendship(harness, user1, user2);
		await createFriendship(harness, user1, user3);
		const groupDm = await createGroupDmChannel(harness, user1.token, [user2.userId, user3.userId]);
		await createBuilder<ChannelResponse>(harness, user1.token)
			.patch(`/channels/${groupDm.id}`)
			.body({name: 'Weekend plans'})
			.expect(HTTP_STATUS.OK)
			.execute();
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
		try {
			const cleared = await createBuilder<ChannelResponse>(harness, user1.token)
				.patch(`/channels/${groupDm.id}`)
				.body({name: clearedName})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(cleared).toHaveProperty('name', null);
			const channelUpdates = dispatchSpy.mock.calls.filter(([params]) => params.event === 'CHANNEL_UPDATE');
			expect(channelUpdates.map(([params]) => params.userId.toString()).sort()).toEqual(
				[user1.userId, user2.userId, user3.userId].sort(),
			);
			for (const [params] of channelUpdates) {
				expect(params.data).toHaveProperty('name', null);
			}
		} finally {
			dispatchSpy.mockRestore();
		}
		expect(await getChannel(harness, user2.token, groupDm.id)).toHaveProperty('name', null);
	});
});
