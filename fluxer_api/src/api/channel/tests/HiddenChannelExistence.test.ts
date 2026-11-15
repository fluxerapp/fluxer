// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createChannel,
	createPermissionOverwrite,
	deleteChannel,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Hidden channel existence', () => {
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

	it('answers a hidden channel the same way as a deleted one', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const hidden = await createChannel(harness, owner.token, guild.id, 'hidden');
		const deleted = await createChannel(harness, owner.token, guild.id, 'deleted');
		await createPermissionOverwrite(harness, owner.token, hidden.id, member.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		await deleteChannel(harness, owner.token, deleted.id);

		for (const channelId of [hidden.id, deleted.id]) {
			await createBuilder(harness, member.token)
				.get(`/channels/${channelId}`)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
			await createBuilder(harness, member.token)
				.get(`/channels/${channelId}/messages`)
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
		}
	});
});
