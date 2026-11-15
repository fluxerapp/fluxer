// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship, createGroupDmChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {setLimitOverride} from '@app/api/test/LimitTestUtils';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Group DM recipient limit', () => {
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
	it('counts a recipient’s open group DMs against the cap', async () => {
		const creator = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const helper = await createTestAccount(harness);
		const other = await createTestAccount(harness);
		for (const account of [creator, target, helper, other]) {
			await ensureSessionStarted(harness, account.token);
		}
		await createFriendship(harness, helper, target);
		await createFriendship(harness, helper, other);
		await createFriendship(harness, creator, target);
		await createFriendship(harness, creator, helper);
		const restore = await setLimitOverride(harness, {max_group_dms_per_user: 1});
		try {
			await createGroupDmChannel(harness, helper.token, [target.userId, other.userId]);
			await createBuilder(harness, creator.token)
				.post('/users/@me/channels')
				.body({recipients: [helper.userId, target.userId]})
				.expect(HTTP_STATUS.BAD_REQUEST, 'MAX_GROUP_DMS')
				.execute();
		} finally {
			await restore();
		}
	});
});
