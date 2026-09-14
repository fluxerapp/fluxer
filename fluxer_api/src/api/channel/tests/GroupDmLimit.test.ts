// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship, seedPrivateChannels} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const MAX_GROUP_DM_LIMIT = 150;
const MAX_GROUP_DMS_ERROR_CODE = 'MAX_GROUP_DMS';

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
	it('rejects creating group DM when user has reached the limit', async () => {
		const creator = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		const helper = await createTestAccount(harness);
		await ensureSessionStarted(harness, creator.token);
		await ensureSessionStarted(harness, target.token);
		await ensureSessionStarted(harness, recipient.token);
		await ensureSessionStarted(harness, helper.token);
		await createFriendship(harness, creator, target);
		await createFriendship(harness, creator, recipient);
		const seedResult = await seedPrivateChannels(harness, target.token, target.userId, {
			group_dm_count: MAX_GROUP_DM_LIMIT,
			recipients: [helper.userId, recipient.userId],
			clear_existing: true,
		});
		expect(seedResult.group_dms.length).toBe(MAX_GROUP_DM_LIMIT);
		await createBuilder(harness, creator.token)
			.post('/users/@me/channels')
			.body({
				recipients: [helper.userId, target.userId],
			})
			.expect(HTTP_STATUS.BAD_REQUEST, MAX_GROUP_DMS_ERROR_CODE)
			.execute();
	});
});
