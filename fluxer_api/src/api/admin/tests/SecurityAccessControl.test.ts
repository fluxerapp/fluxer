// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, test} from 'vitest';

describe('Security Access Control', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	describe('Blocked User Restrictions', () => {
		test('blocked user cannot create DM', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createBuilder(harness, `${user1.token}`)
				.put(`/users/@me/relationships/${user2.userId}`)
				.body({
					type: 2,
				})
				.execute();
			await createBuilder(harness, `${user2.token}`)
				.post('/users/@me/channels')
				.body({
					recipients: [user1.userId],
				})
				.expect(HTTP_STATUS.BAD_REQUEST, 'GROUP_DM_RECIPIENTS_NOT_ADDABLE')
				.execute();
		});
	});
	describe('Unauthorized Access', () => {
		test('unauthorized channel access is rejected', async () => {
			const owner = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);
			const createJson = await createBuilder<{
				id: string;
			}>(harness, `${owner.token}`)
				.post('/guilds')
				.body({
					name: 'Private Guild',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const guildId = createJson.id;
			await createBuilder(harness, `${attacker.token}`)
				.get(`/guilds/${guildId}/channels`)
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
				.execute();
		});
	});
});
