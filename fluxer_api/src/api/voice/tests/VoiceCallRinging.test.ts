// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createFriendship, createGroupDmChannel} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ErrorResponse {
	code: string;
	errors?: Array<{
		code?: string;
	}>;
}

describe('Voice Call Ringing', () => {
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
	async function setupGroupDmUsers() {
		const owner = await createTestAccount(harness);
		const memberOne = await createTestAccount(harness);
		const memberTwo = await createTestAccount(harness);
		const outsider = await createTestAccount(harness);
		await createFriendship(harness, owner, memberOne);
		await createFriendship(harness, owner, memberTwo);
		const groupDm = await createGroupDmChannel(harness, owner.token, [memberOne.userId, memberTwo.userId]);
		return {owner, memberOne, memberTwo, outsider, groupDm};
	}
	describe('Ring call', () => {
		it('non-member cannot ring group DM call', async () => {
			const {owner, memberOne, outsider, groupDm} = await setupGroupDmUsers();
			await createBuilder(harness, owner.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId]})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilder(harness, outsider.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId]})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
		});
		it('rejects non-recipient ids in group DM ring payload', async () => {
			const {owner, memberOne, outsider, groupDm} = await setupGroupDmUsers();
			const error = await createBuilder<ErrorResponse>(harness, owner.token)
				.post(`/channels/${groupDm.id}/call/ring`)
				.body({recipients: [memberOne.userId, outsider.userId]})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			expect(error.errors?.[0]?.code).toBe('USER_NOT_IN_CHANNEL');
		});
	});
});
