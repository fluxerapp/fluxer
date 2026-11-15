// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {createFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import {deleteOneOrMany, fetchMany} from '@app/api/database/CassandraQueryExecution';
import type {RelationshipRow} from '@app/api/database/types/UserTypes';
import {Relationships, RelationshipsByTarget} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {RelationshipResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {beforeEach, describe, expect, test} from 'vitest';

describe('Account deletion with an unindexed inbound relationship', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('removes the friend row pointing at the deleted user even without its index row', async () => {
		const account = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		await createFriendship(harness, account, friend);
		const accountId = createUserID(BigInt(account.userId));
		const friendId = createUserID(BigInt(friend.userId));
		await deleteOneOrMany(
			RelationshipsByTarget.deleteByPk({
				target_user_id: accountId,
				source_user_id: friendId,
				type: RelationshipTypes.FRIEND,
			}),
		);
		await deleteAccount(harness, account.token, account.password);
		const past = new Date();
		past.setMinutes(past.getMinutes() - 1);
		await setPendingDeletionAt(harness, account.userId, past);
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId);
		const friendRows = await fetchMany<RelationshipRow>(
			Relationships.selectCql({where: Relationships.where.eq('source_user_id')}),
			{source_user_id: friendId},
		);
		expect(friendRows.some((row) => row.target_user_id === accountId)).toBe(false);
		const relationships = await createBuilder<Array<RelationshipResponse>>(harness, friend.token)
			.get('/users/@me/relationships')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(relationships.some((rel) => rel.id === account.userId)).toBe(false);
	});
});
