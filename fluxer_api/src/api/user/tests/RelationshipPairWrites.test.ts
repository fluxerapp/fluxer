// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getKvMeta} from '@app/api/database/CassandraMetaRegistry';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	acceptFriendRequest,
	listRelationships,
	removeRelationship,
	sendFriendRequest,
} from '@app/api/user/tests/RelationshipTestUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface WriteFailure {
	sourceUserId: bigint;
	type: number;
}

function failsWrite(failure: WriteFailure | null, meta: KvQueryMeta | null | undefined, params: object): boolean {
	if (!failure || meta?.table.name !== 'relationships' || meta.action === 'select' || meta.action === 'count') {
		return false;
	}
	const row = params as CassandraParams;
	return row.source_user_id === failure.sourceUserId && row.type === failure.type;
}

describe('relationship pair writes', () => {
	let harness: ApiTestHarness;
	let storage: InMemoryCassandraQueryExecutor;
	let writeFailure: WriteFailure | null = null;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		storage = new InMemoryCassandraQueryExecutor();
		const executor: CassandraQueryExecutorForTesting = {
			async executeQuery<T, P extends CassandraParams>(query: PreparedQuery<P>): Promise<Array<T>> {
				if (failsWrite(writeFailure, query.kvMeta ?? getKvMeta(query.cql), query.params)) {
					throw new Error('The database write failed');
				}
				return storage.executeQuery<T>(query);
			},
			async executeBatch(queries, atomic) {
				if (queries.some((query) => failsWrite(writeFailure, query.meta ?? getKvMeta(query.query), query.params))) {
					throw new Error('The database batch failed');
				}
				await storage.executeBatch(queries, atomic);
			},
			reset: () => storage.reset(),
		};
		setCassandraQueryExecutorForTesting(executor);
	});

	beforeEach(async () => {
		writeFailure = null;
		await harness.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function relationshipTypes(account: TestAccount, other: TestAccount): Promise<Array<number>> {
		const {json} = await listRelationships(harness, account.token);
		return json.filter((relationship) => relationship.id === other.userId).map((relationship) => relationship.type);
	}

	async function makeFriends(alice: TestAccount, bob: TestAccount): Promise<void> {
		await sendFriendRequest(harness, alice.token, bob.userId);
		await acceptFriendRequest(harness, bob.token, alice.userId);
	}

	test('a failed unfriend leaves both users friends', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await makeFriends(alice, bob);
		writeFailure = {sourceUserId: BigInt(bob.userId), type: RelationshipTypes.FRIEND};
		await createBuilder(harness, alice.token)
			.delete(`/users/@me/relationships/${bob.userId}`)
			.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
			.execute();
		writeFailure = null;
		expect(await relationshipTypes(alice, bob)).toEqual([RelationshipTypes.FRIEND]);
		expect(await relationshipTypes(bob, alice)).toEqual([RelationshipTypes.FRIEND]);
		await removeRelationship(harness, alice.token, bob.userId);
		expect(await relationshipTypes(alice, bob)).toEqual([]);
		expect(await relationshipTypes(bob, alice)).toEqual([]);
	});

	test('a failed block of a friend leaves both users friends', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await makeFriends(alice, bob);
		writeFailure = {sourceUserId: BigInt(bob.userId), type: RelationshipTypes.FRIEND};
		await createBuilder(harness, alice.token)
			.put(`/users/@me/relationships/${bob.userId}`)
			.body({type: RelationshipTypes.BLOCKED})
			.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
			.execute();
		writeFailure = null;
		expect(await relationshipTypes(alice, bob)).toEqual([RelationshipTypes.FRIEND]);
		expect(await relationshipTypes(bob, alice)).toEqual([RelationshipTypes.FRIEND]);
	});

	test('a failed accept leaves the request pending on both sides', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await sendFriendRequest(harness, alice.token, bob.userId);
		writeFailure = {sourceUserId: BigInt(alice.userId), type: RelationshipTypes.FRIEND};
		await createBuilder(harness, bob.token)
			.put(`/users/@me/relationships/${alice.userId}`)
			.body({})
			.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
			.execute();
		writeFailure = null;
		expect(await relationshipTypes(alice, bob)).toEqual([RelationshipTypes.OUTGOING_REQUEST]);
		expect(await relationshipTypes(bob, alice)).toEqual([RelationshipTypes.INCOMING_REQUEST]);
		await acceptFriendRequest(harness, bob.token, alice.userId);
		expect(await relationshipTypes(alice, bob)).toEqual([RelationshipTypes.FRIEND]);
		expect(await relationshipTypes(bob, alice)).toEqual([RelationshipTypes.FRIEND]);
	});

	test('a failed friend request writes neither side', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		writeFailure = {sourceUserId: BigInt(bob.userId), type: RelationshipTypes.INCOMING_REQUEST};
		await createBuilder(harness, alice.token)
			.post(`/users/@me/relationships/${bob.userId}`)
			.body({})
			.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
			.execute();
		writeFailure = null;
		expect(await relationshipTypes(alice, bob)).toEqual([]);
		expect(await relationshipTypes(bob, alice)).toEqual([]);
	});

	test('a failed withdrawal keeps the request on both sides', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await sendFriendRequest(harness, alice.token, bob.userId);
		writeFailure = {sourceUserId: BigInt(bob.userId), type: RelationshipTypes.INCOMING_REQUEST};
		await createBuilder(harness, alice.token)
			.delete(`/users/@me/relationships/${bob.userId}`)
			.expect(HTTP_STATUS.INTERNAL_SERVER_ERROR)
			.execute();
		writeFailure = null;
		expect(await relationshipTypes(alice, bob)).toEqual([RelationshipTypes.OUTGOING_REQUEST]);
		expect(await relationshipTypes(bob, alice)).toEqual([RelationshipTypes.INCOMING_REQUEST]);
	});
});
