// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID, type UserID} from '@app/api/BrandedTypes';
import {fetchMany} from '@app/api/database/CassandraQueryExecution';
import {GuildModerationRepository} from '@app/api/guild/repositories/GuildModerationRepository';
import {Notes, RelationshipsByTarget} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {UserRelationshipRepository} from '@app/api/user/repositories/UserRelationshipRepository';
import {deleteAccount, setPendingDeletionAt, waitForDeletionCompletion} from '@app/api/user/tests/UserTestUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const RELATIONSHIPS_EACH_WAY = 320;
const GUILD_BANS = 150;
const NOTES_ABOUT_USER = 120;
const FIRST_OTHER_ID = 1_300_000_000_000_000_000n;

function otherUserId(index: number): UserID {
	return createUserID(FIRST_OTHER_ID + BigInt(index));
}

describe('Deleting an account with a large footprint', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		vi.restoreAllMocks();
		await harness?.shutdown();
	});

	test('relationships, guild bans and notes are removed in bounded batches', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const relationships = new UserRelationshipRepository();
		const moderation = new GuildModerationRepository();
		for (let index = 0; index < RELATIONSHIPS_EACH_WAY; index++) {
			const other = otherUserId(index);
			for (const [source, target] of [
				[userId, other],
				[other, userId],
			] as const) {
				await relationships.upsertRelationship({
					source_user_id: source,
					target_user_id: target,
					type: RelationshipTypes.FRIEND,
					nickname: null,
					since: new Date(),
					share_voice_activity: null,
					version: 1,
				});
			}
		}
		for (let index = 0; index < GUILD_BANS; index++) {
			await moderation.upsertBan({
				guild_id: createGuildID(FIRST_OTHER_ID + BigInt(index)),
				user_id: userId,
				moderator_id: otherUserId(index),
				banned_at: new Date(),
				expires_at: null,
				reason: null,
				ip: null,
				email: account.email,
			});
		}
		for (let index = 0; index < NOTES_ABOUT_USER; index++) {
			await relationships.upsertUserNote(otherUserId(index), userId, `note ${index}`);
		}
		await deleteAccount(harness, account.token, account.password);
		await setPendingDeletionAt(harness, account.userId, new Date(Date.now() - 60_000));
		const batchSpy = vi.spyOn(InMemoryCassandraQueryExecutor.prototype, 'executeBatch');
		const result = await createBuilderWithoutAuth<{errors?: Array<{error: string}>}>(harness)
			.post('/test/worker/process-pending-deletions')
			.execute();
		expect(result.errors ?? []).toEqual([]);
		await waitForDeletionCompletion(harness, account.userId);
		expect(Math.max(...batchSpy.mock.calls.map(([queries]) => queries.length))).toBeLessThanOrEqual(60);
		expect(await relationships.listRelationships(userId)).toHaveLength(0);
		const pointingAtUser = await fetchMany(
			RelationshipsByTarget.selectCql({where: RelationshipsByTarget.where.eq('target_user_id')}),
			{target_user_id: userId},
		);
		expect(pointingAtUser).toHaveLength(0);
		expect(await relationships.listRelationships(otherUserId(0))).toHaveLength(0);
		for (const index of [0, GUILD_BANS - 1]) {
			expect(await moderation.getBan(createGuildID(FIRST_OTHER_ID + BigInt(index)), userId)).toBeNull();
		}
		const remainingNotes = await fetchMany<{target_user_id: bigint}>(
			Notes.selectCql({where: Notes.where.eq('source_user_id')}),
			{source_user_id: otherUserId(0)},
		);
		expect(remainingNotes.filter((note) => note.target_user_id === BigInt(userId))).toHaveLength(0);
	}, 120_000);
});
