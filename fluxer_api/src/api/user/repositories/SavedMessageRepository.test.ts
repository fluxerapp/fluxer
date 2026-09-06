// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createChannelID, createMessageID, createUserID, type UserID} from '../../BrandedTypes';
import {setCassandraQueryExecutorForTesting} from '../../database/CassandraQueryExecution';
import {InMemoryCassandraQueryExecutor} from '../../test/InMemoryCassandraQueryExecutor';
import {SavedMessageRepository} from './SavedMessageRepository';

const CHANNEL_ID = createChannelID(10n);

let executor: InMemoryCassandraQueryExecutor;

async function seedSavedMessages(repository: SavedMessageRepository, userId: UserID, count: number) {
	for (let index = 0; index < count; index++) {
		await repository.createSavedMessage(userId, CHANNEL_ID, createMessageID(BigInt(1000 + index)));
	}
}

describe('SavedMessageRepository.countSavedMessages', () => {
	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});
	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('returns zero when the user has saved nothing', async () => {
		const repository = new SavedMessageRepository();
		expect(await repository.countSavedMessages(createUserID(1n))).toBe(0);
	});

	it('counts past the ceiling a listing page can report', async () => {
		const repository = new SavedMessageRepository();
		const userId = createUserID(1n);
		await seedSavedMessages(repository, userId, 1200);
		expect(await repository.listSavedMessages(userId, 1000)).toHaveLength(1000);
		expect(await repository.countSavedMessages(userId)).toBe(1200);
	});

	it('follows creates and deletes and stays scoped to one user', async () => {
		const repository = new SavedMessageRepository();
		const userId = createUserID(1n);
		const otherUserId = createUserID(2n);
		await seedSavedMessages(repository, userId, 3);
		await seedSavedMessages(repository, otherUserId, 7);
		expect(await repository.countSavedMessages(userId)).toBe(3);
		await repository.deleteSavedMessage(userId, createMessageID(1001n));
		expect(await repository.countSavedMessages(userId)).toBe(2);
		expect(await repository.countSavedMessages(otherUserId)).toBe(7);
		await repository.deleteAllSavedMessages(userId);
		expect(await repository.countSavedMessages(userId)).toBe(0);
		expect(await repository.countSavedMessages(otherUserId)).toBe(7);
	});
});
