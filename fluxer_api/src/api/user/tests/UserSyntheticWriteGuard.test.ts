// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {DELETED_USER_ID} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const SYNTHETIC_USER_IDS: Array<[string, UserID]> = [
	['0', createUserID(0n)],
	['1', createUserID(DELETED_USER_ID)],
];

describe('users table write guard for synthetic accounts', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
	});
	test.each(SYNTHETIC_USER_IDS)('patchUpsert refuses to create a row for user %s', async (_label, userId) => {
		const repository = new UserRepository();
		await expect(repository.patchUpsert(userId, {bio: 'written by a test'})).rejects.toThrow();
		expect(await repository.listUsers([userId])).toEqual([]);
	});
	test.each(SYNTHETIC_USER_IDS)('updateLastActiveAt refuses to create a row for user %s', async (_label, userId) => {
		const repository = new UserRepository();
		await expect(repository.updateLastActiveAt({userId, lastActiveAt: new Date(0)})).rejects.toThrow();
		expect(await repository.listUsers([userId])).toEqual([]);
	});
	test.each(SYNTHETIC_USER_IDS)(
		'findUnique still synthesises user %s after a refused write',
		async (_label, userId) => {
			const repository = new UserRepository();
			await expect(repository.patchUpsert(userId, {bio: 'written by a test'})).rejects.toThrow();
			const user = await repository.findUnique(userId);
			expect(user).not.toBeNull();
			expect(user?.id.toString()).toBe(userId.toString());
		},
	);
});
