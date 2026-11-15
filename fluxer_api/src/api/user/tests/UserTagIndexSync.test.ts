// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const USER_ID = createUserID(1500000000000000001n);

function row(username: string, discriminator: number): UserRow {
	return {...EMPTY_USER_ROW, user_id: USER_ID, username, discriminator, bot: false, system: false, version: 1};
}

describe('users_by_username follows the tag stored on the user', () => {
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

	async function expectStoredTagIndexed(users: UserRepository): Promise<void> {
		const stored = await users.findUnique(USER_ID);
		expect(stored).not.toBeNull();
		const holder = await users.findByUsernameDiscriminator(stored!.username, stored!.discriminator);
		expect(holder?.id).toBe(USER_ID);
	}

	test('a discriminator patch from a stale snapshot keeps the stored tag findable', async () => {
		const users = new UserRepository();
		await users.create(row('alpha', 1));
		const stale = (await users.findUnique(USER_ID))!.toRow();
		await users.patchUpsert(USER_ID, {username: 'bravo', discriminator: 2});
		await users.patchUpsert(USER_ID, {discriminator: 7}, stale);
		await expectStoredTagIndexed(users);
	});

	test('a username patch from a stale snapshot keeps the stored tag findable', async () => {
		const users = new UserRepository();
		await users.create(row('alpha', 1));
		const stale = (await users.findUnique(USER_ID))!.toRow();
		await users.patchUpsert(USER_ID, {discriminator: 2});
		await users.patchUpsert(USER_ID, {username: 'charlie'}, stale);
		await expectStoredTagIndexed(users);
	});
});
