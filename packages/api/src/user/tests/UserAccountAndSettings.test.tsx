/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {
	checkUsernameDiscriminatorAvailability,
	fetchUser,
	fetchUserProfile,
	preloadMessages,
	setUserNote,
	updateGuildSettings,
	updateUserProfile,
} from '@fluxer/api/src/user/tests/UserTestUtils';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('User Account And Settings', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('user can update profile and settings', async () => {
		const account = await createTestAccount(harness);
		const newGlobal = `Integration ${Date.now()}`;
		const newBio = 'Integration tests ensure user endpoints behave';

		const updated = await updateUserProfile(harness, account.token, {
			global_name: newGlobal,
			bio: newBio,
		});
		expect(updated.json.global_name).toBe(newGlobal);
		expect(updated.json.bio).toBe(newBio);

		const checkTagResult = await checkUsernameDiscriminatorAvailability(
			harness,
			updated.json.username,
			updated.json.discriminator,
			account.token,
		);
		expect(checkTagResult.json.taken).toBe(false);

		const user = await fetchUser(harness, account.userId, account.token);
		expect(user.json.id).toBe(account.userId);

		const profile = await fetchUserProfile(harness, account.userId, account.token);
		expect(profile.json.user.id).toBe(account.userId);

		const guildSettings = await updateGuildSettings(harness, account.token, {
			suppress_everyone: true,
		});
		const settings = guildSettings.json as Record<string, unknown>;
		expect(settings.suppress_everyone).toBe(true);

		const target = await createTestAccount(harness);
		await setUserNote(harness, account.token, target.userId, 'Great tester');

		const preload = await preloadMessages(harness, account.token, []);
		const preloadData = preload.json as Record<string, unknown>;
		expect(Object.keys(preloadData).length).toBe(0);
	});

	test('nonexistent user returns deleted user fallback', async () => {
		const account = await createTestAccount(harness);

		const user = await createBuilder<{
			id: string;
			username: string;
			discriminator: string;
			global_name: string | null;
			avatar: string | null;
		}>(harness, account.token)
			.get(`/users/${TEST_IDS.NONEXISTENT_USER}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(user.id).toBe(TEST_IDS.NONEXISTENT_USER);
		expect(user.username).toBe('DeletedUser');
		expect(user.discriminator).toBe('0000');
		expect(user.avatar).toBeNull();
	});

	test('reject getting nonexistent user profile', async () => {
		const account = await createTestAccount(harness);

		await createBuilder(harness, account.token)
			.get(`/users/${TEST_IDS.NONEXISTENT_USER}/profile`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	test('check-tag with missing username returns 400', async () => {
		const account = await createTestAccount(harness);

		await createBuilder(harness, account.token)
			.get('/users/check-tag?discriminator=1234')
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('check-tag with missing discriminator returns 400', async () => {
		const account = await createTestAccount(harness);

		await createBuilder(harness, account.token)
			.get('/users/check-tag?username=testuser')
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	test('check-tag with invalid discriminator returns 400', async () => {
		const account = await createTestAccount(harness);

		await createBuilder(harness, account.token)
			.get('/users/check-tag?username=test&discriminator=invalid')
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});
