// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {sendFriendRequest} from '@app/api/user/tests/RelationshipTestUtils';
import {
	checkUsernameDiscriminatorAvailability,
	fetchUser,
	fetchUserProfile,
	preloadMessages,
	setUserNote,
	updateGuildSettings,
	updateUserProfile,
} from '@app/api/user/tests/UserTestUtils';
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
		expect(settings.mobile_push).toBe(true);
		const target = await createTestAccount(harness);
		await setUserNote(harness, account.token, target.userId, 'Great tester');
		const preload = await preloadMessages(harness, account.token, []);
		const preloadData = preload.json as Record<string, unknown>;
		expect(Object.keys(preloadData).length).toBe(0);
	});
	test('nonexistent user returns unknown user', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.get(`/users/${TEST_IDS.NONEXISTENT_USER}`)
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_USER')
			.execute();
	});
	test('reject getting nonexistent user profile', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.get(`/users/${TEST_IDS.NONEXISTENT_USER}/profile`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('pending outgoing friend request allows viewing target profile', async () => {
		const requester = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await createBuilder(harness, requester.token)
			.get(`/users/${target.userId}/profile`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACCESS')
			.execute();
		await sendFriendRequest(harness, requester.token, target.userId);
		const profile = await fetchUserProfile(harness, target.userId, requester.token);
		expect(profile.json.user.id).toBe(target.userId);
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
