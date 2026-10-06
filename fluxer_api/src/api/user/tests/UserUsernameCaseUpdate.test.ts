// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {fetchUserMe, updateUserProfile} from '@app/api/user/tests/UserTestUtils';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function runCaseUpdateTest(
	harness: ApiTestHarness,
	testFn: (params: {
		account: {
			userId: string;
			token: string;
			email: string;
			password: string;
		};
		initialUser: {
			username: string;
			discriminator: string;
		};
	}) => Promise<void>,
): Promise<void> {
	const account = await createTestAccount(harness);
	const {json: initialUser} = await fetchUserMe(harness, account.token);
	await testFn({
		account,
		initialUser: {
			username: initialUser.username,
			discriminator: initialUser.discriminator,
		},
	});
}

describe('User Username Case Update', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('re-sending existing username keeps discriminator', async () => {
		await runCaseUpdateTest(harness, async ({account, initialUser}) => {
			const updated = await updateUserProfile(harness, account.token, {
				username: initialUser.username,
				password: account.password,
			});
			expect(updated.json.username).toBe(initialUser.username);
			expect(updated.json.discriminator).toBe(initialUser.discriminator);
		});
	});
	test('changing username case preserves discriminator', async () => {
		await runCaseUpdateTest(harness, async ({account, initialUser}) => {
			let newUsername = initialUser.username.toUpperCase();
			if (newUsername === initialUser.username) {
				newUsername = initialUser.username.toLowerCase();
			}
			const updated = await updateUserProfile(harness, account.token, {
				username: newUsername,
				password: account.password,
			});
			expect(updated.json.username).toBe(newUsername);
			expect(updated.json.discriminator).toBe(initialUser.discriminator);
		});
	});
	test('changing username completely works', async () => {
		await runCaseUpdateTest(harness, async ({account, initialUser}) => {
			const newUsername = `diff${initialUser.username.slice(0, Math.min(initialUser.username.length, 28))}`;
			const updated = await updateUserProfile(harness, account.token, {
				username: newUsername,
				password: account.password,
			});
			expect(updated.json.username).toBe(newUsername);
		});
	});
	test('no-op username and discriminator stays unchanged', async () => {
		await runCaseUpdateTest(harness, async ({account, initialUser}) => {
			const updated = await updateUserProfile(harness, account.token, {
				username: initialUser.username,
				discriminator: initialUser.discriminator,
				password: account.password,
			});
			expect(updated.json.username).toBe(initialUser.username);
			expect(updated.json.discriminator).toBe(initialUser.discriminator);
		});
	});
	test('case-only change with explicit discriminator keeps discriminator', async () => {
		await runCaseUpdateTest(harness, async ({account, initialUser}) => {
			let newUsername = initialUser.username.toLowerCase();
			if (newUsername === initialUser.username) {
				newUsername = initialUser.username.toUpperCase();
			}
			const updated = await updateUserProfile(harness, account.token, {
				username: newUsername,
				discriminator: initialUser.discriminator,
				password: account.password,
			});
			expect(updated.json.username).toBe(newUsername);
			expect(updated.json.discriminator).toBe(initialUser.discriminator);
		});
	});
	test('non-premium username change always rerolls discriminator even when explicitly echoed', async () => {
		await runCaseUpdateTest(harness, async ({account, initialUser}) => {
			const newUsername = `preserve${initialUser.username.slice(0, Math.min(initialUser.username.length, 24))}`;
			const updated = await updateUserProfile(harness, account.token, {
				username: newUsername,
				discriminator: initialUser.discriminator,
				password: account.password,
			});
			expect(updated.json.username).toBe(newUsername);
			expect(updated.json.discriminator).not.toBe(initialUser.discriminator);
		});
	});
	test('non-premium username change rerolls when requested discriminator is already taken', async () => {
		await runCaseUpdateTest(harness, async ({account, initialUser}) => {
			const otherAccount = await createTestAccount(harness);
			const {json: otherUser} = await fetchUserMe(harness, otherAccount.token);
			await createBuilder(harness, otherAccount.token)
				.patch(`/test/users/${otherAccount.userId}/discriminator`)
				.body({discriminator: initialUser.discriminator})
				.expect(HTTP_STATUS.OK)
				.execute();
			const updated = await updateUserProfile(harness, account.token, {
				username: otherUser.username,
				discriminator: initialUser.discriminator,
				password: account.password,
			});
			expect(updated.json.username).toBe(otherUser.username);
			expect(updated.json.discriminator).not.toBe(initialUser.discriminator);
		});
	});
});
