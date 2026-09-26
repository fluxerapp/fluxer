// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS, TEST_CREDENTIALS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {fetchUserMe} from '@app/api/user/tests/UserTestUtils';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface UserMeResponse {
	id: string;
	username: string;
	discriminator: string;
	email: string | null;
}

async function updateUserUsername(
	harness: ApiTestHarness,
	token: string,
	newUsername: string,
): Promise<UserMeResponse> {
	return createBuilder<UserMeResponse>(harness, token)
		.patch('/users/@me')
		.body({username: newUsername, password: TEST_CREDENTIALS.STRONG_PASSWORD})
		.execute();
}

describe('UserContactChangeLogService', () => {
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
	describe('email change operations', () => {
		test('direct email change is rejected - requires email_token flow', async () => {
			const account = await createTestAccount(harness, {
				email: 'original@example.com',
			});
			const {json} = await createBuilder(harness, account.token)
				.patch('/users/@me')
				.body({email: 'updated@example.com', password: TEST_CREDENTIALS.STRONG_PASSWORD})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			const body = json as {
				errors?: Array<{
					code: string;
				}>;
			};
			expect(body.errors?.[0]?.code).toBe('EMAIL_MUST_BE_CHANGED_VIA_TOKEN');
		});
		test('email change without password is rejected', async () => {
			const account = await createTestAccount(harness);
			const json = await createBuilder(harness, account.token)
				.patch('/users/@me')
				.body({email: 'new@example.com'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
			const body = json as {
				errors?: Array<{
					code: string;
				}>;
			};
			expect(body.errors?.[0]?.code).toBe('EMAIL_MUST_BE_CHANGED_VIA_TOKEN');
		});
	});
	describe('username change operations', () => {
		test('username change updates user profile', async () => {
			const account = await createTestAccount(harness, {
				username: 'originaluser',
			});
			const updatedUser = await updateUserUsername(harness, account.token, 'newusername');
			expect(updatedUser.username).toBe('newusername');
		});
		test('username change without password requires sudo mode', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me')
				.body({username: 'newusername'})
				.expect(HTTP_STATUS.FORBIDDEN, 'SUDO_MODE_REQUIRED')
				.execute();
		});
	});
	describe('no change scenarios', () => {
		test('updating to same username value succeeds', async () => {
			const originalUsername = `sameuser_${Date.now()}`;
			const account = await createTestAccount(harness, {
				username: originalUsername,
			});
			const updatedUser = await updateUserUsername(harness, account.token, originalUsername);
			expect(updatedUser.username).toBe(originalUsername);
		});
	});
	describe('sequential changes', () => {
		test('multiple sequential username changes', async () => {
			const account = await createTestAccount(harness, {
				username: 'seqname1',
			});
			const result1 = await updateUserUsername(harness, account.token, 'seqname2');
			expect(result1.username).toBe('seqname2');
			const result2 = await updateUserUsername(harness, account.token, 'seqname3');
			expect(result2.username).toBe('seqname3');
			const {json: finalState} = await fetchUserMe(harness, account.token);
			expect(finalState.username).toBe('seqname3');
		});
	});
	describe('validation errors', () => {
		test('invalid email format is rejected with format error', async () => {
			const account = await createTestAccount(harness);
			const {json} = await createBuilder(harness, account.token)
				.patch('/users/@me')
				.body({email: 'invalid-email', password: TEST_CREDENTIALS.STRONG_PASSWORD})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.executeWithResponse();
			const body = json as {
				errors?: Array<{
					code: string;
				}>;
			};
			expect(body.errors?.[0]?.code).toBe('INVALID_EMAIL_FORMAT');
		});
		test('empty username rejected', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me')
				.body({username: '', password: TEST_CREDENTIALS.STRONG_PASSWORD})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
});
