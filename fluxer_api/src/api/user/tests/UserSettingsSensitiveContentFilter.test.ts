// SPDX-License-Identifier: AGPL-3.0-or-later

import {SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {fetchUserSettings, updateUserSettings} from './UserTestUtils';

describe('User Settings - Sensitive Content Filters', () => {
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
	describe('default values', () => {
		test('adult account has correct defaults', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
			const {json} = await fetchUserSettings(harness, account.token);
			expect(json.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.SHOW);
			expect(json.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLOCK);
			expect(json.sensitive_content_guild_filter).toBe(SensitiveMediaFilterLevel.BLUR);
		});
		test('teen account has correct defaults', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2010-01-01'});
			const {json} = await fetchUserSettings(harness, account.token);
			expect(json.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
			expect(json.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLOCK);
			expect(json.sensitive_content_guild_filter).toBe(SensitiveMediaFilterLevel.BLUR);
		});
	});
	describe('updating individual fields', () => {
		test('updates sensitive_content_friend_dm_filter', async () => {
			const account = await createTestAccount(harness);
			const {json} = await updateUserSettings(harness, account.token, {
				sensitive_content_friend_dm_filter: SensitiveMediaFilterLevel.BLUR,
			});
			expect(json.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
		});
		test('updates sensitive_content_non_friend_dm_filter', async () => {
			const account = await createTestAccount(harness);
			const {json} = await updateUserSettings(harness, account.token, {
				sensitive_content_non_friend_dm_filter: SensitiveMediaFilterLevel.SHOW,
			});
			expect(json.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.SHOW);
		});
		test('updates sensitive_content_guild_filter', async () => {
			const account = await createTestAccount(harness);
			const {json} = await updateUserSettings(harness, account.token, {
				sensitive_content_guild_filter: SensitiveMediaFilterLevel.SHOW,
			});
			expect(json.sensitive_content_guild_filter).toBe(SensitiveMediaFilterLevel.SHOW);
		});
	});
	describe('updating all fields at once', () => {
		test('updates all three filters simultaneously', async () => {
			const account = await createTestAccount(harness);
			const {json} = await updateUserSettings(harness, account.token, {
				sensitive_content_friend_dm_filter: SensitiveMediaFilterLevel.BLOCK,
				sensitive_content_non_friend_dm_filter: SensitiveMediaFilterLevel.BLUR,
				sensitive_content_guild_filter: SensitiveMediaFilterLevel.SHOW,
			});
			expect(json.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLOCK);
			expect(json.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
			expect(json.sensitive_content_guild_filter).toBe(SensitiveMediaFilterLevel.SHOW);
		});
	});
	describe('validation', () => {
		test('rejects invalid value for sensitive_content_friend_dm_filter', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me/settings')
				.body({sensitive_content_friend_dm_filter: 3})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('rejects a non-adult account relaxing the friend DM filter', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2010-01-01'});
			const response = await createBuilder<{
				code: string;
				errors: Array<{
					path: string;
					code: string;
					message: string;
				}>;
			}>(harness, account.token)
				.patch('/users/@me/settings')
				.body({sensitive_content_friend_dm_filter: SensitiveMediaFilterLevel.SHOW})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(response.code).toBe('VALIDATION_ERROR');
			expect(response.errors[0]?.path).toBe('sensitive_content_friend_dm_filter');
			expect(response.errors[0]?.code).toBe(ValidationErrorCodes.AGE_RESTRICTED);
		});
		test('rejects negative value', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me/settings')
				.body({sensitive_content_friend_dm_filter: -1})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('rejects string value', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me/settings')
				.body({sensitive_content_non_friend_dm_filter: 'blur'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('rejects BLOCK for sensitive_content_guild_filter', async () => {
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.patch('/users/@me/settings')
				.body({sensitive_content_guild_filter: SensitiveMediaFilterLevel.BLOCK})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('persistence', () => {
		test('values persist across GET after PATCH', async () => {
			const account = await createTestAccount(harness);
			await updateUserSettings(harness, account.token, {
				sensitive_content_friend_dm_filter: SensitiveMediaFilterLevel.BLOCK,
				sensitive_content_non_friend_dm_filter: SensitiveMediaFilterLevel.BLUR,
				sensitive_content_guild_filter: SensitiveMediaFilterLevel.SHOW,
			});
			const {json} = await fetchUserSettings(harness, account.token);
			expect(json.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLOCK);
			expect(json.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
			expect(json.sensitive_content_guild_filter).toBe(SensitiveMediaFilterLevel.SHOW);
		});
	});
});
