// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {fetchUserSettings, updateUserSettings} from '@app/api/user/tests/UserTestUtils';
import {DateOfBirthPolicies} from '@fluxer/constants/src/AccountConfigConstants';
import {SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

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
			expect(json.default_guilds_restricted).toBe(false);
		});
		test('teen account has correct defaults', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2010-01-01'});
			const {json} = await fetchUserSettings(harness, account.token);
			expect(json.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
			expect(json.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLOCK);
			expect(json.sensitive_content_guild_filter).toBe(SensitiveMediaFilterLevel.BLUR);
			expect(json.default_guilds_restricted).toBe(true);
		});
	});
	describe('allow_mature_content_on_ios', () => {
		test('adult can turn on allow_mature_content_on_ios and it defaults to false', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
			const {json} = await fetchUserSettings(harness, account.token);
			expect(json.allow_mature_content_on_ios).toBe(false);
			const {json: updated} = await updateUserSettings(harness, account.token, {allow_mature_content_on_ios: true});
			expect(updated.allow_mature_content_on_ios).toBe(true);
		});
		test('teen gets AGE_RESTRICTED for allow_mature_content_on_ios true', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2010-01-01'});
			const response = await createBuilder<{
				code: string;
				errors: Array<{path: string; code: string}>;
			}>(harness, account.token)
				.patch('/users/@me/settings')
				.body({allow_mature_content_on_ios: true})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(response.errors[0]?.path).toBe('allow_mature_content_on_ios');
			expect(response.errors[0]?.code).toBe(ValidationErrorCodes.AGE_RESTRICTED);
		});
		test('setting a teen date of birth turns allow_mature_content_on_ios off', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
			await updateUserSettings(harness, account.token, {allow_mature_content_on_ios: true});
			const user = await getUserRepository().findUniqueAssert(createUserID(BigInt(account.userId)));
			await getUserRepository().patchUpsert(user.id, {date_of_birth: null}, user.toRow());
			const teenBirthday = new Date();
			teenBirthday.setUTCFullYear(teenBirthday.getUTCFullYear() - 15);
			await createBuilder(harness, account.token)
				.put('/users/@me/date-of-birth')
				.body({date_of_birth: teenBirthday.toISOString().slice(0, 10)})
				.execute();
			const {json} = await fetchUserSettings(harness, account.token);
			expect(json.allow_mature_content_on_ios).toBe(false);
		});
	});
	describe('validation', () => {
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
		test('lets a non-adult account tighten the non-friend DM filter', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2010-01-01'});
			const {json} = await updateUserSettings(harness, account.token, {
				sensitive_content_non_friend_dm_filter: SensitiveMediaFilterLevel.BLUR,
			});
			expect(json.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLUR);
			const {json: blocked} = await updateUserSettings(harness, account.token, {
				sensitive_content_non_friend_dm_filter: SensitiveMediaFilterLevel.BLOCK,
			});
			expect(blocked.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.BLOCK);
		});
		test('rejects a non-adult account relaxing the non-friend DM filter', async () => {
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
				.body({sensitive_content_non_friend_dm_filter: SensitiveMediaFilterLevel.SHOW})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(response.code).toBe('VALIDATION_ERROR');
			expect(response.errors[0]?.path).toBe('sensitive_content_non_friend_dm_filter');
			expect(response.errors[0]?.code).toBe(ValidationErrorCodes.AGE_RESTRICTED);
		});
		test('allows an account without a birth date to relax every filter when none is collected', async () => {
			const repository = getInstanceConfigRepository();
			const previous = (await repository.getAccountsConfig()).date_of_birth;
			await repository.updateAccountsConfig({date_of_birth: DateOfBirthPolicies.OFF});
			try {
				const account = await createTestAccount(harness);
				const {json} = await updateUserSettings(harness, account.token, {
					sensitive_content_friend_dm_filter: SensitiveMediaFilterLevel.SHOW,
					sensitive_content_non_friend_dm_filter: SensitiveMediaFilterLevel.SHOW,
					sensitive_content_guild_filter: SensitiveMediaFilterLevel.SHOW,
				});
				expect(json.sensitive_content_friend_dm_filter).toBe(SensitiveMediaFilterLevel.SHOW);
				expect(json.sensitive_content_non_friend_dm_filter).toBe(SensitiveMediaFilterLevel.SHOW);
				expect(json.sensitive_content_guild_filter).toBe(SensitiveMediaFilterLevel.SHOW);
			} finally {
				await repository.updateAccountsConfig({date_of_birth: previous});
			}
		});
		test('rejects a non-adult account relaxing a filter after collection is turned off', async () => {
			const account = await createTestAccount(harness, {dateOfBirth: '2010-01-01'});
			const repository = getInstanceConfigRepository();
			const previous = (await repository.getAccountsConfig()).date_of_birth;
			await repository.updateAccountsConfig({date_of_birth: DateOfBirthPolicies.OFF});
			try {
				const response = await createBuilder<{
					code: string;
					errors: Array<{
						path: string;
						code: string;
						message: string;
					}>;
				}>(harness, account.token)
					.patch('/users/@me/settings')
					.body({sensitive_content_guild_filter: SensitiveMediaFilterLevel.SHOW})
					.expect(HTTP_STATUS.BAD_REQUEST)
					.execute();
				expect(response.errors[0]?.path).toBe('sensitive_content_guild_filter');
				expect(response.errors[0]?.code).toBe(ValidationErrorCodes.AGE_RESTRICTED);
			} finally {
				await repository.updateAccountsConfig({date_of_birth: previous});
			}
		});
	});
});
