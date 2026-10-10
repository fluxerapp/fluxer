// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {updateUserSettings} from '@app/api/user/tests/UserTestUtils';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {beforeEach, describe, expect, test} from 'vitest';

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('User Settings Validation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('trusted_domains rejects the wildcard alongside specific domains', async () => {
		const account = await createTestAccount(harness);
		const response = await createBuilder<{
			code: string;
			errors: Array<{
				path: string;
				code: string;
				message: string;
			}>;
		}>(harness, account.token)
			.patch('/users/@me/settings')
			.body({trusted_domains: ['*', 'example.com']})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(response.code).toBe('VALIDATION_ERROR');
		expect(response.errors[0]?.path).toBe('trusted_domains');
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.INVALID_TRUSTED_DOMAINS);
	});
	test('trusted_domains accepts the wildcard on its own', async () => {
		const account = await createTestAccount(harness);
		const {json} = await updateUserSettings(harness, account.token, {trusted_domains: ['*']});
		expect(json.trusted_domains).toEqual(['*']);
	});
	test('staff users can persist self-mention suppression settings', async () => {
		const account = await createTestAccount(harness);
		await setUserFlags(harness, account.userId, UserFlags.STAFF);
		const {json} = await updateUserSettings(harness, account.token, {
			suppress_unprivileged_self_mentions: true,
			suppress_unprivileged_self_mentions_bypass_user_ids: ['123', '456'],
			staff_dm_access_user_ids: ['789'],
		});
		expect(json.suppress_unprivileged_self_mentions).toBe(true);
		expect(json.suppress_unprivileged_self_mentions_bypass_user_ids).toEqual(['123', '456']);
		expect(json.staff_dm_access_user_ids).toEqual(['789']);
	});
	test('non-staff users cannot persist self-mention suppression settings', async () => {
		const account = await createTestAccount(harness);
		const {json} = await updateUserSettings(harness, account.token, {
			suppress_unprivileged_self_mentions: true,
			suppress_unprivileged_self_mentions_bypass_user_ids: ['123', '456'],
			staff_dm_access_user_ids: ['789'],
		});
		expect(json.suppress_unprivileged_self_mentions).toBe(false);
		expect(json.suppress_unprivileged_self_mentions_bypass_user_ids).toEqual([]);
		expect(json.staff_dm_access_user_ids).toEqual([]);
	});
});
