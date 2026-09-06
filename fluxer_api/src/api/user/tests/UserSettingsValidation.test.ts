// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {updateUserSettings} from './UserTestUtils';

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
	test('boolean fields must be booleans', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.patch('/users/@me/settings')
			.body({inline_attachment_media: 'true'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('status must be known string', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.patch('/users/@me/settings')
			.body({status: 42})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('null theme not allowed', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.patch('/users/@me/settings')
			.body({theme: null})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('mixed invalid shape', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.patch('/users/@me/settings')
			.body({status: 'offline', gif_auto_play: 'nope'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
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
	test('valid settings update', async () => {
		const account = await createTestAccount(harness);
		const {json} = await updateUserSettings(harness, account.token, {
			status: 'online',
			inline_attachment_media: true,
			gif_auto_play: false,
		});
		expect(json.status).toBe('online');
		expect(json.inline_attachment_media).toBe(true);
		expect(json.gif_auto_play).toBe(false);
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
