// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {fetchUserSettings, updateUserSettings} from '@app/api/user/tests/UserTestUtils';
import {beforeEach, describe, expect, test} from 'vitest';

describe('User settings privacy setup', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('new accounts allow community DMs and have not reviewed the privacy setup', async () => {
		const account = await createTestAccount(harness);
		const {json} = await fetchUserSettings(harness, account.token);
		expect(json.default_guilds_restricted).toBe(false);
		expect(json.privacy_setup_version).toBe(0);
		expect(json.privacy_setup_completed_at).toBeNull();
	});
	test('completing the privacy setup stores the version, the time and the chosen setting in one update', async () => {
		const account = await createTestAccount(harness);
		const before = Date.now();
		const {json} = await updateUserSettings(harness, account.token, {
			default_guilds_restricted: true,
			privacy_setup_version: 1,
		});
		expect(json.default_guilds_restricted).toBe(true);
		expect(json.privacy_setup_version).toBe(1);
		expect(Date.parse(json.privacy_setup_completed_at ?? '')).toBeGreaterThanOrEqual(before - 1000);
		const {json: reread} = await fetchUserSettings(harness, account.token);
		expect(reread.privacy_setup_version).toBe(1);
		expect(reread.privacy_setup_completed_at).toBe(json.privacy_setup_completed_at);
		await createBuilder(harness, account.token)
			.patch('/users/@me/settings')
			.body({privacy_setup_version: 0})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});
