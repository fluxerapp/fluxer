// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {deleteAccount, expectDataExists} from '@app/api/user/tests/UserTestUtils';
import {beforeEach, describe, expect, test} from 'vitest';

describe('Account Delete Auto Cancel on Login', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('logging in after account deletion cancels the deletion', async () => {
		const account = await createTestAccount(harness);
		await deleteAccount(harness, account.token, account.password);
		const login = await loginAccount(harness, account);
		expect(login.token).not.toBe('');
		const data = await expectDataExists(harness, account.userId);
		expect(data.hasSelfDeletedFlag).toBe(false);
		expect(data.pendingDeletionAt).toBeNull();
	});
});
