// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {disableAccount, expectDataExists} from '@app/api/user/tests/UserTestUtils';
import {beforeEach, describe, expect, test} from 'vitest';

describe('Account Disable', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('disabling account preserves data and allows re-login', async () => {
		const account = await createTestAccount(harness);
		await disableAccount(harness, account.token, account.password);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const login = await loginAccount(harness, account);
		expect(login.token).not.toBe('');
		const data = await expectDataExists(harness, account.userId);
		expect(data.emailCleared).toBe(false);
		expect(data.passwordCleared).toBe(false);
	});
});
