// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {deleteAccount, expectDataExists, fetchUser} from '@app/api/user/tests/UserTestUtils';
import {DELETED_USER_USERNAME} from '@fluxer/constants/src/UserConstants';
import {beforeEach, describe, expect, test} from 'vitest';

describe('Account Deletion Grace Period', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('account deletion enters grace period and allows login', async () => {
		const account = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		await deleteAccount(harness, account.token, account.password);
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(1);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		const dataBeforeLogin = await expectDataExists(harness, account.userId);
		expect(dataBeforeLogin.hasSelfDeletedFlag).toBe(true);
		expect(dataBeforeLogin.pendingDeletionAt).not.toBeNull();
		const {json: userDuringGracePeriod} = await fetchUser(harness, account.userId, viewer.token);
		expect(userDuringGracePeriod.username).not.toBe(DELETED_USER_USERNAME);
		const loginAfterDelete = await loginAccount(harness, account);
		expect(loginAfterDelete.token).not.toBe('');
		expect(await harness.kvProvider.zcard('deletion_queue')).toBe(0);
		const dataAfterLogin = await expectDataExists(harness, account.userId);
		expect(dataAfterLogin.hasSelfDeletedFlag).toBe(false);
		expect(dataAfterLogin.pendingDeletionAt).toBeNull();
	});
	test('account deletion is blocked while user owns guilds', async () => {
		const account = await createTestAccount(harness);
		await createGuild(harness, account.token, 'Owned Guild');
		await createBuilder(harness, account.token)
			.post('/users/@me/delete')
			.body({password: account.password})
			.expect(HTTP_STATUS.BAD_REQUEST, 'USER_OWNS_GUILDS')
			.execute();
	});
});
