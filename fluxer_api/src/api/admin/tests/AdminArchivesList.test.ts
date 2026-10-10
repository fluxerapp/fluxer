// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, test} from 'vitest';

async function setAdminArchiveAcls(harness: ApiTestHarness, admin: TestAccount): Promise<TestAccount> {
	return await setUserACLs(harness, admin, ['admin:authenticate', 'archive:trigger:guild']);
}

describe('Admin archives list', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('a guild trigger ACL does not read user archives', async () => {
		const admin = await createTestAccount(harness);
		const updatedAdmin = await setAdminArchiveAcls(harness, admin);
		await createBuilder(harness, `${updatedAdmin.token}`)
			.get('/admin/archives/user/1/2')
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
