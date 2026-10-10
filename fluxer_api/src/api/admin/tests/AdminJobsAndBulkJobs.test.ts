// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, test} from 'vitest';

describe('Admin jobs and bulk jobs', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('job cancellation requires jobs:cancel rather than jobs:view', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'jobs:view']);
		await createBuilder(harness, `${updated.token}`)
			.put('/admin/jobs/123456789012345678/cancellation')
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('holding one bulk ACL does not authorise another task', async () => {
		const admin = await createTestAccount(harness);
		const updated = await setUserACLs(harness, admin, ['admin:authenticate', 'bulk:update:user_flags']);
		await createBuilder(harness, `${updated.token}`)
			.post('/admin/bulk-jobs')
			.body({task: 'add_guild_members', guild_id: '1', user_ids: ['2']})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
