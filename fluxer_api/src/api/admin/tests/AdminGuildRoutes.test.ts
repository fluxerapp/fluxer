// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, test} from 'vitest';

describe('Admin guild routes', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	test('PATCH /admin/guilds/{guild_id} requires the ACL selected by every supplied field', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:update:settings']);
		const guild = await createGuild(harness, admin.token, `Partial ACL Guild ${Date.now()}`);
		await createBuilder(harness, `${admin.token}`)
			.patch(`/admin/guilds/${guild.id}`)
			.body({name: 'Not Allowed', verification_level: 1})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
	});
	test('GET /admin/guilds/{guild_id}/members requires guild:list:members', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
		const guild = await createGuild(harness, admin.token, `Member ACL Guild ${Date.now()}`);
		await createBuilder(harness, `${admin.token}`)
			.get(`/admin/guilds/${guild.id}/members`)
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_ACL')
			.execute();
	});
});
