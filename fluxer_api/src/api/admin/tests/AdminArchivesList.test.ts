// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {AdminArchiveResponse} from '@fluxer/schema/src/domains/admin/AdminArchiveSchemas';
import {beforeEach, describe, expect, test} from 'vitest';

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
	test('filtering by requester only returns subject types the caller can view', async () => {
		const requester = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'archive:trigger:user',
			'archive:trigger:guild',
		]);
		const guild = await createGuild(harness, requester.token, 'Archive Guild');
		await createBuilder(harness, `${requester.token}`)
			.post(`/admin/users/${requester.userId}/archives`)
			.body({include_attachments: false})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, `${requester.token}`)
			.post(`/admin/guilds/${guild.id}/archives`)
			.body({include_attachments: false})
			.expect(HTTP_STATUS.OK)
			.execute();
		const viewer = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'archive:trigger:user',
		]);
		const result = await createBuilder<{archives: Array<AdminArchiveResponse>}>(harness, `${viewer.token}`)
			.get(`/admin/archives?requested_by=${requester.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.archives.map((a) => a.subject_type)).toEqual(['user']);
	});
});
