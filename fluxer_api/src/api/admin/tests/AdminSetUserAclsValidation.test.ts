// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface AdminUserMutationResponse {
	user: {
		id: string;
		acls: Array<string>;
	};
}

interface AdminUserLookupResponse {
	users: Array<{
		id: string;
		acls: Array<string>;
	}>;
}

describe('Admin set user ACLs validation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	test('stores a value of the registry', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.ACL_SET_USER,
			AdminACLs.USER_LOOKUP,
		]);
		const target = await createTestAccount(harness);
		const result = await createBuilder<AdminUserMutationResponse>(harness, `${admin.token}`)
			.put(`/admin/users/${target.userId}/acls`)
			.body({acls: [AdminACLs.USER_LOOKUP]})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(result.user.acls).toEqual([AdminACLs.USER_LOOKUP]);
	});
	test('rejects an ACL outside the registry with 400 and leaves the stored set unchanged', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.ACL_SET_USER,
			AdminACLs.USER_LOOKUP,
		]);
		const target = await createTestAccount(harness);
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/users/${target.userId}/acls`)
			.body({acls: [AdminACLs.USER_LOOKUP]})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, `${admin.token}`)
			.put(`/admin/users/${target.userId}/acls`)
			.body({acls: ['user:veiw']})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		const lookup = await createBuilder<AdminUserLookupResponse>(harness, `${admin.token}`)
			.get(`/admin/users/${target.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(lookup.users[0]!.acls).toEqual([AdminACLs.USER_LOOKUP]);
	});
});
