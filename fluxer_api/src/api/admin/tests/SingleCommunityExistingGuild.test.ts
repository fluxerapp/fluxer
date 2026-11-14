// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('single-community mode with an existing community', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	function patchPolicy(policy: Record<string, unknown>) {
		return createBuilder<InstanceConfigResponse>(harness, admin.token).patch('/admin/instance/config').body({policy});
	}

	it('refuses a community ID that does not exist', async () => {
		await patchPolicy({single_community_enabled: true, single_community_guild_id: '1234567890123456789'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
	});
});
