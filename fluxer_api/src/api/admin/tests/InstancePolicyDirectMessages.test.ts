// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {TestAccount} from '../../auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface InstanceConfigPolicyResponse {
	policy: {
		direct_messages_disabled: boolean;
		direct_messages_locked: boolean;
	};
}

describe('instance policy direct messages lock', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	const createAdmin = async (): Promise<TestAccount> =>
		await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);

	const patchPolicy = (admin: TestAccount, policy: Record<string, unknown>) =>
		createBuilder<InstanceConfigPolicyResponse>(harness, admin.token).patch('/admin/instance/config').body({policy});

	const lockDirectMessages = async (admin: TestAccount): Promise<void> => {
		await patchPolicy(admin, {direct_messages_disabled: true}).execute();
		const reenabled = await patchPolicy(admin, {direct_messages_disabled: false}).execute();
		expect(reenabled.policy.direct_messages_locked).toBe(true);
	};

	it('disable then re-enable sets the lock', async () => {
		const admin = await createAdmin();

		const disabled = await patchPolicy(admin, {direct_messages_disabled: true}).execute();
		expect(disabled.policy.direct_messages_disabled).toBe(true);
		expect(disabled.policy.direct_messages_locked).toBe(false);

		const reenabled = await patchPolicy(admin, {direct_messages_disabled: false}).execute();
		expect(reenabled.policy.direct_messages_disabled).toBe(false);
		expect(reenabled.policy.direct_messages_locked).toBe(true);
	});

	it('a change after the lock fails with INSTANCE_POLICY_TRANSITION_NOT_ALLOWED', async () => {
		const admin = await createAdmin();
		await lockDirectMessages(admin);

		await patchPolicy(admin, {direct_messages_disabled: true})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INSTANCE_POLICY_TRANSITION_NOT_ALLOWED)
			.execute();

		const current = await createBuilder<InstanceConfigPolicyResponse>(harness, admin.token)
			.get('/admin/instance/config')
			.execute();
		expect(current.policy.direct_messages_disabled).toBe(false);
		expect(current.policy.direct_messages_locked).toBe(true);
	});

	it('direct_messages_locked false clears the lock and admits the change', async () => {
		const admin = await createAdmin();
		await lockDirectMessages(admin);

		const unlocked = await patchPolicy(admin, {
			direct_messages_locked: false,
			direct_messages_disabled: true,
		}).execute();
		expect(unlocked.policy.direct_messages_disabled).toBe(true);
		expect(unlocked.policy.direct_messages_locked).toBe(false);
	});

	it('rejects a request that tries to set the lock', async () => {
		const admin = await createAdmin();

		await patchPolicy(admin, {direct_messages_locked: true})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
	});
});
