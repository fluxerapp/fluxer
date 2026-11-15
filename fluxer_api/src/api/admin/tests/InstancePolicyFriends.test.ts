// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {InstanceConfigResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('instance policy friends and direct messages', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let alice: TestAccount;
	let bob: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_VIEW,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
		alice = await createTestAccount(harness);
		bob = await createTestAccount(harness);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	const patchPolicy = (policy: Record<string, unknown>) =>
		createBuilder<InstanceConfigResponse>(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy})
			.execute();

	const friendRequest = (from: TestAccount, to: TestAccount) =>
		createBuilder(harness, from.token).post(`/users/@me/relationships/${to.userId}`).body({});

	const openDm = (from: TestAccount, to: TestAccount) =>
		createBuilder(harness, from.token).post('/users/@me/channels').body({recipient_id: to.userId});

	it('turns friend requests off on their own', async () => {
		const config = await patchPolicy({friends_disabled: true});
		expect(config.policy).toMatchObject({friends_disabled: true, direct_messages_disabled: false});
		await friendRequest(alice, bob).expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FRIENDS_DISABLED).execute();
	});

	it('keeps friend requests working while direct messages are off', async () => {
		const config = await patchPolicy({direct_messages_disabled: true, friends_disabled: false});
		expect(config.policy).toMatchObject({friends_disabled: false, direct_messages_disabled: true});
		await friendRequest(alice, bob).expect(HTTP_STATUS.OK).execute();
		await createBuilder(harness, bob.token)
			.put(`/users/@me/relationships/${alice.userId}`)
			.body({})
			.expect(HTTP_STATUS.OK)
			.execute();
		await openDm(alice, bob).expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.DIRECT_MESSAGES_DISABLED).execute();
	});

	it('turns friend requests off and on with direct messages when the friends switch is not sent', async () => {
		const off = await patchPolicy({direct_messages_disabled: true});
		expect(off.policy).toMatchObject({friends_disabled: true, direct_messages_disabled: true});
		await friendRequest(alice, bob).expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FRIENDS_DISABLED).execute();
		const on = await patchPolicy({direct_messages_disabled: false});
		expect(on.policy).toMatchObject({friends_disabled: false, direct_messages_disabled: false});
		await friendRequest(alice, bob).expect(HTTP_STATUS.OK).execute();
	});

	it('refuses accepting a pending request once friends are turned off', async () => {
		await friendRequest(alice, bob).expect(HTTP_STATUS.OK).execute();
		await patchPolicy({friends_disabled: true});
		await createBuilder(harness, bob.token)
			.put(`/users/@me/relationships/${alice.userId}`)
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FRIENDS_DISABLED)
			.execute();
	});

	it('refuses a notice link that is not http or https', async () => {
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({notice: {message: 'Hello', url: 'javascript:alert(1)'}})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
	});
});
