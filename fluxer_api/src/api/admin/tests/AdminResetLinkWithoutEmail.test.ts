// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, listTestEmails, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const NEW_PASSWORD = 'reset-by-the-admin-2026';

describe('admin password reset links on an email instance', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	function createLink(target: TestAccount) {
		return createBuilder<{url: string}>(harness, admin.token).post(`/admin/users/${target.userId}/password-reset-link`);
	}

	it('issues a link that resets the password while email delivery is off', async () => {
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {delivery: 'off'}});
		const target = await createTestAccount(harness);
		const link = await createLink(target).expect(HTTP_STATUS.OK).execute();
		const token = link.url.split('/reset#token=')[1]!;
		expect(
			(await createBuilderWithoutAuth<{valid: boolean}>(harness).get(`/auth/reset/${token}`).execute()).valid,
		).toBe(true);
		const reset = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/reset')
			.body({token, password: NEW_PASSWORD})
			.execute();
		expect(reset.user_id).toBe(target.userId);
		const login = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/login')
			.body({email: target.email, password: NEW_PASSWORD})
			.execute();
		expect(login.user_id).toBe(target.userId);
		expect(await listTestEmails(harness, {recipient: target.email})).toEqual([]);
	});

	it('refuses the link while the instance sends email', async () => {
		const target = await createTestAccount(harness);
		await createLink(target).expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.USERNAME_SIGN_IN_ONLY).execute();
	});

	it('refuses to email a reset while email delivery is off', async () => {
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {delivery: 'off'}});
		const target = await createTestAccount(harness);
		await createBuilder(harness, admin.token)
			.post(`/admin/users/${target.userId}/password-reset`)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.EMAIL_DELIVERY_OFF)
			.execute();
		expect(await listTestEmails(harness, {recipient: target.email})).toEqual([]);
	});

	it('refuses a link token once the account email changed', async () => {
		await getInstanceConfigRepository().setInstanceIntegrationsConfig({email: {delivery: 'off'}});
		const target = await createTestAccount(harness);
		const token = (await createLink(target).execute()).url.split('/reset#token=')[1]!;
		await createBuilder(harness, admin.token)
			.patch(`/admin/users/${target.userId}/email`)
			.body({email: `changed-${target.userId}@example.com`})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(
			(await createBuilderWithoutAuth<{valid: boolean}>(harness).get(`/auth/reset/${token}`).execute()).valid,
		).toBe(false);
	});
});
