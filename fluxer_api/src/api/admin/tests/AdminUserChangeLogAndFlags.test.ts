// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

interface VerifyEmailMutationResponse {
	user: {
		id: string;
		email_verified: boolean;
		email_bounced: boolean;
	};
}

describe('Admin User Change Log and Flags', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		vi.restoreAllMocks();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	describe('GET /admin/users/{user_id}/change-log', () => {
		test('requires USER_VIEW_CONTACT_LOG ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${target.userId}/change-log?limit=50`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('rejects an admin holding USER_LOOKUP but not USER_VIEW_CONTACT_LOG', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${target.userId}/change-log?limit=50`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('DELETE /admin/users/{user_id}/mfa', () => {
		test('requires USER_UPDATE_MFA ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.delete(`/admin/users/${target.userId}/mfa`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('PATCH /admin/users/{user_id}/email', () => {
		test('queues a Stripe customer email sync for users with a Stripe customer', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness)
				.post(`/test/users/${target.userId}/premium`)
				.body({stripe_customer_id: 'cus_admin_email_sync'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/users/${target.userId}/email`)
				.body({email: `admin-changed-${Date.now()}@example.com`})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(addJob).toHaveBeenCalledWith('syncStripeCustomerEmail', {userId: target.userId});
		});
		test('does not queue a Stripe customer email sync for users without a Stripe customer', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			const addJob = vi.spyOn(NoopWorkerService.prototype, 'addJob');
			await createBuilder(harness, `${admin.token}`)
				.patch(`/admin/users/${target.userId}/email`)
				.body({email: `admin-changed-${Date.now()}@example.com`})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(addJob).not.toHaveBeenCalledWith('syncStripeCustomerEmail', expect.anything());
		});
	});
	describe('PUT /admin/users/{user_id}/email-verification', () => {
		test('verifying email clears email_bounced', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness)
				.post(`/test/users/${target.userId}/security-flags`)
				.body({
					email_bounced: true,
					email_verified: false,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<VerifyEmailMutationResponse>(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/email-verification`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.user.email_verified).toBe(true);
			expect(result.user.email_bounced).toBe(false);
		});
	});
	describe('POST /admin/users/{user_id}/verification-email', () => {
		test('requires USER_UPDATE_EMAIL ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.post(`/admin/users/${target.userId}/verification-email`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
});
