// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, TEST_CREDENTIALS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

interface ChangeLogResponse {
	entries: Array<{
		event_id: string;
		field: string;
		old_value: string | null;
		new_value: string | null;
		reason: string | null;
		actor_user_id: string | null;
		event_at: string;
	}>;
	next_page_token: string | null;
}

interface UserMutationResponse {
	user: {
		id: string;
		suspicious_activity_flags: number;
	};
}

interface VerifyEmailMutationResponse {
	user: {
		id: string;
		email_verified: boolean;
		email_bounced: boolean;
		suspicious_activity_flags: number;
	};
}

describe('Admin User Change Log and Suspicious Flags', () => {
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
	describe('GET /admin/users/{user_id}/change-log', () => {
		test('returns empty entries for user with no changes', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			const result = await createBuilder<ChangeLogResponse>(harness, `${admin.token}`)
				.get(`/admin/users/${target.userId}/change-log?limit=50`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.entries).toBeInstanceOf(Array);
			expect(result.next_page_token).toBeNull();
		});
		test('returns entries after a username change without crashing', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness, {username: 'beforechange'});
			await createBuilder(harness, `${target.token}`)
				.patch('/users/@me')
				.body({username: 'afterchange', password: TEST_CREDENTIALS.STRONG_PASSWORD})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<ChangeLogResponse>(harness, `${admin.token}`)
				.get(`/admin/users/${target.userId}/change-log?limit=50`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.entries).toBeInstanceOf(Array);
			expect(result.next_page_token).toBeNull();
		});
		test('accepts request without explicit limit (uses default)', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder<ChangeLogResponse>(harness, `${admin.token}`)
				.get(`/admin/users/${target.userId}/change-log`)
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		test('rejects invalid user_id format', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			await createBuilder(harness, `${admin.token}`)
				.get('/admin/users/not-a-snowflake/change-log?limit=50')
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('rejects limit below minimum', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${target.userId}/change-log?limit=0`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('rejects limit above maximum', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.get(`/admin/users/${target.userId}/change-log?limit=201`)
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
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
	describe('PUT /admin/users/{user_id}/suspicious-activity-flags', () => {
		test('sets suspicious activity flags', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			const flags = SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL | SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE;
			const result = await createBuilder<UserMutationResponse>(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({flags})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.user.suspicious_activity_flags).toBe(flags);
		});
		test('clears all suspicious activity flags by setting to zero', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder<UserMutationResponse>(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({flags: SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<UserMutationResponse>(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({flags: 0})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.user.suspicious_activity_flags).toBe(0);
		});
		test('rejects invalid user_id format', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			await createBuilder(harness, `${admin.token}`)
				.put('/admin/users/not-a-snowflake/suspicious-activity-flags')
				.body({flags: 1})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('rejects missing flags', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('rejects negative flags', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({flags: -1})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('rejects non-existent user', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			await createBuilder(harness, `${admin.token}`)
				.put('/admin/users/999999999999999999/suspicious-activity-flags')
				.body({flags: 1})
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
		test('requires USER_UPDATE_SUSPICIOUS_ACTIVITY ACL', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({flags: 1})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('updates flags multiple times in sequence', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			const result1 = await createBuilder<UserMutationResponse>(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({flags: SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result1.user.suspicious_activity_flags).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL);
			const combined =
				SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL | SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL;
			const result2 = await createBuilder<UserMutationResponse>(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({flags: combined})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result2.user.suspicious_activity_flags).toBe(combined);
		});
	});
	describe('DELETE /admin/users/{user_id}/mfa', () => {
		test('succeeds for user without MFA', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.delete(`/admin/users/${target.userId}/mfa`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		test('rejects invalid user_id format', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			await createBuilder(harness, `${admin.token}`)
				.delete('/admin/users/not-a-snowflake/mfa')
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
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
	describe('PUT /admin/users/{user_id}/email-verification', () => {
		test('verifying email clears email_bounced and only email-related suspicious flags', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilderWithoutAuth(harness)
				.post(`/test/users/${target.userId}/security-flags`)
				.body({
					email_bounced: true,
					email_verified: false,
					suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL', 'REQUIRE_VERIFIED_PHONE'],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const result = await createBuilder<VerifyEmailMutationResponse>(harness, `${admin.token}`)
				.put(`/admin/users/${target.userId}/email-verification`)
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(result.user.email_verified).toBe(true);
			expect(result.user.email_bounced).toBe(false);
			expect(result.user.suspicious_activity_flags).toBe(SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE);
		});
	});
	describe('POST /admin/users/{user_id}/verification-email', () => {
		test('sends verification email for unverified user', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			const target = await createTestAccount(harness);
			await createBuilder(harness, `${admin.token}`)
				.post(`/admin/users/${target.userId}/verification-email`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		test('rejects invalid user_id format', async () => {
			const admin = await createTestAccount(harness);
			await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.WILDCARD]);
			await createBuilder(harness, `${admin.token}`)
				.post('/admin/users/not-a-snowflake/verification-email')
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
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
