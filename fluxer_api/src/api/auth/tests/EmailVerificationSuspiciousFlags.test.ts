// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearTestEmails,
	createAuthHarness,
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SuspiciousActivityErrorResponse {
	error: string;
	data: {
		suspicious_activity_flags: number;
	};
}

describe('Email verification suspicious flags', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('clears only email-related suspicious flags after verification', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL', 'REQUIRE_VERIFIED_PHONE'],
			})
			.expect(200)
			.execute();
		const checkSuspiciousFlags = async (expected: number): Promise<void> => {
			const errBody = await createBuilder<SuspiciousActivityErrorResponse>(harness, account.token)
				.patch('/users/@me')
				.body({bio: 'blocked-while-suspicious'})
				.expect(403)
				.execute();
			expect(errBody.data.suspicious_activity_flags).toBe(expected);
		};
		await checkSuspiciousFlags(
			SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL | SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE,
		);
		await createBuilder(harness, account.token).post('/auth/verify/resend').body({}).expect(204).execute();
		const emails = await listTestEmails(harness, {recipient: account.email});
		const verificationEmail = findLastTestEmail(emails, 'email_verification');
		expect(verificationEmail?.metadata?.token).toBeDefined();
		const token = verificationEmail!.metadata!.token!;
		await createBuilderWithoutAuth(harness).post('/auth/verify').body({token}).expect(204).execute();
		await checkSuspiciousFlags(SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE);
	});
});
