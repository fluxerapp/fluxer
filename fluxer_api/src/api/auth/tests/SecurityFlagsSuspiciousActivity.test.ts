// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginUser} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface UserPrivateResponse {
	required_actions: Array<string> | null;
}

interface UserSettingsResponse {
	incoming_call_flags: number;
}

describe('Auth security flags - suspicious activity flag blocks restricted routes but allows recovery access', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('blocks ordinary authenticated writes when suspicious activity flag is set', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL'],
			})
			.execute();
		await createBuilder(harness, account.token).patch('/users/@me').body({bio: 'still-blocked'}).expect(403).execute();
	});
	it('allows login and self-bootstrap reads when suspicious activity flag is set', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL'],
			})
			.execute();
		const login = await loginUser(harness, {email: account.email, password: account.password});
		if ('mfa' in login) {
			throw new Error('Expected non-MFA login for suspicious activity recovery test');
		}
		const me = await createBuilder<UserPrivateResponse>(harness, login.token).get('/users/@me').expect(200).execute();
		expect(me.required_actions).toContain('REQUIRE_REVERIFIED_EMAIL');
		await createBuilder<UserSettingsResponse>(harness, login.token).get('/users/@me/settings').expect(200).execute();
	});
	it('suppresses REQUIRE_VERIFIED_EMAIL when the account email is verified', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flag_names: ['REQUIRE_VERIFIED_EMAIL'],
			})
			.execute();
		const me = await createBuilder<UserPrivateResponse>(harness, account.token).get('/users/@me').expect(200).execute();
		expect(me.required_actions).not.toContain('REQUIRE_VERIFIED_EMAIL');
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({bio: 'email-flag-suppressed'})
			.expect(200)
			.execute();
	});
	it('keeps REQUIRE_VERIFIED_EMAIL active when the account email is unverified', async () => {
		const account = await createTestAccount(harness, {skipEmailVerification: true});
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				email_verified: false,
				suspicious_activity_flag_names: ['REQUIRE_VERIFIED_EMAIL'],
			})
			.execute();
		const me = await createBuilder<UserPrivateResponse>(harness, account.token).get('/users/@me').expect(200).execute();
		expect(me.required_actions).toContain('REQUIRE_VERIFIED_EMAIL');
		await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({bio: 'email-flag-still-active'})
			.expect(403)
			.execute();
	});
	it('allows /auth/verify/resend even with suspicious activity flag', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL'],
			})
			.execute();
		await createBuilder(harness, account.token).post('/auth/verify/resend').body({}).expect(204).execute();
	});
	it('allows fully restricted routes again after clearing suspicious activity flag', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flag_names: ['REQUIRE_REVERIFIED_EMAIL'],
			})
			.execute();
		await createBuilder(harness, account.token).get(`/users/${account.userId}`).expect(403).execute();
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({
				suspicious_activity_flags: 0,
			})
			.execute();
		await createBuilder(harness, account.token).get(`/users/${account.userId}`).expect(200).execute();
	});
});
