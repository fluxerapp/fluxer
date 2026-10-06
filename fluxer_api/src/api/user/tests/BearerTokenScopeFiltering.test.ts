// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {beforeEach, describe, expect, test} from 'vitest';

interface OAuth2TokenResponse {
	token: string;
	user_id: string;
	scopes: Array<string>;
	application_id: string;
}

async function createOAuth2Token(
	harness: ApiTestHarness,
	userId: string,
	scopes: Array<string>,
): Promise<OAuth2TokenResponse> {
	return createBuilder<OAuth2TokenResponse>(harness, '')
		.post('/test/oauth2/access-token')
		.body({user_id: userId, scopes})
		.execute();
}

describe('Bearer token scope filtering on /users/@me', () => {
	let harness: ApiTestHarness;
	let account: TestAccount;
	beforeEach(async () => {
		harness = await createApiTestHarness();
		account = await createTestAccount(harness);
	});
	test('session token returns full private user response', async () => {
		const json = await createBuilder<UserPrivateResponse>(harness, account.token)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.id).toBe(account.userId);
		expect(json.email).toBe(account.email);
		expect(json.password_last_changed_at).not.toBeNull();
		expect(json.mfa_enabled).toBeDefined();
		expect(Array.isArray(json.acls)).toBe(true);
		expect(Array.isArray(json.traits)).toBe(true);
	});
	test('bearer token with identify scope strips sensitive fields', async () => {
		const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
		const json = await createBuilder<UserPrivateResponse>(harness, `Bearer ${oauth2Token.token}`)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.id).toBe(account.userId);
		expect(json.username).toBe(account.username);
		expect(json.email).toBeNull();
		expect(json.acls).toEqual([]);
		expect(json.traits).toEqual([]);
		expect(json.mfa_enabled).toBe(false);
		expect(json.authenticator_types).toBeUndefined();
		expect(json.password_last_changed_at).toBeNull();
		expect(json.email_bounced).toBeUndefined();
		expect(json.nsfw_allowed).toBe(false);
		expect(json.premium_since).toBeNull();
		expect(json.premium_until).toBeNull();
		expect(json.premium_will_cancel).toBe(false);
		expect(json.premium_billing_cycle).toBeNull();
		expect(json.premium_lifetime_sequence).toBeNull();
		expect(json.premium_badge_hidden).toBe(false);
		expect(json.premium_badge_masked).toBe(false);
		expect(json.premium_badge_timestamp_hidden).toBe(false);
		expect(json.premium_badge_sequence_hidden).toBe(false);
		expect(json.premium_purchase_disabled).toBe(false);
		expect(json.premium_enabled_override).toBe(false);
		expect(json.has_dismissed_premium_onboarding).toBe(false);
		expect(json.has_ever_purchased).toBe(false);
		expect(json.has_unread_gift_inventory).toBe(false);
		expect(json.unread_gift_inventory_count).toBe(0);
		expect(json.pending_bulk_message_deletion).toBeNull();
	});
	test('bearer token with identify scope preserves public identity fields', async () => {
		const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify']);
		const json = await createBuilder<UserPrivateResponse>(harness, `Bearer ${oauth2Token.token}`)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.id).toBe(account.userId);
		expect(json.username).toBe(account.username);
		expect(json.discriminator).toBeDefined();
		expect(typeof json.flags).toBe('number');
		expect(typeof json.verified).toBe('boolean');
		expect(typeof json.is_staff).toBe('boolean');
	});
	test('bearer token with identify and email scope returns email but strips other sensitive fields', async () => {
		const oauth2Token = await createOAuth2Token(harness, account.userId, ['identify', 'email']);
		const json = await createBuilder<UserPrivateResponse>(harness, `Bearer ${oauth2Token.token}`)
			.get('/users/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.id).toBe(account.userId);
		expect(json.email).toBe(account.email);
		expect(json.acls).toEqual([]);
		expect(json.traits).toEqual([]);
		expect(json.mfa_enabled).toBe(false);
		expect(json.password_last_changed_at).toBeNull();
	});
	test('bearer token without identify scope is rejected', async () => {
		const oauth2Token = await createOAuth2Token(harness, account.userId, ['email']);
		await createBuilder(harness, `Bearer ${oauth2Token.token}`)
			.get('/users/@me')
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_OAUTH_SCOPE')
			.execute();
	});
});
