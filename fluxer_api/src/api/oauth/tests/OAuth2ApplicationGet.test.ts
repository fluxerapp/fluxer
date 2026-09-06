// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import type {ApplicationPublicResponse} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';
import {beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, createTotpSecret, generateTotpCode} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {UserRepository} from '../../user/repositories/UserRepository';
import {createOAuth2Application, createUniqueApplicationName, getOAuth2Application} from './OAuth2TestUtils';

describe('OAuth2 Application Get', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('returns application response shape', async () => {
		const account = await createTestAccount(harness);
		const appName = createUniqueApplicationName();
		const redirectURIs = ['https://example.com/callback', 'https://example.com/callback2'];
		const createResult = await createOAuth2Application(harness, account.token, {
			name: appName,
			redirect_uris: redirectURIs,
		});
		const application = await getOAuth2Application(harness, account.token, createResult.application.id);
		expect(application.id).toBeTruthy();
		expect(application.name).toBe(appName);
		expect(application.redirect_uris).toEqual(redirectURIs);
		expect(application.bot).toBeDefined();
		expect(application.bot?.id).toBeTruthy();
		expect(application.bot?.username).toBeTruthy();
		expect(application.bot?.discriminator).toBeTruthy();
		expect(application.bot?.token).toBeUndefined();
		expect(application.client_secret).toBeUndefined();
	});
	test('returns 404 for non-existent application', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.get('/oauth2/applications/999999999999999999')
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('enforces access control - user cannot access another users application', async () => {
		const owner = await createTestAccount(harness);
		const otherUser = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, owner.token, {
			name: createUniqueApplicationName(),
		});
		await createBuilder(harness, otherUser.token)
			.get(`/oauth2/applications/${createResult.application.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('requires authentication', async () => {
		await createBuilderWithoutAuth(harness).get('/oauth2/applications/123').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	test('public bot representation omits the owner MFA mirror the owner view keeps', async () => {
		const account = await createTestAccount(harness);
		const createResult = await createOAuth2Application(harness, account.token, {
			name: createUniqueApplicationName(),
		});
		const secret = createTotpSecret();
		await createBuilder(harness, account.token)
			.post('/users/@me/mfa/totp/enable')
			.body({secret, code: generateTotpCode(secret), password: account.password})
			.expect(HTTP_STATUS.OK)
			.execute();
		const botUser = await new UserRepository().findUnique(createUserID(BigInt(createResult.botUserId)));
		expect(botUser?.authenticatorTypes.has(UserAuthenticatorTypes.TOTP)).toBe(true);
		const ownerView = await getOAuth2Application(harness, account.token, createResult.application.id);
		expect(ownerView.bot).toBeDefined();
		expect(ownerView.bot!.mfa_enabled).toBe(true);
		expect(ownerView.bot!.authenticator_types).toEqual([UserAuthenticatorTypes.TOTP]);
		const publicView = await createBuilderWithoutAuth<ApplicationPublicResponse>(harness)
			.get(`/oauth2/applications/${createResult.application.id}/public`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(publicView.bot).toBeTruthy();
		expect('mfa_enabled' in publicView.bot!).toBe(false);
		expect('authenticator_types' in publicView.bot!).toBe(false);
	});
});
