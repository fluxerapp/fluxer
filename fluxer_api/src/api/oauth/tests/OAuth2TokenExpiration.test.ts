// SPDX-License-Identifier: AGPL-3.0-or-later

import {createApplicationID, createUserID} from '@app/api/BrandedTypes';
import {ACCESS_TOKEN_TTL_SECONDS} from '@app/api/oauth/OAuth2TokenConstants';
import {generateOAuthTokenSecret} from '@app/api/oauth/OAuthTokenSecret';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {
	authorizeOAuth2,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Token Expiration', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should reject userinfo access with an expired access token', async () => {
		const {endUser, application} = await createOAuth2TestSetup(harness);
		const repository = new OAuth2TokenRepository();
		const token = generateOAuthTokenSecret();
		const expiredCreatedAt = new Date(Date.now() - (ACCESS_TOKEN_TTL_SECONDS + 60) * 1000);
		await repository.createAccessToken({
			token_: token,
			application_id: createApplicationID(BigInt(application.id)),
			user_id: createUserID(BigInt(endUser.userId)),
			scope: new Set(['identify']),
			created_at: expiredCreatedAt,
		});
		expect(await repository.getAccessToken(token)).toBeNull();
		await createBuilder(harness, `Bearer ${token}`).get('/oauth2/userinfo').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	test('should provide expires timestamp through oauth2/@me endpoint', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const tokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const json = await createBuilder<{
			application: {
				id: string;
			};
			scopes: Array<string>;
			expires: string;
		}>(harness, `Bearer ${tokens.access_token}`)
			.get('/oauth2/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.application.id).toBe(application.id);
		expect(json.scopes).toContain('identify');
		expect(json.expires).toBeTruthy();
		const expiresDate = new Date(json.expires);
		expect(expiresDate.getTime()).toBeGreaterThan(Date.now());
	});
});
