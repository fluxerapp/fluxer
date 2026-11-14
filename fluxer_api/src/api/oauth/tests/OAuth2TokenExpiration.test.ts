// SPDX-License-Identifier: AGPL-3.0-or-later

import {createApplicationID, createUserID} from '@app/api/BrandedTypes';
import {ACCESS_TOKEN_TTL_SECONDS} from '@app/api/oauth/OAuth2TokenConstants';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {createOAuth2TestSetup} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {randomBase64Url} from '@app/api/utils/SecretHash';
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
		const token = randomBase64Url(32);
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
});
