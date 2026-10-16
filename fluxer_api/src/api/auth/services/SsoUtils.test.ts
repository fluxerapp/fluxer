// SPDX-License-Identifier: AGPL-3.0-or-later

import {tryDiscoverOidcProviderMetadata} from '@app/api/auth/services/SsoUtils';
import {Config} from '@app/api/Config';
import {resetSsoRequestUrlPolicyForTesting} from '@app/api/instance/SsoConfigValidation';
import {server} from '@app/api/test/msw/server';
import {HttpResponse, http} from 'msw';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

const PRIVATE_ISSUER = 'http://10.20.30.40:5556/dex';

describe('tryDiscoverOidcProviderMetadata', () => {
	const previousAllowPrivateAddresses = Config.auth.ssoAllowPrivateAddresses;
	let discoveryRequests = 0;

	beforeEach(() => {
		discoveryRequests = 0;
		server.use(
			http.get(`${PRIVATE_ISSUER}/.well-known/openid-configuration`, () => {
				discoveryRequests++;
				return HttpResponse.json({
					issuer: PRIVATE_ISSUER,
					authorization_endpoint: `${PRIVATE_ISSUER}/auth`,
					token_endpoint: `${PRIVATE_ISSUER}/token`,
					userinfo_endpoint: `${PRIVATE_ISSUER}/userinfo`,
					jwks_uri: `${PRIVATE_ISSUER}/keys`,
				});
			}),
		);
	});

	afterEach(() => {
		Config.auth.ssoAllowPrivateAddresses = previousAllowPrivateAddresses;
		resetSsoRequestUrlPolicyForTesting();
	});

	it('discovers endpoints on a private issuer when private addresses are allowed', async () => {
		Config.auth.ssoAllowPrivateAddresses = true;
		resetSsoRequestUrlPolicyForTesting();
		await expect(tryDiscoverOidcProviderMetadata(PRIVATE_ISSUER)).resolves.toEqual({
			issuer: PRIVATE_ISSUER,
			authorization_endpoint: `${PRIVATE_ISSUER}/auth`,
			token_endpoint: `${PRIVATE_ISSUER}/token`,
			userinfo_endpoint: `${PRIVATE_ISSUER}/userinfo`,
			jwks_uri: `${PRIVATE_ISSUER}/keys`,
		});
		expect(discoveryRequests).toBe(1);
	});

	it('refuses a private issuer when private addresses are not allowed', async () => {
		Config.auth.ssoAllowPrivateAddresses = false;
		resetSsoRequestUrlPolicyForTesting();
		await expect(tryDiscoverOidcProviderMetadata(PRIVATE_ISSUER)).resolves.toBeNull();
		expect(discoveryRequests).toBe(0);
	});
});
