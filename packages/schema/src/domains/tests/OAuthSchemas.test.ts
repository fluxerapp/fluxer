// SPDX-License-Identifier: AGPL-3.0-or-later

import {MAX_APPLICATION_REDIRECT_URIS} from '@fluxer/constants/src/LimitConstants';
import {ApplicationCreateRequest, ApplicationResponse} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';
import {describe, expect, it} from 'vitest';

const buildRedirectURIs = (count: number) =>
	Array.from({length: count}, (_, index) => `https://example.com/callback/${index}`);

const buildApplicationResponse = (redirectURIs: Array<string>) => ({
	id: '1234567890123456789',
	name: 'Test Application',
	redirect_uris: redirectURIs,
	bot_public: true,
	bot_require_code_grant: false,
});

describe('application redirect_uris bounds', () => {
	it('accepts the maximum redirect URI count on both the request and the response', () => {
		const uris = buildRedirectURIs(MAX_APPLICATION_REDIRECT_URIS);
		expect(ApplicationCreateRequest.safeParse({name: 'Test Application', redirect_uris: uris}).success).toBe(true);
		expect(ApplicationResponse.safeParse(buildApplicationResponse(uris)).success).toBe(true);
	});
	it('rejects one redirect URI beyond the maximum on both the request and the response', () => {
		const uris = buildRedirectURIs(MAX_APPLICATION_REDIRECT_URIS + 1);
		expect(ApplicationCreateRequest.safeParse({name: 'Test Application', redirect_uris: uris}).success).toBe(false);
		expect(ApplicationResponse.safeParse(buildApplicationResponse(uris)).success).toBe(false);
	});
});
