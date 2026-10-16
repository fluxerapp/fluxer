// SPDX-License-Identifier: AGPL-3.0-or-later

import {isTestSsoProvider, normalizeAndValidateSsoConfig} from '@app/api/instance/SsoConfigValidation';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {describe, expect, it} from 'vitest';

function ssoConfig(overrides: {authorizationUrl?: string | null; tokenUrl?: string | null} = {}) {
	return {
		enabled: true,
		enforced: false,
		issuer: null,
		authorizationUrl: 'test',
		tokenUrl: 'test',
		userInfoUrl: null,
		jwksUrl: null,
		clientId: 'client',
		allowedEmailDomains: [],
		...overrides,
	};
}

describe('isTestSsoProvider', () => {
	it('recognises the placeholder provider only when test mode is enabled', () => {
		expect(isTestSsoProvider({authorizationUrl: 'test', tokenUrl: null}, true)).toBe(true);
		expect(isTestSsoProvider({authorizationUrl: null, tokenUrl: 'test'}, true)).toBe(true);
		expect(isTestSsoProvider({authorizationUrl: 'test-provider', tokenUrl: null}, true)).toBe(true);
	});

	it('never recognises the placeholder provider outside test mode', () => {
		expect(isTestSsoProvider({authorizationUrl: 'test', tokenUrl: null}, false)).toBe(false);
		expect(isTestSsoProvider({authorizationUrl: null, tokenUrl: 'test'}, false)).toBe(false);
		expect(isTestSsoProvider({authorizationUrl: 'test', tokenUrl: 'test'}, false)).toBe(false);
		expect(isTestSsoProvider({authorizationUrl: 'test-provider', tokenUrl: null}, false)).toBe(false);
	});
});

describe('normalizeAndValidateSsoConfig placeholder endpoints', () => {
	it('accepts placeholder endpoints in test mode', async () => {
		const result = await normalizeAndValidateSsoConfig(ssoConfig(), {testModeEnabled: true});
		expect(result.ready).toBe(true);
		expect(result.authorizationUrl).toBe('test');
	});

	it('rejects a placeholder authorization endpoint outside test mode', async () => {
		await expect(
			normalizeAndValidateSsoConfig(ssoConfig({tokenUrl: null}), {testModeEnabled: false}),
		).rejects.toBeInstanceOf(InputValidationError);
	});

	it('rejects a placeholder token endpoint outside test mode', async () => {
		await expect(
			normalizeAndValidateSsoConfig(ssoConfig({authorizationUrl: null}), {testModeEnabled: false}),
		).rejects.toBeInstanceOf(InputValidationError);
	});
});
