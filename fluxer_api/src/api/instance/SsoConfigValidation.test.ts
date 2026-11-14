// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isTestSsoProvider,
	normalizeAndValidateSsoConfig,
	normalizeSsoButtonLabel,
} from '@app/api/instance/SsoConfigValidation';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
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

describe('normalizeSsoButtonLabel', () => {
	it('trims the text and treats empty text as the default', () => {
		expect(normalizeSsoButtonLabel('  Sign in with Acme  ')).toBe('Sign in with Acme');
		expect(normalizeSsoButtonLabel('   ')).toBeNull();
		expect(normalizeSsoButtonLabel('')).toBeNull();
		expect(normalizeSsoButtonLabel(null)).toBeNull();
	});

	it('accepts 64 characters, including right-to-left text and emoji joiners', () => {
		expect(normalizeSsoButtonLabel('a'.repeat(64))).toBe('a'.repeat(64));
		expect(normalizeSsoButtonLabel('\u0648\u0631\u0648\u062F \u0628\u0627 SSO')).toBe(
			'\u0648\u0631\u0648\u062F \u0628\u0627 SSO',
		);
		expect(normalizeSsoButtonLabel('Acme \u{1F469}\u200D\u{1F4BB}')).toBe('Acme \u{1F469}\u200D\u{1F4BB}');
	});

	it('rejects control, line break and bidirectional formatting characters', () => {
		for (const label of [
			'Sign\tin',
			'Sign\nin',
			'Sign\u0000in',
			'Sign\u0085in',
			'Sign\u2028in',
			'Sign\u202Ein',
			'Sign\u2067in',
		]) {
			expect(() => normalizeSsoButtonLabel(label)).toThrow(InputValidationError);
		}
	});

	it('rejects text made only of invisible characters', () => {
		for (const label of ['\u200B', '\u3164', '\u2800', '\u00AD\u200F', '\u200B \uFEFF']) {
			expect(() => normalizeSsoButtonLabel(label)).toThrow(InputValidationError);
		}
	});
});

describe('normalizeAndValidateSsoConfig URL scheme', () => {
	it('names the https rule when an endpoint uses http', async () => {
		await expect(
			normalizeAndValidateSsoConfig(
				{...ssoConfig({authorizationUrl: null, tokenUrl: null}), issuer: 'http://sso.example.com'},
				{testModeEnabled: false},
			),
		).rejects.toMatchObject({localizedErrors: [{path: 'issuer', code: ValidationErrorCodes.URL_MUST_USE_HTTPS}]});
	});

	it('keeps the format code for text that is not a URL', async () => {
		await expect(
			normalizeAndValidateSsoConfig(
				{...ssoConfig({authorizationUrl: null, tokenUrl: null}), issuer: 'not a url'},
				{testModeEnabled: false},
			),
		).rejects.toMatchObject({localizedErrors: [{path: 'issuer', code: ValidationErrorCodes.INVALID_URL_FORMAT}]});
	});
});

describe('normalizeAndValidateSsoConfig issuer', () => {
	it('requires an issuer for a real provider', async () => {
		await expect(
			normalizeAndValidateSsoConfig(
				ssoConfig({authorizationUrl: 'https://1.1.1.1/auth', tokenUrl: 'https://1.1.1.1/token'}),
				{testModeEnabled: false},
			),
		).rejects.toMatchObject({localizedErrors: [{path: 'issuer', code: ValidationErrorCodes.SSO_ISSUER_REQUIRED}]});
	});
});

describe('normalizeAndValidateSsoConfig provider address', () => {
	it('points at the hostname when it does not resolve', async () => {
		await expect(
			normalizeAndValidateSsoConfig(
				{...ssoConfig({authorizationUrl: null, tokenUrl: null}), issuer: 'https://sso.example.invalid'},
				{testModeEnabled: false},
			),
		).rejects.toMatchObject({
			localizedErrors: [{path: 'issuer', code: ValidationErrorCodes.URL_HOSTNAME_NOT_RESOLVABLE}],
		});
	});

	it('points at the private address rule for a private address', async () => {
		await expect(
			normalizeAndValidateSsoConfig(
				{...ssoConfig({authorizationUrl: null, tokenUrl: null}), issuer: 'https://10.0.0.1'},
				{testModeEnabled: false},
			),
		).rejects.toMatchObject({
			localizedErrors: [{path: 'issuer', code: ValidationErrorCodes.URL_NOT_PUBLICLY_ROUTABLE}],
		});
	});
});
