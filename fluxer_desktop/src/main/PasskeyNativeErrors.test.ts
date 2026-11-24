// SPDX-License-Identifier: AGPL-3.0-or-later

import {webAuthnErrorName} from '@electron/main/PasskeyNativeErrors';
import {describe, expect, it} from 'vitest';

function macOSRegistrationError(domain: string, code: number): string {
	return `WebAuthn registration failed: The operation couldn’t be completed. (${domain} error ${code}.) (${domain} ${code})`;
}

const AUTHORIZATION_ERROR_DOMAIN = 'com.apple.AuthenticationServices.AuthorizationError';

describe('webAuthnErrorName', () => {
	it.each([
		[1001, 'NotAllowedError'],
		[1005, 'NotAllowedError'],
		[1006, 'InvalidStateError'],
		[1010, 'NotSupportedError'],
	])('maps ASAuthorizationError %i to %s', (code, name) => {
		expect(webAuthnErrorName(macOSRegistrationError(AUTHORIZATION_ERROR_DOMAIN, code))).toBe(name);
	});

	it.each([1000, 1002, 1003, 1004])('leaves ASAuthorizationError %i without a WebAuthn name', (code) => {
		expect(webAuthnErrorName(macOSRegistrationError(AUTHORIZATION_ERROR_DOMAIN, code))).toBeNull();
	});

	it('maps a duplicate credential reported by older macOS versions to InvalidStateError', () => {
		expect(webAuthnErrorName(macOSRegistrationError('WKErrorDomain', 8))).toBe('InvalidStateError');
	});
});
