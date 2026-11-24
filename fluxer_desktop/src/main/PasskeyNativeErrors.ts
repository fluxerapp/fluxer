// SPDX-License-Identifier: AGPL-3.0-or-later

const NOT_ALLOWED_NATIVE_ERRORS = [
	/\(com\.apple\.AuthenticationServices\.AuthorizationError 100[15]\)/u,
	/\bWebAuthn (?:authentication|registration) timed out\b/u,
	/\bNotAllowed \(/u,
	/\bTimeout \(/u,
	/\(0x(?:80090036|800704C7|800705B4|80090035|80090011)\)/iu,
];
const NO_AUTHENTICATOR_NATIVE_ERROR = /\bNoAuthenticator \(/u;
const INVALID_STATE_NATIVE_ERRORS = [
	/\(com\.apple\.AuthenticationServices\.AuthorizationError 1006\)/u,
	/\(WKErrorDomain 8\)/u,
	/\(0x8009000F\)/iu,
];
const NOT_SUPPORTED_NATIVE_ERROR = /\(com\.apple\.AuthenticationServices\.AuthorizationError 1010\)/u;

export const webAuthnErrorName = (message: string): string | null => {
	if (NOT_ALLOWED_NATIVE_ERRORS.some((pattern) => pattern.test(message))) {
		return 'NotAllowedError';
	}
	if (INVALID_STATE_NATIVE_ERRORS.some((pattern) => pattern.test(message))) {
		return 'InvalidStateError';
	}
	if (NO_AUTHENTICATOR_NATIVE_ERROR.test(message)) {
		return 'NoAuthenticatorError';
	}
	if (NOT_SUPPORTED_NATIVE_ERROR.test(message)) {
		return 'NotSupportedError';
	}
	return null;
};
