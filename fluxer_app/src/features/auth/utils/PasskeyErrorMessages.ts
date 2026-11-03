// SPDX-License-Identifier: AGPL-3.0-or-later

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const PASSKEYS_UNAVAILABLE_DESCRIPTOR = msg({
	message:
		'Passkeys are not available on this device. On a computer, connect a security key and try again, or use another sign-in method.',
	comment: 'Error shown when adding or using a passkey on a device or app build that cannot run passkeys.',
});
const PASSKEY_NOT_ALLOWED_DESCRIPTOR = msg({
	message:
		'The passkey prompt was cancelled, timed out or was blocked by your browser. Try again and finish the prompt on your device.',
	comment: 'Error shown when the browser passkey prompt ends without a passkey.',
});
const PASSKEY_ALREADY_REGISTERED_DESCRIPTOR = msg({
	message: 'This device or security key already has a passkey for your account.',
	comment: 'Error shown when adding a passkey with an authenticator that already holds one for this account.',
});
const PASSKEY_DOMAIN_MISMATCH_DESCRIPTOR = msg({
	message:
		'Your browser blocked the passkey because this page is not on {rpId}, the passkey domain of this instance. Ask the instance owner to check the passkey domain setting.',
	comment:
		'Error shown when the browser refuses a passkey because the page address does not match the domain the passkey belongs to. {rpId} is a domain name such as example.com.',
});
const PASSKEY_SECURITY_ERROR_DESCRIPTOR = msg({
	message:
		'Your browser blocked the passkey because this page address does not match the passkey domain of this instance. Ask the instance owner to check the passkey domain setting.',
	comment: 'Error shown when the browser refuses a passkey because the page address does not match its domain.',
});
const PASSKEY_NOT_SUPPORTED_DESCRIPTOR = msg({
	message: 'This browser or authenticator cannot create the kind of passkey this instance asks for.',
	comment: 'Error shown when the browser or security key does not support the requested passkey type.',
});

export class PasskeysUnsupportedError extends Error {
	constructor() {
		super('WebAuthn is not supported in this environment.');
		this.name = 'PasskeysUnsupportedError';
	}
}

const DOMAIN_ERROR_CODES = new Set(['ERROR_INVALID_DOMAIN', 'ERROR_INVALID_RP_ID']);

function errorCode(error: Error): string | null {
	const code = (error as {code?: unknown}).code;
	return typeof code === 'string' ? code : null;
}

function withErrorName(message: string, name: string): string {
	return `${message} (${name})`;
}

export function describePasskeyPromptEnded(i18n: I18n): string {
	return i18n._(PASSKEY_NOT_ALLOWED_DESCRIPTOR);
}

export function describePasskeyError(i18n: I18n, error: unknown, rpId: string | null): string | null {
	if (error instanceof PasskeysUnsupportedError) {
		return i18n._(PASSKEYS_UNAVAILABLE_DESCRIPTOR);
	}
	if (!(error instanceof Error)) {
		return null;
	}
	const code = errorCode(error);
	switch (error.name) {
		case 'NotAllowedError':
		case 'AbortError':
			return withErrorName(describePasskeyPromptEnded(i18n), error.name);
		case 'InvalidStateError':
			return withErrorName(i18n._(PASSKEY_ALREADY_REGISTERED_DESCRIPTOR), error.name);
		case 'SecurityError':
			return withErrorName(
				rpId !== null && (code === null || DOMAIN_ERROR_CODES.has(code))
					? i18n._(PASSKEY_DOMAIN_MISMATCH_DESCRIPTOR, {rpId})
					: i18n._(PASSKEY_SECURITY_ERROR_DESCRIPTOR),
				error.name,
			);
		case 'NotSupportedError':
			return withErrorName(i18n._(PASSKEY_NOT_SUPPORTED_DESCRIPTOR), error.name);
		default:
			return null;
	}
}
