// SPDX-License-Identifier: AGPL-3.0-or-later

import {isPageOnIPAddress} from '@app/features/auth/PasskeyAvailability';
import {
	FEATURE_NEEDS_HTTPS_DESCRIPTOR,
	PASSKEYS_NEED_DOMAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isInsecureBrowserContext} from '@app/features/platform/utils/SecureContext';
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
const PASSKEY_NO_AUTHENTICATOR_DESCRIPTOR = msg({
	message:
		"Fluxer couldn't find a security key on this computer. Connect your security key and try again, or open Fluxer in your web browser and use your passkey there.",
	comment:
		'Error shown in the desktop app when a passkey prompt starts but no security key is connected to the computer.',
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

const PASSKEY_FAILED_DESCRIPTOR = msg({
	message:
		"Your passkey or security key couldn't finish the request. Try again, and if it keeps failing, contact support with this error: {detail}",
	comment:
		'Error shown when a passkey or security key prompt fails for a reason Fluxer has no specific message for. {detail} is a technical error code such as UnknownError or com.apple.AuthenticationServices.AuthorizationError 1004. Keep {detail} unchanged.',
});

export const NATIVE_PASSKEY_ERROR_NAME = 'NativePasskeyError';

const UNMAPPED_WEBAUTHN_ERROR_NAMES = new Set(['ConstraintError', 'EncodingError', 'UnknownError']);
const NATIVE_ERROR_CODE = /\(((?:[A-Za-z][\w.]* -?\d+)|0x[0-9A-Fa-f]{8})\)/u;
const IPC_ERROR_PREFIX = /^Error invoking remote method '[^']+': (?:\w+: )?/u;
const MAX_NATIVE_ERROR_DETAIL_LENGTH = 160;

function nativeErrorDetail(error: Error): string {
	const code = NATIVE_ERROR_CODE.exec(error.message)?.[1];
	if (code !== undefined) {
		return code;
	}
	const message = error.message.replace(IPC_ERROR_PREFIX, '').trim();
	return message.length > 0 ? message.slice(0, MAX_NATIVE_ERROR_DETAIL_LENGTH) : error.name;
}

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
		return i18n._(isInsecureBrowserContext() ? FEATURE_NEEDS_HTTPS_DESCRIPTOR : PASSKEYS_UNAVAILABLE_DESCRIPTOR);
	}
	if (!(error instanceof Error)) {
		return null;
	}
	if (isPageOnIPAddress()) {
		return withErrorName(i18n._(PASSKEYS_NEED_DOMAIN_DESCRIPTOR), error.name);
	}
	const code = errorCode(error);
	switch (error.name) {
		case 'NotAllowedError':
		case 'AbortError':
			return withErrorName(describePasskeyPromptEnded(i18n), error.name);
		case 'InvalidStateError':
			return withErrorName(i18n._(PASSKEY_ALREADY_REGISTERED_DESCRIPTOR), error.name);
		case 'NoAuthenticatorError':
			return withErrorName(i18n._(PASSKEY_NO_AUTHENTICATOR_DESCRIPTOR), error.name);
		case 'SecurityError':
			return withErrorName(
				rpId !== null && (code === null || DOMAIN_ERROR_CODES.has(code))
					? i18n._(PASSKEY_DOMAIN_MISMATCH_DESCRIPTOR, {rpId})
					: i18n._(PASSKEY_SECURITY_ERROR_DESCRIPTOR),
				error.name,
			);
		case 'NotSupportedError':
			return withErrorName(i18n._(PASSKEY_NOT_SUPPORTED_DESCRIPTOR), error.name);
		case NATIVE_PASSKEY_ERROR_NAME:
			return i18n._(PASSKEY_FAILED_DESCRIPTOR, {detail: nativeErrorDetail(error)});
		default:
			return UNMAPPED_WEBAUTHN_ERROR_NAMES.has(error.name)
				? i18n._(PASSKEY_FAILED_DESCRIPTOR, {detail: error.name})
				: null;
	}
}
