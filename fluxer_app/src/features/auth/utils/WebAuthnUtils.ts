// SPDX-License-Identifier: AGPL-3.0-or-later

import {promptForSecurityKeyPin} from '@app/features/auth/components/modals/PasskeyPinModal';
import {parsePasskeyPinFailure} from '@app/features/auth/utils/PasskeyPinErrors';
import {Platform} from '@app/features/platform/types/Platform';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {
	type AuthenticationResponseJSON,
	browserSupportsWebAuthn,
	type PublicKeyCredentialCreationOptionsJSON,
	type PublicKeyCredentialRequestOptionsJSON,
	type RegistrationResponseJSON,
	startAuthentication,
	startRegistration,
} from '@simplewebauthn/browser';

export const PASSKEY_DOMAIN_UNSUPPORTED_DESCRIPTOR = msg({
	message:
		'Your browser does not support passkeys on this domain. Update your browser, or sign in with your password and two-factor code.',
	comment:
		'Error shown when a passkey prompt fails because the browser cannot use the passkey on this web address. Keep plain.',
});
const RP_MISMATCH_MESSAGE_PATTERN = /relying party|\brp ?id\b|\bdomain\b|\borigin\b/i;

export class PasskeyDomainUnsupportedError extends Error {
	constructor() {
		super('Passkeys are not supported on this domain in this browser');
		this.name = 'PasskeyDomainUnsupportedError';
	}
}

function isRelatedOriginFailure(error: unknown, rpId: string | undefined): boolean {
	if (!rpId || !(error instanceof Error)) {
		return false;
	}
	const hostname = window.location.hostname.toLowerCase();
	const normalizedRpId = rpId.toLowerCase();
	if (hostname === normalizedRpId || hostname.endsWith(`.${normalizedRpId}`)) {
		return false;
	}
	if (error.name === 'SecurityError') {
		return true;
	}
	return error.name === 'NotAllowedError' && RP_MISMATCH_MESSAGE_PATTERN.test(error.message);
}

async function runBrowserCeremony<T>(rpId: string | undefined, run: () => Promise<T>): Promise<T> {
	try {
		return await run();
	} catch (error) {
		if (isRelatedOriginFailure(error, rpId)) {
			throw new PasskeyDomainUnsupportedError();
		}
		throw error;
	}
}

async function runNativeCeremonyWithPinSupport<T>(run: (requestContext?: {pin?: string}) => Promise<T>): Promise<T> {
	try {
		return await run();
	} catch (error) {
		if (parsePasskeyPinFailure(error)?.kind !== 'required') {
			throw error;
		}
	}
	return promptForSecurityKeyPin((pin) => run({pin}));
}

export async function assertWebAuthnSupported(): Promise<void> {
	if (Platform.isElectron) {
		const electronApi = getElectronAPI();
		const nativeSupported = electronApi && (await electronApi.passkeyIsSupported?.());
		if (nativeSupported) {
			return;
		}
		if (browserSupportsWebAuthn()) {
			return;
		}
		throw new Error('WebAuthn is not supported in this environment.');
	}
	if (!browserSupportsWebAuthn()) {
		throw new Error('WebAuthn is not supported in this environment.');
	}
}

export async function performRegistration(
	options: PublicKeyCredentialCreationOptionsJSON,
): Promise<RegistrationResponseJSON> {
	await assertWebAuthnSupported();
	if (Platform.isElectron) {
		const electronApi = getElectronAPI();
		const nativeSupported = electronApi && (await electronApi.passkeyIsSupported?.());
		const passkeyRegister = electronApi?.passkeyRegister;
		if (nativeSupported && passkeyRegister) {
			return runNativeCeremonyWithPinSupport((requestContext) => passkeyRegister(options, requestContext));
		}
	}
	return await runBrowserCeremony(options.rp.id, () => startRegistration({optionsJSON: options}));
}

export async function performAuthentication(
	options: PublicKeyCredentialRequestOptionsJSON,
): Promise<AuthenticationResponseJSON> {
	await assertWebAuthnSupported();
	if (Platform.isElectron) {
		const electronApi = getElectronAPI();
		const nativeSupported = electronApi && (await electronApi.passkeyIsSupported?.());
		const passkeyAuthenticate = electronApi?.passkeyAuthenticate;
		if (nativeSupported && passkeyAuthenticate) {
			return runNativeCeremonyWithPinSupport((requestContext) => passkeyAuthenticate(options, requestContext));
		}
	}
	return await runBrowserCeremony(options.rpId, () => startAuthentication({optionsJSON: options}));
}
