// SPDX-License-Identifier: AGPL-3.0-or-later

import {promptForSecurityKeyPin} from '@app/features/auth/components/modals/PasskeyPinModal';
import {writePasskeyLoginRoute} from '@app/features/auth/passkey_migration/PasskeyLoginRoute';
import {parsePasskeyPinFailure} from '@app/features/auth/utils/PasskeyPinErrors';
import {Platform} from '@app/features/platform/types/Platform';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {PASSKEY_MIGRATION_RP_ID} from '@fluxer/constants/src/PasskeyConstants';
import {
	type AuthenticationResponseJSON,
	browserSupportsWebAuthn,
	type PublicKeyCredentialCreationOptionsJSON,
	type PublicKeyCredentialRequestOptionsJSON,
	type RegistrationResponseJSON,
	startAuthentication,
	startRegistration,
} from '@simplewebauthn/browser';

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

async function rememberMigratedPasskeyUse<T>(rpId: string | undefined, ceremony: Promise<T>): Promise<T> {
	const result = await ceremony;
	if (rpId === PASSKEY_MIGRATION_RP_ID) {
		writePasskeyLoginRoute('native');
	}
	return result;
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
			return rememberMigratedPasskeyUse(
				options.rp.id,
				runNativeCeremonyWithPinSupport((requestContext) => passkeyRegister(options, requestContext)),
			);
		}
	}
	return rememberMigratedPasskeyUse(options.rp.id, startRegistration({optionsJSON: options}));
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
			return rememberMigratedPasskeyUse(
				options.rpId,
				runNativeCeremonyWithPinSupport((requestContext) => passkeyAuthenticate(options, requestContext)),
			);
		}
	}
	return rememberMigratedPasskeyUse(options.rpId, startAuthentication({optionsJSON: options}));
}
