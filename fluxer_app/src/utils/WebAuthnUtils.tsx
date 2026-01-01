/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/browser';
import {browserSupportsWebAuthn, startAuthentication, startRegistration} from '@simplewebauthn/browser';
import {Platform} from '~/lib/Platform';
import {getElectronAPI} from '~/utils/NativeUtils';

export async function assertWebAuthnSupported(): Promise<void> {
	if (Platform.isElectron) {
		const electronApi = getElectronAPI();
		const nativeSupported = electronApi && (await electronApi.passkeyIsSupported());
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
		const nativeSupported = electronApi && (await electronApi.passkeyIsSupported());
		if (nativeSupported) {
			return electronApi.passkeyRegister(options);
		}
	}

	return await startRegistration({optionsJSON: options});
}

export async function performAuthentication(
	options: PublicKeyCredentialRequestOptionsJSON,
): Promise<AuthenticationResponseJSON> {
	await assertWebAuthnSupported();
	if (Platform.isElectron) {
		const electronApi = getElectronAPI();
		const nativeSupported = electronApi && (await electronApi.passkeyIsSupported());
		if (nativeSupported) {
			return electronApi.passkeyAuthenticate(options);
		}
	}

	return await startAuthentication({optionsJSON: options});
}
