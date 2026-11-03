// SPDX-License-Identifier: AGPL-3.0-or-later

import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import type {PublicKeyCredentialCreationOptionsJSON} from '@simplewebauthn/browser';

const PREFETCHED_OPTIONS_MAX_AGE_MS = 4 * 60 * 1000;

export async function preparePasskeyRegistration(): Promise<PublicKeyCredentialCreationOptionsJSON> {
	await WebAuthnUtils.assertWebAuthnSupported();
	return UserCommands.getWebAuthnRegistrationOptions();
}

export function createPasskeyRegistration(
	prefetched: PublicKeyCredentialCreationOptionsJSON,
	now: () => number = Date.now,
): (name: string) => Promise<void> {
	let unused: PublicKeyCredentialCreationOptionsJSON | null = prefetched;
	const fetchedAt = now();
	return async (name) => {
		const options =
			unused !== null && now() - fetchedAt < PREFETCHED_OPTIONS_MAX_AGE_MS
				? unused
				: await preparePasskeyRegistration();
		const credential = await WebAuthnUtils.performRegistration(options);
		unused = null;
		await UserCommands.registerWebAuthnCredential(credential, options.challenge, name);
	};
}
