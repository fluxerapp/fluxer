// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';

export interface RegisterFormFields {
	showEmail?: boolean;
	showPassword?: boolean;
	showPasswordConfirmation?: boolean;
	showUsername?: boolean;
	showUsernameValidation?: boolean;
	requireUsername?: boolean;
}

export function credentialRegisterFields(usernameSignIn: boolean): RegisterFormFields {
	return {
		showEmail: !usernameSignIn,
		showPassword: true,
		showPasswordConfirmation: true,
		showUsernameValidation: true,
		requireUsername: usernameSignIn,
	};
}

export function readRegistrationUrlCode(params: URLSearchParams): string | undefined {
	return params.get('registration_url')?.trim() || undefined;
}

export function linkRegisterFields(
	snapshot: RuntimeConfigSnapshot,
	registrationUrlCode: string | undefined,
): RegisterFormFields {
	const usernameSignIn = usesUsernameSignIn(snapshot.features);
	if (usernameSignIn || registrationUrlCode || snapshot.registration.mode === 'approval') {
		return credentialRegisterFields(usernameSignIn);
	}
	return {showUsername: false};
}
