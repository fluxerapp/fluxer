// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {
	credentialRegisterFields,
	linkRegisterFields,
	readRegistrationUrlCode,
} from '@app/features/auth/flow/RegisterFormFields';
import {AccountIdentityModes} from '@fluxer/constants/src/AccountIdentityConstants';
import type {InstanceRegistration} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {describe, expect, it} from 'vitest';

function snapshot(usernameSignIn: boolean, mode: InstanceRegistration['mode']): RuntimeConfigSnapshot {
	return {
		features: {account_identity: usernameSignIn ? AccountIdentityModes.USERNAME : AccountIdentityModes.EMAIL},
		registration: {mode, admin_registration_urls_enabled: false},
	} as RuntimeConfigSnapshot;
}

const EMAIL_CREDENTIALS = {
	showEmail: true,
	showPassword: true,
	showPasswordConfirmation: true,
	showUsernameValidation: true,
	requireUsername: false,
};
const USERNAME_CREDENTIALS = {
	showEmail: false,
	showPassword: true,
	showPasswordConfirmation: true,
	showUsernameValidation: true,
	requireUsername: true,
};

describe('register form fields', () => {
	it('asks for credentials on the full register form in both sign-in modes', () => {
		expect(credentialRegisterFields(false)).toEqual(EMAIL_CREDENTIALS);
		expect(credentialRegisterFields(true)).toEqual(USERNAME_CREDENTIALS);
	});

	it('keeps link sign-ups minimal only where the account can sign in right away', () => {
		expect(linkRegisterFields(snapshot(false, 'open'), undefined)).toEqual({showUsername: false});
		expect(linkRegisterFields(snapshot(false, 'closed'), undefined)).toEqual({showUsername: false});
		expect(linkRegisterFields(snapshot(false, 'approval'), undefined)).toEqual(EMAIL_CREDENTIALS);
	});

	it('asks for credentials on link sign-ups through a registration URL, which can require approval', () => {
		const code = readRegistrationUrlCode(new URLSearchParams('?registration_url=%20abc%20'));
		expect(code).toBe('abc');
		for (const mode of ['open', 'approval', 'closed'] as const) {
			expect(linkRegisterFields(snapshot(false, mode), code)).toEqual(EMAIL_CREDENTIALS);
		}
		expect(readRegistrationUrlCode(new URLSearchParams('?registration_url=%20'))).toBeUndefined();
	});

	it('always asks for a username and password on link sign-ups of a username instance', () => {
		for (const mode of ['open', 'approval', 'closed'] as const) {
			expect(linkRegisterFields(snapshot(true, mode), undefined)).toEqual(USERNAME_CREDENTIALS);
		}
	});
});
