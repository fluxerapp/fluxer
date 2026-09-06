// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {describe, expect, it} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '../../database/types/UserTypes';
import {User} from '../../models/User';
import {userHasMfa} from '../services/SudoMethods';
import {hasNoVerifiableCredential} from '../services/SudoVerificationService';

function createUser(overrides: Partial<UserRow> = {}): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: createUserID(1n),
		username: 'test_user',
		discriminator: 1,
		bot: false,
		password_hash: 'hash',
		traits: new Set<string>(),
		...overrides,
	});
}

describe('sudo verification credential capability', () => {
	it('lets an SSO provisioned account without a password satisfy sudo mode', () => {
		const user = createUser({password_hash: null, traits: new Set<string>(['sso'])});
		expect(user.isUnclaimedAccount()).toBe(false);
		expect(hasNoVerifiableCredential(user, userHasMfa(user))).toBe(true);
	});

	it('still lets an unclaimed account satisfy sudo mode', () => {
		const user = createUser({password_hash: null});
		expect(hasNoVerifiableCredential(user, userHasMfa(user))).toBe(true);
	});

	it('still requires a password from accounts that have one', () => {
		const user = createUser({password_hash: 'hash', traits: new Set<string>(['sso'])});
		expect(hasNoVerifiableCredential(user, userHasMfa(user))).toBe(false);
	});

	it('still requires MFA from an SSO account that enrolled a second factor', () => {
		const user = createUser({
			password_hash: null,
			traits: new Set<string>(['sso']),
			authenticator_types: new Set<number>([UserAuthenticatorTypes.TOTP]),
		});
		expect(userHasMfa(user)).toBe(true);
		expect(hasNoVerifiableCredential(user, userHasMfa(user))).toBe(false);
	});

	it('never applies to bots', () => {
		const user = createUser({password_hash: null, bot: true});
		expect(hasNoVerifiableCredential(user, userHasMfa(user))).toBe(false);
	});
});
