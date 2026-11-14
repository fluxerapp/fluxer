// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import {usesUniqueUsernames, usesUsernameSignIn} from '@app/features/app/utils/AccountIdentityFeatures';
import type {Account} from '@app/features/platform/state/AuthSession';
import {formatUserTag} from '@app/features/user/utils/UserTagUtils';

export type LoginIdentifierField = 'email' | 'login';

export function loginIdentifierField(runtimeSnapshot: RuntimeConfigSnapshot | null): LoginIdentifierField {
	return runtimeSnapshot !== null && usesUsernameSignIn(runtimeSnapshot.features) ? 'login' : 'email';
}

export function accountLoginIdentifierField(account: Account): LoginIdentifierField {
	return loginIdentifierField(account.instance ?? null);
}

export function accountSignInIdentifier(account: Account): string | null {
	const userData = account.userData;
	if (userData == null) {
		return null;
	}
	const instance = account.instance ?? null;
	if (instance !== null && usesUsernameSignIn(instance.features)) {
		return formatUserTag(userData, usesUniqueUsernames(instance.features));
	}
	return userData.email ?? null;
}
