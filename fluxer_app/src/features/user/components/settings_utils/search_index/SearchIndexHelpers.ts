// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Users from '@app/features/user/state/Users';

export function shouldShowClaimedAccountSettings(): boolean {
	return Users.getCurrentUser()?.isClaimed() ?? true;
}

export function shouldShowEmailAccountSettings(): boolean {
	return !RuntimeConfig.usesUsernameSignIn && shouldShowClaimedAccountSettings();
}

export function shouldShowRecoveryKitSettings(): boolean {
	return RuntimeConfig.usesUsernameSignIn && shouldShowClaimedAccountSettings();
}
