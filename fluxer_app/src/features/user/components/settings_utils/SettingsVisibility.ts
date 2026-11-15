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

export function shouldShowFriendSettings(): boolean {
	return !RuntimeConfig.friendsDisabled;
}

export function shouldShowDirectMessageSettings(): boolean {
	return !RuntimeConfig.directMessagesDisabled;
}

export function shouldShowFriendsAndDirectMessageSettings(): boolean {
	return shouldShowFriendSettings() || shouldShowDirectMessageSettings();
}
