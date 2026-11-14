// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Profile} from '@app/features/user/models/Profile';
import Users from '@app/features/user/state/Users';
import {type AccountGateUser, canPerform} from '@app/features/user/utils/AccountGates';
import {AccountTiers, TierGatedActions} from '@fluxer/constants/src/AccountTierConstants';

export function canOpenProfileEditor(user: AccountGateUser | null | undefined): boolean {
	const gate = canPerform(user, TierGatedActions.PROFILE_CUSTOMIZE);
	return gate.allowed || gate.tier === AccountTiers.UNVERIFIED;
}

export function canEditOwnProfileBadges(
	userId: string,
	profile: Pick<Profile, 'profileBadges' | 'hiddenProfileBadgeCount'> | null | undefined,
): boolean {
	const currentUser = Users.currentUser;
	if (!profile || !currentUser || currentUser.id !== userId || !canOpenProfileEditor(currentUser)) return false;
	return profile.profileBadges.length > 0 || profile.hiddenProfileBadgeCount > 0;
}
