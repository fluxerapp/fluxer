// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CustomBadgeDefinition} from '@app/api/instance/InstanceConfigRepository';
import type {User} from '@app/api/models/User';
import {heldPremiumBadgeKind} from '@app/api/user/profile_badges/PremiumBadgeMasking';
import {getBadgePremiumType} from '@app/api/user/UserHelpers';
import {
	CustomBadgeTypes,
	customBadgeIdFromProfileBadgeKey,
	PREMIUM_PROFILE_BADGE_KINDS,
	type ProfileBadgeKey,
	type ProfileBadgeKind,
} from '@fluxer/constants/src/BadgeConstants';

export interface HiddenProfileBadgeChanges {
	hidden_profile_badges?: Array<ProfileBadgeKey>;
	premium_badge_hidden?: boolean;
	premium_badge_sequence_hidden?: boolean;
	operator_pass_badge_hidden?: boolean;
}

const DEPRECATED_HIDE_FIELDS: ReadonlyArray<{
	field: 'premium_badge_hidden' | 'premium_badge_sequence_hidden' | 'operator_pass_badge_hidden';
	kinds: ReadonlyArray<ProfileBadgeKind>;
}> = [
	{field: 'premium_badge_hidden', kinds: PREMIUM_PROFILE_BADGE_KINDS},
	{field: 'premium_badge_sequence_hidden', kinds: ['visionary_number']},
	{field: 'operator_pass_badge_hidden', kinds: ['operator_pass']},
];

export function resolveHiddenProfileBadges(
	current: ReadonlySet<ProfileBadgeKey>,
	changes: HiddenProfileBadgeChanges,
): Set<ProfileBadgeKey> | null {
	if (changes.hidden_profile_badges !== undefined) return new Set(changes.hidden_profile_badges);
	if (DEPRECATED_HIDE_FIELDS.every(({field}) => changes[field] === undefined)) return null;
	const next = new Set(current);
	for (const {field, kinds} of DEPRECATED_HIDE_FIELDS) {
		const hide = changes[field];
		if (hide === undefined) continue;
		for (const kind of kinds) {
			if (hide) next.add(kind);
			else next.delete(kind);
		}
	}
	return next;
}

export function dropUndefinedCustomBadgeKeys(
	hidden: ReadonlySet<ProfileBadgeKey>,
	customBadges: ReadonlyArray<CustomBadgeDefinition>,
): Set<ProfileBadgeKey> {
	const userBadgeIds = new Set(
		customBadges.filter((badge) => badge.type === CustomBadgeTypes.USER).map((badge) => badge.id),
	);
	return new Set(
		[...hidden].filter((key) => {
			const id = customBadgeIdFromProfileBadgeKey(key);
			return id === null || userBadgeIds.has(id);
		}),
	);
}

export function deprecatedBadgeHideFields(
	user: Pick<User, 'hiddenProfileBadges' | 'premiumType' | 'premiumFlags'>,
	selfHosted: boolean,
): Required<Omit<HiddenProfileBadgeChanges, 'hidden_profile_badges'>> {
	const hidden = user.hiddenProfileBadges;
	const heldKind = heldPremiumBadgeKind(getBadgePremiumType(user), selfHosted);
	return {
		premium_badge_hidden: heldKind
			? hidden.has(heldKind)
			: PREMIUM_PROFILE_BADGE_KINDS.every((kind) => hidden.has(kind)),
		premium_badge_sequence_hidden: hidden.has('visionary_number'),
		operator_pass_badge_hidden: hidden.has('operator_pass'),
	};
}
