// SPDX-License-Identifier: AGPL-3.0-or-later

import type {User} from '@app/api/models/User';
import {getBadgePremiumType} from '@app/api/user/UserHelpers';
import type {ProfileBadgeKey, ProfileBadgeKind} from '@fluxer/constants/src/BadgeConstants';
import {PremiumFlags, type UserPremiumType, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';

export interface VisiblePremiumBadge {
	premiumType?: UserPremiumType;
	premiumSince?: Date;
	premiumLifetimeSequence?: number;
}

export function heldPremiumBadgeKind(
	premiumType: UserPremiumType | undefined,
	selfHosted: boolean,
): ProfileBadgeKind | null {
	if (premiumType === undefined || premiumType === UserPremiumTypes.NONE) return null;
	if (selfHosted) return 'premium';
	return premiumType === UserPremiumTypes.LIFETIME ? 'visionary' : 'plutonium';
}

export function applyPremiumBadgeMasking(
	user: Pick<User, 'premiumType' | 'premiumFlags' | 'premiumSince' | 'premiumLifetimeSequence'>,
	options: {premiumFlags: number; hidden: ReadonlySet<ProfileBadgeKey>; selfHosted: boolean; restricted: boolean},
): VisiblePremiumBadge {
	if (options.restricted) return {};
	const heldType = getBadgePremiumType(user);
	const heldKind = heldPremiumBadgeKind(heldType, options.selfHosted);
	if (heldKind && options.hidden.has(heldKind)) return {};
	let premiumType = heldType;
	let premiumLifetimeSequence = user.premiumLifetimeSequence ?? undefined;
	if (user.premiumType === UserPremiumTypes.LIFETIME) {
		if (options.premiumFlags & PremiumFlags.BADGE_MASKED) {
			premiumType = UserPremiumTypes.SUBSCRIPTION;
		}
		if (options.hidden.has('visionary_number')) {
			premiumLifetimeSequence = undefined;
		}
	}
	const premiumSince =
		options.premiumFlags & PremiumFlags.BADGE_TIMESTAMP_HIDDEN ? undefined : (user.premiumSince ?? undefined);
	return {premiumType, premiumSince, premiumLifetimeSequence};
}

const PROFILE_BADGE_SETTING_FLAGS = [
	{field: 'premium_badge_masked', flag: PremiumFlags.BADGE_MASKED},
	{field: 'premium_badge_timestamp_hidden', flag: PremiumFlags.BADGE_TIMESTAMP_HIDDEN},
	{field: 'operator_pass_badge_timestamp_hidden', flag: PremiumFlags.OPERATOR_PASS_BADGE_TIMESTAMP_HIDDEN},
	{field: 'operator_pass_badge_count_hidden', flag: PremiumFlags.OPERATOR_PASS_BADGE_COUNT_HIDDEN},
] as const;

export type ProfileBadgeSettings = Partial<Record<(typeof PROFILE_BADGE_SETTING_FLAGS)[number]['field'], boolean>>;

export function isOperatorPassTimestampHidden(premiumFlags: number): boolean {
	return (premiumFlags & PremiumFlags.OPERATOR_PASS_BADGE_TIMESTAMP_HIDDEN) !== 0;
}

export function isOperatorPassCountHidden(premiumFlags: number): boolean {
	return (premiumFlags & PremiumFlags.OPERATOR_PASS_BADGE_COUNT_HIDDEN) !== 0;
}

export function applyProfileBadgeSettings(premiumFlags: number, settings: ProfileBadgeSettings): number {
	let flags = premiumFlags;
	for (const {field, flag} of PROFILE_BADGE_SETTING_FLAGS) {
		const enabled = settings[field];
		if (enabled === undefined) continue;
		flags = enabled ? flags | flag : flags & ~flag;
	}
	return flags;
}
