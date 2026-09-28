// SPDX-License-Identifier: AGPL-3.0-or-later

import {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import ProfileTimezoneRollout from '@app/features/user/state/ProfileTimezoneRollout';
import {ProfileFieldPrivacyFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {getCurrentTimeZoneOffsetMinutes} from '@fluxer/date_utils/src/TimeZoneUtils';

export interface BadgeSettings {
	premium_badge_hidden?: boolean;
	premium_badge_timestamp_hidden?: boolean;
	premium_badge_masked?: boolean;
	premium_badge_sequence_hidden?: boolean;
}

function computeVisiblePremiumData(user: User, previewBadgeSettings?: BadgeSettings) {
	const premiumType = user.premiumType;
	const premiumSince = user.premiumSince;
	const premiumLifetimeSequence = user.premiumLifetimeSequence;
	if (!premiumType || premiumType === UserPremiumTypes.NONE) {
		return {
			premiumType: null,
			premiumSince: null,
			premiumLifetimeSequence: null,
		};
	}
	const premiumBadgeHidden = previewBadgeSettings?.premium_badge_hidden ?? user.premiumBadgeHidden;
	const premiumBadgeTimestampHidden =
		previewBadgeSettings?.premium_badge_timestamp_hidden ?? user.premiumBadgeTimestampHidden;
	const premiumBadgeMasked = previewBadgeSettings?.premium_badge_masked ?? user.premiumBadgeMasked;
	const premiumBadgeSequenceHidden =
		previewBadgeSettings?.premium_badge_sequence_hidden ?? user.premiumBadgeSequenceHidden;
	if (premiumBadgeHidden) {
		return {
			premiumType: null,
			premiumSince: null,
			premiumLifetimeSequence: null,
		};
	}
	let visiblePremiumType = premiumType;
	let visiblePremiumSince = premiumSince;
	let visiblePremiumLifetimeSequence = premiumLifetimeSequence;
	if (premiumType === UserPremiumTypes.LIFETIME) {
		if (premiumBadgeMasked) {
			visiblePremiumType = UserPremiumTypes.SUBSCRIPTION;
		}
		if (premiumBadgeSequenceHidden) {
			visiblePremiumLifetimeSequence = null;
		}
	}
	if (premiumBadgeTimestampHidden) {
		visiblePremiumSince = null;
	}
	let premiumSinceString: string | null = null;
	if (visiblePremiumSince) {
		if (typeof visiblePremiumSince === 'string') {
			premiumSinceString = visiblePremiumSince;
		} else if (visiblePremiumSince instanceof Date) {
			premiumSinceString = visiblePremiumSince.toISOString();
		}
	}
	return {
		premiumType: visiblePremiumType,
		premiumSince: premiumSinceString,
		premiumLifetimeSequence: visiblePremiumLifetimeSequence,
	};
}

export function createMockProfile(
	user: User,
	options?: {
		previewBannerUrl?: string | null;
		hasClearedBanner?: boolean;
		previewBio?: string | null;
		previewPronouns?: string | null;
		previewAccentColor?: number | null;
		previewBadgeSettings?: BadgeSettings;
		previewTimezoneOffset?: number | null;
	},
): Profile {
	const finalBanner = options?.hasClearedBanner
		? null
		: options?.previewBannerUrl
			? options.previewBannerUrl
			: user.banner || null;
	const finalBio = options?.previewBio !== undefined ? options.previewBio : user.bio || null;
	const finalPronouns = options?.previewPronouns !== undefined ? options.previewPronouns : user.pronouns || null;
	const visiblePremiumData = computeVisiblePremiumData(user, options?.previewBadgeSettings);
	return new Profile({
		user: user.toJSON(),
		user_profile: {
			bio: finalBio,
			banner: finalBanner,
			pronouns: finalPronouns,
			accent_color: options?.previewAccentColor !== undefined ? options.previewAccentColor : (user.accentColor ?? null),
		},
		timezone_offset:
			options?.previewTimezoneOffset !== undefined
				? options.previewTimezoneOffset
				: ProfileTimezoneRollout.enabled && (user.timezonePrivacyFlags ?? ProfileFieldPrivacyFlags.EVERYONE) !== 0
					? getCurrentTimeZoneOffsetMinutes(user.timezone)
					: null,
		premium_type: visiblePremiumData.premiumType ?? undefined,
		premium_since: visiblePremiumData.premiumSince ?? undefined,
		premium_lifetime_sequence: visiblePremiumData.premiumLifetimeSequence ?? undefined,
	});
}
