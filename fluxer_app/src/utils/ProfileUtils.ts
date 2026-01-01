/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {UserPremiumTypes} from '~/Constants';
import {ProfileRecord} from '~/records/ProfileRecord';
import type {UserRecord} from '~/records/UserRecord';

export interface BadgeSettings {
	premium_badge_hidden?: boolean;
	premium_badge_timestamp_hidden?: boolean;
	premium_badge_masked?: boolean;
	premium_badge_sequence_hidden?: boolean;
}

function computeVisiblePremiumData(user: UserRecord, previewBadgeSettings?: BadgeSettings) {
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
	user: UserRecord,
	options?: {
		previewBannerUrl?: string | null;
		hasClearedBanner?: boolean;
		previewBio?: string | null;
		previewPronouns?: string | null;
		previewAccentColor?: string | null;
		previewBadgeSettings?: BadgeSettings;
	},
): ProfileRecord {
	const finalBanner = options?.hasClearedBanner
		? null
		: options?.previewBannerUrl
			? options.previewBannerUrl
			: user.banner || null;
	const finalBio = options?.previewBio !== undefined ? options.previewBio : user.bio || null;
	const finalPronouns = options?.previewPronouns !== undefined ? options.previewPronouns : user.pronouns || null;
	const visiblePremiumData = computeVisiblePremiumData(user, options?.previewBadgeSettings);

	return new ProfileRecord({
		user: user.toJSON(),
		user_profile: {
			bio: finalBio,
			banner: finalBanner,
			pronouns: finalPronouns,
			accent_color: options?.previewAccentColor !== undefined ? options.previewAccentColor : user.accentColor || null,
		},
		timezone_offset: null,
		premium_type: visiblePremiumData.premiumType ?? undefined,
		premium_since: visiblePremiumData.premiumSince ?? undefined,
		premium_lifetime_sequence: visiblePremiumData.premiumLifetimeSequence ?? undefined,
	});
}
