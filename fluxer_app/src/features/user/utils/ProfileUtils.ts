// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import {ProfileFieldPrivacyFlags} from '@fluxer/constants/src/UserConstants';
import {getCurrentTimeZoneOffsetMinutes} from '@fluxer/date_utils/src/TimeZoneUtils';
import type {ProfileBadgeResponse} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';

export function createMockProfile(
	user: User,
	options?: {
		previewBannerUrl?: string | null;
		hasClearedBanner?: boolean;
		previewBio?: string | null;
		previewPronouns?: string | null;
		previewAccentColor?: number | null;
		previewTimezoneOffset?: number | null;
		profileBadges?: ReadonlyArray<ProfileBadgeResponse>;
	},
): Profile {
	let finalBanner: string | null;
	if (options?.hasClearedBanner) {
		finalBanner = null;
	} else if (options?.previewBannerUrl) {
		finalBanner = options.previewBannerUrl;
	} else {
		finalBanner = user.banner || null;
	}
	const finalBio = options?.previewBio !== undefined ? options.previewBio : user.bio || null;
	const finalPronouns = options?.previewPronouns !== undefined ? options.previewPronouns : user.pronouns || null;
	const profileUser = user.toJSON();
	const userProfile = {
		bio: finalBio,
		banner: finalBanner,
		pronouns: finalPronouns,
		accent_color: options?.previewAccentColor !== undefined ? options.previewAccentColor : (user.accentColor ?? null),
	};
	let timezoneOffset: number | null;
	if (options?.previewTimezoneOffset !== undefined) {
		timezoneOffset = options.previewTimezoneOffset;
	} else if ((user.timezonePrivacyFlags ?? ProfileFieldPrivacyFlags.EVERYONE) !== 0) {
		timezoneOffset = getCurrentTimeZoneOffsetMinutes(user.timezone);
	} else {
		timezoneOffset = null;
	}
	return new Profile({
		user: profileUser,
		user_profile: userProfile,
		timezone_offset: timezoneOffset,
		profile_badges: options?.profileBadges ?? [],
	});
}

export function customBadgeImageUrl(assetUrl: string): string {
	return `${assetUrl}?size=96`;
}

export function profileBadgeImageUrl(badge: Pick<ProfileBadgeResponse, 'asset' | 'asset_url'>): string | null {
	if (badge.asset !== null) return `${RuntimeConfig.staticCdnEndpoint}/badges/${badge.asset}.webp`;
	if (badge.asset_url) return customBadgeImageUrl(badge.asset_url);
	return null;
}

export function warmProfileBadgeImages(badges: ReadonlyArray<ProfileBadgeResponse>): void {
	for (const badge of badges) {
		const url = profileBadgeImageUrl(badge);
		if (url) ImageCacheUtils.warmImage(url);
	}
}
