// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildMember} from '@app/features/member/models/GuildMember';
import type {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {
	MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
	MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
} from '@fluxer/constants/src/MediaProxyAssetSizes';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import type {UserProfile} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface ProfileDisplayContext {
	user: User;
	profile?: Profile | null;
	guildId?: string | null;
	guildMember?: GuildMember | null;
	guildMemberProfile?: UserProfile | null;
}

export interface ProfilePreviewOverrides {
	previewAvatarUrl?: string | null;
	previewBannerUrl?: string | null;
	previewAccentColor?: number | null;
	hasClearedAvatar?: boolean;
	hasClearedBanner?: boolean;
	ignoreGuildAvatar?: boolean;
	ignoreGuildBanner?: boolean;
}

function getProfileAvatarUrl(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
	animated = false,
	size: MediaProxyImageSize = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
): string | null {
	const {user, guildId, guildMember} = context;
	const {previewAvatarUrl, hasClearedAvatar, ignoreGuildAvatar} = overrides || {};
	if (hasClearedAvatar) {
		return null;
	}
	if (previewAvatarUrl) {
		return previewAvatarUrl;
	}
	if (!ignoreGuildAvatar && guildId && guildMember) {
		return AvatarUtils.getGuildMemberDisplayAvatarURL({
			guildId,
			user,
			memberAvatar: guildMember.avatar,
			avatarUnset: guildMember.isAvatarUnset(),
			animated,
			size,
		});
	}
	return AvatarUtils.getUserAvatarURL(user, animated, size);
}

export function getProfileAvatarMenuUrl(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
	animated = false,
	size: MediaProxyImageSize = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
): string | null {
	const {user, guildId, guildMember} = context;
	const {previewAvatarUrl, hasClearedAvatar, ignoreGuildAvatar} = overrides || {};
	if (hasClearedAvatar) {
		return null;
	}
	if (previewAvatarUrl) {
		return previewAvatarUrl;
	}
	if (!ignoreGuildAvatar && guildId && guildMember) {
		if (guildMember.isAvatarUnset()) {
			return null;
		}
		if (guildMember.avatar) {
			return AvatarUtils.getGuildMemberAvatarURL({
				guildId,
				userId: user.id,
				avatar: user.avatar,
				memberAvatar: guildMember.avatar,
				animated,
				size,
			});
		}
	}
	if (user.avatar) {
		return AvatarUtils.getUserAvatarURL(user, animated, size);
	}
	return null;
}

function getProfileBannerUrl(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
	animated = false,
	size: MediaProxyImageSize = MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
): string | null {
	const {user, profile, guildId, guildMember, guildMemberProfile} = context;
	const {previewBannerUrl, hasClearedBanner, ignoreGuildBanner} = overrides || {};
	if (hasClearedBanner) {
		return null;
	}
	if (previewBannerUrl) {
		return previewBannerUrl;
	}
	let effectiveBanner: string | null = null;
	if (!ignoreGuildBanner && guildId && guildMember) {
		if (guildMember.isBannerUnset()) {
			return null;
		}
		if (guildMemberProfile?.banner) {
			if (guildMemberProfile.banner.startsWith('blob:') || guildMemberProfile.banner.startsWith('data:')) {
				return guildMemberProfile.banner;
			}
			return AvatarUtils.getGuildMemberBannerURL({
				guildId,
				userId: user.id,
				banner: guildMemberProfile.banner,
				memberBanner: guildMemberProfile.banner,
				animated,
				size,
			});
		}
	}
	if (profile?.userProfile?.banner) {
		effectiveBanner = profile.userProfile.banner;
	} else if (user.banner) {
		effectiveBanner = user.banner;
	}
	if (effectiveBanner) {
		if (effectiveBanner.startsWith('blob:') || effectiveBanner.startsWith('data:')) {
			return effectiveBanner;
		}
		return AvatarUtils.getUserBannerURL({id: user.id, banner: effectiveBanner}, animated, size);
	}
	return null;
}

export function getProfileBannerMenuUrl(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
	animated = false,
	size: MediaProxyImageSize = MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
): string | null {
	return getProfileBannerUrl(context, overrides, animated, size);
}

export function getProfileBannerUrls(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
	size: MediaProxyImageSize = MEDIA_PROXY_PROFILE_BANNER_SIZE_MODAL,
): {
	bannerUrl: string | null;
	hoverBannerUrl: string | null;
} {
	return {
		bannerUrl: getProfileBannerUrl(context, overrides, false, size),
		hoverBannerUrl: getProfileBannerUrl(context, overrides, true, size),
	};
}

export function getProfileAvatarUrls(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
	size: MediaProxyImageSize = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
): {
	avatarUrl: string | null;
	hoverAvatarUrl: string | null;
} {
	return {
		avatarUrl: getProfileAvatarUrl(context, overrides, false, size),
		hoverAvatarUrl: getProfileAvatarUrl(context, overrides, true, size),
	};
}
