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

import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {ProfileRecord} from '~/records/ProfileRecord';
import type {UserProfile, UserRecord} from '~/records/UserRecord';
import * as AvatarUtils from '~/utils/AvatarUtils';

export interface ProfileDisplayContext {
	user: UserRecord;
	profile?: ProfileRecord | null;
	guildId?: string | null;
	guildMember?: GuildMemberRecord | null;
	guildMemberProfile?: UserProfile | null;
}

export interface ProfilePreviewOverrides {
	previewAvatarUrl?: string | null;
	previewBannerUrl?: string | null;
	hasClearedAvatar?: boolean;
	hasClearedBanner?: boolean;
	ignoreGuildAvatar?: boolean;
	ignoreGuildBanner?: boolean;
}

function getProfileAvatarUrl(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
	animated = false,
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
			return AvatarUtils.getUserAvatarURL({id: user.id, avatar: null}, animated);
		}
		if (guildMember.avatar) {
			return AvatarUtils.getGuildMemberAvatarURL({
				guildId,
				userId: user.id,
				avatar: guildMember.avatar,
				animated,
			});
		}
	}

	return AvatarUtils.getUserAvatarURL(user, animated);
}

export function getProfileBannerUrl(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
	animated = false,
	size = 1024,
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

export function getProfileAvatarUrls(
	context: ProfileDisplayContext,
	overrides?: ProfilePreviewOverrides,
): {
	avatarUrl: string | null;
	hoverAvatarUrl: string | null;
} {
	return {
		avatarUrl: getProfileAvatarUrl(context, overrides, false),
		hoverAvatarUrl: getProfileAvatarUrl(context, overrides, true),
	};
}
