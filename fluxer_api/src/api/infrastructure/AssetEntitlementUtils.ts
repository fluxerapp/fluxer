// SPDX-License-Identifier: AGPL-3.0-or-later

import {getGlobalLimitConfigSnapshot} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {User} from '@app/api/models/User';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';

const ANIMATED_PREFIX = 'a_';

export function stripAnimationPrefix(hash: string): string {
	return hash.startsWith(ANIMATED_PREFIX) ? hash.substring(ANIMATED_PREFIX.length) : hash;
}

export function stripAnimationPrefixIfNoEntitlement(
	hash: string | null | undefined,
	hasEntitlement: boolean,
): string | null {
	if (hash == null) {
		return null;
	}
	if (hasEntitlement) {
		return hash;
	}
	return stripAnimationPrefix(hash);
}

export function userHasAnimatedAvatarEntitlement(user: User): boolean {
	return userHasAnimatedEntitlement(user, 'feature_animated_avatar');
}

function userHasAnimatedEntitlement(user: User, key: 'feature_animated_avatar' | 'feature_animated_banner'): boolean {
	if (user.isBot) {
		return true;
	}
	const snapshot = getGlobalLimitConfigSnapshot();
	const ctx = createLimitMatchContext({user});
	return resolveLimitSafe(snapshot, ctx, key, 0) > 0;
}

export function stripAvatarForUser(user: User): string | null {
	return stripAnimationPrefixIfNoEntitlement(user.avatarHash, userHasAnimatedAvatarEntitlement(user));
}

export function stripBannerForUser(user: User): string | null {
	if (!userHasAnimatedEntitlement(user, 'feature_animated_banner')) {
		return null;
	}
	return user.bannerHash;
}

export function stripGuildIconForFeatures(
	hash: string | null | undefined,
	features: ReadonlySet<string>,
): string | null {
	return stripAnimationPrefixIfNoEntitlement(hash, features.has(GuildFeatures.ANIMATED_ICON));
}

export function stripGuildBannerForFeatures(
	hash: string | null | undefined,
	features: ReadonlySet<string>,
): string | null {
	if (!features.has(GuildFeatures.BANNER)) {
		return null;
	}
	return stripAnimationPrefixIfNoEntitlement(hash, features.has(GuildFeatures.ANIMATED_BANNER));
}

export function stripGuildSplashForFeatures(
	hash: string | null | undefined,
	features: ReadonlySet<string>,
): string | null {
	return features.has(GuildFeatures.INVITE_SPLASH) ? (hash ?? null) : null;
}
