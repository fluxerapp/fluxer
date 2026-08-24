// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {
	buildMediaProxyURL,
	LARGEST_MEDIA_PROXY_IMAGE_SIZE,
	MEDIA_PROXY_IMAGE_SIZE_LADDER,
	snapMediaProxyImageSize,
} from '@app/features/messaging/utils/MediaProxyUtils';

export {MEDIA_PROXY_IMAGE_SIZE_LADDER, snapMediaProxyImageSize};

import {cdnUrl, mediaUrl, setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import type {User} from '@app/features/user/models/User';
import {
	getDefaultAvatarIndex,
	getDefaultAvatarPrimaryColor as getSharedDefaultAvatarPrimaryColor,
	normalizeEndpoint,
	parseAvatarHash,
} from '@app/features/user/utils/AvatarMediaUtils';
import {
	MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
	MEDIA_PROXY_ICON_SIZE_DEFAULT,
} from '@fluxer/constants/src/MediaProxyAssetSizes';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import {SOUNDBOARD_SOUND_PATH_PREFIX} from '@fluxer/constants/src/SoundboardConstants';

const GUILD_BANNER_CSS_WIDTH = 360;
const GUILD_EMBED_SPLASH_CSS_WIDTH = 360;
const GUILD_SPLASH_FALLBACK_CSS_WIDTH = 1024;

const MEDIA_PROXY_WIDE_ASSET_SERVER_MIN_DIMENSION = 480;
const MEDIA_PROXY_WIDE_ASSET_SERVER_MAX_DIMENSION = 2400;
const MEDIA_PROXY_ICON_SERVER_MIN_DIMENSION = 128;
const MEDIA_PROXY_ICON_SERVER_MAX_DIMENSION = 1024;

function snapImageSizeWithinServerRange(
	cssPixels: number,
	minDimension: number,
	maxDimension: number,
): MediaProxyImageSize {
	const floor = MEDIA_PROXY_IMAGE_SIZE_LADDER.find((rung) => rung >= minDimension) ?? LARGEST_MEDIA_PROXY_IMAGE_SIZE;
	const ceiling = MEDIA_PROXY_IMAGE_SIZE_LADDER.reduce(
		(largest, rung) => (rung <= maxDimension ? rung : largest),
		MEDIA_PROXY_IMAGE_SIZE_LADDER[0],
	);
	const snapped = snapMediaProxyImageSize(cssPixels);
	if (snapped < floor) return floor;
	if (snapped > ceiling) return ceiling;
	return snapped;
}

function snapWideAssetImageSize(cssPixels: number): MediaProxyImageSize {
	return snapImageSizeWithinServerRange(
		cssPixels,
		MEDIA_PROXY_WIDE_ASSET_SERVER_MIN_DIMENSION,
		MEDIA_PROXY_WIDE_ASSET_SERVER_MAX_DIMENSION,
	);
}

function snapIconImageSize(cssPixels: number): MediaProxyImageSize {
	return snapImageSizeWithinServerRange(
		cssPixels,
		MEDIA_PROXY_ICON_SERVER_MIN_DIMENSION,
		MEDIA_PROXY_ICON_SERVER_MAX_DIMENSION,
	);
}

const getViewportSplashSize = (): MediaProxyImageSize => {
	const screenWidth = typeof window === 'undefined' ? Number.NaN : window.screen.width;
	const cssWidth = Number.isFinite(screenWidth) && screenWidth > 0 ? screenWidth : GUILD_SPLASH_FALLBACK_CSS_WIDTH;
	return snapWideAssetImageSize(cssWidth);
};

const DEFAULT_AVATAR_ASSET_VERSION = '1';

export const getDefaultAvatarURLForIndex = (index: number): string =>
	cdnUrl(`avatars/${index}.png?v=${DEFAULT_AVATAR_ASSET_VERSION}`);

export function getDefaultAvatarPrimaryColor(id: string) {
	return getSharedDefaultAvatarPrimaryColor(id);
}

export function getDefaultAvatarURL(id: string) {
	return getDefaultAvatarURLForIndex(getDefaultAvatarIndex(id));
}

type AvatarOptions = Pick<User, 'id' | 'avatar'>;
type BannerOptions = Pick<User, 'id' | 'banner'>;

interface IconOptions {
	id: string;
	icon: string | null;
}

type MediaURLParams = {
	path: string;
	id: string;
	hash: string;
	size?: MediaProxyImageSize;
	format: string;
	animated?: boolean;
	endpoint?: string;
};

const parseMediaHashForRequest = (value: string, animated = false) => {
	const {animated: isAnimated, hash} = parseAvatarHash(value);
	const shouldAnimate = isAnimated && animated;
	return {
		hash: shouldAnimate ? value : hash,
		animated: shouldAnimate ? true : undefined,
	};
};
const getMediaURL = ({path, id, hash, size, format, animated, endpoint}: MediaURLParams) => {
	if (DeveloperOptions.forceRenderPlaceholders) {
		return '';
	}
	const baseEndpoint = endpoint === undefined ? RuntimeConfig.mediaEndpoint : endpoint;
	if (!baseEndpoint) {
		return '';
	}
	const basePath = `${path}/${id}/${hash}.${format}`;
	const url = size ? setPathQueryParams(basePath, {size}) : basePath;
	const proxyOptions = animated === undefined ? undefined : {animated};
	return buildMediaProxyURL(`${normalizeEndpoint(baseEndpoint)}/${url}`, proxyOptions);
};

type GuildMemberMediaURLParams = {
	path: string;
	guildId: string;
	userId: string;
	hash: string;
	size?: MediaProxyImageSize;
	format: string;
	animated?: boolean;
};

const getGuildMemberMediaURL = ({path, guildId, userId, hash, size, format, animated}: GuildMemberMediaURLParams) => {
	if (DeveloperOptions.forceRenderPlaceholders) {
		return '';
	}
	const baseEndpoint = RuntimeConfig.mediaEndpoint;
	if (!baseEndpoint) {
		return '';
	}
	const basePath = `guilds/${guildId}/users/${userId}/${path}/${hash}.${format}`;
	const url = size ? setPathQueryParams(basePath, {size}) : basePath;
	const proxyOptions = animated === undefined ? undefined : {animated};
	return buildMediaProxyURL(`${normalizeEndpoint(baseEndpoint)}/${url}`, proxyOptions);
};
const buildWebpMediaUrl = (params: Omit<MediaURLParams, 'format'>) => getMediaURL({...params, format: 'webp'});
const buildPngMediaUrl = (params: Omit<MediaURLParams, 'format'>) => getMediaURL({...params, format: 'png'});
const buildGuildMemberWebpUrl = (params: Omit<GuildMemberMediaURLParams, 'format'>) =>
	getGuildMemberMediaURL({...params, format: 'webp'});
const buildGuildMemberPngUrl = (params: Omit<GuildMemberMediaURLParams, 'format'>) =>
	getGuildMemberMediaURL({...params, format: 'png'});

export function getUserAvatarURL(
	{id, avatar}: AvatarOptions,
	animated = false,
	size: MediaProxyImageSize = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
) {
	if (!avatar) {
		return getDefaultAvatarURLForIndex(getDefaultAvatarIndex(id));
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(avatar, animated);
	return buildWebpMediaUrl({
		path: 'avatars',
		id,
		hash,
		size: snapIconImageSize(size),
		animated: shouldAnimate,
	});
}

export function getUserNotificationAvatarURL(
	{id, avatar}: AvatarOptions,
	size: MediaProxyImageSize = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
) {
	if (!avatar) {
		return getDefaultAvatarURLForIndex(getDefaultAvatarIndex(id));
	}
	const {hash, animated} = parseMediaHashForRequest(avatar, false);
	return buildPngMediaUrl({
		path: 'avatars',
		id,
		hash,
		size: snapIconImageSize(size),
		animated,
	});
}

export function getGuildMemberNotificationAvatarURL({
	guildId,
	userId,
	avatar,
	memberAvatar,
	avatarUnset = false,
	size = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
}: {
	guildId: string;
	userId: string;
	avatar: string | null;
	memberAvatar?: string | null;
	avatarUnset?: boolean;
	size?: MediaProxyImageSize;
}) {
	if (avatarUnset) {
		return getUserNotificationAvatarURL({id: userId, avatar: null}, size);
	}
	if (memberAvatar) {
		const {hash} = parseMediaHashForRequest(memberAvatar, false);
		return buildGuildMemberPngUrl({
			path: 'avatars',
			guildId,
			userId,
			hash,
			size: snapIconImageSize(size),
		});
	}
	return getUserNotificationAvatarURL({id: userId, avatar}, size);
}

export function getUserBannerURL({id, banner}: BannerOptions, animated = false, size: MediaProxyImageSize = 1024) {
	if (!banner) {
		return '';
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(banner, animated);
	return buildWebpMediaUrl({
		path: 'banners',
		id,
		hash,
		size: snapWideAssetImageSize(size),
		animated: shouldAnimate,
	});
}

const SOUNDBOARD_SOUND_URL_CACHE = new Map<string, string>();
const SOUNDBOARD_SOUND_URL_CACHE_LIMIT = 4096;

export function getSoundboardSoundURL(soundId: string): string {
	if (DeveloperOptions.forceRenderPlaceholders) {
		return '';
	}
	const key = `${RuntimeConfig.mediaEndpoint}:${soundId}`;
	const cached = SOUNDBOARD_SOUND_URL_CACHE.get(key);
	if (cached !== undefined) return cached;
	const result = mediaUrl(`${SOUNDBOARD_SOUND_PATH_PREFIX}/${soundId}`);
	if (SOUNDBOARD_SOUND_URL_CACHE.size >= SOUNDBOARD_SOUND_URL_CACHE_LIMIT) SOUNDBOARD_SOUND_URL_CACHE.clear();
	SOUNDBOARD_SOUND_URL_CACHE.set(key, result);
	return result;
}

export function getGuildIconURL({id, icon}: IconOptions, animated = false) {
	if (!icon) {
		return '';
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(icon, animated);
	return buildWebpMediaUrl({
		path: 'icons',
		id,
		hash,
		size: snapIconImageSize(MEDIA_PROXY_ICON_SIZE_DEFAULT),
		animated: shouldAnimate,
	});
}

export function getGuildSplashURL({id, splash}: {id: string; splash: string | null}) {
	if (!splash) {
		return '';
	}
	return buildWebpMediaUrl({
		path: 'splashes',
		id,
		hash: splash,
		size: getViewportSplashSize(),
	});
}

export function getGuildBannerURL(
	{
		id,
		banner,
	}: {
		id: string;
		banner: string | null;
	},
	animated = false,
) {
	if (!banner) {
		return '';
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(banner, animated);
	return buildWebpMediaUrl({
		path: 'banners',
		id,
		hash,
		size: snapWideAssetImageSize(GUILD_BANNER_CSS_WIDTH),
		animated: shouldAnimate,
	});
}

export function getGuildMemberAvatarURL({
	guildId,
	userId,
	avatar,
	memberAvatar,
	animated = false,
	size = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
}: {
	guildId: string;
	userId: string;
	avatar: string | null;
	memberAvatar?: string | null;
	animated?: boolean;
	size?: MediaProxyImageSize;
}) {
	if (memberAvatar) {
		const {hash, animated: shouldAnimate} = parseMediaHashForRequest(memberAvatar, animated);
		return buildGuildMemberWebpUrl({
			path: 'avatars',
			guildId,
			userId,
			hash,
			size: snapIconImageSize(size),
			animated: shouldAnimate,
		});
	}
	if (avatar) {
		const {hash, animated: shouldAnimate} = parseMediaHashForRequest(avatar, animated);
		return buildWebpMediaUrl({
			path: 'avatars',
			id: userId,
			hash,
			size: snapIconImageSize(size),
			animated: shouldAnimate,
		});
	}
	return getDefaultAvatarURLForIndex(getDefaultAvatarIndex(userId));
}

export function getGuildMemberDisplayAvatarURL({
	guildId,
	user,
	memberAvatar,
	avatarUnset = false,
	animated = false,
	size = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
}: {
	guildId: string;
	user: AvatarOptions;
	memberAvatar?: string | null;
	avatarUnset?: boolean;
	animated?: boolean;
	size?: MediaProxyImageSize;
}) {
	if (avatarUnset) {
		return getUserAvatarURL({id: user.id, avatar: null}, animated, size);
	}
	return getGuildMemberAvatarURL({
		guildId,
		userId: user.id,
		avatar: user.avatar,
		memberAvatar,
		animated,
		size,
	});
}

export function getGuildMemberBannerURL({
	guildId,
	userId,
	banner,
	memberBanner,
	animated = false,
	size = 1024,
}: {
	guildId: string;
	userId: string;
	banner: string | null;
	memberBanner?: string | null;
	animated?: boolean;
	size?: MediaProxyImageSize;
}) {
	if (memberBanner) {
		const {hash, animated: shouldAnimate} = parseMediaHashForRequest(memberBanner, animated);
		return buildGuildMemberWebpUrl({
			path: 'banners',
			guildId,
			userId,
			hash,
			size: snapWideAssetImageSize(size),
			animated: shouldAnimate,
		});
	}
	if (banner) {
		const {hash, animated: shouldAnimate} = parseMediaHashForRequest(banner, animated);
		return buildWebpMediaUrl({
			path: 'banners',
			id: userId,
			hash,
			size: snapWideAssetImageSize(size),
			animated: shouldAnimate,
		});
	}
	return '';
}

export function getUserAvatarURLWithProxy(
	options: AvatarOptions,
	endpoint: string,
	animated = false,
	size: MediaProxyImageSize = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
) {
	if (!endpoint) {
		return getUserAvatarURL(options, animated, size);
	}
	const {id, avatar} = options;
	if (!avatar) {
		return getDefaultAvatarURLForIndex(getDefaultAvatarIndex(id));
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(avatar, animated);
	return buildWebpMediaUrl({
		path: 'avatars',
		id,
		hash,
		size: snapIconImageSize(size),
		animated: shouldAnimate,
		endpoint,
	});
}

export function getGuildEmbedSplashURL({id, embedSplash}: {id: string; embedSplash: string | null}) {
	if (!embedSplash) {
		return '';
	}
	return buildWebpMediaUrl({
		path: 'embed-splashes',
		id,
		hash: embedSplash,
		size: snapWideAssetImageSize(GUILD_EMBED_SPLASH_CSS_WIDTH),
	});
}

export function getChannelIconURL(
	{id, icon}: IconOptions,
	size: MediaProxyImageSize = MEDIA_PROXY_ICON_SIZE_DEFAULT,
	animated = false,
) {
	if (!icon) {
		return '';
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(icon, animated);
	return buildWebpMediaUrl({
		path: 'icons',
		id,
		hash,
		size: snapIconImageSize(size),
		animated: shouldAnimate,
	});
}

export function getWebhookAvatarURL({id, avatar}: AvatarOptions, animated = false) {
	if (!avatar) {
		return getDefaultAvatarURLForIndex(getDefaultAvatarIndex(id));
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(avatar, animated);
	return buildWebpMediaUrl({
		path: 'avatars',
		id,
		hash,
		size: snapIconImageSize(MEDIA_PROXY_AVATAR_SIZE_DEFAULT),
		animated: shouldAnimate,
	});
}

const EMOJI_URL_CACHE = new Map<string, string>();
const EMOJI_URL_CACHE_LIMIT = 4096;

export function getEmojiURL({id, animated, isAnimatable}: {id: string; animated?: boolean; isAnimatable?: boolean}) {
	if (DeveloperOptions.forceRenderPlaceholders) {
		return '';
	}
	const animatedFlag = isAnimatable === false ? false : animated === true;
	const key = `${RuntimeConfig.mediaEndpoint}:${animatedFlag ? 'a' : 's'}:${id}`;
	const cached = EMOJI_URL_CACHE.get(key);
	if (cached !== undefined) return cached;
	const result = mediaUrl(`emojis/${id}.webp`, {animated: animatedFlag});
	if (EMOJI_URL_CACHE.size >= EMOJI_URL_CACHE_LIMIT) EMOJI_URL_CACHE.clear();
	EMOJI_URL_CACHE.set(key, result);
	return result;
}

type StickerSize = 160 | 320;

const STICKER_URL_CACHE = new Map<string, string>();
const STICKER_URL_CACHE_LIMIT = 4096;

export function getStickerURL({
	id,
	animated,
	isAnimatable,
	size = 320,
}: {
	id: string;
	animated?: boolean;
	isAnimatable?: boolean;
	size?: StickerSize;
}) {
	if (DeveloperOptions.forceRenderPlaceholders) {
		return '';
	}
	const animatedFlag = isAnimatable === false ? false : animated === true;
	const safeSize: StickerSize = size === 320 ? 320 : 160;
	const key = `${RuntimeConfig.mediaEndpoint}:${animatedFlag ? 'a' : 's'}:${safeSize}:${id}`;
	const cached = STICKER_URL_CACHE.get(key);
	if (cached !== undefined) return cached;
	const result = mediaUrl(setPathQueryParams(`stickers/${id}.webp`, {size: safeSize}), {animated: animatedFlag});
	if (STICKER_URL_CACHE.size >= STICKER_URL_CACHE_LIMIT) STICKER_URL_CACHE.clear();
	STICKER_URL_CACHE.set(key, result);
	return result;
}

export function fileToBase64(file: File) {
	return new Promise<string>((resolve, reject) => {
		const reader = new FileReader();
		reader.onload = () => resolve(reader.result as string);
		reader.onerror = reject;
		reader.readAsDataURL(file);
	});
}
