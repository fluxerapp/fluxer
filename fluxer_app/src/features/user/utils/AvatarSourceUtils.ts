// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {buildMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
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

const getDefaultAvatar = (index: number): string => cdnUrl(`avatars/${index}.png`);

export function getDefaultAvatarPrimaryColor(id: string) {
	return getSharedDefaultAvatarPrimaryColor(id);
}

export function getDefaultAvatarURL(id: string) {
	return getDefaultAvatar(getDefaultAvatarIndex(id));
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
	const {animated: isAnimated} = parseAvatarHash(value);
	return {
		hash: value,
		animated: isAnimated ? animated : undefined,
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

export function getUserAvatarURL(
	{id, avatar}: AvatarOptions,
	animated = false,
	size: MediaProxyImageSize = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
) {
	if (!avatar) {
		return getDefaultAvatar(getDefaultAvatarIndex(id));
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(avatar, animated);
	return buildWebpMediaUrl({
		path: 'avatars',
		id,
		hash,
		size,
		animated: shouldAnimate,
	});
}

export function getUserNotificationAvatarURL(
	{id, avatar}: AvatarOptions,
	size: MediaProxyImageSize = MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
) {
	if (!avatar) {
		return getDefaultAvatar(getDefaultAvatarIndex(id));
	}
	const {hash, animated} = parseMediaHashForRequest(avatar, false);
	return buildPngMediaUrl({
		path: 'avatars',
		id,
		hash,
		size,
		animated,
	});
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
		size,
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
		size: MEDIA_PROXY_ICON_SIZE_DEFAULT,
		animated: shouldAnimate,
	});
}

export function getGuildIconURLWithProxy({id, icon}: IconOptions, endpoint: string, animated = false) {
	if (!endpoint || !icon) {
		return '';
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(icon, animated);
	return buildWebpMediaUrl({
		path: 'icons',
		id,
		hash,
		size: MEDIA_PROXY_ICON_SIZE_DEFAULT,
		animated: shouldAnimate,
		endpoint,
	});
}

export function getGuildSplashURL(
	{
		id,
		splash,
	}: {
		id: string;
		splash: string | null;
	},
	size: MediaProxyImageSize = 1024,
) {
	if (!splash) {
		return '';
	}
	return buildWebpMediaUrl({
		path: 'splashes',
		id,
		hash: splash,
		size,
	});
}

export function getGuildDiscoverySplashURL({id, splash}: {id: string; splash: string | null}) {
	if (!splash) {
		return '';
	}
	return buildWebpMediaUrl({
		path: 'discovery-splashes',
		id,
		hash: splash,
		size: 1024,
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
		size: 1024,
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
			size,
			animated: shouldAnimate,
		});
	}
	if (avatar) {
		const {hash, animated: shouldAnimate} = parseMediaHashForRequest(avatar, animated);
		return buildWebpMediaUrl({
			path: 'avatars',
			id: userId,
			hash,
			size,
			animated: shouldAnimate,
		});
	}
	return getDefaultAvatar(getDefaultAvatarIndex(userId));
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
			size,
			animated: shouldAnimate,
		});
	}
	if (banner) {
		const {hash, animated: shouldAnimate} = parseMediaHashForRequest(banner, animated);
		return buildWebpMediaUrl({
			path: 'banners',
			id: userId,
			hash,
			size,
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
	const {id, avatar} = options;
	if (!endpoint || !avatar) {
		return getDefaultAvatar(getDefaultAvatarIndex(id));
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(avatar, animated);
	return buildWebpMediaUrl({
		path: 'avatars',
		id,
		hash,
		size,
		animated: shouldAnimate,
		endpoint,
	});
}

export function getGuildEmbedSplashURL(
	{
		id,
		embedSplash,
	}: {
		id: string;
		embedSplash: string | null;
	},
	size: MediaProxyImageSize = 1024,
) {
	if (!embedSplash) {
		return '';
	}
	return buildWebpMediaUrl({
		path: 'embed-splashes',
		id,
		hash: embedSplash,
		size,
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
		size,
		animated: shouldAnimate,
	});
}

export function getChannelIconURLWithProxy(
	{id, icon}: IconOptions,
	endpoint: string,
	size: MediaProxyImageSize = MEDIA_PROXY_ICON_SIZE_DEFAULT,
	animated = false,
) {
	if (!endpoint || !icon) {
		return '';
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(icon, animated);
	return buildWebpMediaUrl({
		path: 'icons',
		id,
		hash,
		size,
		animated: shouldAnimate,
		endpoint,
	});
}

export function getWebhookAvatarURL({id, avatar}: AvatarOptions, animated = false) {
	if (!avatar) {
		return getDefaultAvatar(getDefaultAvatarIndex(id));
	}
	const {hash, animated: shouldAnimate} = parseMediaHashForRequest(avatar, animated);
	return buildWebpMediaUrl({
		path: 'avatars',
		id,
		hash,
		size: MEDIA_PROXY_AVATAR_SIZE_DEFAULT,
		animated: shouldAnimate,
	});
}

const EMOJI_URL_CACHE = new Map<string, string>();
const EMOJI_URL_CACHE_LIMIT = 4096;

export function getEmojiURL({id, animated}: {id: string; animated?: boolean}) {
	if (DeveloperOptions.forceRenderPlaceholders) {
		return '';
	}
	const animatedFlag = Boolean(animated);
	const key = `${RuntimeConfig.mediaEndpoint}:${animatedFlag ? 'a' : 's'}:${id}`;
	const cached = EMOJI_URL_CACHE.get(key);
	if (cached !== undefined) return cached;
	const result = mediaUrl(setPathQueryParams(`emojis/${id}.webp`, {v: 5}), {animated: animatedFlag});
	if (EMOJI_URL_CACHE.size >= EMOJI_URL_CACHE_LIMIT) EMOJI_URL_CACHE.clear();
	EMOJI_URL_CACHE.set(key, result);
	return result;
}

type StickerSize = 160 | 320;

export function getStickerURL({id, animated, size = 320}: {id: string; animated?: boolean; size?: StickerSize}) {
	if (DeveloperOptions.forceRenderPlaceholders) {
		return '';
	}
	const safeSize: StickerSize = size === 320 ? 320 : 160;
	return mediaUrl(setPathQueryParams(`stickers/${id}.webp`, {size: safeSize}), {animated: Boolean(animated)});
}

export function fileToBase64(file: File) {
	return new Promise<string>((resolve, reject) => {
		const reader = new FileReader();
		reader.onload = () => resolve(reader.result as string);
		reader.onerror = reject;
		reader.readAsDataURL(file);
	});
}
