// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getEmbedMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {
	buildAnimatedImageProxyURL,
	buildMediaProxyURL,
	mediaDevicePixelRatio,
	resolvePreferredImageFormat,
	stripMediaProxyParams,
} from '@app/features/messaging/utils/MediaProxyUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import {createCalculator, type MediaDimensionCalculator} from '@app/features/ui/utils/DimensionUtils';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import type {EmbedMedia, MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import type React from 'react';

export const logger = new Logger('Embed');
export const THUMBNAIL_SIZE = 80;
export const MAX_GALLERY_MEDIA = 10;
export const EMBED_PADDING_X = 12;
export const EMBED_LEFT_BORDER_WIDTH = 4;
export const EMBED_RIGHT_BORDER_WIDTH = 1;
export const EMBED_MEDIA_CHROME_WIDTH = EMBED_PADDING_X * 2 + EMBED_LEFT_BORDER_WIDTH + EMBED_RIGHT_BORDER_WIDTH;
export const EMBED_MEDIA_MAX_WIDTH = 432;

export interface EmbedProps {
	embed: MessageEmbed;
	message: Message;
	embedIndex?: number;
	onDelete?: (bypassConfirm?: boolean) => void;
	contextualEmbeds?: ReadonlyArray<MessageEmbed>;
	isPreview?: boolean;
}

export interface LinkComponentProps {
	url: string;
	children: React.ReactNode;
	className?: string;
}

export interface MediaDimensions {
	width: number;
	height: number;
}

export interface EmbedMediaRendererProps {
	embed: MessageEmbed;
	message: Message;
	embedIndex?: number;
	onDelete?: (bypassConfirm?: boolean) => void;
	isPreview?: boolean;
}

export const REPOST_DESCRIPTOR = msg({
	message: 'repost',
	comment:
		'Singular external-post stat label on a social post embed (e.g. Twitter/X). Lowercase to appear after the count.',
});
export const REPOSTS_DESCRIPTOR = msg({
	message: 'reposts',
	comment:
		'Plural external-post stat label on a social post embed (e.g. Twitter/X). Lowercase to appear after the count.',
});
export const QUOTE_DESCRIPTOR = msg({
	message: 'quote',
	comment: 'Singular external-post stat label on a social post embed. Lowercase to appear after the count.',
});
export const QUOTES_DESCRIPTOR = msg({
	message: 'quotes',
	comment: 'Plural external-post stat label on a social post embed. Lowercase to appear after the count.',
});
export const LIKE_DESCRIPTOR = msg({
	message: 'like',
	comment: 'Singular external-post stat label on a social post embed. Lowercase to appear after the count.',
});
export const LIKES_DESCRIPTOR = msg({
	message: 'likes',
	comment: 'Plural external-post stat label on a social post embed. Lowercase to appear after the count.',
});
export const SAVE_DESCRIPTOR = msg({
	message: 'save',
	comment: 'Singular external-post stat label on a social post embed. Lowercase to appear after the count.',
});
export const SAVES_DESCRIPTOR = msg({
	message: 'saves',
	comment: 'Plural external-post stat label on a social post embed. Lowercase to appear after the count.',
});
const URL_CACHE_CAPACITY = 4096;
const normalizedUrlCache = new Map<string, string | null>();
const hostnameCache = new Map<string, string | null>();

let mediaCalculatorCache:
	| {
			maxWidth: number;
			maxHeight: number;
			calculator: MediaDimensionCalculator;
	  }
	| undefined;

const trimOldestCacheEntry = <T>(cache: Map<string, T>) => {
	if (cache.size <= URL_CACHE_CAPACITY) return;
	const firstKey = cache.keys().next().value;
	if (firstKey !== undefined) {
		cache.delete(firstKey);
	}
};
export const isValidMedia = (media?: Partial<EmbedMedia>): media is Required<EmbedMedia> => {
	return !!(
		media &&
		typeof media.proxy_url === 'string' &&
		typeof media.url === 'string' &&
		typeof media.width === 'number' &&
		typeof media.height === 'number'
	);
};
export const calculateMediaDimensions = (media: Required<EmbedMedia>): MediaDimensions => {
	const embedDimensions = getEmbedMediaDimensions();
	if (
		!mediaCalculatorCache ||
		mediaCalculatorCache.maxWidth !== embedDimensions.maxWidth ||
		mediaCalculatorCache.maxHeight !== embedDimensions.maxHeight
	) {
		mediaCalculatorCache = {
			maxWidth: embedDimensions.maxWidth,
			maxHeight: embedDimensions.maxHeight,
			calculator: createCalculator({maxWidth: embedDimensions.maxWidth, maxHeight: embedDimensions.maxHeight}),
		};
	}
	const mediaCalculator = mediaCalculatorCache.calculator;
	const {dimensions} = mediaCalculator.calculate({width: media.width, height: media.height});
	return dimensions;
};
const toEmbedMediaPixels = (layoutWidth: number, layoutHeight: number): MediaDimensions => {
	const ratio = mediaDevicePixelRatio();
	return {width: Math.round(layoutWidth * ratio), height: Math.round(layoutHeight * ratio)};
};
export const getOptimizedMediaURL = (proxyURL: string, width: number, height: number, contentType?: string): string => {
	const target = toEmbedMediaPixels(width, height);
	return buildMediaProxyURL(proxyURL, {
		format: resolvePreferredImageFormat(contentType),
		width: target.width,
		height: target.height,
	});
};
export const getOptimizedAnimatedMediaURL = (proxyURL: string, width: number, height: number): string => {
	if (!proxyURL) return proxyURL;
	const target = toEmbedMediaPixels(width, height);
	return buildAnimatedImageProxyURL(stripMediaProxyParams(proxyURL), target.width, target.height);
};
export const isAnimatedEmbedMedia = (media: Pick<EmbedMedia, 'content_type' | 'flags'>): boolean => {
	if (media.content_type === 'image/gif') return true;
	return ((media.flags ?? 0) & MessageAttachmentFlags.IS_ANIMATED) === MessageAttachmentFlags.IS_ANIMATED;
};
export const resolveEmbedImageSource = (
	media: Pick<EmbedMedia, 'content_type' | 'flags'> & {proxy_url: string},
	width: number,
	height: number,
): {src: string; animated: boolean} => {
	const animated = isAnimatedEmbedMedia(media);
	return {
		src: animated
			? getOptimizedAnimatedMediaURL(media.proxy_url, width, height)
			: getOptimizedMediaURL(media.proxy_url, width, height, media.content_type),
		animated,
	};
};
export const mediaIdentityKey = (media?: EmbedMedia): string => {
	if (!media) return '';
	return [
		media.url ?? '',
		media.proxy_url ?? '',
		media.width ?? '',
		media.height ?? '',
		media.content_hash ?? '',
		media.content_type ?? '',
		media.flags ?? '',
		media.duration ?? '',
		media.placeholder ?? '',
		media.nsfw ? '1' : '0',
		media.description ?? '',
	].join('|');
};
export const embedMediaSignature = (embed: MessageEmbed): string => {
	return [
		embed.url ?? '',
		embed.provider?.url ?? '',
		embed.type ?? '',
		mediaIdentityKey(embed.video),
		mediaIdentityKey(embed.image),
		mediaIdentityKey(embed.thumbnail),
	].join('::');
};
export const mediaPropsEqual = <
	P extends {embed: MessageEmbed; message: Message; embedIndex?: number; onDelete?: unknown; isPreview?: boolean},
>(
	prev: P,
	next: P,
): boolean => {
	if (prev.message.id !== next.message.id) return false;
	if (prev.message.channelId !== next.message.channelId) return false;
	if (prev.embedIndex !== next.embedIndex) return false;
	if (prev.onDelete !== next.onDelete) return false;
	if (prev.isPreview !== next.isPreview) return false;
	return embedMediaSignature(prev.embed) === embedMediaSignature(next.embed);
};
export const isMediaMatureContent = (media?: EmbedMedia): boolean => {
	if (!media) return false;
	return Boolean(media.nsfw || ((media.flags ?? 0) & MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA) !== 0);
};
export const normalizeUrl = (url?: string): string | null => {
	if (!url) return null;
	if (normalizedUrlCache.has(url)) {
		return normalizedUrlCache.get(url) ?? null;
	}
	let normalizedUrl: string | null;
	try {
		normalizedUrl = new URL(url).href.replace(/\/$/, '');
	} catch {
		normalizedUrl = null;
	}
	normalizedUrlCache.set(url, normalizedUrl);
	trimOldestCacheEntry(normalizedUrlCache);
	return normalizedUrl;
};
export const getUrlHostname = (url?: string): string | null => {
	if (!url) return null;
	if (hostnameCache.has(url)) {
		return hostnameCache.get(url) ?? null;
	}
	let hostname: string | null;
	try {
		hostname = new URL(url).hostname;
	} catch {
		hostname = null;
	}
	hostnameCache.set(url, hostname);
	trimOldestCacheEntry(hostnameCache);
	return hostname;
};
const deriveFilenameFromUrl = (url: string): string => {
	try {
		const parsed = new URL(url);
		const filename = parsed.pathname.split('/').pop()?.trim();
		return filename && filename.length > 0 ? filename : 'embed-media';
	} catch {
		return 'embed-media';
	}
};
export const buildGalleryAttachments = (
	images: Array<Required<EmbedMedia>>,
	embed: MessageEmbed,
	embedIndex?: number,
): Array<MessageAttachment> => {
	return images.map((media, index) => ({
		id: `${embed.id ?? embedIndex ?? 'embed'}-gallery-${index}`,
		filename: deriveFilenameFromUrl(media.url),
		title: embed.title ?? undefined,
		description: media.description ?? undefined,
		content_type: media.content_type ?? undefined,
		size: 0,
		url: media.url,
		proxy_url: media.proxy_url ?? media.url,
		width: media.width ?? undefined,
		height: media.height ?? undefined,
		placeholder: media.placeholder ?? undefined,
		placeholder_version: undefined,
		flags: media.flags,
		duration: media.duration,
		waveform: undefined,
		content_hash: media.content_hash ?? undefined,
		nsfw: media.nsfw,
	}));
};

interface EmbedListRenderMetadata {
	normalizedUrls: Array<string | null>;
	firstIndexByNormalizedUrl: Map<string, number>;
	galleryImagesByFirstIndex: Map<number, Array<Required<EmbedMedia>>>;
}

const embedListRenderMetadataCache = new WeakMap<ReadonlyArray<MessageEmbed>, EmbedListRenderMetadata>();
const getEmbedListRenderMetadata = (embedList: ReadonlyArray<MessageEmbed>): EmbedListRenderMetadata => {
	const cached = embedListRenderMetadataCache.get(embedList);
	if (cached) return cached;
	const normalizedUrls = new Array<string | null>(embedList.length);
	const firstIndexByNormalizedUrl = new Map<string, number>();
	for (let index = 0; index < embedList.length; index++) {
		const normalizedUrl = normalizeUrl(embedList[index]?.url);
		normalizedUrls[index] = normalizedUrl;
		if (normalizedUrl && !firstIndexByNormalizedUrl.has(normalizedUrl)) {
			firstIndexByNormalizedUrl.set(normalizedUrl, index);
		}
	}
	const galleryImagesByFirstIndex = new Map<number, Array<Required<EmbedMedia>>>();
	const seenMediaKeysByFirstIndex = new Map<number, Set<string>>();
	const tryAddGalleryMedia = (firstIndex: number, media?: Required<EmbedMedia>) => {
		if (!media) return;
		let images = galleryImagesByFirstIndex.get(firstIndex);
		if (!images) {
			images = [];
			galleryImagesByFirstIndex.set(firstIndex, images);
		}
		if (images.length >= MAX_GALLERY_MEDIA) return;
		let seenMediaKeys = seenMediaKeysByFirstIndex.get(firstIndex);
		if (!seenMediaKeys) {
			seenMediaKeys = new Set<string>();
			seenMediaKeysByFirstIndex.set(firstIndex, seenMediaKeys);
		}
		const mediaKey = media.content_hash || media.url;
		if (seenMediaKeys.has(mediaKey)) return;
		seenMediaKeys.add(mediaKey);
		images.push(media);
	};
	for (let index = 0; index < embedList.length; index++) {
		const normalizedUrl = normalizedUrls[index];
		if (!normalizedUrl) continue;
		const firstIndex = firstIndexByNormalizedUrl.get(normalizedUrl);
		if (firstIndex === undefined) continue;
		const candidate = embedList[index];
		const candidateMedia = isValidMedia(candidate?.image)
			? candidate.image
			: isValidMedia(candidate?.thumbnail)
				? candidate.thumbnail
				: undefined;
		tryAddGalleryMedia(firstIndex, candidateMedia);
	}
	const metadata = {normalizedUrls, firstIndexByNormalizedUrl, galleryImagesByFirstIndex};
	embedListRenderMetadataCache.set(embedList, metadata);
	return metadata;
};
export const isDuplicateEmbedAtIndex = (
	embedIndex: number | undefined,
	embedList: ReadonlyArray<MessageEmbed>,
): boolean => {
	if (embedIndex === undefined) return false;
	const metadata = getEmbedListRenderMetadata(embedList);
	const normalizedUrl = metadata.normalizedUrls[embedIndex];
	if (!normalizedUrl) return false;
	const firstIndex = metadata.firstIndexByNormalizedUrl.get(normalizedUrl);
	return firstIndex !== undefined && firstIndex < embedIndex;
};
export const collectGalleryImages = ({
	embedIndex,
	embedList,
}: {
	embed: MessageEmbed;
	embedIndex?: number;
	embedList: ReadonlyArray<MessageEmbed>;
}): Array<Required<EmbedMedia>> => {
	if (embedIndex === undefined) return [];
	return getEmbedListRenderMetadata(embedList).galleryImagesByFirstIndex.get(embedIndex) ?? [];
};
export const getBorderColor = (color: number | undefined) => {
	if (color === undefined || color === 0) {
		return 'var(--brand-primary)';
	}
	return ColorUtils.int2rgb(color);
};
