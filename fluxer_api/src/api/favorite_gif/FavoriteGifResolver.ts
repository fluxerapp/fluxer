// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@fluxer/logger/src/Logger';
import type {ResolvedGifEntrySchema} from '@fluxer/schema/src/domains/gif/FavoriteGifSchemas';
import type {GifMediaFormat, GifResponse} from '@fluxer/schema/src/domains/gif/GifSchemas';
import type {EmbedMediaResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {tryExtractGifProviderSlug} from '../gif/GifProviderUtils';
import type {GifService} from '../gif/GifService';
import type {IGifProvider} from '../gif/IGifProvider';
import type {IMediaService, MediaProxyMetadataResponse} from '../infrastructure/IMediaService';
import type {IUnfurlerService} from '../infrastructure/IUnfurlerService';

const PREVIEW_FORMAT_PRIORITY = ['webm', 'mp4', 'tinywebm', 'tinymp4', 'webp', 'gif', 'tinygif', 'nanogif'] as const;
const FORMAT_CONTENT_TYPES: Record<string, string> = {
	webm: 'video/webm',
	tinywebm: 'video/webm',
	mp4: 'video/mp4',
	tinymp4: 'video/mp4',
	webp: 'image/webp',
	gif: 'image/gif',
	tinygif: 'image/gif',
	nanogif: 'image/gif',
};
const logger = new Logger('FavoriteGifResolver');

function pickFavoriteGifPreviewFormat(
	media: Record<string, GifMediaFormat> | null | undefined,
): {key: string; format: GifMediaFormat} | null {
	if (!media) return null;
	for (const key of PREVIEW_FORMAT_PRIORITY) {
		const format = media[key];
		if (isUsableGifMediaFormat(format)) return {key, format};
	}
	for (const [key, format] of Object.entries(media)) {
		if (isUsableGifMediaFormat(format)) return {key, format};
	}
	return null;
}

function favoriteGifEntryFromGifResponse(gif: GifResponse, fallbackUrl: string): ResolvedGifEntrySchema {
	const media = gif.media ?? {};
	const best = pickFavoriteGifPreviewFormat(media);
	return {
		url: gif.url || fallbackUrl,
		proxy_url: best?.format.proxy_src ?? gif.proxy_src,
		width: best?.format.width ?? gif.width,
		height: best?.format.height ?? gif.height,
		media,
		content_type: best ? inferFormatContentType(best.key) : '',
		placeholder: gif.placeholder ?? null,
	};
}

export async function resolveFavoriteGifEntry({
	url,
	locale,
	country,
	gifService,
	mediaService,
	unfurlerService,
}: {
	url: string;
	locale: string;
	country: string;
	gifService: GifService;
	mediaService: IMediaService;
	unfurlerService: IUnfurlerService;
}): Promise<ResolvedGifEntrySchema> {
	const providerGif = await resolveProviderGifUrl({url, locale, country, gifService}).catch((error) => {
		logger.warn({error, url}, 'Failed to resolve favorite GIF provider URL');
		return null;
	});
	if (providerGif) {
		return favoriteGifEntryFromGifResponse(providerGif, url);
	}
	const metadata = await mediaService.getMetadata({type: 'external', url, nsfw: 'allow'}).catch(() => null);
	if (!isRenderableMediaType(metadata?.content_type)) {
		const unfurled = await resolveUnfurledFavoriteGifEntry({url, unfurlerService, mediaService});
		if (unfurled) return unfurled;
	}
	return favoriteGifEntryFromExternalMedia({url, mediaService, metadata});
}

async function resolveProviderGifUrl({
	url,
	locale,
	country,
	gifService,
}: {
	url: string;
	locale: string;
	country: string;
	gifService: GifService;
}): Promise<GifResponse | null> {
	const provider = gifService.getProvider();
	if (!(await tryExtractGifProviderSlug(provider, url))) return null;
	return resolveProviderUrl(provider, {url, locale, country});
}

async function resolveProviderUrl(
	provider: IGifProvider,
	params: {url: string; locale: string; country: string},
): Promise<GifResponse | null> {
	try {
		return await provider.resolveByUrl(params);
	} catch (error) {
		logger.warn({error, provider: provider.meta.name, url: params.url}, 'Failed to resolve GIF provider URL');
		return null;
	}
}

function favoriteGifEntryFromExternalMedia({
	url,
	mediaService,
	metadata,
}: {
	url: string;
	mediaService: IMediaService;
	metadata: MediaProxyMetadataResponse | null;
}): ResolvedGifEntrySchema {
	const proxyUrl = mediaService.getExternalMediaProxyURL(url);
	const media = directMediaFormatFromMetadata({url, proxyUrl, metadata});
	return {
		url,
		proxy_url: proxyUrl,
		width: metadata?.width ?? 0,
		height: metadata?.height ?? 0,
		media,
		content_type: metadata?.content_type ?? '',
		placeholder: metadata?.placeholder ?? null,
	};
}

async function resolveUnfurledFavoriteGifEntry({
	url,
	unfurlerService,
	mediaService,
}: {
	url: string;
	unfurlerService: IUnfurlerService;
	mediaService: IMediaService;
}): Promise<ResolvedGifEntrySchema | null> {
	const embeds = await unfurlerService.unfurl(url, 'allow');
	for (const embed of embeds) {
		const media = [embed.video, embed.image, embed.thumbnail].find((candidate) =>
			isRenderableMediaType(candidate?.content_type),
		);
		if (!media?.url) continue;
		return favoriteGifEntryFromEmbedMedia({url, mediaService, media});
	}
	return null;
}

function favoriteGifEntryFromEmbedMedia({
	url,
	mediaService,
	media,
}: {
	url: string;
	mediaService: IMediaService;
	media: EmbedMediaResponse;
}): ResolvedGifEntrySchema {
	const proxyUrl = media.proxy_url ?? mediaService.getExternalMediaProxyURL(media.url);
	const width = media.width ?? 0;
	const height = media.height ?? 0;
	const contentType = media.content_type ?? '';
	return {
		url,
		proxy_url: proxyUrl,
		width,
		height,
		media: directMediaFormatFromDetails({url: media.url, proxyUrl, contentType, width, height}),
		content_type: contentType,
		placeholder: media.placeholder ?? null,
	};
}

function directMediaFormatFromMetadata({
	url,
	proxyUrl,
	metadata,
}: {
	url: string;
	proxyUrl: string;
	metadata: MediaProxyMetadataResponse | null;
}): Record<string, GifMediaFormat> {
	const width = metadata?.width ?? 0;
	const height = metadata?.height ?? 0;
	if (!metadata?.content_type || width <= 0 || height <= 0) return {};
	const key = formatKeyFromContentType(metadata.content_type);
	if (!key) return {};
	return directMediaFormatFromDetails({url, proxyUrl, contentType: metadata.content_type, width, height});
}

function directMediaFormatFromDetails({
	url,
	proxyUrl,
	contentType,
	width,
	height,
}: {
	url: string;
	proxyUrl: string;
	contentType: string;
	width: number;
	height: number;
}): Record<string, GifMediaFormat> {
	const key = formatKeyFromContentType(contentType);
	if (!key || width <= 0 || height <= 0) return {};
	return {
		[key]: {
			src: url,
			proxy_src: proxyUrl,
			width,
			height,
		},
	};
}

function isUsableGifMediaFormat(format: GifMediaFormat | undefined): format is GifMediaFormat {
	return Boolean(format?.src && format.proxy_src && format.width > 0 && format.height > 0);
}

function inferFormatContentType(formatKey: string): string {
	return FORMAT_CONTENT_TYPES[formatKey] ?? '';
}

function isRenderableMediaType(contentType: string | null | undefined): boolean {
	if (!contentType) return false;
	return contentType.startsWith('image/') || contentType.startsWith('video/');
}

function formatKeyFromContentType(contentType: string): string | null {
	const normalized = contentType.toLowerCase().split(';', 1)[0]?.trim();
	switch (normalized) {
		case 'video/webm':
			return 'webm';
		case 'video/mp4':
			return 'mp4';
		case 'image/webp':
			return 'webp';
		case 'image/gif':
			return 'gif';
		default:
			return null;
	}
}
