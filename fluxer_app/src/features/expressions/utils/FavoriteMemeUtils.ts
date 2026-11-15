// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FavoriteMeme} from '@app/features/expressions/models/FavoriteMeme';
import {AUDIO_DESCRIPTOR, MEDIA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {EmbedMedia, MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const GIF_DESCRIPTOR = msg({
	message: 'GIF',
	comment: 'Type label for GIF media.',
});
const IMAGE_DESCRIPTOR = msg({
	message: 'Image',
	comment: 'Type label for image media.',
});
const VIDEO_DESCRIPTOR = msg({
	message: 'Video',
	comment: 'Type label for video media.',
});

function extractKlipyName(url: string): string | null {
	try {
		const klipyRegex = /klipy\.com\/clips\/([a-z0-9-]+)-(?:gif|gifv?)-\d+/i;
		const match = url.match(klipyRegex);
		if (match?.[1]) {
			return match[1].split('-').join(' ');
		}
	} catch {}
	return null;
}

function extractFilenameFromUrl(url: string): string | null {
	try {
		const urlObj = new URL(url);
		const pathname = urlObj.pathname;
		const filename = pathname.split('/').pop();
		if (!filename) return null;
		const nameWithoutExt = filename.replace(/\.[^.]+$/, '');
		const cleaned = nameWithoutExt.replace(/[-_]/g, ' ').trim();
		return cleaned || null;
	} catch {
		return null;
	}
}

export function deriveDefaultNameFromAttachment(i18n: I18n, attachment: MessageAttachment): string {
	if (attachment.title?.trim()) {
		return attachment.title.trim();
	}
	if (attachment.filename) {
		const nameWithoutExt = attachment.filename.replace(/\.[^.]+$/, '');
		const cleaned = nameWithoutExt.replace(/[-_]/g, ' ').trim();
		if (cleaned) return cleaned;
	}
	if (attachment.url) {
		const urlName = extractFilenameFromUrl(attachment.url);
		if (urlName) return urlName;
	}
	if (attachment.content_type) {
		if (attachment.content_type.startsWith('image/gif')) return i18n._(GIF_DESCRIPTOR);
		if (attachment.content_type.startsWith('image/')) return i18n._(IMAGE_DESCRIPTOR);
		if (attachment.content_type.startsWith('video/')) return i18n._(VIDEO_DESCRIPTOR);
		if (attachment.content_type.startsWith('audio/')) return i18n._(AUDIO_DESCRIPTOR);
	}
	return i18n._(MEDIA_DESCRIPTOR);
}

export function deriveDefaultNameFromEmbedMedia(i18n: I18n, embedMedia: EmbedMedia, embed?: MessageEmbed): string {
	if (embed?.title?.trim()) {
		return embed.title.trim();
	}
	if (embedMedia.description?.trim()) {
		return embedMedia.description.trim();
	}
	if (embedMedia.url) {
		const klipyName = extractKlipyName(embedMedia.url);
		if (klipyName) return klipyName;
		const urlName = extractFilenameFromUrl(embedMedia.url);
		if (urlName) return urlName;
	}
	if (embedMedia.content_type) {
		if (embedMedia.content_type.startsWith('image/gif')) return i18n._(GIF_DESCRIPTOR);
		if (embedMedia.content_type.startsWith('image/')) return i18n._(IMAGE_DESCRIPTOR);
		if (embedMedia.content_type.startsWith('video/')) return i18n._(VIDEO_DESCRIPTOR);
		if (embedMedia.content_type.startsWith('audio/')) return i18n._(AUDIO_DESCRIPTOR);
	}
	return i18n._(MEDIA_DESCRIPTOR);
}

function isFavoritedByContentHash(memes: ReadonlyArray<FavoriteMeme>, contentHash: string | null | undefined): boolean {
	if (!contentHash) return false;
	return memes.some((meme) => meme.contentHash === contentHash);
}

function isFavoritedByGifSlug(
	memes: ReadonlyArray<FavoriteMeme>,
	gifProvider: string | null | undefined,
	gifSlug: string | null | undefined,
): boolean {
	if (!gifProvider || !gifSlug) return false;
	return memes.some((meme) => meme.gifProvider === gifProvider && meme.gifSlug === gifSlug);
}

export function isFavorited(
	memes: ReadonlyArray<FavoriteMeme>,
	params: {
		contentHash?: string | null;
		gifProvider?: string | null;
		gifSlug?: string | null;
	},
): boolean {
	if (params.gifSlug && params.gifProvider) {
		return isFavoritedByGifSlug(memes, params.gifProvider, params.gifSlug);
	}
	if (params.contentHash) {
		return isFavoritedByContentHash(memes, params.contentHash);
	}
	return false;
}

export function findFavoritedMeme(
	memes: ReadonlyArray<FavoriteMeme>,
	params: {
		contentHash?: string | null;
		gifProvider?: string | null;
		gifSlug?: string | null;
	},
): FavoriteMeme | null {
	if (params.gifSlug && params.gifProvider) {
		return memes.find((meme) => meme.gifProvider === params.gifProvider && meme.gifSlug === params.gifSlug) ?? null;
	}
	if (params.contentHash) {
		return memes.find((meme) => meme.contentHash === params.contentHash) ?? null;
	}
	return null;
}
