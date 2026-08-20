// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MessageEmbedTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageEmbed} from '@fluxer/schema/src/domains/message/EmbedSchemas';

type RichEmbedContentFields = Pick<
	MessageEmbed,
	'title' | 'description' | 'author' | 'footer' | 'fields' | 'provider' | 'type'
>;

export function hasRichEmbedContent(embed: RichEmbedContentFields): boolean {
	return Boolean(
		embed.title != null ||
			embed.description ||
			embed.author ||
			embed.footer ||
			embed.fields?.length ||
			(embed.provider && embed.type !== MessageEmbedTypes.GIFV),
	);
}

type MediaOnlyEmbedFields = RichEmbedContentFields & Pick<MessageEmbed, 'image' | 'thumbnail' | 'video' | 'audio'>;

export function isMediaOnlyEmbed(embed: MediaOnlyEmbedFields): boolean {
	if (hasRichEmbedContent(embed)) return false;
	return Boolean(embed.image || embed.thumbnail || embed.video || embed.audio);
}

export const BLUESKY_EMBED_MIN_CONTENT_WIDTH = 320;
export const BLUESKY_EMBED_MEDIA_FALLBACK_OUTER_WIDTH = 432;
export const EMBED_TEXT_OUTER_WIDTH = 516;
const normalizePositiveWidth = (width: number): number => {
	if (!Number.isFinite(width) || width <= 0) {
		return 0;
	}
	return Math.round(width);
};

export function calculateBlueskyMediaContainerWidth(mediaWidth?: number): number | undefined {
	if (mediaWidth === undefined) return undefined;
	const normalizedWidth = normalizePositiveWidth(mediaWidth);
	return Math.max(normalizedWidth, BLUESKY_EMBED_MIN_CONTENT_WIDTH);
}

export function calculateBlueskyOuterMaxWidth({
	mediaWidth,
	hasMedia,
	chromeWidth,
}: {
	mediaWidth?: number;
	hasMedia: boolean;
	chromeWidth: number;
}): number {
	const mediaContainerWidth = calculateBlueskyMediaContainerWidth(mediaWidth);
	if (mediaContainerWidth !== undefined) {
		return mediaContainerWidth + normalizePositiveWidth(chromeWidth);
	}
	return hasMedia ? BLUESKY_EMBED_MEDIA_FALLBACK_OUTER_WIDTH : EMBED_TEXT_OUTER_WIDTH;
}

export function formatResponsiveEmbedWidth(width: number): string {
	return `min(100%, ${remFromPx(normalizePositiveWidth(width))})`;
}
