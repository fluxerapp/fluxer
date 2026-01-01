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

import type {IMediaService} from '~/infrastructure/IMediaService';
import {Logger} from '~/Logger';
import {buildEmbedMediaPayload} from '~/unfurler/resolvers/media/MediaMetadataHelpers';
import {parseString} from '~/utils/StringUtils';
import type {BlueskyApiClient} from './BlueskyApiClient';
import type {BlueskyTextFormatter} from './BlueskyTextFormatter';
import type {
	BlueskyAspectRatio,
	BlueskyPost,
	BlueskyPostEmbed,
	ProcessedMedia,
	ProcessedVideoResult,
} from './BlueskyTypes';

export class BlueskyEmbedProcessor {
	private static readonly MAX_ALT_TEXT_LENGTH = 4096;
	private static readonly MAX_GALLERY_IMAGES = 10;

	constructor(
		private mediaService: IMediaService,
		private apiClient: BlueskyApiClient,
		private textFormatter: BlueskyTextFormatter,
	) {}

	async processImage(
		imageUrl?: string,
		aspectRatio?: BlueskyAspectRatio,
		altText?: string,
		isNSFWAllowed: boolean = false,
	): Promise<ProcessedMedia | undefined> {
		if (!imageUrl) {
			Logger.debug('No image URL provided to process');
			return;
		}

		try {
			Logger.debug({imageUrl, aspectRatio, hasAltText: !!altText}, 'Processing image');
			const metadata = await this.mediaService.getMetadata({type: 'external', url: imageUrl, isNSFWAllowed});
			let description: string | undefined;
			if (altText) {
				description = parseString(altText, BlueskyEmbedProcessor.MAX_ALT_TEXT_LENGTH);
				Logger.debug(
					{imageUrl, altTextLength: altText.length, processedLength: description.length},
					'Added alt text as description to image',
				);
			}

			const result = buildEmbedMediaPayload(imageUrl, metadata, {
				width: aspectRatio?.width,
				height: aspectRatio?.height,
				description,
			}) as ProcessedMedia;

			Logger.debug({imageUrl, metadata: result}, 'Image processed successfully');
			return result;
		} catch (error) {
			Logger.error({error, imageUrl}, 'Failed to process image');
			return;
		}
	}

	async processVideoEmbed(
		embed: BlueskyPostEmbed['video'],
		did: string,
		isNSFWAllowed: boolean,
	): Promise<ProcessedVideoResult> {
		if (!embed || embed.$type !== 'app.bsky.embed.video#view') {
			Logger.debug({embedType: embed?.$type}, 'Not a video embed');
			return {};
		}

		Logger.debug(
			{embedType: embed.$type, hasThumbnail: !!embed.thumbnail, cid: embed.cid, aspectRatio: embed.aspectRatio},
			'Processing video embed',
		);

		try {
			const thumbnail = await this.processImage(embed.thumbnail, embed.aspectRatio, undefined, isNSFWAllowed);
			if (!thumbnail || !embed.cid) {
				Logger.debug(
					{embedType: embed.$type, hasThumbnail: !!thumbnail, hasCid: !!embed.cid},
					'Missing required video data',
				);
				return {};
			}

			const serviceEndpoint = await this.apiClient.getServiceEndpoint(did);
			const directUrl = `${serviceEndpoint}/xrpc/com.atproto.sync.getBlob?did=${did}&cid=${embed.cid}`;
			const videoMetadata = await this.mediaService.getMetadata({type: 'external', url: directUrl, isNSFWAllowed});
			const video = buildEmbedMediaPayload(directUrl, videoMetadata, {
				width: videoMetadata?.width ?? thumbnail.width,
				height: videoMetadata?.height ?? thumbnail.height,
			}) as ProcessedMedia;

			Logger.debug(
				{thumbnailProcessed: !!thumbnail, videoMetadata, aspectRatio: embed.aspectRatio, serviceEndpoint},
				'Successfully processed video embed',
			);
			return {thumbnail, video};
		} catch (error) {
			Logger.error({error, embedType: embed.$type}, 'Failed to process video embed');
			return {};
		}
	}

	async processPostEmbed(
		post: BlueskyPost,
		isNSFWAllowed: boolean,
	): Promise<{
		image?: ProcessedMedia;
		thumbnail?: ProcessedMedia;
		video?: ProcessedMedia;
		quotedText?: string;
		galleryImages?: Array<ProcessedMedia>;
	}> {
		let image: ProcessedMedia | undefined;
		let thumbnail: ProcessedMedia | undefined;
		let video: ProcessedMedia | undefined;
		let quotedText: string | undefined;

		if (!post.embed) return {image, thumbnail, video, quotedText};

		Logger.debug({embedType: post.embed.$type, hasEmbed: true, authorDid: post.author.did}, 'Processing post embed');

		const processedImages = await this.processEmbedImages(post.embed, isNSFWAllowed);
		if (processedImages.length > 0) {
			image = processedImages[0];
			if (post.embed.$type === 'app.bsky.embed.images#view') {
				const firstImage = post.embed.images?.[0];
				Logger.debug(
					{imageUrl: firstImage?.thumb, hasAltText: !!firstImage?.alt, altTextLength: firstImage?.alt?.length},
					'Processed image with alt text',
				);
			} else if (post.embed.$type === 'app.bsky.embed.recordWithMedia#view') {
				const firstMediaImage = post.embed.media?.images?.[0];
				Logger.debug(
					{
						imageUrl: firstMediaImage?.thumb,
						hasAltText: !!firstMediaImage?.alt,
						altTextLength: firstMediaImage?.alt?.length,
					},
					'Processed media image with alt text',
				);
			}
		}

		if (post.embed.$type === 'app.bsky.embed.video#view') {
			const processed = await this.processVideoEmbed(post.embed, post.author.did, isNSFWAllowed);
			thumbnail = processed.thumbnail;
			video = processed.video;
		}
		if (post.embed.$type === 'app.bsky.embed.recordWithMedia#view') {
			if (post.embed.media?.$type === 'app.bsky.embed.video#view') {
				const processed = await this.processVideoEmbed(post.embed.media, post.author.did, isNSFWAllowed);
				thumbnail = processed.thumbnail;
				video = processed.video;
			}

			if (post.embed.record?.record) {
				const quoteAuthor = post.embed.record.record.author;
				const quoteText = post.embed.record.record.value.text;
				if (quoteAuthor && quoteText) {
					const formattedAuthor = this.textFormatter.formatAuthor(quoteAuthor);
					quotedText = `>>> ${formattedAuthor}\n${this.textFormatter.embedLinksInText(quoteText, post.embed.record.record.value.facets)}`;
				}
			}
		}

		const galleryImages = processedImages.length > 1 ? processedImages.slice(1) : undefined;
		return {image, thumbnail, video, quotedText, galleryImages};
	}

	private async processEmbedImages(embed: BlueskyPostEmbed, isNSFWAllowed: boolean): Promise<Array<ProcessedMedia>> {
		const imageEntries = this.collectImageEntries(embed);
		if (imageEntries.length === 0) return [];

		const processedImages: Array<ProcessedMedia> = [];
		const seenUrls = new Set<string>();

		for (const entry of imageEntries) {
			const normalizedUrl = this.normalizeUrl(entry.url);
			if (!normalizedUrl) continue;
			if (seenUrls.has(normalizedUrl)) continue;
			seenUrls.add(normalizedUrl);

			const processedImage = await this.processImage(entry.url, entry.aspectRatio, entry.alt, isNSFWAllowed);
			if (processedImage) {
				processedImages.push(processedImage);
				if (processedImages.length >= BlueskyEmbedProcessor.MAX_GALLERY_IMAGES) break;
			}
		}

		return processedImages;
	}

	private collectImageEntries(embed: BlueskyPostEmbed): Array<{
		url: string;
		aspectRatio?: BlueskyAspectRatio;
		alt?: string;
	}> {
		const entries: Array<{url: string; aspectRatio?: BlueskyAspectRatio; alt?: string}> = [];

		const addImages = (
			images?: Array<{thumb: string; fullsize?: string; alt?: string; aspectRatio?: BlueskyAspectRatio}>,
		) => {
			if (!images) return;
			for (const image of images) {
				const resolvedUrl = image.fullsize ?? image.thumb;
				if (!resolvedUrl) continue;
				entries.push({url: resolvedUrl, aspectRatio: image.aspectRatio, alt: image.alt});
			}
		};

		addImages(embed.images);
		if (embed.media?.$type === 'app.bsky.embed.images#view') {
			addImages(embed.media.images);
		}

		return entries;
	}

	private normalizeUrl(url: string): string | null {
		try {
			return new URL(url).href.replace(/\/$/, '');
		} catch (error) {
			Logger.error({error, url}, 'Failed to normalize image URL');
			return null;
		}
	}
}
