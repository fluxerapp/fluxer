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

import type {MessageEmbedResponse} from '~/channel/EmbedTypes';
import {Logger} from '~/Logger';
import {BaseResolver} from '~/unfurler/resolvers/BaseResolver';
import {buildEmbedMediaPayload} from '~/unfurler/resolvers/media/MediaMetadataHelpers';
import * as FetchUtils from '~/utils/FetchUtils';
import {parseString} from '~/utils/StringUtils';

interface WikiSummaryResponse {
	type: string;
	title: string;
	extract: string;
	thumbnail?: {
		source: string;
		width: number;
		height: number;
	};
	originalimage?: {
		source: string;
		width: number;
		height: number;
	};
	description?: string;
	pageid: number;
}

type ProcessedThumbnail = NonNullable<MessageEmbedResponse['image']>;

export class WikipediaResolver extends BaseResolver {
	private readonly SUPPORTED_DOMAINS = [
		'wikipedia.org',
		'www.wikipedia.org',
		...['en', 'de', 'fr', 'es', 'it', 'ja', 'ru', 'zh'].map((lang) => `${lang}.wikipedia.org`),
	];

	match(url: URL, mimeType: string, _content: Uint8Array): boolean {
		return (
			this.SUPPORTED_DOMAINS.includes(url.hostname) &&
			url.pathname.startsWith('/wiki/') &&
			mimeType.startsWith('text/html')
		);
	}

	private getLanguageFromURL(url: URL): string {
		const subdomain = url.hostname.split('.')[0];
		return this.SUPPORTED_DOMAINS.includes(`${subdomain}.wikipedia.org`) ? subdomain : 'en';
	}

	private async fetchArticleSummary(title: string, baseUrl: string): Promise<WikiSummaryResponse | null> {
		const apiUrl = `${baseUrl}/api/rest_v1/page/summary/${encodeURIComponent(title)}`;
		try {
			const response = await FetchUtils.sendRequest({
				url: apiUrl,
			});
			if (response.status !== 200) {
				Logger.debug({title, status: response.status}, 'Failed to fetch Wikipedia article summary');
				return null;
			}
			const responseText = await FetchUtils.streamToString(response.stream);
			return JSON.parse(responseText) as WikiSummaryResponse;
		} catch (error) {
			Logger.error({error, title}, 'Failed to fetch or parse Wikipedia response');
			return null;
		}
	}

	private async processThumbnail(
		thumbnailData: WikiSummaryResponse['thumbnail'],
		isNSFWAllowed: boolean,
	): Promise<ProcessedThumbnail | null> {
		if (!thumbnailData) return null;
		const thumbnailMetadata = await this.mediaService.getMetadata({
			type: 'external',
			url: thumbnailData.source,
			isNSFWAllowed,
		});
		return buildEmbedMediaPayload(thumbnailData.source, thumbnailMetadata, {
			width: thumbnailData.width,
			height: thumbnailData.height,
		}) as ProcessedThumbnail;
	}

	async resolve(url: URL, _content: Uint8Array, isNSFWAllowed: boolean = false): Promise<Array<MessageEmbedResponse>> {
		try {
			const title = decodeURIComponent(url.pathname.split('/wiki/')[1]);
			if (!title) return [];
			const language = this.getLanguageFromURL(url);
			const baseUrl = `https://${language}.wikipedia.org`;
			const article = await this.fetchArticleSummary(title, baseUrl);
			if (!article) return [];
			const thumbnail = await this.processThumbnail(article.thumbnail, isNSFWAllowed);
			const originalImage = await this.processThumbnail(article.originalimage, isNSFWAllowed);
			const uniqueImages = this.deduplicateThumbnails([thumbnail, originalImage]);
			const primaryThumbnail = uniqueImages[0];

			const embed: MessageEmbedResponse = {
				type: 'article',
				url: url.href,
				title: parseString(article.title, 256),
				description: parseString(article.extract, 350),
				thumbnail: primaryThumbnail ?? undefined,
			};

			const extraImageEmbeds = uniqueImages.slice(1).map((image) => ({
				type: 'rich' as const,
				url: url.href,
				image,
			}));

			return [embed, ...extraImageEmbeds];
		} catch (error) {
			Logger.error({error, url: url.toString()}, 'Failed to resolve Wikipedia article');
			return [];
		}
	}

	private deduplicateThumbnails(images: Array<ProcessedThumbnail | null>): Array<ProcessedThumbnail> {
		const seen = new Set<string>();
		const unique: Array<ProcessedThumbnail> = [];

		for (const image of images) {
			if (!image) continue;

			const normalized = this.normalizeUrl(image.url);
			if (!normalized) {
				unique.push(image);
				continue;
			}

			if (seen.has(normalized)) continue;
			seen.add(normalized);
			unique.push(image);
		}

		return unique;
	}

	private normalizeUrl(url?: string): string | null {
		if (!url) return null;

		try {
			const normalizedUrl = new URL(url);
			this.normalizeWikipediaImagePath(normalizedUrl);
			return normalizedUrl.href.replace(/\/$/, '');
		} catch (error) {
			Logger.debug({error, url}, 'Failed to normalize Wikipedia image URL');
			return null;
		}
	}

	private normalizeWikipediaImagePath(imageUrl: URL): void {
		if (imageUrl.hostname !== 'upload.wikimedia.org' || !imageUrl.pathname.includes('/wikipedia/commons/thumb/')) {
			return;
		}

		const segments = imageUrl.pathname.split('/');
		const thumbIndex = segments.indexOf('thumb');
		if (thumbIndex === -1 || segments.length <= thumbIndex + 2) {
			return;
		}

		const normalizedSegments = [...segments.slice(0, thumbIndex), ...segments.slice(thumbIndex + 1, -1)];

		const normalizedPath = normalizedSegments.join('/') || '/';
		imageUrl.pathname = normalizedPath.startsWith('/') ? normalizedPath : `/${normalizedPath}`;
	}
}
