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

import {selectAll, selectOne} from 'css-select';
import type {Document, Element, Text} from 'domhandler';
import {parseDocument} from 'htmlparser2';
import type {MessageEmbedResponse} from '~/channel/EmbedTypes';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import {Logger} from '~/Logger';
import {BaseResolver} from '~/unfurler/resolvers/BaseResolver';
import * as FetchUtils from '~/utils/FetchUtils';
import {parseString} from '~/utils/StringUtils';
import {ActivityPubResolver} from './subresolvers/ActivityPubResolver';

interface OEmbedResponse {
	provider_name?: string;
	provider_url?: string;
	author_name?: string;
	author_url?: string;
}

const COLOR_REGEX = /^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$/;

export class DefaultResolver extends BaseResolver {
	private activityPubResolver: ActivityPubResolver;
	private static readonly MAX_GALLERY_IMAGES = 10;

	constructor(
		private cacheService: ICacheService,
		mediaService: IMediaService,
	) {
		super(mediaService);
		this.activityPubResolver = new ActivityPubResolver(this.cacheService, mediaService);
	}

	match(_url: URL, mimeType: string, _content: Uint8Array): boolean {
		Logger.debug({mimeType}, 'Checking if content type matches HTML');
		const matches = mimeType.startsWith('text/html');
		Logger.debug({matches}, 'Content type match result');
		return matches;
	}

	async resolve(url: URL, content: Uint8Array, isNSFWAllowed: boolean = false): Promise<Array<MessageEmbedResponse>> {
		Logger.debug({url: url.href}, 'Starting HTML resolution');

		let document: Document;
		let htmlString: string;

		try {
			htmlString = Buffer.from(content).toString('utf-8');
			document = parseDocument(htmlString);
			Logger.debug('Successfully parsed HTML document');
		} catch (error) {
			Logger.error({error}, 'Failed to parse HTML document');
			throw error;
		}

		const activityPubLink = this.findActivityPubLink(document);
		let activityPubUrl: string | null = null;

		if (activityPubLink) {
			activityPubUrl = activityPubLink.startsWith('http')
				? activityPubLink
				: new URL(activityPubLink, url.origin).toString();
			Logger.debug({activityPubUrl}, 'Found ActivityPub link in HTML');

			try {
				const activityPubEmbeds = await this.activityPubResolver.resolveActivityPub(url, activityPubUrl, htmlString);
				if (activityPubEmbeds && activityPubEmbeds.length > 0) {
					Logger.debug({url: url.href}, 'Resolved as ActivityPub');
					return activityPubEmbeds;
				}
			} catch (error) {
				Logger.error({error, url: url.href}, 'Failed to resolve as ActivityPub');
			}
		}

		const title = this.extractTitle(document);
		Logger.debug({title}, 'Extracted title');

		const description = this.extractDescription(document);
		Logger.debug({description}, 'Extracted description');

		const rawColor = this.extractMetaField(document, 'theme-color');
		const color = this.extractColor(rawColor);
		Logger.debug({rawColor, color}, 'Extracted and parsed color');

		const oEmbedData = await this.fetchOEmbedData(url, document);
		Logger.debug({oEmbedData}, 'Fetched oEmbed data');

		const siteName = oEmbedData.providerName ?? this.extractSiteName(document);
		Logger.debug({siteName}, 'Determined site name');

		const imageUrls = this.extractImageURLs(document);
		Logger.debug({imageUrls}, 'Extracted image URLs');

		const resolvedImages: Array<MessageEmbedResponse['image']> = [];
		for (const imageUrl of imageUrls) {
			if (resolvedImages.length >= DefaultResolver.MAX_GALLERY_IMAGES) break;
			const media = await this.resolveMediaURL(url, imageUrl, isNSFWAllowed);
			if (media) {
				resolvedImages.push(media);
			}
		}

		const imageMedia = resolvedImages.shift();

		if (imageMedia) {
			const imageDescription =
				this.extractMetaField(document, 'og:image:alt') ??
				this.extractMetaField(document, 'twitter:image:alt') ??
				this.extractMetaField(document, 'og:image:description');
			if (imageDescription) {
				imageMedia.description = parseString(imageDescription, 4096);
				Logger.debug({imageDescription: imageMedia.description}, 'Applied description to image media');
			}
		}

		Logger.debug({imageMedia}, 'Resolved image media');

		const videoUrl = this.extractMediaURL(document, 'video');
		const videoMedia = await this.resolveMediaURL(url, videoUrl, isNSFWAllowed);
		Logger.debug({videoUrl, videoMedia}, 'Resolved video media');

		const audioUrl = this.extractMediaURL(document, 'audio');
		const audioMedia = await this.resolveMediaURL(url, audioUrl, isNSFWAllowed);
		Logger.debug({audioUrl, audioMedia}, 'Resolved audio media');

		const embed: MessageEmbedResponse = {
			type: 'link',
			url: url.href,
			...(title && {title: parseString(title, 70)}),
			...(description && {description: parseString(description, 350)}),
			...(color !== undefined && {color}),
			...(oEmbedData.authorName &&
				oEmbedData.authorURL && {
					author: {name: parseString(oEmbedData.authorName, 256), url: oEmbedData.authorURL},
				}),
			...(siteName && {provider: {name: parseString(siteName, 256), url: oEmbedData.providerURL ?? url.origin}}),
			...(imageMedia && {thumbnail: imageMedia}),
			...(videoMedia && {video: videoMedia}),
			...(audioMedia && {audio: audioMedia}),
		};

		const extraImageEmbeds = resolvedImages.map((image) => ({
			type: 'rich' as const,
			url: url.href,
			image,
		}));

		Logger.debug({embed, galleryImages: extraImageEmbeds.length}, 'Successfully created link embed');
		return [embed, ...extraImageEmbeds];
	}

	private findActivityPubLink(document: Document): string | null {
		const linkElement = selectOne(
			'link[rel="alternate"][type="application/activity+json"]',
			document,
		) as Element | null;
		return linkElement?.attribs.href || null;
	}

	private extractMetaField(document: Document, property: string, attribute = 'content'): string | undefined {
		Logger.debug({property, attribute}, 'Extracting meta field');

		const values = this.extractMetaFieldValues(document, property, attribute);
		return values.length > 0 ? values[values.length - 1] : undefined;
	}

	private extractMetaFieldValues(document: Document, property: string, attribute = 'content'): Array<string> {
		const selectors = [
			`meta[property="${property}"]`,
			`meta[name="${property}"]`,
			`meta[property="twitter:${property.replace('og:', '')}"]`,
			`meta[name="twitter:${property.replace('og:', '')}"]`,
		];

		const values: Array<string> = [];

		for (const selector of selectors) {
			const nodes = selectAll(selector, document) as Array<Element | Document>;
			const elements = nodes.flatMap((node) => (node && 'attribs' in (node as Element) ? [node as Element] : []));
			for (const element of elements) {
				if (element?.attribs[attribute]) {
					Logger.debug({selector, value: element.attribs[attribute]}, 'Found meta value');
					values.push(element.attribs[attribute]);
				}
			}
		}

		return values;
	}

	private extractTitle(document: Document): string | undefined {
		const ogTitle = this.extractMetaField(document, 'og:title');
		if (ogTitle) {
			Logger.debug({ogTitle}, 'Found OpenGraph title');
			return ogTitle;
		}

		const twitterTitle = this.extractMetaField(document, 'twitter:title');
		if (twitterTitle) {
			Logger.debug({twitterTitle}, 'Found Twitter title');
			return twitterTitle;
		}

		const titleElement = selectOne('title', document) as Element | null;
		if (titleElement?.children[0]) {
			const titleText = (titleElement.children[0] as Text).data?.trim();
			if (titleText) {
				Logger.debug({titleText}, 'Found HTML title');
				return titleText;
			}
		}

		const metaTitle = this.extractMetaField(document, 'title');
		if (metaTitle) {
			Logger.debug({metaTitle}, 'Found meta title');
			return metaTitle;
		}

		return;
	}

	private extractDescription(document: Document): string | undefined {
		Logger.debug('Extracting description');
		const description =
			this.extractMetaField(document, 'og:description') ||
			this.extractMetaField(document, 'description') ||
			this.extractMetaField(document, 'twitter:description');
		Logger.debug({description}, 'Found description');
		return description;
	}

	private extractSiteName(document: Document): string | undefined {
		Logger.debug('Extracting site name');
		const siteName =
			this.extractMetaField(document, 'og:site_name') ||
			this.extractMetaField(document, 'twitter:site:name') ||
			this.extractMetaField(document, 'application-name');
		Logger.debug({siteName}, 'Found site name');
		return siteName;
	}

	private extractImageURLs(document: Document): Array<string> {
		Logger.debug('Extracting image URLs');

		const properties = ['og:image', 'og:image:secure_url', 'twitter:image', 'twitter:image:src', 'image'];
		const seen = new Set<string>();
		const values: Array<string> = [];

		for (const property of properties) {
			const metaValues = this.extractMetaFieldValues(document, property);
			for (const metaValue of metaValues) {
				const normalized = this.normalizeUrl(metaValue);
				if (!normalized || seen.has(normalized)) continue;
				seen.add(normalized);
				values.push(metaValue);
			}
		}

		return values;
	}

	private normalizeUrl(value: string): string | null {
		try {
			return new URL(value).href.replace(/\/$/, '');
		} catch (error) {
			Logger.debug({error, value}, 'Failed to normalize URL');
			return null;
		}
	}

	private extractMediaURL(document: Document, type: 'video' | 'audio'): string | undefined {
		Logger.debug({type}, 'Extracting media URL');
		const mediaUrl =
			this.extractMetaField(document, `og:${type}`) ||
			this.extractMetaField(document, `og:${type}:url`) ||
			this.extractMetaField(document, `og:${type}:secure_url`) ||
			this.extractMetaField(document, `twitter:${type}`) ||
			this.extractMetaField(document, `twitter:${type}:url`) ||
			(type === 'video' ? this.extractMetaField(document, 'twitter:player') : undefined) ||
			(type === 'video' ? this.extractMetaField(document, 'twitter:player:stream') : undefined);
		Logger.debug({mediaUrl}, `Found ${type} URL`);
		return mediaUrl;
	}

	private extractColor(color: string | undefined): number | undefined {
		if (!color) return;

		const normalizedColor = color.toLowerCase();
		if (!COLOR_REGEX.test(normalizedColor)) {
			Logger.debug({color}, 'Invalid color format');
			return;
		}

		try {
			const parsed = Number.parseInt(normalizedColor.slice(1), 16);
			Logger.debug({color, parsed}, 'Successfully parsed color');
			return parsed;
		} catch (error) {
			Logger.debug({error, color}, 'Failed to parse color');
			return;
		}
	}

	private async fetchOEmbedData(
		url: URL,
		document: Document,
	): Promise<{
		providerName?: string;
		providerURL?: string;
		authorName?: string;
		authorURL?: string;
	}> {
		Logger.debug({url: url.href}, 'Attempting to fetch oEmbed data');

		const oEmbedLink = selectOne('link[type="application/json+oembed"]', document) as Element | null;
		if (!oEmbedLink?.attribs.href) {
			Logger.debug('No oEmbed link found');
			return {};
		}

		const oEmbedUrl = this.resolveRelativeURL(url.href, oEmbedLink.attribs.href);
		if (!oEmbedUrl) {
			Logger.debug('Could not resolve oEmbed URL');
			return {};
		}

		try {
			Logger.debug({url: oEmbedUrl}, 'Fetching oEmbed data');
			const response = await FetchUtils.sendRequest({url: oEmbedUrl});

			if (response.status !== 200) {
				Logger.debug({status: response.status}, 'Failed to fetch oEmbed data');
				return {};
			}

			const responseText = await FetchUtils.streamToString(response.stream);
			const oEmbedJson = JSON.parse(responseText) as OEmbedResponse;
			Logger.debug({oEmbedJson}, 'Successfully parsed oEmbed response');

			return {
				providerName: oEmbedJson.provider_name,
				providerURL: oEmbedJson.provider_url,
				authorName: oEmbedJson.author_name,
				authorURL: oEmbedJson.author_url,
			};
		} catch (error) {
			Logger.error({error, url: oEmbedUrl}, 'Failed to fetch oEmbed JSON');
			return {};
		}
	}
}
