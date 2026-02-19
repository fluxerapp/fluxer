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

import {Logger} from '@fluxer/api/src/Logger';
import {BaseResolver} from '@fluxer/api/src/unfurler/resolvers/BaseResolver';
import type {MessageEmbedResponse} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {selectOne} from 'css-select';
import type {Document, Element, Text} from 'domhandler';
import {parseDocument} from 'htmlparser2';

interface TenorJsonLd {
	image?: {thumbnailUrl?: string};
	video?: {contentUrl?: string};
}

export class TenorResolver extends BaseResolver {
	match(url: URL, mimeType: string, _content: Uint8Array): boolean {
		return mimeType.startsWith('text/html') && url.hostname === 'tenor.com';
	}

	async resolve(url: URL, content: Uint8Array, isNSFWAllowed: boolean = false): Promise<Array<MessageEmbedResponse>> {
		const document = parseDocument(Buffer.from(content).toString('utf-8'));

		const gifEmbed = await this.resolveFromOgImage(url, document, isNSFWAllowed);
		if (gifEmbed) {
			return [gifEmbed];
		}

		return this.resolveFromJsonLd(url, document, isNSFWAllowed);
	}

	private async resolveFromOgImage(
		url: URL,
		document: Document,
		isNSFWAllowed: boolean,
	): Promise<MessageEmbedResponse | null> {
		const ogImageUrl = this.extractMetaContent(document, 'og:image');
		if (!ogImageUrl || !this.isGifUrl(ogImageUrl)) {
			return null;
		}

		const thumbnail = await this.resolveMediaURL(url, ogImageUrl, isNSFWAllowed);
		if (!thumbnail) {
			return null;
		}

		return {
			type: 'gifv',
			url: url.href,
			provider: {name: 'Tenor', url: 'https://tenor.com'},
			thumbnail,
		};
	}

	private async resolveFromJsonLd(
		url: URL,
		document: Document,
		isNSFWAllowed: boolean,
	): Promise<Array<MessageEmbedResponse>> {
		const jsonLdContent = this.extractJsonLdContent(document);
		if (!jsonLdContent) {
			return [];
		}
		const {thumbnailURL, videoURL} = this.extractURLsFromJsonLd(jsonLdContent);
		const thumbnail = thumbnailURL ? await this.resolveMediaURL(url, thumbnailURL, isNSFWAllowed) : undefined;
		const video = videoURL ? await this.resolveMediaURL(url, videoURL, isNSFWAllowed) : undefined;
		const embed: MessageEmbedResponse = {
			type: 'gifv',
			url: url.href,
			provider: {name: 'Tenor', url: 'https://tenor.com'},
			thumbnail: thumbnail ?? undefined,
			video: video ?? undefined,
		};
		return [embed];
	}

	private extractMetaContent(document: Document, property: string): string | null {
		const element = selectOne(`meta[property="${property}"]`, document) as Element | null;
		return element?.attribs['content'] ?? null;
	}

	private isGifUrl(url: string): boolean {
		try {
			const pathname = new URL(url).pathname;
			return pathname.toLowerCase().endsWith('.gif');
		} catch {
			return url.toLowerCase().endsWith('.gif');
		}
	}

	private extractJsonLdContent(document: Document): TenorJsonLd | null {
		const scriptElement = selectOne('script.dynamic[type="application/ld+json"]', document) as Element | null;
		if (scriptElement && scriptElement.children.length > 0) {
			const scriptContentNode = scriptElement.children[0] as Text;
			const scriptContent = scriptContentNode.data;
			try {
				return JSON.parse(scriptContent) as TenorJsonLd;
			} catch (error) {
				Logger.error({error}, 'Failed to parse JSON-LD content');
			}
		}
		return null;
	}

	private extractURLsFromJsonLd(jsonLdContent: TenorJsonLd): {thumbnailURL?: string; videoURL?: string} {
		const thumbnailUrl = jsonLdContent.image?.thumbnailUrl;
		const videoUrl = jsonLdContent.video?.contentUrl;
		return {thumbnailURL: thumbnailUrl, videoURL: videoUrl};
	}
}
