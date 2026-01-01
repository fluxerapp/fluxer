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

import {filetypemime} from 'magic-bytes.js';
import type {MessageEmbedResponse} from '~/channel/EmbedTypes';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import {IUnfurlerService} from '~/infrastructure/IUnfurlerService';
import {Logger} from '~/Logger';
import {AudioResolver} from '~/unfurler/resolvers/AudioResolver';
import type {BaseResolver} from '~/unfurler/resolvers/BaseResolver';
import {BlueskyResolver} from '~/unfurler/resolvers/BlueskyResolver';
import {DefaultResolver} from '~/unfurler/resolvers/DefaultResolver';
import {HackerNewsResolver} from '~/unfurler/resolvers/HackerNewsResolver';
import {ImageResolver} from '~/unfurler/resolvers/ImageResolver';
import {TenorResolver} from '~/unfurler/resolvers/TenorResolver';
import {VideoResolver} from '~/unfurler/resolvers/VideoResolver';
import {WikipediaResolver} from '~/unfurler/resolvers/WikipediaResolver';
import {XkcdResolver} from '~/unfurler/resolvers/XkcdResolver';
import {YouTubeResolver} from '~/unfurler/resolvers/YouTubeResolver';
import * as FetchUtils from '~/utils/FetchUtils';

export class UnfurlerService extends IUnfurlerService {
	private readonly resolvers: Array<BaseResolver>;

	constructor(
		private cacheService: ICacheService,
		private mediaService: IMediaService,
	) {
		super();
		this.resolvers = [
			new AudioResolver(this.mediaService),
			new HackerNewsResolver(this.mediaService),
			new ImageResolver(this.mediaService),
			new TenorResolver(this.mediaService),
			new VideoResolver(this.mediaService),
			new XkcdResolver(this.mediaService),
			new YouTubeResolver(this.mediaService),
			new WikipediaResolver(this.mediaService),
			new BlueskyResolver(this.cacheService, this.mediaService),
			new DefaultResolver(this.cacheService, this.mediaService),
		];
	}

	async unfurl(url: string, isNSFWAllowed: boolean = false): Promise<Array<MessageEmbedResponse>> {
		try {
			const response = await FetchUtils.sendRequest({url, timeout: 10_000});
			if (response.status !== 200) {
				Logger.debug({url, status: response.status}, 'Non-200 response received');
				return [];
			}
			const contentBuffer = await this.streamToBuffer(response.stream);
			const mimeType = this.determineMimeType(contentBuffer, response.headers);
			if (!mimeType) {
				Logger.error({url}, 'Unable to determine MIME type');
				return [];
			}
			const finalUrl = new URL(response.url);
			for (const resolver of this.resolvers) {
				if (resolver.match(finalUrl, mimeType, contentBuffer)) {
					return resolver.resolve(finalUrl, contentBuffer, isNSFWAllowed);
				}
			}
			return [];
		} catch (error) {
			Logger.error({error, url}, 'Failed to unfurl URL');
			return [];
		}
	}

	private async streamToBuffer(stream: NodeJS.ReadableStream): Promise<Uint8Array> {
		const chunks: Array<Uint8Array> = [];
		for await (const chunk of stream) {
			chunks.push(new Uint8Array(Buffer.from(chunk)));
		}
		return new Uint8Array(Buffer.concat(chunks.map((chunk) => Buffer.from(chunk))));
	}

	private determineMimeType(content: Uint8Array, headers: Headers): string | undefined {
		const headerMimeType = headers.get('content-type')?.split(';')[0];
		if (headerMimeType) return headerMimeType;
		const [mimeTypeFromMagicBytes] = filetypemime(new Uint8Array(content));
		return mimeTypeFromMagicBytes;
	}
}
