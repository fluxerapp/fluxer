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

import {AppBskyRichtextFacet, RichText} from '@atproto/api';
import {Logger} from '~/Logger';
import type {BlueskyAuthor, BlueskyPost, BlueskyPostThread, Facet, ReplyContext} from './BlueskyTypes';

export class BlueskyTextFormatter {
	embedLinksInText(text: string, facets?: Array<Facet>): string {
		if (!facets) return text;

		const richText = new RichText({text, facets});
		let result = '';
		let lastIndex = 0;
		const utf8ToUtf16Map = new Map<number, number>();

		for (let i = 0; i < text.length; i++) {
			const utf8Index = richText.unicodeText.utf16IndexToUtf8Index(i);
			utf8ToUtf16Map.set(utf8Index, i);
		}

		const sortedFacets = [...(richText.facets || [])].sort((a, b) => a.index.byteStart - b.index.byteStart);

		for (const facet of sortedFacets) {
			const startUtf16 = utf8ToUtf16Map.get(facet.index.byteStart) ?? lastIndex;
			const endUtf16 = utf8ToUtf16Map.get(facet.index.byteEnd) ?? text.length;
			result += text.slice(lastIndex, startUtf16);
			const facetText = text.slice(startUtf16, endUtf16);
			const feature = facet.features[0];

			if (AppBskyRichtextFacet.isLink(feature)) {
				result += `[${this.getLinkDisplayText(feature.uri)}](${feature.uri})`;
			} else if (AppBskyRichtextFacet.isMention(feature)) {
				result += `[${facetText}](https://bsky.app/profile/${feature.did})`;
			} else if (AppBskyRichtextFacet.isTag(feature)) {
				const tagText = facetText.startsWith('#') ? facetText.slice(1) : facetText;
				result += `[${facetText}](https://bsky.app/search?q=%23${encodeURIComponent(tagText)})`;
			} else {
				result += facetText;
			}

			lastIndex = endUtf16;
		}

		result += text.slice(lastIndex);
		return result;
	}

	getLinkDisplayText(uri: string): string {
		const url = new URL(uri);
		const hostname = url.hostname;
		const path = url.pathname;
		const pathParts = path.split('/');
		const pathPart = pathParts[pathParts.length - 1];
		const truncatedPath = pathPart.length > 12 ? `${pathPart.slice(0, 12)}...` : pathPart;
		return `${hostname}${pathPart ? `/${truncatedPath}` : ''}`;
	}

	formatAuthor(author: BlueskyAuthor): string {
		const displayName = author.displayName || author.handle;
		const handle = author.handle;
		const profileUrl = `https://bsky.app/profile/${handle}`;
		return `**[${displayName} (@${handle})](${profileUrl})**`;
	}

	formatPostContent(post: BlueskyPost, thread: BlueskyPostThread): string {
		let processedText = this.embedLinksInText(post.record.text, post.record.facets);
		const replyContext = this.extractReplyContext(post, thread);

		if (replyContext) {
			processedText = `-# ↩ [${replyContext.authorName} (@${replyContext.authorHandle})](${replyContext.postUrl})\n${processedText}`;
			Logger.debug(
				{postUri: post.uri, replyingTo: replyContext.authorName, replyingToHandle: replyContext.authorHandle},
				'Added reply indicator to post content',
			);
		}

		return processedText;
	}

	extractReplyContext(post: BlueskyPost, thread: BlueskyPostThread): ReplyContext | null {
		if (!post.record.reply) {
			Logger.debug({postUri: post.uri}, 'Post is not a reply');
			return null;
		}

		if (thread.thread.parent?.post) {
			const parentPost = thread.thread.parent.post;
			const authorName = parentPost.author.displayName || parentPost.author.handle;
			const authorHandle = parentPost.author.handle;
			const postUrl = `https://bsky.app/profile/${authorHandle}/post/${parentPost.uri.split('/').pop()}`;
			Logger.debug({parentAuthor: authorName, parentHandle: authorHandle, postUrl}, 'Found parent post in thread data');
			return {authorName, authorHandle, postUrl};
		}

		Logger.debug({postUri: post.uri}, 'Parent post not found in thread data');
		return null;
	}
}
