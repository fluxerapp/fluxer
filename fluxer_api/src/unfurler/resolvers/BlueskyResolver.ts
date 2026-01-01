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
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import {Logger} from '~/Logger';
import {BaseResolver} from '~/unfurler/resolvers/BaseResolver';
import {BlueskyApiClient} from './bluesky/BlueskyApiClient';
import {BlueskyEmbedProcessor} from './bluesky/BlueskyEmbedProcessor';
import {BlueskyTextFormatter} from './bluesky/BlueskyTextFormatter';

export class BlueskyResolver extends BaseResolver {
	private static readonly BLUESKY_COLOR = 0x1185fe;
	private static readonly BLUESKY_ICON = 'https://bsky.app/static/apple-touch-icon.png';
	private static readonly PATH_SEPARATOR = '/';
	private static readonly COUNTER_DISPLAY_THRESHOLD = 100;

	private apiClient: BlueskyApiClient;
	private textFormatter: BlueskyTextFormatter;
	private embedProcessor: BlueskyEmbedProcessor;

	constructor(cacheService: ICacheService, mediaService: IMediaService) {
		super(mediaService);
		this.apiClient = new BlueskyApiClient(cacheService);
		this.textFormatter = new BlueskyTextFormatter();
		this.embedProcessor = new BlueskyEmbedProcessor(mediaService, this.apiClient, this.textFormatter);
	}

	match(url: URL, mimeType: string, _content: Uint8Array): boolean {
		const isMatch = url.hostname === 'bsky.app' && mimeType.startsWith('text/html');
		Logger.debug({url: url.toString(), mimeType, isMatch}, 'BlueskyResolver match check');
		return isMatch;
	}

	async resolve(url: URL, _content: Uint8Array, isNSFWAllowed: boolean = false): Promise<Array<MessageEmbedResponse>> {
		try {
			Logger.debug({url: url.toString()}, 'Starting URL resolution');

			if (this.isPostUrl(url)) {
				Logger.debug({url: url.toString()}, 'Resolving post URL');
				const atUri = await this.getAtUri(url);
				if (!atUri) return [];

				const thread = await this.apiClient.fetchPost(atUri);
				if (!thread) return [];

				const {post} = thread.thread;
				const {image, thumbnail, video, quotedText, galleryImages} = await this.embedProcessor.processPostEmbed(
					post,
					isNSFWAllowed,
				);

				let processedText = this.textFormatter.formatPostContent(post, thread);
				if (quotedText) processedText += `\n\n${quotedText}`;

				Logger.debug(
					{
						url: url.toString(),
						embedType: post.embed?.$type,
						hasImage: !!image,
						hasThumbnail: !!thumbnail,
						hasVideo: !!video,
						hasImageAltText: !!image?.description,
						isReply: !!post.record.reply,
						replyCount: post.replyCount,
						repostCount: post.repostCount,
						likeCount: post.likeCount,
						quoteCount: post.quoteCount,
					},
					'Processed post embeds',
				);

				const fields: Array<{name: string; value: string; inline: boolean}> = [];
				if (post.replyCount > BlueskyResolver.COUNTER_DISPLAY_THRESHOLD)
					fields.push({name: 'Replies', value: post.replyCount.toString(), inline: true});
				if (post.repostCount > BlueskyResolver.COUNTER_DISPLAY_THRESHOLD)
					fields.push({name: 'Reposts', value: post.repostCount.toString(), inline: true});
				if (post.likeCount > BlueskyResolver.COUNTER_DISPLAY_THRESHOLD)
					fields.push({name: 'Likes', value: post.likeCount.toString(), inline: true});
				if (post.quoteCount > BlueskyResolver.COUNTER_DISPLAY_THRESHOLD)
					fields.push({name: 'Quotes', value: post.quoteCount.toString(), inline: true});

				const embed: MessageEmbedResponse = {
					type: 'rich',
					url: url.href,
					description: processedText,
					color: BlueskyResolver.BLUESKY_COLOR,
					timestamp: new Date(post.record.createdAt).toISOString(),
					author: {
						name: `${post.author.displayName || post.author.handle} (@${post.author.handle})`,
						url: `https://bsky.app/profile/${post.author.handle}`,
						icon_url: post.author.avatar,
					},
					...(image ? {image} : {}),
					...(video ? {thumbnail, video} : {}),
					fields,
					footer: {text: 'Bluesky', icon_url: BlueskyResolver.BLUESKY_ICON},
				};
				const galleryEmbeds =
					galleryImages?.map((galleryImage) => ({
						type: 'rich',
						url: url.href,
						image: galleryImage,
					})) ?? [];

				return [embed, ...galleryEmbeds];
			}

			if (this.isProfileUrl(url)) {
				Logger.debug({url: url.toString()}, 'Resolving profile URL');
				const handle = this.parsePathParts(url)[1];
				const profile = await this.apiClient.fetchProfile(handle);
				if (!profile) return [];

				const embed: MessageEmbedResponse = {
					type: 'rich',
					url: url.href,
					title: profile.displayName ? `${profile.displayName} (@${profile.handle})` : `@${profile.handle}`,
					description: profile.description,
					color: BlueskyResolver.BLUESKY_COLOR,
					footer: {text: 'Bluesky', icon_url: BlueskyResolver.BLUESKY_ICON},
				};
				return [embed];
			}

			Logger.debug({url: url.toString()}, 'URL does not match any supported patterns');
			return [];
		} catch (error) {
			Logger.error({error, url: url.toString()}, 'Failed to resolve URL');
			return [];
		}
	}

	private parsePathParts(url: URL): Array<string> {
		return url.pathname.replace(/^\/+|\/+$/g, '').split(BlueskyResolver.PATH_SEPARATOR);
	}

	private isProfileUrl(url: URL): boolean {
		const parts = this.parsePathParts(url);
		const isProfile = parts.length === 2 && parts[0] === 'profile';
		Logger.debug({url: url.toString(), parts, isProfile}, 'Profile URL check');
		return isProfile;
	}

	private isPostUrl(url: URL): boolean {
		const parts = this.parsePathParts(url);
		const isPost = parts.length === 4 && parts[0] === 'profile' && parts[2] === 'post' && parts[3].length > 0;
		Logger.debug({url: url.toString(), parts, isPost}, 'Post URL check');
		return isPost;
	}

	private async getAtUri(url: URL): Promise<string | null> {
		const parts = this.parsePathParts(url);
		if (parts.length !== 4) throw new Error('Invalid URL format for AT URI conversion');

		const handle = parts[1];
		const postId = parts[3];
		const did = await this.apiClient.resolveDid(handle);
		if (!did) return null;

		const atUri = `at://${did}/app.bsky.feed.post/${postId}`;
		Logger.debug({url: url.toString(), handle, did, postId, atUri}, 'Generated AT URI');
		return atUri;
	}
}
