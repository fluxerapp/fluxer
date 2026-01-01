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

import type {MessageEmbedResponse} from '~/channel/ChannelModel';
import type {IMediaService} from '~/infrastructure/IMediaService';
import {Logger} from '~/Logger';
import {buildEmbedMediaPayload} from '~/unfurler/resolvers/media/MediaMetadataHelpers';
import * as DOMUtils from '~/utils/DOMUtils';
import {parseString} from '~/utils/StringUtils';
import type {
	ActivityPubAttachment,
	ActivityPubAuthor,
	ActivityPubContext,
	ActivityPubPost,
	MastodonMediaAttachment,
	MastodonPost,
	ProcessedMedia,
} from './ActivityPubTypes';
import {escapeMarkdownChars} from './ActivityPubUtils';

export class ActivityPubFormatter {
	private static readonly DEFAULT_COLOR = 0x6364ff;
	private static readonly FAVORITE_THRESHOLD = 100;
	private static readonly MAX_ALT_TEXT_LENGTH = 4096;

	constructor(private mediaService: IMediaService) {}

	async processMedia(attachment: MastodonMediaAttachment | ActivityPubAttachment): Promise<ProcessedMedia | undefined> {
		try {
			if (!('url' in attachment) || attachment.url == null) return;
			const url = attachment.url;
			let altText: string | null = null;
			if ('description' in attachment) {
				altText = attachment.description;
				Logger.debug({url, hasAltText: !!altText}, 'Found Mastodon alt text');
			} else if ('name' in attachment) {
				altText = attachment.name || null;
				Logger.debug({url, hasAltText: !!altText}, 'Found ActivityPub alt text');
			}
			const metadata = await this.mediaService.getMetadata({type: 'external', url: url, isNSFWAllowed: false});
			if (!metadata) {
				Logger.debug({url}, 'Failed to get media metadata');
				return;
			}

			let description: string | undefined;
			if (altText) {
				description = parseString(altText, ActivityPubFormatter.MAX_ALT_TEXT_LENGTH);
				Logger.debug(
					{url, originalAltLength: altText.length, processedAltLength: description.length},
					'Added sanitized alt text as description',
				);
			}

			const widthOverride = this.getAttachmentDimension(attachment, 'width') ?? metadata.width;
			const heightOverride = this.getAttachmentDimension(attachment, 'height') ?? metadata.height;

			return buildEmbedMediaPayload(url, metadata, {
				width: widthOverride,
				height: heightOverride,
				description,
			}) as ProcessedMedia;
		} catch (error) {
			Logger.error({error}, 'Failed to process media');
			return;
		}
	}

	formatMastodonContent(post: MastodonPost, context?: ActivityPubContext): string {
		let text = DOMUtils.htmlToMarkdown(post.content);

		if (post.spoiler_text) {
			text = `**${post.spoiler_text}**\n\n${text}`;
		}

		if (post.reblog) {
			const reblogAuthor = post.reblog.account.display_name || post.reblog.account.username;
			const reblogText = DOMUtils.htmlToMarkdown(post.reblog.content);
			text = `**Boosted from ${reblogAuthor}**\n\n${reblogText}`;
		}

		if (context?.inReplyTo) {
			text = `-# ↩ [${context.inReplyTo.author}](${context.inReplyTo.url})\n${text}`;
		}

		return escapeMarkdownChars(text);
	}

	formatActivityPubContent(post: ActivityPubPost, context?: ActivityPubContext): string {
		let text = post.content ? DOMUtils.htmlToMarkdown(post.content) : '';

		if (post.summary) {
			text = `**${post.summary}**\n\n${text}`;
		}

		if (context?.inReplyTo) {
			text = `-# ↩ [${context.inReplyTo.author}](${context.inReplyTo.url})\n${text}`;
		}

		return escapeMarkdownChars(text);
	}

	async buildMastodonEmbed(post: MastodonPost, url: URL, context: ActivityPubContext): Promise<MessageEmbedResponse> {
		const authorName = post.account.display_name || post.account.username;
		const authorFullName = `${authorName} (@${post.account.username}@${context.serverDomain})`;
		const authorUrl = post.account.url;
		const content = this.formatMastodonContent(post, context);
		let image: ProcessedMedia | undefined;
		let video: ProcessedMedia | undefined;
		let thumbnail: ProcessedMedia | undefined;
		if (post.media_attachments?.length > 0) {
			const firstMedia = post.media_attachments[0];
			if (firstMedia.type === 'image' || firstMedia.type === 'gifv') {
				image = await this.processMedia(firstMedia);
				Logger.debug(
					{
						mediaType: firstMedia.type,
						url: firstMedia.url,
						hasAltText: !!firstMedia.description,
						hasProcessedDescription: !!image?.description,
					},
					'Processed image media attachment',
				);
			} else if (firstMedia.type === 'video') {
				video = await this.processMedia(firstMedia);
				if (firstMedia.preview_url) {
					const previewAttachment = {...firstMedia, url: firstMedia.preview_url};
					thumbnail = await this.processMedia(previewAttachment);
				}
				Logger.debug(
					{
						mediaType: firstMedia.type,
						url: firstMedia.url,
						hasAltText: !!firstMedia.description,
						hasVideoDescription: !!video?.description,
						hasThumbnailDescription: !!thumbnail?.description,
					},
					'Processed video media attachment',
				);
			}
		}
		const fields = [];
		if (post.favourites_count >= ActivityPubFormatter.FAVORITE_THRESHOLD)
			fields.push({name: 'Favorites', value: post.favourites_count.toString(), inline: true});
		if (post.reblogs_count >= ActivityPubFormatter.FAVORITE_THRESHOLD)
			fields.push({name: 'Boosts', value: post.reblogs_count.toString(), inline: true});
		if (post.replies_count >= ActivityPubFormatter.FAVORITE_THRESHOLD)
			fields.push({name: 'Replies', value: post.replies_count.toString(), inline: true});
		if (post.poll) {
			const pollOptions = post.poll.options
				.map((option) => {
					const votes = option.votes_count != null ? `: ${option.votes_count}` : '';
					return `• ${option.title}${votes}`;
				})
				.join('\n');
			fields.push({name: `Poll (${post.poll.votes_count} votes)`, value: pollOptions, inline: false});
		}
		const embed: MessageEmbedResponse = {
			type: 'rich',
			url: url.toString(),
			description: content,
			color: ActivityPubFormatter.DEFAULT_COLOR,
			timestamp: new Date(post.created_at).toISOString(),
			author: {name: authorFullName, url: authorUrl, icon_url: post.account.avatar},
			footer: {text: context.serverTitle, icon_url: context.serverIcon},
			fields: fields.length > 0 ? fields : undefined,
		};

		if (image) {
			embed.image = image;
		}

		if (video) {
			embed.video = video;
			if (thumbnail) {
				embed.thumbnail = thumbnail;
			}
		}

		return embed;
	}

	async buildActivityPubEmbed(
		post: ActivityPubPost,
		url: URL,
		context: ActivityPubContext,
		fetchAuthorData: (url: string) => Promise<ActivityPubPost | null>,
	): Promise<MessageEmbedResponse> {
		const isActivityPubAuthor = (data: unknown): data is ActivityPubAuthor =>
			typeof data === 'object' &&
			data !== null &&
			('name' in data || 'preferredUsername' in data || 'url' in data || 'icon' in data);

		let authorName = '';
		let authorUrl = '';
		let authorIcon = '';
		if (typeof post.attributedTo === 'string') {
			const authorData = await fetchAuthorData(post.attributedTo);
			if (authorData) {
				if (isActivityPubAuthor(authorData)) {
					authorName = authorData.name || authorData.preferredUsername || '';
					authorUrl = authorData.url || post.attributedTo;
					authorIcon = authorData.icon?.url || '';
				} else {
					const authorUrlObj = new URL(post.attributedTo);
					authorName = authorUrlObj.pathname.split('/').pop() || '';
					authorUrl = post.attributedTo;
				}
			} else {
				const authorUrlObj = new URL(post.attributedTo);
				authorName = authorUrlObj.pathname.split('/').pop() || '';
				authorUrl = post.attributedTo;
			}
		} else if (post.attributedTo && typeof post.attributedTo === 'object') {
			const author = post.attributedTo as ActivityPubAuthor;
			authorName = author.name || author.preferredUsername || '';
			authorUrl = author.url || '';
			authorIcon = author.icon?.url || '';
		}
		let authorFullName = authorName;
		const authorUsername = authorUrl.split('/').pop() || '';
		authorFullName = `${authorName} (@${authorUsername}@${context.serverDomain})`;
		const content = this.formatActivityPubContent(post, context);
		let image: ProcessedMedia | undefined;
		let video: ProcessedMedia | undefined;
		let thumbnail: ProcessedMedia | undefined;
		if (post.attachment && post.attachment.length > 0) {
			const firstMedia = post.attachment[0];
			if (firstMedia.mediaType.startsWith('image/')) {
				image = await this.processMedia(firstMedia);
				Logger.debug(
					{
						mediaType: firstMedia.mediaType,
						url: firstMedia.url,
						hasAltText: !!firstMedia.name,
						hasProcessedDescription: !!image?.description,
					},
					'Processed ActivityPub image attachment',
				);
			} else if (firstMedia.mediaType.startsWith('video/')) {
				video = await this.processMedia(firstMedia);
				const thumbnailAttachment = post.attachment?.find(
					(a) => a.type === 'Image' && a.mediaType.startsWith('image/'),
				);
				if (thumbnailAttachment) thumbnail = await this.processMedia(thumbnailAttachment);
				Logger.debug(
					{
						mediaType: firstMedia.mediaType,
						url: firstMedia.url,
						hasAltText: !!firstMedia.name,
						hasVideoDescription: !!video?.description,
						hasThumbnailDescription: !!thumbnail?.description,
					},
					'Processed ActivityPub video attachment',
				);
			}
		}
		const fields = [];
		const likesCount = typeof post.likes === 'number' ? post.likes : 0;
		const sharesCount = typeof post.shares === 'number' ? post.shares : 0;
		const repliesCount = post.replies?.totalItems || 0;
		if (likesCount >= ActivityPubFormatter.FAVORITE_THRESHOLD)
			fields.push({name: 'Likes', value: likesCount.toString(), inline: true});
		if (sharesCount >= ActivityPubFormatter.FAVORITE_THRESHOLD)
			fields.push({name: 'Shares', value: sharesCount.toString(), inline: true});
		if (repliesCount >= ActivityPubFormatter.FAVORITE_THRESHOLD)
			fields.push({name: 'Replies', value: repliesCount.toString(), inline: true});
		const embed: MessageEmbedResponse = {
			type: 'rich',
			url: url.toString(),
			description: content,
			color: ActivityPubFormatter.DEFAULT_COLOR,
			timestamp: new Date(post.published).toISOString(),
			author: {name: authorFullName, url: authorUrl, icon_url: authorIcon},
			footer: {text: context.serverTitle, icon_url: context.serverIcon},
			fields: fields.length > 0 ? fields : undefined,
		};
		if (image) embed.image = image;
		if (video) {
			embed.video = video;
			if (thumbnail) embed.thumbnail = thumbnail;
		}
		return embed;
	}

	private getAttachmentDimension(
		attachment: MastodonMediaAttachment | ActivityPubAttachment,
		dimension: 'width' | 'height',
	): number | undefined {
		if (dimension === 'width' && 'width' in attachment && typeof attachment.width === 'number') {
			return attachment.width;
		}

		if (dimension === 'height' && 'height' in attachment && typeof attachment.height === 'number') {
			return attachment.height;
		}

		if (this.isMastodonMediaAttachment(attachment)) {
			return attachment.meta.original?.[dimension] ?? attachment.meta.small?.[dimension];
		}

		return undefined;
	}

	private isMastodonMediaAttachment(
		attachment: MastodonMediaAttachment | ActivityPubAttachment,
	): attachment is MastodonMediaAttachment {
		return 'meta' in attachment;
	}
}
