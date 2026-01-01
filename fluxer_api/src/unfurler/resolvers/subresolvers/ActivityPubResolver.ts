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
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import {Logger} from '~/Logger';
import * as DOMUtils from '~/utils/DOMUtils';
import * as FetchUtils from '~/utils/FetchUtils';
import {ActivityPubFetcher} from './ActivityPubFetcher';
import {ActivityPubFormatter} from './ActivityPubFormatter';
import type {ActivityPubContext, MastodonPost} from './ActivityPubTypes';
import {extractAppleTouchIcon, extractPostId} from './ActivityPubUtils';

export class ActivityPubResolver {
	private fetcher: ActivityPubFetcher;
	private formatter: ActivityPubFormatter;

	constructor(cacheService: ICacheService, mediaService: IMediaService) {
		this.fetcher = new ActivityPubFetcher(cacheService);
		this.formatter = new ActivityPubFormatter(mediaService);
	}

	private async tryFetchParentPost(inReplyToUrl: string): Promise<ActivityPubContext['inReplyTo'] | undefined> {
		try {
			const parentPost = await this.fetcher.tryFetchActivityPubData(inReplyToUrl);
			if (!parentPost) return;

			let authorName = '';
			if (typeof parentPost.attributedTo === 'string') {
				const authorUrl = new URL(parentPost.attributedTo);
				authorName = authorUrl.pathname.split('/').pop() || '';
			} else if (parentPost.attributedTo) {
				authorName = parentPost.attributedTo.preferredUsername || parentPost.attributedTo.name || '';
			}

			const content = parentPost.content ? DOMUtils.htmlToMarkdown(parentPost.content) : '';
			const urlObj = new URL(parentPost.url);
			const idMatch = extractPostId(urlObj);

			return {author: authorName, content, url: parentPost.url, id: idMatch || undefined};
		} catch (error) {
			Logger.error({error, inReplyToUrl}, 'Failed to fetch parent post');
			return;
		}
	}

	async resolveActivityPub(
		url: URL,
		activityPubUrl: string | null,
		html: string,
	): Promise<Array<MessageEmbedResponse> | null> {
		try {
			Logger.debug({url: url.toString()}, 'Resolving ActivityPub URL');
			const postId = extractPostId(url);
			if (!postId) {
				Logger.debug({url: url.toString()}, 'No post ID found in URL');
				return null;
			}
			const instanceInfo = await this.fetcher.fetchInstanceInfo(url.origin);
			const appleTouchIcon = extractAppleTouchIcon(html, url);
			const cleanedHostname = url.hostname.replace(/^(?:www\.|social\.|mstdn\.)/, '');
			const context: ActivityPubContext = {
				serverDomain: instanceInfo?.domain || cleanedHostname,
				serverName: instanceInfo?.domain || cleanedHostname,
				serverTitle: instanceInfo?.title || `${cleanedHostname} Mastodon`,
				serverIcon: appleTouchIcon,
			};
			const mastodonPost = await this.fetcher.tryFetchMastodonApi(url.origin, postId);
			if (mastodonPost) {
				Logger.debug({url: url.toString(), postId}, 'Successfully fetched Mastodon API data');
				if (mastodonPost.in_reply_to_id && mastodonPost.in_reply_to_account_id) {
					try {
						const parentPostUrl = `${url.origin}/api/v1/statuses/${mastodonPost.in_reply_to_id}`;
						const response = await FetchUtils.sendRequest({
							url: parentPostUrl,
							method: 'GET',
							timeout: 5000,
							headers: {Accept: 'application/json'},
						});
						if (response.status === 200) {
							const data = await FetchUtils.streamToString(response.stream);
							const parentPost = JSON.parse(data) as MastodonPost;
							const parentAuthor = parentPost.account.display_name || parentPost.account.username;
							context.inReplyTo = {
								author: parentAuthor,
								content: DOMUtils.htmlToMarkdown(parentPost.content),
								url: parentPost.url,
								id: mastodonPost.in_reply_to_id,
							};
						}
					} catch (error) {
						Logger.error({error, inReplyToId: mastodonPost.in_reply_to_id}, 'Failed to fetch parent post for Mastodon');
					}
				}
				const embed = await this.formatter.buildMastodonEmbed(mastodonPost, url, context);
				return [embed];
			}
			if (activityPubUrl) {
				Logger.debug({url: url.toString(), activityPubUrl}, 'Found ActivityPub link');
				const activityPubPost = await this.fetcher.tryFetchActivityPubData(activityPubUrl);
				if (activityPubPost) {
					Logger.debug({url: url.toString(), postId}, 'Successfully fetched ActivityPub data');
					if (activityPubPost.inReplyTo) {
						const parentUrl = typeof activityPubPost.inReplyTo === 'string' ? activityPubPost.inReplyTo : null;
						if (parentUrl) context.inReplyTo = await this.tryFetchParentPost(parentUrl);
					}
					const embed = await this.formatter.buildActivityPubEmbed(
						activityPubPost,
						url,
						context,
						this.fetcher.tryFetchActivityPubData.bind(this.fetcher),
					);
					return [embed];
				}
			}
			if (url.pathname.includes('/notice/')) {
				const noticeId = url.pathname.split('/notice/')[1]?.split('/')[0];
				if (noticeId) {
					const pleromaApiUrl = `${url.origin}/api/v1/statuses/${noticeId}`;
					Logger.debug({pleromaApiUrl}, 'Trying Pleroma-compatible API endpoint');
					const pleromaPost = await this.fetcher.tryFetchMastodonApi(url.origin, noticeId);
					if (pleromaPost) {
						Logger.debug({url: url.toString(), noticeId}, 'Successfully fetched Pleroma API data');
						const embed = await this.formatter.buildMastodonEmbed(pleromaPost, url, context);
						return [embed];
					}
				}
			}
			Logger.debug({url: url.toString()}, 'Could not resolve as ActivityPub');
			return null;
		} catch (error) {
			Logger.error({error, url: url.toString()}, 'Failed to resolve ActivityPub URL');
			return null;
		}
	}
}
