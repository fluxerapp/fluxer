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
import {Logger} from '~/Logger';
import {BaseResolver} from '~/unfurler/resolvers/BaseResolver';
import {htmlToMarkdown} from '~/utils/DOMUtils';
import * as FetchUtils from '~/utils/FetchUtils';
import {parseString} from '~/utils/StringUtils';

interface HnItem {
	id: number;
	type: 'story' | 'comment' | 'job' | 'poll' | 'pollopt';
	by?: string;
	time: number;
	text?: string;
	dead?: boolean;
	deleted?: boolean;
	url?: string;
	title?: string;
	score?: number;
	descendants?: number;
	kids?: Array<number>;
	parent?: number;
	parts?: Array<number>;
	poll?: number;
}

export class HackerNewsResolver extends BaseResolver {
	private readonly API_BASE = 'https://hacker-news.firebaseio.com/v0';
	private readonly SITE_BASE = 'https://news.ycombinator.com';
	private readonly HN_COLOR = 0xff6600;
	private readonly HN_ICON = 'https://fluxerstatic.com/embeds/icons/hn.webp';
	private readonly MAX_DESCRIPTION_LENGTH = 400;

	match(url: URL, mimeType: string, _content: Uint8Array): boolean {
		return (
			url.hostname === 'news.ycombinator.com' && url.pathname.startsWith('/item') && mimeType.startsWith('text/html')
		);
	}

	async resolve(url: URL, _content: Uint8Array, _isNSFWAllowed: boolean = false): Promise<Array<MessageEmbedResponse>> {
		try {
			const itemId = new URLSearchParams(url.search).get('id');
			if (!itemId) return [];

			const item = await this.fetchItem(itemId);
			if (!item) return [];

			if (item.deleted || item.dead) {
				Logger.debug({itemId}, 'Skipping deleted or dead HN item');
				return [];
			}

			const embed = this.buildEmbed(item);
			return [embed];
		} catch (error) {
			Logger.error({error, url: url.toString()}, 'Failed to resolve Hacker News item');
			return [];
		}
	}

	private async fetchItem(itemId: string): Promise<HnItem | null> {
		try {
			const response = await FetchUtils.sendRequest({
				url: `${this.API_BASE}/item/${itemId}.json`,
				timeout: 5000,
			});

			if (response.status !== 200) {
				Logger.debug({itemId, status: response.status}, 'Failed to fetch HN item');
				return null;
			}

			const responseText = await FetchUtils.streamToString(response.stream);
			return JSON.parse(responseText) as HnItem;
		} catch (error) {
			Logger.error({error, itemId}, 'Failed to fetch or parse HN item');
			return null;
		}
	}

	private buildEmbed(item: HnItem): MessageEmbedResponse {
		const embed: MessageEmbedResponse = {
			type: 'rich',
			url: this.getItemUrl(item.id),
			color: this.HN_COLOR,
			timestamp: this.formatTimestamp(item.time),
			footer: {
				text: 'Hacker News',
				icon_url: this.HN_ICON,
			},
		};

		if (this.hasTitle(item) && item.title) {
			embed.title = parseString(item.title, 256);
		}

		if (item.by) {
			embed.author = {
				name: parseString(item.by, 256),
			};
		}

		const description = this.buildDescription(item);
		if (description) {
			embed.description = description;
		}

		return embed;
	}

	private buildDescription(item: HnItem): string | undefined {
		if (!item.text) return undefined;

		const markdown = htmlToMarkdown(item.text);
		const singleLine = markdown.replace(/\s+/g, ' ').trim();
		if (!singleLine) return undefined;

		return parseString(singleLine, this.MAX_DESCRIPTION_LENGTH);
	}

	private formatTimestamp(unixSeconds: number): string {
		return new Date(unixSeconds * 1000).toISOString();
	}

	private hasTitle(item: HnItem): boolean {
		return item.type === 'story' || item.type === 'job' || item.type === 'poll';
	}

	private getItemUrl(id: number): string {
		return `${this.SITE_BASE}/item?id=${id}`;
	}
}
