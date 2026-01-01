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

import {URL} from 'node:url';
import {Config} from '~/Config';
import type {MessageEmbedResponse} from '~/channel/EmbedTypes';
import {Logger} from '~/Logger';
import {BaseResolver} from '~/unfurler/resolvers/BaseResolver';
import {buildEmbedMediaPayload} from '~/unfurler/resolvers/media/MediaMetadataHelpers';
import * as FetchUtils from '~/utils/FetchUtils';
import {parseString} from '~/utils/StringUtils';

interface YouTubeApiResponse {
	items?: Array<{
		snippet: {
			title: string;
			description: string;
			channelTitle: string;
			channelId: string;
			thumbnails: {
				maxres?: {
					url: string;
					width: number;
					height: number;
				};
				high: {
					url: string;
					width: number;
					height: number;
				};
			};
		};
		player: {
			embedHtml: string;
		};
		status: {
			uploadStatus?: string;
			privacyStatus?: string;
			embeddable?: boolean;
			publicStatsViewable?: boolean;
			madeForKids?: boolean;
		};
	}>;
}

export class YouTubeResolver extends BaseResolver {
	private readonly API_BASE = 'https://www.googleapis.com/youtube/v3';
	private readonly YOUTUBE_COLOR = 0xff0000;

	match(url: URL, _mimeType: string, _content: Uint8Array): boolean {
		if (!['www.youtube.com', 'youtube.com', 'youtu.be'].includes(url.hostname)) {
			return false;
		}
		return (
			url.pathname.startsWith('/watch') ||
			url.pathname.startsWith('/shorts') ||
			url.pathname.startsWith('/v/') ||
			url.hostname === 'youtu.be'
		);
	}

	async resolve(url: URL, _content: Uint8Array, isNSFWAllowed: boolean = false): Promise<Array<MessageEmbedResponse>> {
		if (!Config.youtube.apiKey) {
			Logger.debug('No Google API key configured');
			return [];
		}

		const videoId = this.extractVideoId(url);
		if (!videoId) {
			Logger.error('No video ID found in URL');
			return [];
		}

		try {
			const timestamp = this.extractTimestamp(url);
			const apiUrl = new URL(`${this.API_BASE}/videos`);
			apiUrl.searchParams.set('key', Config.youtube.apiKey);
			apiUrl.searchParams.set('id', videoId);
			apiUrl.searchParams.set('part', 'snippet,player,status');

			const response = await FetchUtils.sendRequest({
				url: apiUrl.toString(),
			});

			if (response.status !== 200) {
				Logger.error({videoId, status: response.status}, 'Failed to fetch YouTube API data');
				return [];
			}

			const responseText = await FetchUtils.streamToString(response.stream);
			const data = JSON.parse(responseText) as YouTubeApiResponse;
			const video = data.items?.[0];

			if (!video) {
				Logger.error({videoId}, 'No video data found');
				return [];
			}

			const thumbnailUrl = video.snippet.thumbnails.maxres?.url || video.snippet.thumbnails.high.url;
			const thumbnailMetadata = await this.mediaService.getMetadata({
				type: 'external',
				url: thumbnailUrl,
				isNSFWAllowed,
			});

			const embedHtmlMatch = video.player.embedHtml.match(/width="(\d+)"\s+height="(\d+)"/);
			const embedWidth = embedHtmlMatch ? Number.parseInt(embedHtmlMatch[1], 10) : 1280;
			const embedHeight = embedHtmlMatch ? Number.parseInt(embedHtmlMatch[2], 10) : 720;

			const mainUrl = new URL('https://www.youtube.com/watch');
			mainUrl.searchParams.set('v', videoId);
			if (timestamp !== undefined) {
				mainUrl.searchParams.set('start', timestamp.toString());
			}

			const embedUrl = new URL(`https://www.youtube.com/embed/${videoId}`);
			if (timestamp !== undefined) {
				embedUrl.searchParams.set('start', timestamp.toString());
			}

			const embed: MessageEmbedResponse = {
				type: 'video',
				url: mainUrl.toString(),
				title: video.snippet.title,
				description: parseString(video.snippet.description, 350),
				color: this.YOUTUBE_COLOR,
				author: video.snippet.channelTitle
					? {
							name: video.snippet.channelTitle,
							url: `https://www.youtube.com/channel/${video.snippet.channelId}`,
						}
					: undefined,
				provider: {
					name: 'YouTube',
					url: 'https://www.youtube.com',
				},
				thumbnail: buildEmbedMediaPayload(thumbnailUrl, thumbnailMetadata, {
					width: video.snippet.thumbnails.maxres?.width || video.snippet.thumbnails.high.width,
					height: video.snippet.thumbnails.maxres?.height || video.snippet.thumbnails.high.height,
				}),
				video: {
					url: embedUrl.toString(),
					width: embedWidth,
					height: embedHeight,
					flags: 0,
				},
			};
			return [embed];
		} catch (error) {
			Logger.error({error, videoId: this.extractVideoId(url)}, 'Failed to resolve YouTube URL');
			return [];
		}
	}

	private extractVideoId(url: URL): string {
		if (url.pathname.startsWith('/shorts/')) {
			return url.pathname.split('/shorts/')[1];
		}
		if (url.pathname.startsWith('/v/')) {
			return url.pathname.split('/v/')[1];
		}
		if (url.hostname === 'youtu.be') {
			return url.pathname.slice(1);
		}
		return url.searchParams.get('v') || '';
	}

	private extractTimestamp(url: URL): number | undefined {
		const tParam = url.searchParams.get('t');
		if (tParam) {
			if (tParam.endsWith('s')) {
				return Number.parseInt(tParam.slice(0, -1), 10);
			}
			return Number.parseInt(tParam, 10);
		}
		return;
	}
}
