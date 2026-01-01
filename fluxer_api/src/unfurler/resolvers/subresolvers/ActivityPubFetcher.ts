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

import type {ICacheService} from '~/infrastructure/ICacheService';
import {Logger} from '~/Logger';
import * as FetchUtils from '~/utils/FetchUtils';
import type {ActivityPubPost, MastodonInstance, MastodonPost} from './ActivityPubTypes';

export class ActivityPubFetcher {
	constructor(private cacheService: ICacheService) {}

	async fetchInstanceInfo(baseUrl: string): Promise<MastodonInstance | null> {
		const cacheKey = `activitypub:instance:${baseUrl}`;
		const cached = await this.cacheService.get<MastodonInstance>(cacheKey);
		if (cached) return cached;
		try {
			const apiUrl = `${baseUrl}/api/v2/instance`;
			Logger.debug({apiUrl}, 'Fetching instance info');
			const response = await FetchUtils.sendRequest({
				url: apiUrl,
				method: 'GET',
				timeout: 5000,
				headers: {Accept: 'application/json'},
			});
			if (response.status !== 200) {
				Logger.debug({apiUrl, status: response.status}, 'Instance info request failed');
				return null;
			}
			const data = await FetchUtils.streamToString(response.stream);
			const instanceInfo = JSON.parse(data) as MastodonInstance;
			await this.cacheService.set(cacheKey, JSON.stringify(instanceInfo), 3600);
			return instanceInfo;
		} catch (error) {
			Logger.error({error, baseUrl}, 'Failed to fetch instance info');
			return null;
		}
	}

	async tryFetchActivityPubData(url: string): Promise<ActivityPubPost | null> {
		try {
			const response = await FetchUtils.sendRequest({
				url,
				method: 'GET',
				timeout: 5000,
				headers: {
					Accept: 'application/activity+json, application/ld+json; profile="https://www.w3.org/ns/activitystreams"',
				},
			});
			if (response.status !== 200) {
				Logger.debug({url, status: response.status}, 'Failed to fetch ActivityPub data');
				return null;
			}
			const data = await FetchUtils.streamToString(response.stream);
			const parsedData = JSON.parse(data);
			if (!parsedData || typeof parsedData !== 'object' || !('id' in parsedData) || !('type' in parsedData)) {
				Logger.debug({url}, 'Response is not a valid ActivityPub object');
				return null;
			}
			return parsedData as ActivityPubPost;
		} catch (error) {
			Logger.error({error, url}, 'Failed to fetch or parse ActivityPub data');
			return null;
		}
	}

	async tryFetchMastodonApi(baseUrl: string, postId: string): Promise<MastodonPost | null> {
		try {
			const apiUrl = `${baseUrl}/api/v1/statuses/${postId}`;
			Logger.debug({apiUrl}, 'Attempting to fetch from Mastodon API');
			const response = await FetchUtils.sendRequest({
				url: apiUrl,
				method: 'GET',
				timeout: 5000,
				headers: {Accept: 'application/json'},
			});
			if (response.status !== 200) {
				Logger.debug({apiUrl, status: response.status}, 'Mastodon API request failed');
				return null;
			}
			const data = await FetchUtils.streamToString(response.stream);
			return JSON.parse(data) as MastodonPost;
		} catch (error) {
			Logger.error({error, baseUrl, postId}, 'Failed to fetch or parse Mastodon API data');
			return null;
		}
	}
}
