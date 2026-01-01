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
import type {BlueskyPostThread, BlueskyProfile, HandleResolution} from './BlueskyTypes';

export class BlueskyApiClient {
	private static readonly API_BASE = 'https://api.bsky.app/xrpc';

	constructor(private cacheService: ICacheService) {}

	async resolveDid(handle: string): Promise<string | null> {
		Logger.debug({handle}, 'Resolving handle to DID');
		if (handle.startsWith('did:')) {
			Logger.debug({handle}, 'Handle is already a DID');
			return handle;
		}

		try {
			const response = await FetchUtils.sendRequest({
				url: `${BlueskyApiClient.API_BASE}/com.atproto.identity.resolveHandle?handle=${encodeURIComponent(handle)}`,
				method: 'GET',
			});

			if (response.status !== 200) {
				Logger.debug({handle, status: response.status}, 'Failed to resolve handle to DID');
				return null;
			}

			const responseText = await FetchUtils.streamToString(response.stream);
			const resolution = JSON.parse(responseText) as HandleResolution;
			Logger.debug({handle, did: resolution.did}, 'Successfully resolved handle to DID');
			return resolution.did;
		} catch (error) {
			Logger.error({error, handle}, 'Failed to resolve handle to DID');
			return null;
		}
	}

	async getServiceEndpoint(did: string): Promise<string> {
		const cacheKey = `bluesky:service-endpoint:${did}`;
		const cached = await this.cacheService.get<string>(cacheKey);
		if (cached) return cached;

		try {
			let url: string;
			if (did.startsWith('did:web:')) {
				url = `https://${did.split(':')[2]}/.well-known/did.json`;
			} else {
				url = `https://plc.directory/${did}`;
			}

			const response = await FetchUtils.sendRequest({url, method: 'GET'});
			if (response.status !== 200) {
				Logger.debug({did, status: response.status}, 'Failed to fetch service endpoint');
				return 'https://bsky.social';
			}

			const responseText = await FetchUtils.streamToString(response.stream);
			const didDoc = JSON.parse(responseText);
			let serviceEndpoint = 'https://bsky.social';

			for (const service of didDoc.service || []) {
				if (service.type === 'AtprotoPersonalDataServer') {
					serviceEndpoint = service.serviceEndpoint;
					break;
				}
			}

			await this.cacheService.set(cacheKey, serviceEndpoint, 3600);
			Logger.debug({did, serviceEndpoint}, 'Retrieved and cached service endpoint');
			return serviceEndpoint;
		} catch (error) {
			Logger.error({error, did}, 'Failed to fetch service endpoint');
			return 'https://bsky.social';
		}
	}

	async fetchPost(atUri: string): Promise<BlueskyPostThread | null> {
		Logger.debug({atUri}, 'Fetching post');
		try {
			const response = await FetchUtils.sendRequest({
				url: `${BlueskyApiClient.API_BASE}/app.bsky.feed.getPostThread?uri=${encodeURIComponent(atUri)}&depth=0`,
				method: 'GET',
			});

			if (response.status !== 200) {
				Logger.debug({atUri, status: response.status}, 'Failed to fetch post');
				return null;
			}

			const responseText = await FetchUtils.streamToString(response.stream);
			const thread = JSON.parse(responseText) as BlueskyPostThread;
			Logger.debug(
				{
					atUri,
					author: thread.thread.post.author.handle,
					hasEmbed: !!thread.thread.post.embed,
					isReply: !!thread.thread.post.record.reply,
					hasParent: !!thread.thread.parent,
				},
				'Post fetched and parsed successfully',
			);
			return thread;
		} catch (error) {
			Logger.error({error, atUri}, 'Failed to fetch post');
			return null;
		}
	}

	async fetchProfile(handle: string): Promise<BlueskyProfile | null> {
		Logger.debug({handle}, 'Fetching profile');
		try {
			const response = await FetchUtils.sendRequest({
				url: `${BlueskyApiClient.API_BASE}/app.bsky.actor.getProfile?actor=${encodeURIComponent(handle)}`,
				method: 'GET',
			});

			if (response.status !== 200) {
				Logger.debug({handle, status: response.status}, 'Failed to fetch profile');
				return null;
			}

			const responseText = await FetchUtils.streamToString(response.stream);
			const profile = JSON.parse(responseText) as BlueskyProfile;
			Logger.debug(
				{handle, did: profile.did, hasAvatar: !!profile.avatar, hasBanner: !!profile.banner},
				'Profile fetched and parsed successfully',
			);
			return profile;
		} catch (error) {
			Logger.error({error, handle}, 'Failed to fetch profile');
			return null;
		}
	}
}
