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

import type {UserID} from '@fluxer/api/src/BrandedTypes';
import {Logger} from '@fluxer/api/src/Logger';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {User} from '@fluxer/api/src/models/User';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {mapUserToPartialResponse} from '@fluxer/api/src/user/UserMappers';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {Coalescer} from '@fluxer/cache/src/utils/Coalescer';
import {
	DELETED_USER_DISCRIMINATOR,
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
} from '@fluxer/constants/src/UserConstants';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {seconds} from 'itty-time';

const USER_PARTIAL_CACHE_TTL = seconds('5 minutes');

export class UserCacheService {
	private coalescer = new Coalescer();

	constructor(
		public readonly cacheService: ICacheService,
		private userRepository: IUserRepository,
	) {}

	private getUserPartialCacheKey(userId: UserID): string {
		return `user:partial:${userId}`;
	}

	private getDeprecatedIncludeDeletedUserPartialCacheKey(userId: UserID): string {
		// Kept for cleanup only. We no longer write this variant key.
		return `user:partial:include_deleted:${userId}`;
	}

	async getUserPartialResponse(userId: UserID, requestCache: RequestCache): Promise<UserPartialResponse> {
		const cached = requestCache.userPartials.get(userId);
		if (cached) {
			return cached;
		}

		const cacheKey = this.getUserPartialCacheKey(userId);
		const kvCached = await this.cacheService.getAndRenewTtl<UserPartialResponse>(cacheKey, USER_PARTIAL_CACHE_TTL);

		if (kvCached) {
			requestCache.userPartials.set(userId, kvCached);
			return kvCached;
		}

		const userPartialResponse = await this.coalescer.coalesce(cacheKey, async () => {
			const user = await this.userRepository.findUnique(userId);
			if (!user) {
				Logger.warn({userId}, 'User not found during partial resolution, returning deleted user fallback');
				return {
					id: userId.toString(),
					username: DELETED_USER_USERNAME,
					discriminator: DELETED_USER_DISCRIMINATOR.toString().padStart(4, '0'),
					global_name: DELETED_USER_GLOBAL_NAME,
					avatar: null,
					avatar_color: null,
					flags: 0,
				};
			}
			return mapUserToPartialResponse(user);
		});

		await this.cacheService.set(cacheKey, userPartialResponse, USER_PARTIAL_CACHE_TTL);
		requestCache.userPartials.set(userId, userPartialResponse);
		return userPartialResponse;
	}

	async invalidateUserCache(userId: UserID): Promise<void> {
		await Promise.all([
			this.cacheService.delete(this.getUserPartialCacheKey(userId)),
			this.cacheService.delete(this.getDeprecatedIncludeDeletedUserPartialCacheKey(userId)),
		]);
	}

	async getUserPartialResponses(
		userIds: Array<UserID>,
		requestCache: RequestCache,
	): Promise<Map<UserID, UserPartialResponse>> {
		const results = new Map<UserID, UserPartialResponse>();

		const promises = userIds.map(async (userId) => {
			const userResponse = await this.getUserPartialResponse(userId, requestCache);
			results.set(userId, userResponse);
		});

		await Promise.all(promises);
		return results;
	}

	async setUserPartialResponseFromUser(user: User, requestCache?: RequestCache): Promise<UserPartialResponse> {
		const response = mapUserToPartialResponse(user);
		requestCache?.userPartials.set(user.id, response);
		const cacheKey = this.getUserPartialCacheKey(user.id);
		await this.cacheService.set(cacheKey, response, USER_PARTIAL_CACHE_TTL);
		return response;
	}

	setUserPartialResponseFromUserInBackground(user: User, requestCache?: RequestCache): UserPartialResponse {
		const response = mapUserToPartialResponse(user);
		requestCache?.userPartials.set(user.id, response);
		const cacheKey = this.getUserPartialCacheKey(user.id);
		Promise.resolve(this.cacheService.set(cacheKey, response, USER_PARTIAL_CACHE_TTL)).catch((error) =>
			Logger.error({error, cacheKey, userId: user.id}, 'Failed to set user partial cache'),
		);
		return response;
	}
}
