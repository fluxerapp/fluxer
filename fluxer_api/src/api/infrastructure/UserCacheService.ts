// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DELETED_USER_DISCRIMINATOR,
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
} from '@fluxer/constants/src/UserConstants';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {UserID} from '../BrandedTypes';
import {Logger} from '../Logger';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {User} from '../models/User';
import {mapUserToPartialResponse} from '../user/UserMappers';
import type {IUsersServiceClient} from './UsersServiceClient';

type UserPartialResponseTimingRecorder = <T>(name: string, operation: () => Promise<T>) => Promise<T>;

interface UserPartialResponseResolutionOptions {
	timeStep?: UserPartialResponseTimingRecorder;
}

export class UserCacheService {
	constructor(private usersServiceClient: IUsersServiceClient) {}

	async getUserPartialResponse(userId: UserID, requestCache: RequestCache): Promise<UserPartialResponse> {
		const cached = requestCache.userPartials.get(userId);
		if (cached) {
			return cached;
		}
		const partials = await this.getUserPartialResponses([userId], requestCache);
		const userPartialResponse = partials.get(userId) ?? this.createDeletedUserPartialFallback(userId);
		requestCache.userPartials.set(userId, userPartialResponse);
		return userPartialResponse;
	}

	async invalidateUserCache(userId: UserID): Promise<void> {
		await this.usersServiceClient.invalidateUserCache(userId).catch((error) => {
			Logger.warn({userId: userId.toString(), error}, 'Failed to invalidate users service cache');
		});
	}

	async getUserPartialResponses(
		userIds: Array<UserID>,
		requestCache: RequestCache,
		options: UserPartialResponseResolutionOptions = {},
	): Promise<Map<UserID, UserPartialResponse>> {
		const timeStep: UserPartialResponseTimingRecorder = options.timeStep ?? ((_name, operation) => operation());
		const results = new Map<UserID, UserPartialResponse>();
		const missingFromRequestCache: Array<UserID> = [];
		const seenMissing = new Set<UserID>();
		for (const userId of userIds) {
			const cached = requestCache.userPartials.get(userId);
			if (cached) {
				results.set(userId, cached);
				continue;
			}
			if (!seenMissing.has(userId)) {
				seenMissing.add(userId);
				missingFromRequestCache.push(userId);
			}
		}
		if (missingFromRequestCache.length === 0) {
			return results;
		}
		const servicePartials = await timeStep('users_service_request', async () =>
			this.usersServiceClient.getUserPartialResponses(missingFromRequestCache),
		);
		for (const [userId, partial] of servicePartials) {
			results.set(userId, partial);
			requestCache.userPartials.set(userId, partial);
		}
		for (const userId of missingFromRequestCache) {
			if (results.has(userId)) {
				continue;
			}
			const deleted = this.createDeletedUserPartialFallback(userId);
			results.set(userId, deleted);
			requestCache.userPartials.set(userId, deleted);
		}
		return results;
	}

	async setUserPartialResponseFromUser(user: User, requestCache?: RequestCache): Promise<UserPartialResponse> {
		const response = mapUserToPartialResponse(user);
		requestCache?.userPartials.set(user.id, response);
		await this.invalidateUserCache(user.id);
		return response;
	}

	setUserPartialResponseFromUserInBackground(user: User, requestCache?: RequestCache): UserPartialResponse {
		const response = mapUserToPartialResponse(user);
		requestCache?.userPartials.set(user.id, response);
		void this.invalidateUserCache(user.id);
		return response;
	}

	setUserPartialResponseFromUserInRequestCache(user: User, requestCache: RequestCache): UserPartialResponse {
		const response = mapUserToPartialResponse(user);
		requestCache.userPartials.set(user.id, response);
		return response;
	}

	private createDeletedUserPartialFallback(userId: UserID): UserPartialResponse {
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
}
