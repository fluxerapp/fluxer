// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {UserID} from '../BrandedTypes';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {User} from '../models/User';

export async function getCachedUserPartialResponse(params: {
	userId: UserID;
	userCacheService: Pick<UserCacheService, 'getUserPartialResponse'>;
	requestCache: RequestCache;
}): Promise<UserPartialResponse> {
	const {userId, userCacheService, requestCache} = params;
	return await userCacheService.getUserPartialResponse(userId, requestCache);
}

export async function getCachedUserPartialResponses(params: {
	userIds: Array<UserID>;
	userCacheService: Pick<UserCacheService, 'getUserPartialResponses'>;
	requestCache: RequestCache;
}): Promise<Map<UserID, UserPartialResponse>> {
	const {userIds, userCacheService, requestCache} = params;
	return await userCacheService.getUserPartialResponses(userIds, requestCache);
}

export function mapUserToPartialResponseWithCache(params: {
	user: User;
	userCacheService: Pick<UserCacheService, 'setUserPartialResponseFromUserInRequestCache'>;
	requestCache: RequestCache;
}): UserPartialResponse {
	const {user, userCacheService, requestCache} = params;
	const cached = requestCache.userPartials.get(user.id);
	if (cached) {
		return cached;
	}
	return userCacheService.setUserPartialResponseFromUserInRequestCache(user, requestCache);
}

export async function invalidateUserCache(params: {
	userId: UserID;
	userCacheService: Pick<UserCacheService, 'invalidateUserCache'>;
}): Promise<void> {
	const {userId, userCacheService} = params;
	await userCacheService.invalidateUserCache(userId);
}

export async function updateUserCache(params: {
	user: User;
	userCacheService: Pick<UserCacheService, 'setUserPartialResponseFromUser'>;
}): Promise<void> {
	const {user, userCacheService} = params;
	await userCacheService.setUserPartialResponseFromUser(user);
}
