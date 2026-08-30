// SPDX-License-Identifier: AGPL-3.0-or-later

import {DELETED_USER_GLOBAL_NAME, DELETED_USER_USERNAME} from '@fluxer/constants/src/UserConstants';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {describe, expect, it} from 'vitest';
import {createUserID, type UserID} from '../BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '../database/types/UserTypes';
import {createRequestCache} from '../middleware/RequestCacheMiddleware';
import {User} from '../models/User';
import {UserCacheService} from './UserCacheService';
import type {IUsersServiceClient} from './UsersServiceClient';

class FakeUsersServiceClient implements IUsersServiceClient {
	readonly requests: Array<Array<UserID>> = [];
	readonly invalidated: Array<UserID> = [];
	shouldFail = false;

	constructor(private readonly partialsById = new Map<UserID, UserPartialResponse>()) {}

	async getUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>> {
		this.requests.push([...userIds]);
		if (this.shouldFail) {
			throw new Error('user partial source unavailable');
		}
		const result = new Map<UserID, UserPartialResponse>();
		for (const userId of userIds) {
			const partial = this.partialsById.get(userId);
			if (partial) {
				result.set(userId, partial);
			}
		}
		return result;
	}

	async invalidateUserCache(userId: UserID): Promise<void> {
		this.invalidated.push(userId);
	}
}

function createPartial(userId: UserID, username: string): UserPartialResponse {
	return {
		id: userId.toString(),
		username,
		discriminator: '0001',
		global_name: null,
		avatar: null,
		avatar_color: null,
		flags: 0,
	};
}

function createUser(userId: UserID, username: string): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: userId,
		username,
		discriminator: 42,
		global_name: `${username} Global`,
		bot: true,
		flags: 0n,
		version: 1,
	} satisfies UserRow);
}

describe('UserCacheService', () => {
	it('uses the users service for request-cache misses in one bulk call', async () => {
		const userId1 = createUserID(1001n);
		const userId2 = createUserID(1002n);
		const partial1 = createPartial(userId1, 'CacheOne');
		const partial2 = createPartial(userId2, 'CacheTwo');
		const usersServiceClient = new FakeUsersServiceClient(
			new Map([
				[userId1, partial1],
				[userId2, partial2],
			]),
		);
		const service = new UserCacheService(usersServiceClient);
		const requestCache = createRequestCache();

		const result = await service.getUserPartialResponses([userId1, userId2, userId1], requestCache);

		expect(usersServiceClient.requests).toEqual([[userId1, userId2]]);
		expect(result.get(userId1)).toBe(partial1);
		expect(result.get(userId2)).toBe(partial2);
		expect(requestCache.userPartials.get(userId1)).toBe(partial1);

		await service.getUserPartialResponses([userId1, userId2], requestCache);
		expect(usersServiceClient.requests).toHaveLength(1);
	});

	it('returns deleted-user partials for users the service reports as missing', async () => {
		const serviceHitId = createUserID(2001n);
		const missingId = createUserID(2002n);
		const servicePartial = createPartial(serviceHitId, 'FromUsersService');
		const usersServiceClient = new FakeUsersServiceClient(new Map([[serviceHitId, servicePartial]]));
		const service = new UserCacheService(usersServiceClient);

		const result = await service.getUserPartialResponses([serviceHitId, missingId], createRequestCache());

		expect(usersServiceClient.requests).toEqual([[serviceHitId, missingId]]);
		expect(result.get(serviceHitId)).toBe(servicePartial);
		expect(result.get(missingId)).toMatchObject({
			id: missingId.toString(),
			username: DELETED_USER_USERNAME,
			global_name: DELETED_USER_GLOBAL_NAME,
			flags: 0,
		});
	});

	it('records only the users-service timing boundary', async () => {
		const userId = createUserID(2501n);
		const servicePartial = createPartial(userId, 'TimedUsersService');
		const usersServiceClient = new FakeUsersServiceClient(new Map([[userId, servicePartial]]));
		const service = new UserCacheService(usersServiceClient);
		const timedSteps: Array<string> = [];

		await service.getUserPartialResponses([userId], createRequestCache(), {
			timeStep: async (name, operation) => {
				timedSteps.push(name);
				return await operation();
			},
		});

		expect(timedSteps).toEqual(['users_service_request']);
	});

	it('surfaces user partial source failures', async () => {
		const userId = createUserID(3001n);
		const usersServiceClient = new FakeUsersServiceClient();
		usersServiceClient.shouldFail = true;
		const service = new UserCacheService(usersServiceClient);

		await expect(service.getUserPartialResponse(userId, createRequestCache())).rejects.toThrow(
			'user partial source unavailable',
		);
		expect(usersServiceClient.requests).toEqual([[userId]]);
	});

	it('seeds only the request cache when mapping a user on a read path', () => {
		const userId = createUserID(4501n);
		const usersServiceClient = new FakeUsersServiceClient();
		const service = new UserCacheService(usersServiceClient);
		const user = createUser(userId, 'ReadUser');
		const requestCache = createRequestCache();

		const partial = service.setUserPartialResponseFromUserInRequestCache(user, requestCache);

		expect(partial.username).toBe('ReadUser');
		expect(requestCache.userPartials.get(userId)).toBe(partial);
		expect(usersServiceClient.invalidated).toEqual([]);
		expect(usersServiceClient.requests).toEqual([]);
	});

	it('invalidates the users service cache when seeding a partial from a user update', async () => {
		const userId = createUserID(4001n);
		const usersServiceClient = new FakeUsersServiceClient();
		const service = new UserCacheService(usersServiceClient);
		const user = createUser(userId, 'UpdatedUser');
		const requestCache = createRequestCache();

		const partial = await service.setUserPartialResponseFromUser(user, requestCache);

		expect(partial.username).toBe('UpdatedUser');
		expect(requestCache.userPartials.get(userId)).toBe(partial);
		expect(usersServiceClient.invalidated).toEqual([userId]);
	});
});
