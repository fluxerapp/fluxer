// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {NatsUsersServiceClient} from '@app/api/infrastructure/UsersServiceClient';
import {FakeNatsConnectionManager} from '@app/api/test/mocks/FakeNatsConnectionManager';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {describe, expect, it} from 'vitest';

function createConnectionManager(responses: Array<unknown>): FakeNatsConnectionManager {
	const pending = [...responses];
	return new FakeNatsConnectionManager(() => {
		const response = pending.shift();
		return response instanceof Error ? response : JSON.stringify(response);
	});
}

describe('NatsUsersServiceClient', () => {
	it('requests API user partials with string snowflakes and maps the response by id', async () => {
		const userId = createUserID(9223372036854775807n);
		const partial: UserPartialResponse = {
			id: userId.toString(),
			username: 'Ada',
			discriminator: '0007',
			global_name: 'Ada Lovelace',
			avatar: 'avatar_hash',
			avatar_color: 0x336699,
			flags: 1,
		};
		const manager = createConnectionManager([{FoundApiPartials: [partial]}]);
		const client = new NatsUsersServiceClient(manager, 321, 'svc.users.test');

		const result = await client.getUserPartialResponses([userId]);

		expect(manager.connectCalls).toBe(1);
		expect(manager.requests).toEqual([
			{
				subject: 'svc.users.test',
				body: {
					op: 'GetApiPartialsByIds',
					user_ids: ['9223372036854775807'],
				},
				timeout: 321,
			},
		]);
		expect(result.get(userId)).toEqual(partial);
	});

	it('sends invalidation requests with a string user id', async () => {
		const userId = createUserID(9007199254740993n);
		const manager = createConnectionManager(['Invalidated']);
		const client = new NatsUsersServiceClient(manager, 250, 'svc.users.test');

		await client.invalidateUserCache(userId);

		expect(manager.requests).toEqual([
			{
				subject: 'svc.users.test',
				body: {
					op: 'Invalidate',
					user_id: '9007199254740993',
				},
				timeout: 250,
			},
		]);
	});

	it('coalesces concurrent overlapping partial lookups without retaining a completed cache entry', async () => {
		const userId = createUserID(9223372036854775807n);
		const secondUserId = createUserID(9223372036854775806n);
		const partial: UserPartialResponse = {
			id: userId.toString(),
			username: 'Cached',
			discriminator: '0001',
			global_name: null,
			avatar: null,
			avatar_color: null,
			flags: 0,
		};
		const secondPartial: UserPartialResponse = {
			...partial,
			id: secondUserId.toString(),
			username: 'Second',
		};
		const manager = createConnectionManager([
			{FoundApiPartials: [secondPartial, partial]},
			{FoundApiPartials: [partial]},
		]);
		const client = new NatsUsersServiceClient(manager, 250, 'svc.users.test', 100);

		const [first, second] = await Promise.all([
			client.getUserPartialResponses([secondUserId, userId, userId]),
			client.getUserPartialResponses([userId]),
		]);
		await client.getUserPartialResponses([userId]);

		expect(manager.requests).toHaveLength(2);
		expect(first.get(userId)).toEqual(partial);
		expect(first.get(secondUserId)).toEqual(secondPartial);
		expect(second.get(userId)).toEqual(partial);
	});
});
