// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {NatsUsersServiceClient} from '@app/api/infrastructure/UsersServiceClient';
import {getInstanceProductName} from '@app/api/instance/ProductName';
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
					instance_name: getInstanceProductName(),
				},
				timeout: 321,
			},
		]);
		expect(result.get(userId)).toEqual(partial);
	});
});
