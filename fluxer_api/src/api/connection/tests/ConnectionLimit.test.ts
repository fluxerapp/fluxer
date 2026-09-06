// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	ConnectionTypes,
	ConnectionVisibilityFlags,
	MAX_CONNECTIONS_PER_USER,
} from '@fluxer/constants/src/ConnectionConstants';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createUserID, type UserID} from '../../BrandedTypes';
import type {IBlueskyOAuthService} from '../../bluesky/IBlueskyOAuthService';
import {setCassandraQueryExecutorForTesting} from '../../database/CassandraQueryExecution';
import type {UserConnectionRow} from '../../database/types/ConnectionTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {InMemoryCassandraQueryExecutor} from '../../test/InMemoryCassandraQueryExecutor';
import {ConnectionRepository} from '../ConnectionRepository';
import {ConnectionService} from '../ConnectionService';
import {ConnectionLimitReachedError} from '../errors/ConnectionLimitReachedError';

function connectionRow(userId: UserID, connectionId: string, sortOrder: number): UserConnectionRow {
	return {
		user_id: userId,
		connection_id: connectionId,
		connection_type: ConnectionTypes.DOMAIN,
		identifier: `${connectionId}.example`,
		name: connectionId,
		verified: true,
		visibility_flags: ConnectionVisibilityFlags.EVERYONE,
		sort_order: sortOrder,
		verification_token: 'token',
		verified_at: null,
		last_verified_at: null,
		created_at: new Date(0),
		version: 1,
	};
}

describe('ConnectionService connection limit', () => {
	let executor: InMemoryCassandraQueryExecutor;

	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('rejects the connection past the ceiling with 400 and no retry guidance', async () => {
		const userId = createUserID(1n);
		const repository = new ConnectionRepository();
		for (let index = 0; index < MAX_CONNECTIONS_PER_USER; index++) {
			await repository.create(connectionRow(userId, `connection-${index}`, index));
		}
		const service = new ConnectionService(
			repository,
			{} as unknown as IGatewayService,
			{} as unknown as IBlueskyOAuthService,
		);

		const error = await service.initiateConnection(userId, ConnectionTypes.DOMAIN, 'over-the-limit.example').then(
			() => null,
			(caught: unknown) => caught,
		);

		expect(error).toBeInstanceOf(ConnectionLimitReachedError);
		const response = (error as ConnectionLimitReachedError).getResponse();
		expect(response.status).toBe(400);
		expect(response.headers.get('Retry-After')).toBeNull();
		expect(await response.json()).toMatchObject({code: APIErrorCodes.CONNECTION_LIMIT_REACHED});
	});

	it('allows a connection while a slot is free', async () => {
		const userId = createUserID(2n);
		const repository = new ConnectionRepository();
		for (let index = 0; index < MAX_CONNECTIONS_PER_USER - 1; index++) {
			await repository.create(connectionRow(userId, `connection-${index}`, index));
		}
		const service = new ConnectionService(
			repository,
			{} as unknown as IGatewayService,
			{} as unknown as IBlueskyOAuthService,
		);

		await expect(
			service.initiateConnection(userId, ConnectionTypes.DOMAIN, 'under-the-limit.example'),
		).resolves.toEqual({});
	});
});
