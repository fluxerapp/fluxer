// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConnectionTypes, ConnectionVisibilityFlags} from '@fluxer/constants/src/ConnectionConstants';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {createUserID, type UserID} from '../../BrandedTypes';
import type {IBlueskyOAuthService} from '../../bluesky/IBlueskyOAuthService';
import {setCassandraQueryExecutorForTesting} from '../../database/CassandraQueryExecution';
import type {UserConnectionRow} from '../../database/types/ConnectionTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {InMemoryCassandraQueryExecutor} from '../../test/InMemoryCassandraQueryExecutor';
import {ConnectionRepository} from '../ConnectionRepository';
import {ConnectionService} from '../ConnectionService';
import {ConnectionVerificationFailedError} from '../errors/ConnectionVerificationFailedError';

function connectionRow(userId: UserID, connectionId: string, verified: boolean): UserConnectionRow {
	return {
		user_id: userId,
		connection_id: connectionId,
		connection_type: ConnectionTypes.BLUESKY,
		identifier: `did:plc:${connectionId}`,
		name: `${connectionId}.bsky.social`,
		verified,
		visibility_flags: ConnectionVisibilityFlags.EVERYONE,
		sort_order: 0,
		verification_token: '',
		verified_at: verified ? new Date(0) : null,
		last_verified_at: verified ? new Date(0) : null,
		created_at: new Date(0),
		version: 1,
	};
}

function failingBlueskyOAuthService(): IBlueskyOAuthService {
	return {
		restoreAndVerify: vi.fn().mockResolvedValue(null),
	} as unknown as IBlueskyOAuthService;
}

describe('ConnectionService.verifyConnection failed recheck', () => {
	let executor: InMemoryCassandraQueryExecutor;

	beforeEach(() => {
		executor = new InMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('publishes the connections update before rejecting a recheck that unverified the connection', async () => {
		const userId = createUserID(1n);
		const repository = new ConnectionRepository();
		await repository.create(connectionRow(userId, 'alice', true));
		const dispatchPresence = vi.fn().mockResolvedValue(undefined);
		const service = new ConnectionService(
			repository,
			{dispatchPresence} as unknown as IGatewayService,
			failingBlueskyOAuthService(),
		);

		const error = await service.verifyConnection(userId, ConnectionTypes.BLUESKY, 'alice').then(
			() => null,
			(caught: unknown) => caught,
		);

		expect(error).toBeInstanceOf(ConnectionVerificationFailedError);
		const response = (error as ConnectionVerificationFailedError).getResponse();
		expect(response.status).toBe(403);
		expect(await response.json()).toMatchObject({code: APIErrorCodes.CONNECTION_VERIFICATION_FAILED});

		expect(dispatchPresence).toHaveBeenCalledTimes(1);
		expect(dispatchPresence).toHaveBeenCalledWith({
			userId,
			event: 'USER_CONNECTIONS_UPDATE',
			data: {connections: [expect.objectContaining({id: 'alice', verified: false})]},
		});

		const stored = await repository.findById(userId, ConnectionTypes.BLUESKY, 'alice');
		expect(stored?.verified).toBe(false);
		expect(stored?.verified_at).toBeNull();
	});

	it('publishes the connections update when the recheck fails against an already unverified connection', async () => {
		const userId = createUserID(2n);
		const repository = new ConnectionRepository();
		await repository.create(connectionRow(userId, 'bob', false));
		const dispatchPresence = vi.fn().mockResolvedValue(undefined);
		const service = new ConnectionService(
			repository,
			{dispatchPresence} as unknown as IGatewayService,
			failingBlueskyOAuthService(),
		);

		await expect(service.verifyConnection(userId, ConnectionTypes.BLUESKY, 'bob')).rejects.toBeInstanceOf(
			ConnectionVerificationFailedError,
		);

		expect(dispatchPresence).toHaveBeenCalledTimes(1);
		expect(dispatchPresence).toHaveBeenCalledWith({
			userId,
			event: 'USER_CONNECTIONS_UPDATE',
			data: {connections: [expect.objectContaining({id: 'bob', verified: false})]},
		});
	});
});
