// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConnectionTypes, ConnectionVisibilityFlags} from '@fluxer/constants/src/ConnectionConstants';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {createUserID, type UserID} from '../../BrandedTypes';
import type {IBlueskyOAuthService} from '../../bluesky/IBlueskyOAuthService';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '../../database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '../../database/CassandraTypes';
import type {UserConnectionRow} from '../../database/types/ConnectionTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {InMemoryCassandraQueryExecutor} from '../../test/InMemoryCassandraQueryExecutor';
import {ConnectionRepository} from '../ConnectionRepository';
import {ConnectionService} from '../ConnectionService';
import type {IConnectionRepository} from '../IConnectionRepository';

class BatchFaultInjectingExecutor implements CassandraQueryExecutorForTesting {
	failNextBatch = false;
	private readonly inner = new InMemoryCassandraQueryExecutor();

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		return this.inner.executeQuery<T>(query);
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic?: boolean,
	): Promise<void> {
		if (this.failNextBatch) {
			this.failNextBatch = false;
			throw new Error('batch rejected');
		}
		await this.inner.executeBatch(queries, atomic);
	}

	reset(): void {
		this.inner.reset();
	}
}

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

describe('ConnectionService.reorderConnections', () => {
	it('writes every sort order through a single batched repository call', async () => {
		const userId = createUserID(1n);
		const updateSortOrders = vi.fn().mockResolvedValue(undefined);
		const update = vi.fn().mockResolvedValue(undefined);
		const repository = {
			findByUserId: vi
				.fn()
				.mockResolvedValue([
					connectionRow(userId, 'a', 0),
					connectionRow(userId, 'b', 1),
					connectionRow(userId, 'c', 2),
				]),
			update,
			updateSortOrders,
		} as unknown as IConnectionRepository;
		const dispatchPresence = vi.fn().mockResolvedValue(undefined);
		const service = new ConnectionService(
			repository,
			{dispatchPresence} as unknown as IGatewayService,
			{} as unknown as IBlueskyOAuthService,
		);

		await service.reorderConnections(userId, ['c', 'missing', 'a', 'b']);

		expect(update).not.toHaveBeenCalled();
		expect(updateSortOrders).toHaveBeenCalledTimes(1);
		expect(updateSortOrders).toHaveBeenCalledWith(userId, [
			{connectionType: ConnectionTypes.DOMAIN, connectionId: 'c', sortOrder: 0},
			{connectionType: ConnectionTypes.DOMAIN, connectionId: 'a', sortOrder: 2},
			{connectionType: ConnectionTypes.DOMAIN, connectionId: 'b', sortOrder: 3},
		]);
		expect(dispatchPresence).toHaveBeenCalledTimes(1);
	});
});

describe('ConnectionService.reorderConnections batch failure', () => {
	let executor: BatchFaultInjectingExecutor;

	beforeEach(() => {
		executor = new BatchFaultInjectingExecutor();
		setCassandraQueryExecutorForTesting(executor);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('leaves every sort order untouched and publishes no dispatch when the batch fails', async () => {
		const userId = createUserID(1n);
		const repository = new ConnectionRepository();
		for (const [index, connectionId] of ['a', 'b', 'c'].entries()) {
			await repository.create(connectionRow(userId, connectionId, index));
		}
		const dispatchPresence = vi.fn().mockResolvedValue(undefined);
		const service = new ConnectionService(
			repository,
			{dispatchPresence} as unknown as IGatewayService,
			{} as unknown as IBlueskyOAuthService,
		);

		executor.failNextBatch = true;
		await expect(service.reorderConnections(userId, ['c', 'b', 'a'])).rejects.toThrow('batch rejected');

		const afterFailure = await repository.findByUserId(userId);
		expect(new Map(afterFailure.map((row) => [row.connection_id, row.sort_order]))).toEqual(
			new Map([
				['a', 0],
				['b', 1],
				['c', 2],
			]),
		);
		expect(dispatchPresence).not.toHaveBeenCalled();

		await service.reorderConnections(userId, ['c', 'b', 'a']);

		const afterRetry = await repository.findByUserId(userId);
		expect(new Map(afterRetry.map((row) => [row.connection_id, row.sort_order]))).toEqual(
			new Map([
				['c', 0],
				['b', 1],
				['a', 2],
			]),
		);
		expect(dispatchPresence).toHaveBeenCalledTimes(1);
	});
});
