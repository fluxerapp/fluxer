// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {ConnectionRepository} from '@app/api/connection/ConnectionRepository';
import {ConnectionService} from '@app/api/connection/ConnectionService';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import type {UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {ConnectionTypes, ConnectionVisibilityFlags} from '@fluxer/constants/src/ConnectionConstants';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

class ConditionalBatchFaultExecutor implements CassandraQueryExecutorForTesting {
	failNextBatch = false;
	private readonly inner = new InMemoryCassandraQueryExecutor();

	async executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>> {
		if (this.failNextBatch && query.kvMeta?.action === 'batch' && query.kvMeta.batchEntries) {
			this.failNextBatch = false;
			throw new Error('batch rejected');
		}
		return this.inner.executeQuery<T>(query);
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic?: boolean,
	): Promise<void> {
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
	const userId = createUserID(1n);
	const dispatchPresence = vi.fn().mockResolvedValue(undefined);
	let executor: ConditionalBatchFaultExecutor;
	let repository: ConnectionRepository;
	let service: ConnectionService;

	beforeEach(async () => {
		executor = new ConditionalBatchFaultExecutor();
		setCassandraQueryExecutorForTesting(executor);
		repository = new ConnectionRepository();
		for (const [index, connectionId] of ['a', 'b', 'c'].entries()) {
			await repository.create(connectionRow(userId, connectionId, index));
		}
		dispatchPresence.mockClear();
		service = new ConnectionService(repository, {dispatchPresence} as unknown as IGatewayService);
	});

	afterEach(() => {
		executor.reset();
		setCassandraQueryExecutorForTesting(null);
	});

	it('writes every sort order through a single batched repository call', async () => {
		const updateSortOrders = vi.spyOn(repository, 'updateSortOrders');
		await service.reorderConnections(userId, ['c', 'missing', 'a', 'b']);

		expect(updateSortOrders).toHaveBeenCalledTimes(1);
		const connections = await repository.findByUserId(userId);
		expect(new Map(connections.map((row) => [row.connection_id, row.sort_order]))).toEqual(
			new Map([
				['c', 0],
				['a', 2],
				['b', 3],
			]),
		);
		expect(dispatchPresence).toHaveBeenCalledTimes(1);
	});

	it('leaves every sort order untouched and publishes no dispatch when the batch fails', async () => {
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
