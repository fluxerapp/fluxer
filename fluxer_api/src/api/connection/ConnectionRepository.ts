// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';
import type {UserID} from '../BrandedTypes';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '../database/CassandraQueryExecution';
import {Db, type DbOp} from '../database/CassandraTypes';
import type {UserConnectionRow} from '../database/types/ConnectionTypes';
import {UserConnections} from '../Tables';
import {type CreateConnectionParams, IConnectionRepository, type UpdateConnectionParams} from './IConnectionRepository';

const FETCH_CONNECTIONS_BY_USER_CQL = UserConnections.selectCql({
	where: UserConnections.where.eq('user_id'),
});
const FETCH_CONNECTION_BY_ID_CQL = UserConnections.selectCql({
	where: [
		UserConnections.where.eq('user_id'),
		UserConnections.where.eq('connection_type'),
		UserConnections.where.eq('connection_id'),
	],
	limit: 1,
});
const COUNT_CONNECTIONS_CQL = UserConnections.selectCountCql({
	where: UserConnections.where.eq('user_id'),
});

export class ConnectionRepository extends IConnectionRepository {
	async findByUserId(userId: UserID): Promise<Array<UserConnectionRow>> {
		return fetchMany<UserConnectionRow>(FETCH_CONNECTIONS_BY_USER_CQL, {user_id: userId});
	}

	async findById(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
	): Promise<UserConnectionRow | null> {
		return fetchOne<UserConnectionRow>(FETCH_CONNECTION_BY_ID_CQL, {
			user_id: userId,
			connection_type: connectionType,
			connection_id: connectionId,
		});
	}

	async findByTypeAndIdentifier(
		userId: UserID,
		connectionType: ConnectionType,
		identifier: string,
	): Promise<UserConnectionRow | null> {
		const connections = await this.findByUserId(userId);
		return (
			connections.find(
				(c) => c.connection_type === connectionType && c.identifier.toLowerCase() === identifier.toLowerCase(),
			) ?? null
		);
	}

	async create(params: CreateConnectionParams): Promise<UserConnectionRow> {
		const now = new Date();
		const row: UserConnectionRow = {
			user_id: params.user_id,
			connection_id: params.connection_id,
			connection_type: params.connection_type,
			identifier: params.identifier,
			name: params.name,
			verified: params.verified ?? false,
			visibility_flags: params.visibility_flags,
			sort_order: params.sort_order,
			verification_token: params.verification_token,
			verified_at: params.verified_at ?? null,
			last_verified_at: params.last_verified_at ?? null,
			created_at: now,
			version: 1,
		};
		await upsertOne(UserConnections.upsertAll(row));
		return row;
	}

	async update(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
		params: UpdateConnectionParams,
	): Promise<void> {
		const patch: Record<string, DbOp<unknown>> = {};
		if (params.name !== undefined) {
			patch['name'] = Db.set(params.name);
		}
		if (params.visibility_flags !== undefined) {
			patch['visibility_flags'] = Db.set(params.visibility_flags);
		}
		if (params.sort_order !== undefined) {
			patch['sort_order'] = Db.set(params.sort_order);
		}
		if (params.verified !== undefined) {
			patch['verified'] = Db.set(params.verified);
		}
		if (params.verified_at !== undefined) {
			patch['verified_at'] = Db.set(params.verified_at);
		}
		if (params.last_verified_at !== undefined) {
			patch['last_verified_at'] = Db.set(params.last_verified_at);
		}
		if (Object.keys(patch).length > 0) {
			await upsertOne(
				UserConnections.patchByPk(
					{user_id: userId, connection_type: connectionType, connection_id: connectionId},
					patch,
				),
			);
		}
	}

	async updateSortOrders(
		userId: UserID,
		entries: Array<{connectionType: ConnectionType; connectionId: string; sortOrder: number}>,
	): Promise<void> {
		const batch = new BatchBuilder();
		for (const entry of entries) {
			batch.addPrepared(
				UserConnections.patchByPk(
					{user_id: userId, connection_type: entry.connectionType, connection_id: entry.connectionId},
					{sort_order: Db.set(entry.sortOrder)},
				),
			);
		}
		await batch.execute();
	}

	async delete(userId: UserID, connectionType: ConnectionType, connectionId: string): Promise<void> {
		await deleteOneOrMany(
			UserConnections.deleteByPk({user_id: userId, connection_type: connectionType, connection_id: connectionId}),
		);
	}

	async count(userId: UserID): Promise<number> {
		const result = await fetchOne<{
			count: bigint;
		}>(COUNT_CONNECTIONS_CQL, {user_id: userId});
		return result ? Number(result.count) : 0;
	}
}
