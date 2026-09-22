// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {connectionCredentialKey, readConnectionCredential} from '@app/api/connection/ConnectionCredentialRepository';
import {
	CONNECTION_WRITE_ATTEMPTS,
	ConnectionOwnerClosedError,
	connectionMembershipPatch,
	isConnectionMembershipRow,
	loadConnectionMembership,
	sealConnectionMembership,
} from '@app/api/connection/ConnectionMembership';
import {
	type ConnectionCreationResult,
	type ConnectionSortOrderUpdate,
	type CreateConnectionParams,
	IConnectionRepository,
	type UpdateConnectionParams,
} from '@app/api/connection/IConnectionRepository';
import {BatchBuilder, executeConditional, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {type ConditionalWriteEntry, Db, type DbOp} from '@app/api/database/CassandraTypes';
import {
	type RevisionedUserConnectionRow,
	USER_CONNECTION_COLUMNS,
	type UserConnectionRow,
	type UserConnectionStorageRow,
} from '@app/api/database/types/ConnectionTypes';
import {UserConnections} from '@app/api/Tables';
import {
	type ConnectionType,
	ConnectionTypes,
	MAX_CONNECTIONS_PER_USER,
} from '@fluxer/constants/src/ConnectionConstants';

const FETCH_CONNECTIONS_BY_USER_CQL = UserConnections.selectCql({
	columns: USER_CONNECTION_COLUMNS,
	where: [UserConnections.where.eq('user_id'), UserConnections.where.in('connection_type', 'connection_types')],
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
	where: [UserConnections.where.eq('user_id'), UserConnections.where.in('connection_type', 'connection_types')],
});
const FETCH_DELETION_CONNECTIONS_CQL = UserConnections.selectCql({
	where: UserConnections.where.eq('user_id'),
	limit: 101,
});

type ConnectionWrite = ConditionalWriteEntry<UserConnectionStorageRow, 'user_id' | 'connection_type' | 'connection_id'>;

function hasConnectionRevision(connection: UserConnectionRow): connection is RevisionedUserConnectionRow {
	return typeof connection.revision === 'string';
}

export class ConnectionRepository extends IConnectionRepository {
	async findByUserId(userId: UserID): Promise<Array<UserConnectionRow>> {
		return fetchMany<UserConnectionRow>(FETCH_CONNECTIONS_BY_USER_CQL, {
			user_id: userId,
			connection_types: Object.values(ConnectionTypes),
		});
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

	async create(params: CreateConnectionParams): Promise<ConnectionCreationResult> {
		const membership = await loadConnectionMembership(params.user_id);
		if (!membership) return {status: 'conflict'};
		if (membership.state === 'closed') throw new ConnectionOwnerClosedError();
		const identifier = params.identifier.toLowerCase();
		if (
			membership.entries.some(
				(entry) => entry.connection_type === params.connection_type && entry.identifier === identifier,
			)
		) {
			return {status: 'duplicate'};
		}
		if (membership.entries.length >= MAX_CONNECTIONS_PER_USER) return {status: 'limit_reached'};
		if (membership.entries.some((entry) => entry.connection_id === params.connection_id)) return {status: 'conflict'};
		const now = new Date();
		const row: RevisionedUserConnectionRow = {
			user_id: params.user_id,
			connection_id: params.connection_id,
			connection_type: params.connection_type,
			identifier: params.identifier,
			name: params.name,
			verified: params.verified ?? false,
			visibility_flags: params.visibility_flags,
			sort_order: params.sort_order ?? membership.entries.length,
			verification_token: params.verification_token,
			oauth_grant_id: params.oauth_grant_id ?? null,
			verified_at: params.verified_at ?? null,
			last_verified_at: params.last_verified_at ?? null,
			created_at: now,
			revision: randomUUID(),
			version: 1,
			membership: null,
			credential_payload: null,
			credential_expires_at: null,
		};
		const applied = await executeConditional(
			UserConnections.conditionalBatch([
				connectionMembershipPatch(membership, [
					...membership.entries,
					{
						connection_type: params.connection_type,
						identifier,
						connection_id: params.connection_id,
					},
				]),
				{action: 'insert', row},
			]),
		);
		return applied ? {status: 'created', connection: row} : {status: 'conflict'};
	}

	async ensureRevision(snapshot: UserConnectionRow): Promise<RevisionedUserConnectionRow | null> {
		if (hasConnectionRevision(snapshot)) return snapshot;
		const revised = {...snapshot, revision: randomUUID()};
		const applied = await this.writeWithOwner(snapshot.user_id, [
			{
				action: 'patch',
				pk: {
					user_id: snapshot.user_id,
					connection_type: snapshot.connection_type,
					connection_id: snapshot.connection_id,
				},
				patch: {revision: Db.set(revised.revision)},
				expected: {revision: null, created_at: snapshot.created_at},
			},
		]);
		return applied ? revised : null;
	}

	async update(
		snapshot: RevisionedUserConnectionRow,
		params: UpdateConnectionParams,
	): Promise<RevisionedUserConnectionRow | null> {
		const updated = {...snapshot, revision: randomUUID()};
		const patch: Record<string, DbOp<unknown>> = {revision: Db.set(updated.revision)};
		if (params.name !== undefined) {
			patch['name'] = Db.set(params.name);
			updated.name = params.name;
		}
		if (params.visibility_flags !== undefined) {
			patch['visibility_flags'] = Db.set(params.visibility_flags);
			updated.visibility_flags = params.visibility_flags;
		}
		if (params.sort_order !== undefined) {
			patch['sort_order'] = Db.set(params.sort_order);
			updated.sort_order = params.sort_order;
		}
		if (params.oauth_grant_id !== undefined) {
			patch['oauth_grant_id'] = Db.set(params.oauth_grant_id);
			updated.oauth_grant_id = params.oauth_grant_id;
		}
		if (params.verified !== undefined) {
			patch['verified'] = Db.set(params.verified);
			updated.verified = params.verified;
		}
		if (params.verified_at !== undefined) {
			patch['verified_at'] = Db.set(params.verified_at);
			updated.verified_at = params.verified_at;
		}
		if (params.last_verified_at !== undefined) {
			patch['last_verified_at'] = Db.set(params.last_verified_at);
			updated.last_verified_at = params.last_verified_at;
		}
		const applied = await this.writeWithOwner(snapshot.user_id, [
			{
				action: 'patch',
				pk: {
					user_id: snapshot.user_id,
					connection_type: snapshot.connection_type,
					connection_id: snapshot.connection_id,
				},
				patch,
				expected: {revision: snapshot.revision},
			},
		]);
		return applied ? updated : null;
	}

	async updateSortOrders(entries: ReadonlyArray<ConnectionSortOrderUpdate>): Promise<boolean> {
		if (entries.length === 0) return true;
		return this.writeWithOwner(
			entries[0].snapshot.user_id,
			entries.map(({snapshot, sortOrder}) => ({
				action: 'patch',
				pk: {
					user_id: snapshot.user_id,
					connection_type: snapshot.connection_type,
					connection_id: snapshot.connection_id,
				},
				patch: {sort_order: Db.set(sortOrder), revision: Db.set(randomUUID())},
				expected: {revision: snapshot.revision},
			})),
		);
	}

	async delete(snapshot: RevisionedUserConnectionRow): Promise<boolean> {
		const membership = await loadConnectionMembership(snapshot.user_id);
		if (!membership) return false;
		if (membership.state === 'closed') throw new ConnectionOwnerClosedError();
		const entries = membership.entries.filter(
			(entry) =>
				!(
					entry.connection_type === snapshot.connection_type &&
					entry.identifier === snapshot.identifier.toLowerCase() &&
					entry.connection_id === snapshot.connection_id
				),
		);
		if (entries.length === membership.entries.length) return false;
		const writes: Array<ConnectionWrite> = [
			connectionMembershipPatch(membership, entries),
			{
				action: 'delete',
				pk: {
					user_id: snapshot.user_id,
					connection_type: snapshot.connection_type,
					connection_id: snapshot.connection_id,
				},
				expected: {revision: snapshot.revision},
			},
		];
		if (snapshot.oauth_grant_id) {
			const owner = {userId: snapshot.user_id, grantId: snapshot.oauth_grant_id};
			const key = connectionCredentialKey(owner);
			const grant = await readConnectionCredential(owner);
			if (grant) {
				if (typeof grant.revision !== 'string') throw new Error('Connection credentials have no stored revision');
				writes.push({action: 'delete', pk: key, expected: {revision: grant.revision}});
			}
		}
		return executeConditional(UserConnections.conditionalBatch(writes));
	}

	async sealAndDeleteForUser(userId: UserID): Promise<void> {
		await sealConnectionMembership(userId);
		for (;;) {
			const rows = await fetchMany<UserConnectionStorageRow>(FETCH_DELETION_CONNECTIONS_CQL, {user_id: userId});
			const remaining = rows.filter((row) => !isConnectionMembershipRow(row));
			if (remaining.length === 0) return;
			const batch = new BatchBuilder();
			for (const row of remaining) {
				batch.addPrepared(
					UserConnections.deleteByPk({
						user_id: userId,
						connection_type: row.connection_type,
						connection_id: row.connection_id,
					}),
				);
			}
			await batch.execute();
		}
	}

	private async writeWithOwner(userId: UserID, writes: ReadonlyArray<ConnectionWrite>): Promise<boolean> {
		for (let attempt = 0; attempt < CONNECTION_WRITE_ATTEMPTS; attempt++) {
			const membership = await loadConnectionMembership(userId);
			if (!membership) continue;
			if (membership.state === 'closed') throw new ConnectionOwnerClosedError();
			const applied = await executeConditional(
				UserConnections.conditionalBatch([connectionMembershipPatch(membership, membership.entries), ...writes]),
			);
			if (applied) return true;
		}
		return false;
	}

	async count(userId: UserID): Promise<number> {
		const result = await fetchOne<{
			count: bigint;
		}>(COUNT_CONNECTIONS_CQL, {user_id: userId, connection_types: Object.values(ConnectionTypes)});
		return result ? Number(result.count) : 0;
	}
}
