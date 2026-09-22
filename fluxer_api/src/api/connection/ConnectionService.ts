// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import type {BlueskyCallbackResult} from '@app/api/bluesky/IBlueskyOAuthService';
import {mapConnectionToResponse} from '@app/api/connection/ConnectionMappers';
import {createDomainConnectionId} from '@app/api/connection/DomainConnectionId';
import {BlueskyOAuthNotEnabledError} from '@app/api/connection/errors/BlueskyOAuthNotEnabledError';
import {ConnectionAlreadyExistsError} from '@app/api/connection/errors/ConnectionAlreadyExistsError';
import {ConnectionInvalidTypeError} from '@app/api/connection/errors/ConnectionInvalidTypeError';
import {ConnectionLimitReachedError} from '@app/api/connection/errors/ConnectionLimitReachedError';
import {ConnectionNotFoundError} from '@app/api/connection/errors/ConnectionNotFoundError';
import {ConnectionVerificationFailedError} from '@app/api/connection/errors/ConnectionVerificationFailedError';
import type {
	ConnectionSortOrderUpdate,
	CreateConnectionParams,
	IConnectionRepository,
	UpdateConnectionParams,
} from '@app/api/connection/IConnectionRepository';
import {IConnectionService, type InitiateConnectionResult} from '@app/api/connection/IConnectionService';
import {DomainConnectionVerifier} from '@app/api/connection/verification/DomainConnectionVerifier';
import type {RevisionedUserConnectionRow, UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	type ConnectionType,
	ConnectionTypes,
	ConnectionVisibilityFlags,
	MAX_CONNECTIONS_PER_USER,
} from '@fluxer/constants/src/ConnectionConstants';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';

export class ConnectionService extends IConnectionService {
	constructor(
		private readonly repository: IConnectionRepository,
		private readonly gateway: IGatewayService,
	) {
		super();
	}

	async getConnectionsForUser(userId: UserID): Promise<Array<UserConnectionRow>> {
		return this.repository.findByUserId(userId);
	}

	async initiateConnection(
		userId: UserID,
		type: ConnectionType,
		identifier: string,
	): Promise<InitiateConnectionResult> {
		await this.requireConnectionCreationAllowed(userId, type, identifier);
		return {};
	}

	private assertConnectionTypeCanBeCreated(type: ConnectionType): void {
		if (type === ConnectionTypes.BLUESKY) {
			throw new BlueskyOAuthNotEnabledError();
		}
		if (type !== ConnectionTypes.DOMAIN) {
			throw new ConnectionInvalidTypeError();
		}
	}

	private async requireAvailableConnectionSlot(userId: UserID): Promise<void> {
		const count = await this.repository.count(userId);
		if (count >= MAX_CONNECTIONS_PER_USER) {
			throw new ConnectionLimitReachedError();
		}
	}

	private async assertConnectionDoesNotExist(userId: UserID, type: ConnectionType, identifier: string): Promise<void> {
		const existing = await this.repository.findByTypeAndIdentifier(userId, type, identifier);
		if (existing) {
			throw new ConnectionAlreadyExistsError();
		}
	}

	private async requireConnectionCreationAllowed(
		userId: UserID,
		type: ConnectionType,
		identifier: string,
	): Promise<void> {
		this.assertConnectionTypeCanBeCreated(type);
		await this.requireAvailableConnectionSlot(userId);
		await this.assertConnectionDoesNotExist(userId, type, identifier);
	}

	private async createConnection(params: CreateConnectionParams): Promise<RevisionedUserConnectionRow> {
		const result = await this.repository.create(params);
		switch (result.status) {
			case 'duplicate':
				throw new ConnectionAlreadyExistsError();
			case 'limit_reached':
				throw new ConnectionLimitReachedError();
			case 'conflict':
				throw new ConflictError({code: APIErrorCodes.CONFLICT});
			case 'created':
				await this.dispatchConnectionsUpdate(params.user_id);
				return result.connection;
		}
	}

	private async dispatchConnectionsUpdate(userId: UserID): Promise<void> {
		const connections = await this.repository.findByUserId(userId);
		await this.gateway.dispatchPresence({
			userId,
			event: 'USER_CONNECTIONS_UPDATE',
			data: {connections: connections.map(mapConnectionToResponse)},
		});
	}

	private async requireConnection(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
	): Promise<UserConnectionRow> {
		const connection = await this.repository.findById(userId, connectionType, connectionId);
		if (!connection) throw new ConnectionNotFoundError();
		return connection;
	}

	private async rejectSupersededConnection(snapshot: UserConnectionRow): Promise<never> {
		await this.requireConnection(snapshot.user_id, snapshot.connection_type, snapshot.connection_id);
		throw new ConflictError({code: APIErrorCodes.CONFLICT});
	}

	private async requireRevision(snapshot: UserConnectionRow): Promise<RevisionedUserConnectionRow> {
		const connection = await this.repository.ensureRevision(snapshot);
		return connection ?? this.rejectSupersededConnection(snapshot);
	}

	async verifyAndCreateConnection(
		userId: UserID,
		type: ConnectionType,
		identifier: string,
		verificationCode: string,
		visibilityFlags: number,
	): Promise<UserConnectionRow> {
		await this.requireConnectionCreationAllowed(userId, type, identifier);
		const isValid = await new DomainConnectionVerifier().verify({identifier, verification_token: verificationCode});
		if (!isValid) {
			throw new ConnectionVerificationFailedError();
		}
		const connectionId = createDomainConnectionId(userId, identifier);
		const now = new Date();
		return this.createConnection({
			user_id: userId,
			connection_id: connectionId,
			connection_type: type,
			identifier,
			name: identifier,
			visibility_flags: visibilityFlags,
			verification_token: verificationCode,
			verified: true,
			verified_at: now,
			last_verified_at: now,
		});
	}

	async updateConnection(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
		patch: UpdateConnectionParams,
	): Promise<void> {
		const snapshot = await this.requireConnection(userId, connectionType, connectionId);
		const connection = await this.requireRevision(snapshot);
		const updated = await this.repository.update(connection, patch);
		if (!updated) return this.rejectSupersededConnection(connection);
		await this.dispatchConnectionsUpdate(userId);
	}

	async deleteConnection(userId: UserID, connectionType: ConnectionType, connectionId: string): Promise<void> {
		const snapshot = await this.requireConnection(userId, connectionType, connectionId);
		const connection = await this.requireRevision(snapshot);
		if (!(await this.repository.delete(connection))) return this.rejectSupersededConnection(connection);
		await this.dispatchConnectionsUpdate(userId);
	}

	async reorderConnections(userId: UserID, connectionIds: Array<string>): Promise<void> {
		const connections = await this.repository.findByUserId(userId);
		const byId = new Map(connections.map((connection) => [connection.connection_id, connection]));
		const positions = new Map<UserConnectionRow, number>();
		connectionIds.forEach((connectionId, sortOrder) => {
			const connection = byId.get(connectionId);
			if (connection) positions.set(connection, sortOrder);
		});
		const entries: Array<ConnectionSortOrderUpdate> = [];
		for (const [connection, sortOrder] of positions) {
			entries.push({snapshot: await this.requireRevision(connection), sortOrder});
		}
		if (!(await this.repository.updateSortOrders(entries))) throw new ConflictError({code: APIErrorCodes.CONFLICT});
		await this.dispatchConnectionsUpdate(userId);
	}

	async createOrUpdateBlueskyConnection(result: BlueskyCallbackResult): Promise<UserConnectionRow> {
		const {userId, did, handle, grantId} = result;
		const existing = await this.repository.findByTypeAndIdentifier(userId, ConnectionTypes.BLUESKY, did);
		if (existing) {
			const snapshot = await this.requireRevision(existing);
			const now = new Date();
			const updated = await this.repository.update(snapshot, {
				name: handle,
				oauth_grant_id: grantId,
				verified: true,
				verified_at: snapshot.verified_at ?? now,
				last_verified_at: now,
			});
			if (!updated) return this.rejectSupersededConnection(snapshot);
			await this.dispatchConnectionsUpdate(userId);
			return updated;
		}
		await this.requireAvailableConnectionSlot(userId);
		const connectionId = randomUUID();
		const now = new Date();
		return this.createConnection({
			user_id: userId,
			connection_id: connectionId,
			connection_type: ConnectionTypes.BLUESKY,
			identifier: did,
			name: handle,
			visibility_flags: ConnectionVisibilityFlags.EVERYONE,
			verification_token: '',
			oauth_grant_id: grantId,
			verified: true,
			verified_at: now,
			last_verified_at: now,
		});
	}
}
