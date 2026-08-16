// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {
	type ConnectionType,
	ConnectionTypes,
	ConnectionVisibilityFlags,
	MAX_CONNECTIONS_PER_USER,
} from '@fluxer/constants/src/ConnectionConstants';
import type {UserID} from '../BrandedTypes';
import type {IBlueskyOAuthService} from '../bluesky/IBlueskyOAuthService';
import type {ITraktOAuthService} from '../trakt/ITraktOAuthService';
import type {UserConnectionRow} from '../database/types/ConnectionTypes';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import {mapConnectionToResponse} from './ConnectionMappers';
import {createDomainConnectionId} from './DomainConnectionId';
import {BlueskyOAuthNotEnabledError} from './errors/BlueskyOAuthNotEnabledError';
import {TraktOAuthNotEnabledError} from './errors/TraktOAuthNotEnabledError';
import {ConnectionAlreadyExistsError} from './errors/ConnectionAlreadyExistsError';
import {ConnectionInvalidTypeError} from './errors/ConnectionInvalidTypeError';
import {ConnectionLimitReachedError} from './errors/ConnectionLimitReachedError';
import {ConnectionNotFoundError} from './errors/ConnectionNotFoundError';
import {ConnectionVerificationFailedError} from './errors/ConnectionVerificationFailedError';
import type {IConnectionRepository, UpdateConnectionParams} from './IConnectionRepository';
import {IConnectionService, type InitiateConnectionResult} from './IConnectionService';
import {BlueskyOAuthVerifier} from './verification/BlueskyOAuthVerifier';
import {DomainConnectionVerifier} from './verification/DomainConnectionVerifier';
import {TraktOAuthVerifier} from './verification/TraktOAuthVerifier';
import type {IConnectionVerifier} from './verification/IConnectionVerifier';

export class ConnectionService extends IConnectionService {
	constructor(
		private readonly repository: IConnectionRepository,
		private readonly gateway: IGatewayService,
		private readonly blueskyOAuthService: IBlueskyOAuthService,
		private readonly traktOAuthService: ITraktOAuthService,
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
		if (type === ConnectionTypes.TRAKT) {
			throw new TraktOAuthNotEnabledError();
		}
		if (type !== ConnectionTypes.DOMAIN) {
			throw new ConnectionInvalidTypeError();
		}
	}

	private async requireAvailableConnectionSlot(userId: UserID): Promise<number> {
		const count = await this.repository.count(userId);
		if (count >= MAX_CONNECTIONS_PER_USER) {
			throw new ConnectionLimitReachedError();
		}
		return count;
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
	): Promise<number> {
		this.assertConnectionTypeCanBeCreated(type);
		const count = await this.requireAvailableConnectionSlot(userId);
		await this.assertConnectionDoesNotExist(userId, type, identifier);
		return count;
	}

	private async dispatchConnectionsUpdate(userId: UserID): Promise<void> {
		const connections = await this.repository.findByUserId(userId);
		await this.gateway.dispatchPresence({
			userId,
			event: 'USER_CONNECTIONS_UPDATE',
			data: {connections: connections.map(mapConnectionToResponse)},
		});
	}

	async verifyAndCreateConnection(
		userId: UserID,
		type: ConnectionType,
		identifier: string,
		verificationCode: string,
		visibilityFlags: number,
	): Promise<UserConnectionRow> {
		const count = await this.requireConnectionCreationAllowed(userId, type, identifier);
		const verifier = this.getVerifier(type);
		const isValid = await verifier.verify({identifier, verification_token: verificationCode});
		if (!isValid) {
			throw new ConnectionVerificationFailedError();
		}
		const connectionId = createDomainConnectionId(userId, identifier);
		const sortOrder = count;
		const now = new Date();
		const created = await this.repository.create({
			user_id: userId,
			connection_id: connectionId,
			connection_type: type,
			identifier,
			name: identifier,
			visibility_flags: visibilityFlags,
			sort_order: sortOrder,
			verification_token: verificationCode,
			verified: true,
			verified_at: now,
			last_verified_at: now,
		});
		await this.dispatchConnectionsUpdate(userId);
		return created;
	}

	async updateConnection(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
		patch: UpdateConnectionParams,
	): Promise<void> {
		const connection = await this.repository.findById(userId, connectionType, connectionId);
		if (!connection) {
			throw new ConnectionNotFoundError();
		}
		await this.repository.update(userId, connectionType, connectionId, patch);
		await this.dispatchConnectionsUpdate(userId);
	}

	async deleteConnection(userId: UserID, connectionType: ConnectionType, connectionId: string): Promise<void> {
		const connection = await this.repository.findById(userId, connectionType, connectionId);
		if (!connection) {
			throw new ConnectionNotFoundError();
		}
		if (connectionType === ConnectionTypes.TRAKT) {
			await this.traktOAuthService.revoke(connection.identifier);
		}
		await this.repository.delete(userId, connectionType, connectionId);
		await this.dispatchConnectionsUpdate(userId);
	}

	async verifyConnection(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
	): Promise<UserConnectionRow> {
		const connection = await this.repository.findById(userId, connectionType, connectionId);
		if (!connection) {
			throw new ConnectionNotFoundError();
		}
		const {isValid, updateParams} = await this.revalidateConnection(connection);
		if (updateParams) {
			await this.repository.update(userId, connectionType, connectionId, updateParams);
		}
		const updated = await this.repository.findById(userId, connectionType, connectionId);
		if (!updated) {
			throw new ConnectionNotFoundError();
		}
		if (!isValid) {
			throw new ConnectionVerificationFailedError();
		}
		await this.dispatchConnectionsUpdate(userId);
		return updated;
	}

	async reorderConnections(userId: UserID, connectionIds: Array<string>): Promise<void> {
		const connections = await this.repository.findByUserId(userId);
		for (let i = 0; i < connectionIds.length; i++) {
			const connectionId = connectionIds[i];
			const connection = connections.find((c) => c.connection_id === connectionId);
			if (connection) {
				await this.repository.update(userId, connection.connection_type, connectionId, {
					sort_order: i,
				});
			}
		}
		await this.dispatchConnectionsUpdate(userId);
	}

	async createOrUpdateBlueskyConnection(userId: UserID, did: string, handle: string): Promise<UserConnectionRow> {
		const existing = await this.repository.findByTypeAndIdentifier(userId, ConnectionTypes.BLUESKY, did);
		if (existing) {
			const now = new Date();
			await this.repository.update(userId, ConnectionTypes.BLUESKY, existing.connection_id, {
				name: handle,
				verified: true,
				verified_at: existing.verified_at ?? now,
				last_verified_at: now,
			});
			const updated = await this.repository.findById(userId, ConnectionTypes.BLUESKY, existing.connection_id);
			await this.dispatchConnectionsUpdate(userId);
			return updated!;
		}
		const count = await this.requireAvailableConnectionSlot(userId);
		const connectionId = randomUUID();
		const now = new Date();
		const created = await this.repository.create({
			user_id: userId,
			connection_id: connectionId,
			connection_type: ConnectionTypes.BLUESKY,
			identifier: did,
			name: handle,
			visibility_flags: ConnectionVisibilityFlags.EVERYONE,
			sort_order: count,
			verification_token: '',
			verified: true,
			verified_at: now,
			last_verified_at: now,
		});
		await this.dispatchConnectionsUpdate(userId);
		return created;
	}

	async createOrUpdateTraktConnection(
		userId: UserID,
		traktUserId: string,
		username: string,
	): Promise<UserConnectionRow> {
		const existing = await this.repository.findByTypeAndIdentifier(userId, ConnectionTypes.TRAKT, traktUserId);
		if (existing) {
			const now = new Date();
			await this.repository.update(userId, ConnectionTypes.TRAKT, existing.connection_id, {
				name: username,
				verified: true,
				verified_at: existing.verified_at ?? now,
				last_verified_at: now,
			});
			const updated = await this.repository.findById(userId, ConnectionTypes.TRAKT, existing.connection_id);
			await this.dispatchConnectionsUpdate(userId);
			return updated!;
		}
		const count = await this.requireAvailableConnectionSlot(userId);
		const connectionId = randomUUID();
		const now = new Date();
		const created = await this.repository.create({
			user_id: userId,
			connection_id: connectionId,
			connection_type: ConnectionTypes.TRAKT,
			identifier: traktUserId,
			name: username,
			visibility_flags: ConnectionVisibilityFlags.EVERYONE,
			sort_order: count,
			verification_token: '',
			verified: true,
			verified_at: now,
			last_verified_at: now,
		});
		await this.dispatchConnectionsUpdate(userId);
		return created;
	}

	async revalidateConnection(connection: UserConnectionRow): Promise<{
		isValid: boolean;
		updateParams: UpdateConnectionParams | null;
	}> {
		const verifier = this.getVerifier(connection.connection_type);
		const isValid = await verifier.verify({
			identifier: connection.identifier,
			verification_token: connection.verification_token,
		});
		const now = new Date();
		if (!isValid && connection.verified) {
			return {
				isValid: false,
				updateParams: {
					verified: false,
					verified_at: null,
					last_verified_at: now,
				},
			};
		}
		if (isValid) {
			return {
				isValid: true,
				updateParams: {
					verified: true,
					verified_at: connection.verified_at ? connection.verified_at : now,
					last_verified_at: now,
				},
			};
		}
		return {isValid: false, updateParams: null};
	}

	private getVerifier(type: ConnectionType): IConnectionVerifier {
		if (type === ConnectionTypes.BLUESKY) {
			return new BlueskyOAuthVerifier(this.blueskyOAuthService);
		}
		if (type === ConnectionTypes.TRAKT) {
			return new TraktOAuthVerifier(this.traktOAuthService);
		}
		if (type === ConnectionTypes.DOMAIN) {
			return new DomainConnectionVerifier();
		}
		throw new ConnectionInvalidTypeError();
	}
}
