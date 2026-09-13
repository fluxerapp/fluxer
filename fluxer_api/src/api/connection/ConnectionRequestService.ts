// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {signInitiationToken, verifyInitiationToken} from '@app/api/connection/ConnectionInitiationToken';
import {mapConnectionToResponse} from '@app/api/connection/ConnectionMappers';
import {ConnectionInitiationTokenInvalidError} from '@app/api/connection/errors/ConnectionInitiationTokenInvalidError';
import type {IConnectionService} from '@app/api/connection/IConnectionService';
import {
	CONNECTION_INITIATION_TOKEN_EXPIRY_MS,
	CONNECTION_VERIFICATION_TOKEN_LENGTH,
	type ConnectionType,
	ConnectionTypes,
} from '@fluxer/constants/src/ConnectionConstants';
import type {
	ConnectionResponse,
	ConnectionVerificationResponse,
	CreateConnectionRequest,
	UpdateConnectionRequest,
	VerifyAndCreateConnectionRequest,
} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';

export class ConnectionRequestService {
	constructor(
		private readonly connectionService: IConnectionService,
		private readonly connectionInitiationSecret: string,
	) {}

	async listConnections(userId: UserID): Promise<Array<ConnectionResponse>> {
		const rows = await this.connectionService.getConnectionsForUser(userId);
		return rows.sort((a, b) => a.sort_order - b.sort_order).map((row) => mapConnectionToResponse(row));
	}

	async initiateConnection(userId: UserID, body: CreateConnectionRequest): Promise<ConnectionVerificationResponse> {
		await this.connectionService.initiateConnection(userId, body.type, body.identifier);
		const verificationCode = this.generateVerificationCode(userId, body.type, body.identifier);
		const instructions = this.generateVerificationInstructions(body.type, body.identifier);
		const initiationToken = signInitiationToken(
			{
				userId: String(userId),
				type: body.type,
				identifier: body.identifier,
				verificationCode,
				expiresAt: Date.now() + CONNECTION_INITIATION_TOKEN_EXPIRY_MS,
			},
			this.connectionInitiationSecret,
		);
		return {
			token: verificationCode,
			type: body.type,
			id: body.identifier,
			instructions,
			initiation_token: initiationToken,
		};
	}

	async verifyAndCreateConnection(userId: UserID, body: VerifyAndCreateConnectionRequest): Promise<ConnectionResponse> {
		const payload = verifyInitiationToken(body.initiation_token, this.connectionInitiationSecret);
		if (!payload || payload.userId !== String(userId)) {
			throw new ConnectionInitiationTokenInvalidError();
		}
		const row = await this.connectionService.verifyAndCreateConnection(
			userId,
			payload.type,
			payload.identifier,
			payload.verificationCode,
			body.visibility_flags ?? 1,
		);
		return mapConnectionToResponse(row);
	}

	async updateConnection(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
		body: UpdateConnectionRequest,
	): Promise<void> {
		await this.connectionService.updateConnection(userId, connectionType, connectionId, body);
	}

	async deleteConnection(userId: UserID, connectionType: ConnectionType, connectionId: string): Promise<void> {
		await this.connectionService.deleteConnection(userId, connectionType, connectionId);
	}

	async reorderConnections(userId: UserID, connectionIds: Array<string>): Promise<void> {
		await this.connectionService.reorderConnections(userId, connectionIds);
	}

	private generateVerificationInstructions(connectionType: ConnectionType, identifier: string): string {
		switch (connectionType) {
			case ConnectionTypes.DOMAIN:
				return `Add a DNS TXT record at _fluxer.${identifier} with the value fluxer-verification=<token>, or serve the token at https://${identifier}/.well-known/fluxer-verification`;
			default:
				return 'Follow the platform-specific verification instructions';
		}
	}

	private generateVerificationCode(userId: UserID, connectionType: ConnectionType, identifier: string): string {
		const normalizedIdentifier = identifier.toLowerCase();
		return createHmac('sha256', this.connectionInitiationSecret)
			.update(`connection-verification:v1:${String(userId)}:${connectionType}:${normalizedIdentifier}`)
			.digest('hex')
			.slice(0, CONNECTION_VERIFICATION_TOKEN_LENGTH * 2);
	}
}
