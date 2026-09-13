// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {BlueskyCallbackResult} from '@app/api/bluesky/IBlueskyOAuthService';
import type {UpdateConnectionParams} from '@app/api/connection/IConnectionRepository';
import type {UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';

export type InitiateConnectionResult = Record<string, never>;

export abstract class IConnectionService {
	abstract getConnectionsForUser(userId: UserID): Promise<Array<UserConnectionRow>>;

	abstract initiateConnection(
		userId: UserID,
		type: ConnectionType,
		identifier: string,
	): Promise<InitiateConnectionResult>;

	abstract verifyAndCreateConnection(
		userId: UserID,
		type: ConnectionType,
		identifier: string,
		verificationCode: string,
		visibilityFlags: number,
	): Promise<UserConnectionRow>;

	abstract updateConnection(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
		patch: UpdateConnectionParams,
	): Promise<void>;

	abstract deleteConnection(userId: UserID, connectionType: ConnectionType, connectionId: string): Promise<void>;

	abstract reorderConnections(userId: UserID, connectionIds: Array<string>): Promise<void>;

	abstract createOrUpdateBlueskyConnection(result: BlueskyCallbackResult): Promise<UserConnectionRow>;
}
