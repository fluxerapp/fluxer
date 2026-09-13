// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';
import type {UserID} from '../BrandedTypes';
import type {BlueskyCallbackResult} from '../bluesky/IBlueskyOAuthService';
import type {UserConnectionRow} from '../database/types/ConnectionTypes';
import type {UpdateConnectionParams} from './IConnectionRepository';

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
