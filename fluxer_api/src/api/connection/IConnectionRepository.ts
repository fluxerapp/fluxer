// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';
import type {UserID} from '../BrandedTypes';
import type {UserConnectionRow} from '../database/types/ConnectionTypes';

export interface CreateConnectionParams {
	user_id: UserID;
	connection_id: string;
	connection_type: ConnectionType;
	identifier: string;
	name: string;
	visibility_flags: number;
	sort_order: number;
	verification_token: string;
	verified?: boolean;
	verified_at?: Date | null;
	last_verified_at?: Date | null;
}

export interface UpdateConnectionParams {
	name?: string;
	visibility_flags?: number;
	sort_order?: number;
	verified?: boolean;
	verified_at?: Date | null;
	last_verified_at?: Date | null;
}

export abstract class IConnectionRepository {
	abstract findByUserId(userId: UserID): Promise<Array<UserConnectionRow>>;

	abstract findById(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
	): Promise<UserConnectionRow | null>;

	abstract findByTypeAndIdentifier(
		userId: UserID,
		connectionType: ConnectionType,
		identifier: string,
	): Promise<UserConnectionRow | null>;

	abstract create(params: CreateConnectionParams): Promise<UserConnectionRow>;

	abstract update(
		userId: UserID,
		connectionType: ConnectionType,
		connectionId: string,
		params: UpdateConnectionParams,
	): Promise<void>;

	abstract updateSortOrders(
		userId: UserID,
		entries: Array<{connectionType: ConnectionType; connectionId: string; sortOrder: number}>,
	): Promise<void>;

	abstract delete(userId: UserID, connectionType: ConnectionType, connectionId: string): Promise<void>;

	abstract count(userId: UserID): Promise<number>;
}
