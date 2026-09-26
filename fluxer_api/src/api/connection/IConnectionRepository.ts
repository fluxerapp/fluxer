// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {RevisionedUserConnectionRow, UserConnectionRow} from '@app/api/database/types/ConnectionTypes';
import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';

export interface CreateConnectionParams {
	user_id: UserID;
	connection_id: string;
	connection_type: ConnectionType;
	identifier: string;
	name: string;
	visibility_flags: number;
	sort_order?: number;
	verification_token: string;
	oauth_grant_id?: string | null;
	verified?: boolean;
	verified_at?: Date | null;
	last_verified_at?: Date | null;
}

export interface UpdateConnectionParams {
	name?: string;
	visibility_flags?: number;
	sort_order?: number;
	oauth_grant_id?: string | null;
	verified?: boolean;
	verified_at?: Date | null;
	last_verified_at?: Date | null;
}

export interface ConnectionSortOrderUpdate {
	snapshot: RevisionedUserConnectionRow;
	sortOrder: number;
}

export type ConnectionCreationResult =
	| {status: 'created'; connection: RevisionedUserConnectionRow}
	| {status: 'duplicate' | 'limit_reached' | 'conflict'};

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

	abstract create(params: CreateConnectionParams): Promise<ConnectionCreationResult>;

	abstract ensureRevision(snapshot: UserConnectionRow): Promise<RevisionedUserConnectionRow | null>;

	abstract update(
		snapshot: RevisionedUserConnectionRow,
		params: UpdateConnectionParams,
	): Promise<RevisionedUserConnectionRow | null>;

	abstract updateSortOrders(entries: ReadonlyArray<ConnectionSortOrderUpdate>): Promise<boolean>;

	abstract delete(snapshot: RevisionedUserConnectionRow): Promise<boolean>;

	abstract sealAndDeleteForUser(userId: UserID): Promise<void>;

	abstract count(userId: UserID): Promise<number>;
}
