// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';

export interface UserConnectionStorageRow {
	user_id: UserID;
	connection_id: string;
	connection_type: string;
	identifier: string | null;
	name: string | null;
	verified: boolean | null;
	visibility_flags: number | null;
	sort_order: number | null;
	verification_token: string | null;
	oauth_grant_id?: string | null;
	verified_at: Date | null;
	last_verified_at: Date | null;
	created_at: Date | null;
	revision?: string | null;
	version: number | null;
	membership?: string | null;
	credential_payload?: string | null;
	credential_expires_at?: Date | null;
}

export interface UserConnectionRow extends UserConnectionStorageRow {
	connection_type: ConnectionType;
	identifier: string;
	name: string;
	verified: boolean;
	visibility_flags: number;
	sort_order: number;
	verification_token: string;
	created_at: Date;
	version: number;
	membership?: null;
	credential_payload?: null;
	credential_expires_at?: null;
}

export interface RevisionedUserConnectionRow extends UserConnectionRow {
	revision: string;
}

export const USER_CONNECTION_COLUMNS = [
	'user_id',
	'connection_id',
	'connection_type',
	'identifier',
	'name',
	'verified',
	'visibility_flags',
	'sort_order',
	'verification_token',
	'oauth_grant_id',
	'verified_at',
	'last_verified_at',
	'created_at',
	'revision',
	'version',
] as const satisfies ReadonlyArray<keyof UserConnectionRow>;

export const USER_CONNECTION_STORAGE_COLUMNS = [
	...USER_CONNECTION_COLUMNS,
	'membership',
	'credential_payload',
	'credential_expires_at',
] as const satisfies ReadonlyArray<keyof UserConnectionStorageRow>;

export const USER_CONNECTION_CREDENTIAL_TYPE = '_oauth_grant';
