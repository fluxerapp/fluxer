// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';

export interface AdminApiKeyRow {
	key_id: bigint;
	key_hash: string;
	name: string;
	created_by_user_id: UserID;
	created_at: Date;
	last_used_at: Date | null;
	expires_at: Date | null;
	version: number;
	acls: Set<string> | null;
}

export interface AdminApiKeyByCreatorRow {
	created_by_user_id: UserID;
	key_id: bigint;
	created_at: Date;
	name: string;
	expires_at: Date | null;
	last_used_at: Date | null;
	version: number;
	acls: Set<string> | null;
}

export const ADMIN_API_KEY_COLUMNS = [
	'key_id',
	'key_hash',
	'name',
	'created_by_user_id',
	'created_at',
	'last_used_at',
	'expires_at',
	'version',
	'acls',
] as const satisfies ReadonlyArray<keyof AdminApiKeyRow>;
export const ADMIN_API_KEY_BY_CREATOR_COLUMNS = [
	'created_by_user_id',
	'key_id',
	'created_at',
	'name',
	'expires_at',
	'last_used_at',
	'version',
	'acls',
] as const satisfies ReadonlyArray<keyof AdminApiKeyByCreatorRow>;
