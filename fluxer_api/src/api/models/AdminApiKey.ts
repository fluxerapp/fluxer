// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {AdminApiKeyRow} from '@app/api/database/types/AdminAuthTypes';

export class AdminApiKey {
	readonly keyId: bigint;
	readonly keyHash: string;
	readonly name: string;
	readonly createdById: UserID;
	readonly createdAt: Date;
	readonly lastUsedAt: Date | null;
	readonly expiresAt: Date | null;
	readonly version: number;
	readonly acls: Set<string>;

	constructor(row: AdminApiKeyRow) {
		this.keyId = row.key_id;
		this.keyHash = row.key_hash;
		this.name = row.name;
		this.createdById = row.created_by_user_id;
		this.createdAt = row.created_at;
		this.lastUsedAt = row.last_used_at ?? null;
		this.expiresAt = row.expires_at ?? null;
		this.version = row.version;
		this.acls = row.acls ?? new Set();
	}

	toRow(): AdminApiKeyRow {
		return {
			key_id: this.keyId,
			key_hash: this.keyHash,
			name: this.name,
			created_by_user_id: this.createdById,
			created_at: this.createdAt,
			last_used_at: this.lastUsedAt,
			expires_at: this.expiresAt,
			version: this.version,
			acls: this.acls.size > 0 ? this.acls : new Set(),
		};
	}

	isExpired(): boolean {
		if (!this.expiresAt) {
			return false;
		}
		return this.expiresAt < new Date();
	}
}
