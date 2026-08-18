// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../BrandedTypes';
import type {AuthSessionRow, AuthSessionTombstoneRow} from '../database/types/AuthTypes';

export class AuthSession {
	readonly userId: UserID;
	readonly sessionIdHash: Buffer;
	readonly createdAt: Date;
	readonly approximateLastUsedAt: Date;
	readonly clientIp: string;
	readonly clientUserAgent: string | null;
	readonly clientOs: string | null;
	readonly clientCountry: string | null;
	readonly version: number;

	constructor(row: AuthSessionRow) {
		this.userId = row.user_id;
		this.sessionIdHash = row.session_id_hash;
		this.createdAt = row.created_at;
		this.approximateLastUsedAt = row.approx_last_used_at;
		this.clientIp = row.client_ip;
		this.clientUserAgent = row.client_user_agent ?? null;
		this.clientOs = row.client_os ?? null;
		this.clientCountry = row.client_country ?? null;
		this.version = row.version;
	}

	toRow(): AuthSessionRow {
		return {
			user_id: this.userId,
			session_id_hash: this.sessionIdHash,
			created_at: this.createdAt,
			approx_last_used_at: this.approximateLastUsedAt,
			client_ip: this.clientIp,
			client_user_agent: this.clientUserAgent,
			client_os: this.clientOs,
			client_country: this.clientCountry,
			version: this.version,
		};
	}
}

export class AuthSessionTombstone {
	readonly userId: UserID;
	readonly sessionIdHash: Buffer;
	readonly createdAt: Date;
	readonly approximateLastUsedAt: Date;
	readonly clientIp: string;
	readonly clientUserAgent: string | null;
	readonly clientOs: string | null;
	readonly clientCountry: string | null;
	readonly deletedAt: Date;
	readonly version: number;

	constructor(row: AuthSessionTombstoneRow) {
		this.userId = row.user_id;
		this.sessionIdHash = row.session_id_hash;
		this.createdAt = row.created_at;
		this.approximateLastUsedAt = row.approx_last_used_at;
		this.clientIp = row.client_ip;
		this.clientUserAgent = row.client_user_agent ?? null;
		this.clientOs = row.client_os ?? null;
		this.clientCountry = row.client_country ?? null;
		this.deletedAt = row.deleted_at;
		this.version = row.version;
	}
}
