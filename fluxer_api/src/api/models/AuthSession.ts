// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {AuthSessionRow, AuthSessionTombstoneRow} from '@app/api/database/types/AuthTypes';

export class AuthSession {
	readonly userId: UserID;
	readonly sessionIdHash: Buffer;
	readonly createdAt: Date;
	readonly approximateLastUsedAt: Date;
	readonly clientIp: string;
	readonly clientUserAgent: string | null;
	readonly clientOs: string | null;
	readonly clientCountry: string | null;
	readonly appPlatform: string | null;
	readonly appVersion: string | null;
	readonly appReleaseChannel: string | null;
	readonly appBuildNumber: number | null;
	readonly appBetaNumber: number | null;
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
		this.appPlatform = row.app_platform ?? null;
		this.appVersion = row.app_version ?? null;
		this.appReleaseChannel = row.app_release_channel ?? null;
		this.appBuildNumber = row.app_build_number ?? null;
		this.appBetaNumber = row.app_beta_number ?? null;
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
			app_platform: this.appPlatform,
			app_version: this.appVersion,
			app_release_channel: this.appReleaseChannel,
			app_build_number: this.appBuildNumber,
			app_beta_number: this.appBetaNumber,
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
