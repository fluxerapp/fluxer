/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {AuthSessionRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';

export class AuthSession {
	readonly userId: UserID;
	readonly sessionIdHash: Buffer;
	readonly createdAt: Date;
	readonly approximateLastUsedAt: Date;
	readonly clientIp: string;
	readonly clientUserAgent: string | null;
	readonly clientIsDesktop: boolean | null;
	readonly version: number;

	constructor(row: AuthSessionRow) {
		this.userId = row.user_id;
		this.sessionIdHash = row.session_id_hash;
		this.createdAt = row.created_at;
		this.approximateLastUsedAt = row.approx_last_used_at;
		this.clientIp = row.client_ip;
		this.clientUserAgent = row.client_user_agent ?? null;
		this.clientIsDesktop = row.client_is_desktop ?? null;
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
			client_is_desktop: this.clientIsDesktop,
			version: this.version,
		};
	}
}
