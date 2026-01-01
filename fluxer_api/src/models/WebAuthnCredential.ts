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

import type {WebAuthnCredentialRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';

export class WebAuthnCredential {
	readonly credentialId: string;
	readonly publicKey: Buffer;
	readonly counter: bigint;
	readonly transports: Set<string> | null;
	readonly name: string;
	readonly createdAt: Date;
	readonly lastUsedAt: Date | null;
	readonly version: number;

	constructor(row: WebAuthnCredentialRow) {
		this.credentialId = row.credential_id;
		this.publicKey = row.public_key;
		this.counter = row.counter;
		this.transports = row.transports ?? null;
		this.name = row.name;
		this.createdAt = row.created_at;
		this.lastUsedAt = row.last_used_at ?? null;
		this.version = row.version;
	}

	toRow(userId: UserID): WebAuthnCredentialRow {
		return {
			user_id: userId,
			credential_id: this.credentialId,
			public_key: this.publicKey,
			counter: this.counter,
			transports: this.transports,
			name: this.name,
			created_at: this.createdAt,
			last_used_at: this.lastUsedAt,
			version: this.version,
		};
	}
}
