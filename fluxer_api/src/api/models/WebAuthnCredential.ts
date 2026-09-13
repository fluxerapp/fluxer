// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {WebAuthnCredentialRow} from '@app/api/database/types/AuthTypes';

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
