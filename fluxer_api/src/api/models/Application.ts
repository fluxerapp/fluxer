// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import type {ApplicationRow} from '@app/api/database/types/OAuth2Types';

export class Application {
	readonly applicationId: ApplicationID;
	readonly ownerUserId: UserID;
	readonly name: string;
	readonly botUserId: UserID | null;
	readonly botIsPublic: boolean;
	readonly botRequireCodeGrant: boolean;
	readonly oauth2RedirectUris: Set<string>;
	readonly clientSecretHash: string | null;
	readonly botTokenHash: string | null;
	readonly botTokenPreview: string | null;
	readonly botTokenCreatedAt: Date | null;
	readonly clientSecretCreatedAt: Date | null;
	readonly version: number;

	constructor(row: ApplicationRow) {
		this.applicationId = row.application_id;
		this.ownerUserId = row.owner_user_id;
		this.name = row.name;
		this.botUserId = row.bot_user_id;
		this.botIsPublic = row.bot_is_public ?? row.bot_user_id !== null;
		this.botRequireCodeGrant = row.bot_require_code_grant ?? false;
		this.oauth2RedirectUris = row.oauth2_redirect_uris ?? new Set<string>();
		this.clientSecretHash = row.client_secret_hash;
		this.botTokenHash = row.bot_token_hash;
		this.botTokenPreview = row.bot_token_preview;
		this.botTokenCreatedAt = row.bot_token_created_at;
		this.clientSecretCreatedAt = row.client_secret_created_at;
		this.version = row.version ?? 1;
	}

	toRow(): ApplicationRow {
		return {
			application_id: this.applicationId,
			owner_user_id: this.ownerUserId,
			name: this.name,
			bot_user_id: this.botUserId,
			bot_is_public: this.botIsPublic,
			bot_require_code_grant: this.botRequireCodeGrant,
			oauth2_redirect_uris: this.oauth2RedirectUris,
			client_secret_hash: this.clientSecretHash,
			bot_token_hash: this.botTokenHash,
			bot_token_preview: this.botTokenPreview,
			bot_token_created_at: this.botTokenCreatedAt,
			client_secret_created_at: this.clientSecretCreatedAt,
			version: this.version,
		};
	}

	hasBotUser(): boolean {
		return this.botUserId !== null;
	}

	getBotUserId(): UserID | null {
		return this.botUserId;
	}
}
