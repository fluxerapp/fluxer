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

import type {ApplicationID, UserID} from '~/BrandedTypes';
import type {ApplicationRow} from '~/database/types/OAuth2Types';

export class Application {
	readonly applicationId: ApplicationID;
	readonly ownerUserId: UserID;
	readonly name: string;
	readonly botUserId: UserID | null;
	readonly botIsPublic: boolean;
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
