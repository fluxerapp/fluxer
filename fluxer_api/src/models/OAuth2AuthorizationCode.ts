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
import type {OAuth2AuthorizationCodeRow} from '~/database/types/OAuth2Types';

export class OAuth2AuthorizationCode {
	readonly code: string;
	readonly applicationId: ApplicationID;
	readonly userId: UserID;
	readonly redirectUri: string;
	readonly scope: Set<string>;
	readonly nonce: string | null;
	readonly createdAt: Date;

	constructor(row: OAuth2AuthorizationCodeRow) {
		this.code = row.code;
		this.applicationId = row.application_id;
		this.userId = row.user_id;
		this.redirectUri = row.redirect_uri;
		this.scope = row.scope;
		this.nonce = row.nonce;
		this.createdAt = row.created_at;
	}

	toRow(): OAuth2AuthorizationCodeRow {
		return {
			code: this.code,
			application_id: this.applicationId,
			user_id: this.userId,
			redirect_uri: this.redirectUri,
			scope: this.scope,
			nonce: this.nonce,
			created_at: this.createdAt,
		};
	}

	hasScope(scope: string): boolean {
		return this.scope.has(scope);
	}
}
