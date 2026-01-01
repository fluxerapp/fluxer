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
import type {OAuth2RefreshTokenRow} from '~/database/types/OAuth2Types';

export class OAuth2RefreshToken {
	readonly token: string;
	readonly applicationId: ApplicationID;
	readonly userId: UserID;
	readonly scope: Set<string>;
	readonly createdAt: Date;

	constructor(row: OAuth2RefreshTokenRow) {
		this.token = row.token_;
		this.applicationId = row.application_id;
		this.userId = row.user_id;
		this.scope = row.scope;
		this.createdAt = row.created_at;
	}

	toRow(): OAuth2RefreshTokenRow {
		return {
			token_: this.token,
			application_id: this.applicationId,
			user_id: this.userId,
			scope: this.scope,
			created_at: this.createdAt,
		};
	}

	hasScope(scope: string): boolean {
		return this.scope.has(scope);
	}
}
