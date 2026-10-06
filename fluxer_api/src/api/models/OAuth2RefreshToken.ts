// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import type {OAuth2RefreshTokenRow} from '@app/api/database/types/OAuth2Types';

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
