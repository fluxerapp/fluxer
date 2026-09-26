// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import type {OAuth2AuthorizationCodeRow} from '@app/api/database/types/OAuth2Types';

export class OAuth2AuthorizationCode {
	readonly code: string;
	readonly applicationId: ApplicationID;
	readonly userId: UserID;
	readonly redirectUri: string;
	readonly scope: Set<string>;
	readonly nonce: string | null;
	readonly codeChallenge: string | null;
	readonly codeChallengeMethod: string | null;
	readonly createdAt: Date;

	constructor(row: OAuth2AuthorizationCodeRow) {
		this.code = row.code;
		this.applicationId = row.application_id;
		this.userId = row.user_id;
		this.redirectUri = row.redirect_uri;
		this.scope = row.scope;
		this.nonce = row.nonce;
		this.codeChallenge = row.code_challenge;
		this.codeChallengeMethod = row.code_challenge_method;
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
			code_challenge: this.codeChallenge,
			code_challenge_method: this.codeChallengeMethod,
			created_at: this.createdAt,
		};
	}

	hasScope(scope: string): boolean {
		return this.scope.has(scope);
	}
}
