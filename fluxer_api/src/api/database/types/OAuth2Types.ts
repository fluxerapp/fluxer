// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApplicationID, UserID} from '@app/api/BrandedTypes';

export interface ApplicationRow {
	application_id: ApplicationID;
	owner_user_id: UserID;
	name: string;
	bot_user_id: UserID | null;
	bot_is_public: boolean | null;
	bot_require_code_grant?: boolean | null;
	oauth2_redirect_uris: Set<string>;
	client_secret_hash: string | null;
	bot_token_hash: string | null;
	bot_token_preview: string | null;
	bot_token_created_at: Date | null;
	client_secret_created_at: Date | null;
	version?: number | null;
}

export interface ApplicationByOwnerRow {
	owner_user_id: UserID;
	application_id: ApplicationID;
}

export interface OAuth2AuthorizationCodeRow {
	code: string;
	application_id: ApplicationID;
	user_id: UserID;
	redirect_uri: string;
	scope: Set<string>;
	nonce: string | null;
	code_challenge: string | null;
	code_challenge_method: string | null;
	created_at: Date;
}

export interface OAuth2AccessTokenRow {
	token_: string;
	application_id: ApplicationID;
	user_id: UserID | null;
	scope: Set<string>;
	created_at: Date;
}

export interface OAuth2AccessTokenByUserRow {
	user_id: UserID;
	token_: string;
}

export interface OAuth2RefreshTokenRow {
	token_: string;
	application_id: ApplicationID;
	user_id: UserID;
	scope: Set<string>;
	created_at: Date;
}

export interface OAuth2RefreshTokenByUserRow {
	user_id: UserID;
	token_: string;
}

export const APPLICATION_COLUMNS = [
	'application_id',
	'owner_user_id',
	'name',
	'bot_user_id',
	'bot_is_public',
	'bot_require_code_grant',
	'oauth2_redirect_uris',
	'client_secret_hash',
	'bot_token_hash',
	'bot_token_preview',
	'bot_token_created_at',
	'client_secret_created_at',
	'version',
] as const satisfies ReadonlyArray<keyof ApplicationRow>;
export const OAUTH2_AUTHORIZATION_CODE_COLUMNS = [
	'code',
	'application_id',
	'user_id',
	'redirect_uri',
	'scope',
	'nonce',
	'code_challenge',
	'code_challenge_method',
	'created_at',
] as const satisfies ReadonlyArray<keyof OAuth2AuthorizationCodeRow>;
export const OAUTH2_ACCESS_TOKEN_COLUMNS = [
	'token_',
	'application_id',
	'user_id',
	'scope',
	'created_at',
] as const satisfies ReadonlyArray<keyof OAuth2AccessTokenRow>;
export const OAUTH2_REFRESH_TOKEN_COLUMNS = [
	'token_',
	'application_id',
	'user_id',
	'scope',
	'created_at',
] as const satisfies ReadonlyArray<keyof OAuth2RefreshTokenRow>;
