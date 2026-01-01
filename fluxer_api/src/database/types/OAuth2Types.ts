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

export interface ApplicationRow {
	application_id: ApplicationID;
	owner_user_id: UserID;
	name: string;
	bot_user_id: UserID | null;
	bot_is_public: boolean | null;
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
