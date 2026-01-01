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
import type {
	OAuth2AccessTokenRow,
	OAuth2AuthorizationCodeRow,
	OAuth2RefreshTokenRow,
} from '~/database/types/OAuth2Types';
import type {OAuth2AccessToken} from '~/models/OAuth2AccessToken';
import type {OAuth2AuthorizationCode} from '~/models/OAuth2AuthorizationCode';
import type {OAuth2RefreshToken} from '~/models/OAuth2RefreshToken';

export interface IOAuth2TokenRepository {
	createAuthorizationCode(data: OAuth2AuthorizationCodeRow): Promise<OAuth2AuthorizationCode>;
	getAuthorizationCode(code: string): Promise<OAuth2AuthorizationCode | null>;
	deleteAuthorizationCode(code: string): Promise<void>;

	createAccessToken(data: OAuth2AccessTokenRow): Promise<OAuth2AccessToken>;
	getAccessToken(token: string): Promise<OAuth2AccessToken | null>;
	deleteAccessToken(token: string, applicationId: ApplicationID, userId: UserID | null): Promise<void>;
	deleteAllAccessTokensForUser(userId: UserID): Promise<void>;

	createRefreshToken(data: OAuth2RefreshTokenRow): Promise<OAuth2RefreshToken>;
	getRefreshToken(token: string): Promise<OAuth2RefreshToken | null>;
	deleteRefreshToken(token: string, applicationId: ApplicationID, userId: UserID): Promise<void>;
	deleteAllRefreshTokensForUser(userId: UserID): Promise<void>;

	listRefreshTokensForUser(userId: UserID): Promise<Array<OAuth2RefreshToken>>;
	deleteAllTokensForUserAndApplication(userId: UserID, applicationId: ApplicationID): Promise<void>;
}
