// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import type {
	OAuth2AccessTokenRow,
	OAuth2AuthorizationCodeRow,
	OAuth2RefreshTokenRow,
} from '@app/api/database/types/OAuth2Types';
import type {OAuth2AccessToken} from '@app/api/models/OAuth2AccessToken';
import type {OAuth2AuthorizationCode} from '@app/api/models/OAuth2AuthorizationCode';
import type {OAuth2RefreshToken} from '@app/api/models/OAuth2RefreshToken';

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
