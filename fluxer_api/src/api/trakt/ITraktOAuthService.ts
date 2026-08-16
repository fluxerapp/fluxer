// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../BrandedTypes';

export interface TraktAuthorizeResult {
	authorizeUrl: string;
}

export interface TraktCallbackResult {
	userId: UserID;
	traktUserId: string;
	username: string;
}

export interface ITraktOAuthService {
	authorize(userId: UserID): Promise<TraktAuthorizeResult>;
	callback(params: URLSearchParams): Promise<TraktCallbackResult>;
	restoreAndVerify(traktUserId: string): Promise<{
		username: string;
	} | null>;
	storeRefreshToken(traktUserId: string, refreshToken: string): Promise<void>;
	revoke(traktUserId: string): Promise<void>;
}
