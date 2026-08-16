// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../BrandedTypes';
import {TraktOAuthNotEnabledError} from '../connection/errors/TraktOAuthNotEnabledError';
import type {ITraktOAuthService, TraktAuthorizeResult, TraktCallbackResult} from './ITraktOAuthService';

export class DisabledTraktOAuthService implements ITraktOAuthService {
	async authorize(_userId: UserID): Promise<TraktAuthorizeResult> {
		throw new TraktOAuthNotEnabledError();
	}

	async callback(_params: URLSearchParams): Promise<TraktCallbackResult> {
		throw new TraktOAuthNotEnabledError();
	}

	async restoreAndVerify(_traktUserId: string): Promise<{
		username: string;
	} | null> {
		throw new TraktOAuthNotEnabledError();
	}

	async storeRefreshToken(_traktUserId: string, _refreshToken: string): Promise<void> {
		throw new TraktOAuthNotEnabledError();
	}

	async revoke(_traktUserId: string): Promise<void> {
		throw new TraktOAuthNotEnabledError();
	}
}
