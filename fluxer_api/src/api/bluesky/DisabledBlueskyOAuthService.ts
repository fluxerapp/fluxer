// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../BrandedTypes';
import {BlueskyOAuthNotEnabledError} from '../connection/errors/BlueskyOAuthNotEnabledError';
import type {BlueskyAuthorizeResult, BlueskyCallbackResult, IBlueskyOAuthService} from './IBlueskyOAuthService';

export class DisabledBlueskyOAuthService implements IBlueskyOAuthService {
	readonly clientMetadata: Record<string, unknown> = {};
	readonly jwks: Record<string, unknown> = {keys: []};

	async authorize(_handle: string, _userId: UserID): Promise<BlueskyAuthorizeResult> {
		throw new BlueskyOAuthNotEnabledError();
	}

	async callback(_params: URLSearchParams): Promise<BlueskyCallbackResult> {
		throw new BlueskyOAuthNotEnabledError();
	}
}
