// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {
	BlueskyAuthorizeResult,
	BlueskyCallbackResult,
	IBlueskyOAuthService,
} from '@app/api/bluesky/IBlueskyOAuthService';
import {BlueskyOAuthNotEnabledError} from '@app/api/connection/errors/BlueskyOAuthNotEnabledError';

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
