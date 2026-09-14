// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';

export interface BlueskyAuthorizeResult {
	authorizeUrl: string;
}

export interface BlueskyOAuthGrantOwner {
	userId: UserID;
	grantId: string;
}

interface BlueskyOAuthGrant extends BlueskyOAuthGrantOwner {
	did: string;
}

export interface BlueskyCallbackResult extends BlueskyOAuthGrant {
	handle: string;
}

export interface IBlueskyOAuthService {
	readonly clientMetadata: Record<string, unknown>;
	readonly jwks: Record<string, unknown>;
	authorize(handle: string, userId: UserID): Promise<BlueskyAuthorizeResult>;
	callback(params: URLSearchParams): Promise<BlueskyCallbackResult>;
}
