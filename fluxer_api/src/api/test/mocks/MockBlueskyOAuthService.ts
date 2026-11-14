// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {
	BlueskyAuthorizeResult,
	BlueskyCallbackResult,
	IBlueskyOAuthService,
} from '@app/api/bluesky/IBlueskyOAuthService';

export class MockBlueskyOAuthService implements IBlueskyOAuthService {
	readonly clientMetadata: Record<string, unknown> = {client_id: 'https://test/metadata.json'};
	readonly jwks: Record<string, unknown> = {keys: []};
	async authorize(_handle: string, _userId: UserID): Promise<BlueskyAuthorizeResult> {
		return {authorizeUrl: 'https://bsky.social/oauth/authorize?mock=true'};
	}
	async callback(_params: URLSearchParams): Promise<BlueskyCallbackResult> {
		throw new Error('No callbackResult configured in mock');
	}
}
