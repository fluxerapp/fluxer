// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {parseJsonWithGuard} from '../utils/JsonBoundaryUtils';

const STATE_PREFIX = 'trakt:oauth:state:';
const REFRESH_TOKEN_PREFIX = 'trakt:oauth:refresh:';

interface TraktOAuthState {
	userId: string;
}

function isTraktOAuthState(value: unknown): value is TraktOAuthState {
	return (
		typeof value === 'object' &&
		value !== null &&
		'userId' in value &&
		typeof (value as TraktOAuthState).userId === 'string'
	);
}

export function createTraktOAuthStateStore(kvClient: IKVProvider, ttlSeconds: number) {
	return {
		async set(key: string, state: TraktOAuthState): Promise<void> {
			await kvClient.setex(`${STATE_PREFIX}${key}`, ttlSeconds, JSON.stringify(state));
		},
		async get(key: string): Promise<TraktOAuthState | undefined> {
			const data = await kvClient.getdel(`${STATE_PREFIX}${key}`);
			if (!data) return undefined;
			return parseJsonWithGuard(data, isTraktOAuthState) ?? undefined;
		},
		async del(key: string): Promise<void> {
			await kvClient.del(`${STATE_PREFIX}${key}`);
		},
	};
}

export function createTraktRefreshTokenStore(kvClient: IKVProvider, ttlSeconds: number) {
	return {
		async set(traktUserId: string, refreshToken: string): Promise<void> {
			await kvClient.setex(`${REFRESH_TOKEN_PREFIX}${traktUserId}`, ttlSeconds, refreshToken);
		},
		async get(traktUserId: string): Promise<string | undefined> {
			const data = await kvClient.get(`${REFRESH_TOKEN_PREFIX}${traktUserId}`);
			return data ?? undefined;
		},
		async del(traktUserId: string): Promise<void> {
			await kvClient.del(`${REFRESH_TOKEN_PREFIX}${traktUserId}`);
		},
	};
}
