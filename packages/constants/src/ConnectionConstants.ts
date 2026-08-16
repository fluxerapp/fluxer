// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const ConnectionTypes = {
	BLUESKY: 'bsky',
	DOMAIN: 'domain',
	TRAKT: 'trakt',
} as const;

export type ConnectionType = ValueOf<typeof ConnectionTypes>;
export const ConnectionVisibilityFlags = {
	EVERYONE: 1 << 0,
	FRIENDS: 1 << 1,
	MUTUAL_GUILDS: 1 << 2,
} as const;

export const ConnectionVisibilityFlagsDescriptions: Record<keyof typeof ConnectionVisibilityFlags, string> = {
	EVERYONE: 'Allow anyone to see this connection',
	FRIENDS: 'Allow friends to see this connection',
	MUTUAL_GUILDS: 'Allow members from mutual guilds to see this connection',
};
export const MAX_CONNECTIONS_PER_USER = 20;
export const CONNECTION_VERIFICATION_TOKEN_LENGTH = 32;
export const CONNECTION_INITIATION_TOKEN_EXPIRY_MS = 30 * 60 * 1000;
