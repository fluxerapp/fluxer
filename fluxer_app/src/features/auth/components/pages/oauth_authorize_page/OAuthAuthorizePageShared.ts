// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AuthResponseUser} from '@app/features/auth/commands/AuthenticationCommands';
import {Logger} from '@app/features/platform/utils/AppLogger';

export const logger = new Logger('OAuthAuthorizePage');

export interface AuthorizeParams {
	clientId: string;
	redirectUri: string | null;
	scope: string;
	state: string | null;
	permissions: string | null;
	guildId: string | null;
	channelId: string | null;
	prompt: string | null;
	responseType: string;
	codeChallenge: string | null;
	codeChallengeMethod: string | null;
}

export interface PublicAppBot {
	id: string;
	avatar: string | null;
	username?: string | null;
}

export interface PublicAppData {
	id: string;
	name: string;
	icon: string | null;
	description: string | null;
	redirect_uris: Array<string>;
	scopes: Array<string>;
	bot_public: boolean;
	bot?: PublicAppBot | null;
	current_user?: AuthResponseUser | null;
}

export interface GuildSummary {
	id: string;
	name: string | null;
	icon: string | null;
	permissions?: string | null;
	threads_active?: true;
}

export interface GuildWithPermissions {
	id: string;
	name: string;
	icon: string | null;
	canAuthorizeBotInvite: boolean;
	threadsActive: boolean;
}

export interface SignedInView {
	userId: string;
	displayName: string;
	username: string;
	discriminator: string;
	avatarUrl: string | undefined;
}

export function isSafeRedirectUri(uri: string): boolean {
	try {
		const u = new URL(uri);
		return u.protocol === 'http:' || u.protocol === 'https:';
	} catch {
		return false;
	}
}

export function parseAuthorizeQuery(search: string): AuthorizeParams | null {
	const qp = new URLSearchParams(search);
	const clientId = qp.get('client_id') ?? '';
	if (!clientId) return null;
	return {
		clientId,
		redirectUri: qp.get('redirect_uri'),
		scope: qp.get('scope') ?? '',
		state: qp.get('state'),
		permissions: qp.get('permissions'),
		guildId: qp.get('guild_id'),
		channelId: qp.get('channel_id'),
		prompt: qp.get('prompt'),
		responseType: qp.get('response_type') ?? 'code',
		codeChallenge: qp.get('code_challenge'),
		codeChallengeMethod: qp.get('code_challenge_method'),
	};
}

export function splitScopes(scope: string): Array<string> {
	if (!scope) return [];
	return scope.split(/[\s+]+/).filter(Boolean);
}

export function safeRedirectHostname(uri: string | null): string | null {
	if (!uri) return null;
	try {
		return new URL(uri).hostname;
	} catch {
		return null;
	}
}
