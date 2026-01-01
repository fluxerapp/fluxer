/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createStringType, Int64Type, z} from '~/Schema';

const RedirectURIString = createStringType(1).refine((value) => {
	try {
		const u = new URL(value);
		return !!u.protocol && !!u.host;
	} catch {
		return false;
	}
}, 'Invalid URL format');

export const OAuthScopes = ['identify', 'email', 'guilds', 'bot', 'applications.commands'] as const;

export type OAuthScope = (typeof OAuthScopes)[number];

export const AuthorizeRequest = z.object({
	response_type: z.literal('code').optional(),
	client_id: Int64Type,
	redirect_uri: RedirectURIString.optional(),
	scope: createStringType(1),
	state: createStringType(1).optional(),
	prompt: z.enum(['consent', 'none']).optional(),
	guild_id: Int64Type.optional(),
	permissions: z.string().optional(),
	disable_guild_select: z.enum(['true', 'false']).optional(),
});

export const AuthorizeConsentRequest = z.object({
	response_type: z.string().optional(),
	client_id: Int64Type,
	redirect_uri: RedirectURIString.optional(),
	scope: createStringType(1),
	state: createStringType(1).optional(),
	permissions: z.string().optional(),
	guild_id: Int64Type.optional(),
});

export const TokenRequest = z.discriminatedUnion('grant_type', [
	z.object({
		grant_type: z.literal('authorization_code'),
		code: createStringType(1),
		redirect_uri: RedirectURIString,
		client_id: Int64Type.optional(),
		client_secret: createStringType(1).optional(),
	}),
	z.object({
		grant_type: z.literal('refresh_token'),
		refresh_token: createStringType(1),
		client_id: Int64Type.optional(),
		client_secret: createStringType(1).optional(),
	}),
]);

export const IntrospectRequestForm = z.object({
	token: createStringType(1),
	client_id: Int64Type.optional(),
	client_secret: createStringType(1).optional(),
});

export const RevokeRequestForm = z.object({
	token: createStringType(1),
	token_type_hint: z.enum(['access_token', 'refresh_token']).optional(),
	client_id: Int64Type.optional(),
	client_secret: createStringType(1).optional(),
});

export type AuthorizeRequest = z.infer<typeof AuthorizeRequest>;
export type TokenRequest = z.infer<typeof TokenRequest>;
