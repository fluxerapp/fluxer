// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

export const ConnectionProviderIdType = z
	.string()
	.regex(/^[a-z0-9][a-z0-9_-]{0,31}$/)
	.describe('Connection provider id: lowercase letters, digits, hyphens and underscores, up to 32 characters');

export const OAuthConnectionProviderParam = z.object({
	provider_id: ConnectionProviderIdType,
});

export type OAuthConnectionProviderParam = z.infer<typeof OAuthConnectionProviderParam>;

export const OAuthConnectionAuthorizeResponse = z.object({
	authorize_url: z.string().describe('The URL to open so the user can approve the connection at the provider'),
});

export type OAuthConnectionAuthorizeResponse = z.infer<typeof OAuthConnectionAuthorizeResponse>;
