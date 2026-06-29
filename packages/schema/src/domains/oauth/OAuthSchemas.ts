// SPDX-License-Identifier: AGPL-3.0-or-later

import {ApplicationFlags, BotFlags, BotFlagsDescriptions} from '@fluxer/constants/src/BotConstants';
import {AVATAR_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {
	PublicUserFlags,
	PublicUserFlagsDescriptions,
	UserAuthenticatorTypes,
	UserAuthenticatorTypesDescriptions,
} from '@fluxer/constants/src/UserConstants';
import {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {createBase64StringType} from '@fluxer/schema/src/primitives/FileValidators';
import {
	createBitflagInt32Type,
	createInt32EnumType,
	createNamedObject,
	createNamedStringLiteralUnion,
	createStringType,
	Int32Type,
	SnowflakeStringType,
	SnowflakeType,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {DiscriminatorType, UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

const RedirectURIString = createStringType(1).refine((value) => {
	try {
		const u = new URL(value);
		return !!u.protocol && !!u.host;
	} catch {
		return false;
	}
}, 'Invalid URL format');

const AuthenticatorTypeEnum = withOpenApiType(
	createInt32EnumType(
		[
			[UserAuthenticatorTypes.TOTP, 'TOTP', UserAuthenticatorTypesDescriptions.TOTP],
			[UserAuthenticatorTypes.WEBAUTHN, 'WEBAUTHN', UserAuthenticatorTypesDescriptions.WEBAUTHN],
		],
		'The type of authenticator',
		'AuthenticatorType',
	),
	'AuthenticatorType',
);
const PromptType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['consent', 'CONSENT', 'Always prompt the user for consent'],
			['none', 'NONE', 'Do not prompt the user for consent if already authorized'],
		] as const,
		'Whether to prompt the user for consent',
	),
	'PromptType',
);
const DisableGuildSelectType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['true', 'TRUE', 'Disable guild selection'],
			['false', 'FALSE', 'Allow guild selection'],
		] as const,
		'Whether to disable guild selection',
	),
	'DisableGuildSelectType',
);
export const AuthorizeRequest = z.object({
	response_type: z.literal('code').optional().describe('The OAuth2 response type, must be "code"'),
	client_id: SnowflakeType.describe('The application client ID'),
	redirect_uri: RedirectURIString.optional().describe('The URI to redirect to after authorization'),
	scope: createStringType(1).describe('The space-separated list of requested scopes'),
	state: createStringType(1).optional().describe('A random string for CSRF protection'),
	prompt: PromptType.optional(),
	guild_id: SnowflakeType.optional().describe('The guild ID to pre-select for bot authorization'),
	channel_id: SnowflakeType.optional().describe('The group DM channel ID to pre-select for bot authorization'),
	permissions: z.string().optional().describe('The bot permissions to request'),
	disable_guild_select: DisableGuildSelectType.optional(),
	code_challenge: createStringType(1).optional().describe('The PKCE code challenge'),
	code_challenge_method: createNamedStringLiteralUnion(
		[
			['S256', 'S256', 'SHA-256 hash of code verifier'],
			['plain', 'PLAIN', 'Plain text code verifier'],
		] as const,
		'The PKCE code challenge method',
	).optional(),
});

export type AuthorizeRequest = z.infer<typeof AuthorizeRequest>;

export const AuthorizeConsentRequest = z.object({
	response_type: z.string().optional().describe('The OAuth2 response type'),
	client_id: SnowflakeType.describe('The application client ID'),
	redirect_uri: RedirectURIString.optional().describe('The URI to redirect to after authorization'),
	scope: createStringType(1).describe('The space-separated list of requested scopes'),
	state: createStringType(1).optional().describe('A random string for CSRF protection'),
	permissions: z.string().optional().describe('The bot permissions to request'),
	guild_id: SnowflakeType.optional().describe('The guild ID to add the bot to'),
	channel_id: SnowflakeType.optional().describe('The group DM channel ID to add the bot to'),
	code_challenge: createStringType(1).optional().describe('The PKCE code challenge'),
	code_challenge_method: createNamedStringLiteralUnion(
		[
			['S256', 'S256', 'SHA-256 hash of code verifier'],
			['plain', 'PLAIN', 'Plain text code verifier'],
		] as const,
		'The PKCE code challenge method',
	).optional(),
});

export type AuthorizeConsentRequest = z.infer<typeof AuthorizeConsentRequest>;

export const TokenRequest = z.discriminatedUnion('grant_type', [
	z.object({
		grant_type: z.literal('authorization_code').describe('The grant type for exchanging an authorization code'),
		code: createStringType(1).describe('The authorization code received from the authorize endpoint'),
		redirect_uri: RedirectURIString.describe('The redirect URI used in the authorization request'),
		client_id: SnowflakeType.optional().describe('The application client ID'),
		client_secret: createStringType(1).optional().describe('The application client secret'),
		code_verifier: createStringType(1).optional().describe('The PKCE code verifier for the authorization request'),
	}),
	z.object({
		grant_type: z.literal('refresh_token').describe('The grant type for refreshing an access token'),
		refresh_token: createStringType(1).describe('The refresh token to exchange for a new access token'),
		client_id: SnowflakeType.optional().describe('The application client ID'),
		client_secret: createStringType(1).optional().describe('The application client secret'),
	}),
]);

export type TokenRequest = z.infer<typeof TokenRequest>;

export const IntrospectRequestForm = z.object({
	token: createStringType(1).describe('The token to introspect'),
	client_id: SnowflakeType.optional().describe('The application client ID'),
	client_secret: createStringType(1).optional().describe('The application client secret'),
});

export type IntrospectRequestForm = z.infer<typeof IntrospectRequestForm>;

const TokenTypeHint = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['access_token', 'ACCESS_TOKEN', 'An OAuth2 access token'],
			['refresh_token', 'REFRESH_TOKEN', 'An OAuth2 refresh token'],
		] as const,
		'A hint about the type of token being revoked',
	),
	'TokenTypeHint',
);
export const RevokeRequestForm = z.object({
	token: createStringType(1).describe('The token to revoke'),
	token_type_hint: TokenTypeHint.optional(),
	client_id: SnowflakeType.optional().describe('The application client ID'),
	client_secret: createStringType(1).optional().describe('The application client secret'),
});

export type RevokeRequestForm = z.infer<typeof RevokeRequestForm>;

const ApplicationBotResponse = z
	.object({
		id: SnowflakeStringType.describe('The unique identifier of the bot user'),
		username: z.string().describe('The username of the bot'),
		discriminator: z.string().describe('The discriminator of the bot'),
		avatar: z.string().nullable().optional().describe('The avatar hash of the bot'),
		banner: z.string().nullable().optional().describe('The banner hash of the bot'),
		bio: z.string().nullable().describe('The bio or description of the bot'),
		token: z.string().optional().describe('The bot token for authentication'),
		mfa_enabled: z.boolean().optional().describe('Whether the bot has MFA enabled'),
		authenticator_types: z
			.array(AuthenticatorTypeEnum)
			.max(10)
			.optional()
			.describe('The types of authenticators enabled'),
		flags: createBitflagInt32Type(BotFlags, BotFlagsDescriptions, 'The bot user flags', 'BotFlags'),
	})
	.describe('Detailed bot user metadata');

export const ApplicationResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier of the application'),
	name: z.string().describe('The name of the application'),
	redirect_uris: z.array(z.string()).max(20).describe('The registered redirect URIs for OAuth2'),
	bot_public: z.boolean().describe('Whether the bot can be invited by anyone'),
	bot_require_code_grant: z.boolean().describe('Whether the bot requires OAuth2 code grant'),
	client_secret: z.string().optional().describe('The client secret for OAuth2 authentication'),
	bot: ApplicationBotResponse.optional().describe('The bot user associated with the application'),
});

export type ApplicationResponse = z.infer<typeof ApplicationResponse>;

export const ApplicationListResponse = z.array(ApplicationResponse);

export type ApplicationListResponse = z.infer<typeof ApplicationListResponse>;

export const BotTokenResetResponse = z.object({
	token: z.string().describe('The new bot token'),
	bot: ApplicationBotResponse,
});

export type BotTokenResetResponse = z.infer<typeof BotTokenResetResponse>;

export const BotProfileResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier of the bot user'),
	username: z.string().describe('The username of the bot'),
	discriminator: z.string().describe('The discriminator of the bot'),
	avatar: z.string().nullable().describe('The avatar hash of the bot'),
	banner: z.string().nullable().describe('The banner hash of the bot'),
	bio: z.string().nullable().describe('The bio or description of the bot'),
	flags: createBitflagInt32Type(BotFlags, BotFlagsDescriptions, 'The bot user flags', 'BotFlags'),
});

export type BotProfileResponse = z.infer<typeof BotProfileResponse>;

export const OAuth2TokenResponse = z.object({
	access_token: z.string().describe('The access token for API authorization'),
	token_type: z.string().describe('The type of token, typically "Bearer"'),
	expires_in: Int32Type.describe('The number of seconds until the access token expires'),
	refresh_token: z.string().describe('The refresh token for obtaining new access tokens'),
	scope: z.string().describe('The space-separated list of granted scopes'),
});

export type OAuth2TokenResponse = z.infer<typeof OAuth2TokenResponse>;

export const OAuth2UserInfoResponse = z.object({
	sub: SnowflakeStringType.describe('The subject identifier of the user'),
	id: SnowflakeStringType.describe('The unique identifier of the user'),
	username: z.string().describe('The username of the user'),
	discriminator: z.string().describe('The discriminator of the user'),
	global_name: z.string().nullable().describe('The global display name of the user'),
	avatar: z.string().nullable().describe('The avatar hash of the user'),
	email: z.string().nullable().optional().describe('The email address of the user'),
	verified: z.boolean().nullable().optional().describe('Whether the user has verified their email'),
	flags: createBitflagInt32Type(
		PublicUserFlags,
		PublicUserFlagsDescriptions,
		'The user flags',
		'PublicUserFlags',
	).optional(),
});

export type OAuth2UserInfoResponse = z.infer<typeof OAuth2UserInfoResponse>;

export const OAuth2IntrospectResponse = z.object({
	active: z.boolean().describe('Whether the token is currently active'),
	scope: z.string().optional().describe('The space-separated list of scopes'),
	client_id: SnowflakeStringType.optional().describe('The client identifier for the token'),
	username: z.string().optional().describe('The username of the token owner'),
	token_type: z.string().optional().describe('The type of token'),
	exp: Int32Type.optional().describe('The expiration timestamp in seconds'),
	iat: Int32Type.optional().describe('The issued-at timestamp in seconds'),
	sub: SnowflakeStringType.optional().describe('The subject identifier (user ID)'),
});

export type OAuth2IntrospectResponse = z.infer<typeof OAuth2IntrospectResponse>;

export const OAuth2ConsentResponse = z.object({
	redirect_to: z.string().describe('The URL to redirect the user to after consent'),
});

export type OAuth2ConsentResponse = z.infer<typeof OAuth2ConsentResponse>;

export const OAuth2MeResponse = z.object({
	application: z
		.object({
			id: SnowflakeStringType.describe('The unique identifier of the application'),
			name: z.string().describe('The name of the application'),
			icon: z.string().nullable().describe('The icon hash of the application'),
			description: z.string().nullable().describe('The description of the application'),
			bot_public: z.boolean().describe('Whether the bot can be invited by anyone'),
			bot_require_code_grant: z.boolean().describe('Whether the bot requires OAuth2 code grant'),
			flags: createBitflagInt32Type(ApplicationFlags, 'The application flags', undefined, 'ApplicationFlags'),
		})
		.describe('The application associated with the token'),
	scopes: z.array(z.string()).max(50).describe('The list of granted OAuth2 scopes'),
	expires: z.string().describe('The expiration timestamp of the token'),
	user: z
		.object({
			id: SnowflakeStringType.describe('The unique identifier of the user'),
			username: z.string().describe('The username of the user'),
			discriminator: z.string().describe('The discriminator of the user'),
			global_name: z.string().nullable().describe('The global display name of the user'),
			avatar: z.string().nullable().describe('The avatar hash of the user'),
			avatar_color: Int32Type.nullable().describe('The default avatar color of the user'),
			bot: z.boolean().optional().describe('Whether the user is a bot'),
			system: z.boolean().optional().describe('Whether the user is a system user'),
			flags: createBitflagInt32Type(PublicUserFlags, PublicUserFlagsDescriptions, 'The user flags', 'PublicUserFlags'),
			email: z.string().nullable().optional().describe('The email address of the user'),
			verified: z.boolean().nullable().optional().describe('Whether the user has verified their email'),
		})
		.optional()
		.describe('The user associated with the token'),
});

export type OAuth2MeResponse = z.infer<typeof OAuth2MeResponse>;

export const ApplicationPublicResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier of the application'),
	name: z.string().describe('The name of the application'),
	icon: z.string().nullable().describe('The icon hash of the application'),
	description: z.string().nullable().describe('The description of the application'),
	redirect_uris: z.array(z.string()).max(20).describe('The registered redirect URIs for OAuth2'),
	scopes: z.array(z.string()).max(50).describe('The available OAuth2 scopes'),
	bot_public: z.boolean().describe('Whether the bot can be invited by anyone'),
	bot: ApplicationBotResponse.nullable().describe('The bot user associated with the application'),
	current_user: UserPartialResponse.nullable()
		.optional()
		.describe('Partial user data for the authenticated requester, when a session token is present'),
});

export type ApplicationPublicResponse = z.infer<typeof ApplicationPublicResponse>;

export const ApplicationsMeResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier of the application'),
	name: z.string().describe('The name of the application'),
	icon: z.string().nullable().describe('The persisted bot avatar hash used as the application icon, if available'),
	description: z.string().nullable().describe('The persisted bot profile bio used as the application description'),
	bot_public: z.boolean().describe('Whether the bot can be invited by anyone'),
	bot_require_code_grant: z.boolean().describe('Whether the bot requires OAuth2 code grant'),
	verify_key: z.string().describe('Compatibility placeholder for AppInfo clients until keys are persisted'),
	owner: UserPartialResponse.describe('The owner of the application'),
	bot: ApplicationBotResponse.optional().describe('The bot user associated with the application'),
	redirect_uris: z.array(z.string()).max(20).optional().describe('The registered redirect URIs for OAuth2'),
});

export type ApplicationsMeResponse = z.infer<typeof ApplicationsMeResponse>;

export const OAuth2ApplicationsMeResponse = z.union([ApplicationListResponse, ApplicationsMeResponse]);

export type OAuth2ApplicationsMeResponse = z.infer<typeof OAuth2ApplicationsMeResponse>;

const OAuth2AuthorizationResponse = createNamedObject('OAuth2AuthorizationResponse', {
	application: z
		.object({
			id: SnowflakeStringType.describe('The unique identifier of the application'),
			name: z.string().describe('The name of the application'),
			icon: z.string().nullable().describe('The icon hash of the application'),
			description: z.string().nullable().describe('The description of the application'),
			bot_public: z.boolean().describe('Whether the bot can be invited by anyone'),
		})
		.describe('The application that was authorized'),
	scopes: z.array(z.string()).max(50).describe('The list of granted OAuth2 scopes'),
	authorized_at: z.string().describe('The timestamp when the authorization was granted'),
});

export const OAuth2AuthorizationsListResponse = z.array(OAuth2AuthorizationResponse);

export type OAuth2AuthorizationsListResponse = z.infer<typeof OAuth2AuthorizationsListResponse>;

export const OAuth2AuthorizationsBulkRevokeRequest = z.object({
	application_ids: z
		.array(SnowflakeType)
		.min(1)
		.max(100)
		.describe('Application IDs whose OAuth2 authorizations should be revoked'),
});

export type OAuth2AuthorizationsBulkRevokeRequest = z.infer<typeof OAuth2AuthorizationsBulkRevokeRequest>;

function isLoopbackHost(hostname: string) {
	const lowercaseHost = hostname.toLowerCase();
	return (
		lowercaseHost === 'localhost' ||
		lowercaseHost === '127.0.0.1' ||
		lowercaseHost === '[::1]' ||
		lowercaseHost.endsWith('.localhost')
	);
}

function isIPv4Host(hostname: string) {
	const parts = hostname.split('.');
	if (parts.length !== 4) return false;
	return parts.every((part) => {
		if (!/^\d+$/.test(part)) return false;
		const value = Number(part);
		return Number.isInteger(value) && value >= 0 && value <= 255;
	});
}

function isIPLiteralHost(hostname: string) {
	return isIPv4Host(hostname) || (hostname.startsWith('[') && hostname.endsWith(']') && hostname.includes(':'));
}

function isValidRedirectURI(value: string, allowAnyHttp: boolean) {
	try {
		const url = new URL(value);
		if (url.protocol !== 'http:' && url.protocol !== 'https:') {
			return false;
		}
		if (!allowAnyHttp && url.protocol === 'http:' && !isLoopbackHost(url.hostname) && !isIPLiteralHost(url.hostname)) {
			return false;
		}
		return !!url.host;
	} catch {
		return false;
	}
}

const createRedirectURIType = (allowAnyHttp: boolean, message: string) =>
	createStringType(1).refine((value) => isValidRedirectURI(value, allowAnyHttp), message);
const OAuth2RedirectURICreateType = createRedirectURIType(
	false,
	'Redirect URIs must use HTTPS, or HTTP for localhost and IP addresses only',
);
const OAuth2RedirectURIUpdateType = createRedirectURIType(
	false,
	'Redirect URIs must use HTTPS, or HTTP for localhost and IP addresses only',
);
export const ApplicationCreateRequest = z.object({
	name: createStringType(1, 100).describe('The name of the application'),
	redirect_uris: z
		.array(OAuth2RedirectURICreateType)
		.max(10, 'Maximum of 10 redirect URIs allowed')
		.optional()
		.nullable()
		.transform((value) => value ?? [])
		.describe('The redirect URIs for OAuth2 flows'),
	bot_public: z.boolean().optional().describe('Whether the bot can be invited by anyone'),
	bot_require_code_grant: z.boolean().optional().describe('Whether the bot requires OAuth2 code grant'),
});

export type ApplicationCreateRequest = z.infer<typeof ApplicationCreateRequest>;

export const ApplicationUpdateRequest = z.object({
	name: createStringType(1, 100).optional().describe('The name of the application'),
	redirect_uris: z
		.array(OAuth2RedirectURIUpdateType)
		.max(10, 'Maximum of 10 redirect URIs allowed')
		.optional()
		.nullable()
		.transform((value) => (value === undefined ? undefined : (value ?? [])))
		.describe('The redirect URIs for OAuth2 flows'),
	bot_public: z.boolean().optional().describe('Whether the bot can be invited by anyone'),
	bot_require_code_grant: z.boolean().optional().describe('Whether the bot requires OAuth2 code grant'),
});

export type ApplicationUpdateRequest = z.infer<typeof ApplicationUpdateRequest>;

export const BotProfileUpdateRequest = z.object({
	username: UsernameType.optional().describe('The username of the bot'),
	discriminator: DiscriminatorType.optional().describe('The discriminator of the bot'),
	avatar: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('The avatar image as base64'),
	banner: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('The banner image as base64'),
	bio: createStringType(0, 1024).nullish().describe('The bio or description of the bot'),
	bot_flags: createBitflagInt32Type(BotFlags, BotFlagsDescriptions, 'The bot user flags', 'BotFlags').optional(),
});

export type BotProfileUpdateRequest = z.infer<typeof BotProfileUpdateRequest>;
