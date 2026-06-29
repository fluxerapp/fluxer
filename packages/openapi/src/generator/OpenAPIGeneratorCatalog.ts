// SPDX-License-Identifier: AGPL-3.0-or-later
import {
	APIErrorCodeSchema,
	Base64ImageTypeSchema,
	DiscriminatorTypeSchema,
	EmailTypeSchema,
	Int32TypeSchema,
	Int64StringTypeSchema,
	Int64TypeSchema,
	LocaleSchema,
	PasswordTypeSchema,
	PhoneNumberTypeSchema,
	SnowflakeTypeSchema,
	UnsignedInt64TypeSchema,
	UsernameTypeSchema,
	ValidationErrorItemSchema,
} from '@fluxer/openapi/src/converters/BuiltInSchemas';
import type {OpenAPISchema, OpenAPISecurityScheme} from '@fluxer/openapi/src/OpenAPITypes';
import {ERROR_SCHEMA} from '@fluxer/openapi/src/registry/ResponseRegistry';

const ORDERED_TAG_NAMES = [
	'Auth',
	'Users',
	'Guilds',
	'Channels',
	'Invites',
	'Packs',
	'Webhooks',
	'OAuth2',
	'Gateway',
	'Search',
	'Read States',
	'KLIPY',
	'Saved Media',
	'Themes',
	'Downloads',
	'Reports',
	'Instance',
	'Admin',
	'Billing',
	'Premium',
	'Gifts',
	'RPC',
] as const;
interface TagDefinition {
	readonly name: (typeof ORDERED_TAG_NAMES)[number];
	readonly description: string;
}
const TAG_DEFINITIONS: ReadonlyArray<TagDefinition> = [
	{name: 'Auth', description: 'Authentication and session management'},
	{name: 'Users', description: 'User accounts and profiles'},
	{name: 'Guilds', description: 'Guild management'},
	{name: 'Channels', description: 'Channel management and messaging'},
	{name: 'Invites', description: 'Guild invitations'},
	{name: 'Packs', description: 'Sticker and emoji packs'},
	{name: 'Webhooks', description: 'Webhook management'},
	{name: 'OAuth2', description: 'OAuth2 applications and authorization'},
	{name: 'Gateway', description: 'WebSocket gateway information'},
	{name: 'Search', description: 'Search functionality'},
	{name: 'Read States', description: 'Message read state tracking'},
	{name: 'KLIPY', description: 'GIF search via KLIPY'},
	{name: 'Saved Media', description: 'User saved media management'},
	{name: 'Themes', description: 'User interface themes'},
	{name: 'Downloads', description: 'App downloads'},
	{name: 'Reports', description: 'Content reporting'},
	{name: 'Instance', description: 'Instance configuration and info'},
	{name: 'Admin', description: 'Administrative operations for instance management'},
	{name: 'Billing', description: 'Subscription and payment management via Stripe'},
	{name: 'Premium', description: 'Premium subscription features and benefits'},
	{name: 'Gifts', description: 'Gift codes and redemption'},
	{name: 'RPC', description: 'Remote procedure call endpoints for internal operations'},
] as const;
const TAG_DESCRIPTIONS = TAG_DEFINITIONS.reduce<Record<(typeof ORDERED_TAG_NAMES)[number], string>>(
	(acc, definition) => {
		acc[definition.name] = definition.description;
		return acc;
	},
	{} as Record<(typeof ORDERED_TAG_NAMES)[number], string>,
);
const SECURITY_SCHEMES: Record<string, OpenAPISecurityScheme> = {
	botToken: {
		type: 'apiKey',
		in: 'header',
		name: 'Authorization',
		description:
			'Bot token: `Authorization: Bot <token>`. This is the primary authentication method for bot applications.',
	},
	oauth2Token: {
		type: 'oauth2',
		description: 'OAuth2 access token: `Authorization: Bearer <token>`.',
		flows: {
			authorizationCode: {
				authorizationUrl: '/oauth2/authorize',
				tokenUrl: '/oauth2/token',
				scopes: {
					identify: 'Read basic user identity information.',
					email: 'Read the user email address.',
					guilds: 'Read guild membership information for the current user.',
					connections: 'Read linked third-party account connections for the current user.',
					bot: 'Add a bot user to a guild.',
				},
			},
		},
	},
	bearerToken: {
		type: 'http',
		scheme: 'bearer',
		description:
			'Bearer-form token: `Authorization: Bearer <token>`. Use `oauth2Token` when a route requires OAuth2 scopes.',
	},
	sessionToken: {
		type: 'apiKey',
		in: 'header',
		name: 'Authorization',
		description:
			'User session token from login: `Authorization: <token>` (no prefix). Prefer a bot account over user tokens where possible.',
	},
	adminApiKey: {
		type: 'apiKey',
		in: 'header',
		name: 'Authorization',
		description: 'Admin API key: `Authorization: Admin <token>`. Only valid for `/admin/*` endpoints.',
	},
};
const BUILT_IN_SCHEMAS: ReadonlyArray<readonly [string, OpenAPISchema]> = [
	['Error', ERROR_SCHEMA],
	['APIErrorCode', APIErrorCodeSchema],
	['SnowflakeType', SnowflakeTypeSchema],
	['Int32Type', Int32TypeSchema],
	['Int64Type', Int64TypeSchema],
	['Int64StringType', Int64StringTypeSchema],
	['UnsignedInt64Type', UnsignedInt64TypeSchema],
	['UsernameType', UsernameTypeSchema],
	['DiscriminatorType', DiscriminatorTypeSchema],
	['EmailType', EmailTypeSchema],
	['PasswordType', PasswordTypeSchema],
	['PhoneNumberType', PhoneNumberTypeSchema],
	['Base64ImageType', Base64ImageTypeSchema],
	['Locale', LocaleSchema],
	['ValidationErrorItem', ValidationErrorItemSchema],
];
interface IOpenAPIGeneratorCatalog {
	readonly excluded: {
		readonly prefixes: ReadonlyArray<string>;
		readonly paths: ReadonlySet<string>;
	};
	readonly tags: {
		readonly order: ReadonlyArray<string>;
		readonly descriptions: Record<string, string>;
	};
	readonly securitySchemes: Record<string, OpenAPISecurityScheme>;
	readonly builtInSchemas: ReadonlyArray<readonly [string, OpenAPISchema]>;
}
export const OpenAPIGeneratorCatalog: IOpenAPIGeneratorCatalog = {
	excluded: {
		prefixes: ['/test/'],
		paths: new Set<string>(['/_rpc', '/oauth2/authorize']),
	},
	tags: {
		order: ORDERED_TAG_NAMES,
		descriptions: TAG_DESCRIPTIONS,
	},
	securitySchemes: SECURITY_SCHEMES,
	builtInSchemas: BUILT_IN_SCHEMAS,
};
export function isExcludedRoutePath(routePath: string): boolean {
	if (OpenAPIGeneratorCatalog.excluded.paths.has(routePath)) {
		return true;
	}
	for (const prefix of OpenAPIGeneratorCatalog.excluded.prefixes) {
		if (routePath.startsWith(prefix) || routePath === prefix.slice(0, -1)) {
			return true;
		}
	}
	return false;
}
