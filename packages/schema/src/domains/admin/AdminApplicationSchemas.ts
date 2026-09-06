// SPDX-License-Identifier: AGPL-3.0-or-later

import {MAX_APPLICATION_REDIRECT_URIS} from '@fluxer/constants/src/LimitConstants';
import {
	createStringType,
	Int32Type,
	SnowflakeStringType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ApplicationAdminResponse = z.object({
	id: SnowflakeStringType.describe('The unique identifier for this application'),
	name: createStringType(1, 256).describe('The display name of the application'),
	owner_user_id: SnowflakeStringType.describe('The user ID that owns this application'),
	owner_username: z.string().nullable().describe('The username of the owner, if resolvable'),
	owner_global_name: z.string().nullable().describe('The display name of the owner, if set'),
	owner_discriminator: z.string().nullable().describe('The discriminator of the owner, if resolvable'),
	bot_user_id: SnowflakeStringType.nullable().describe('The user ID of the associated bot user, if any'),
	bot_username: z.string().nullable().describe('The username of the bot user, if any'),
	bot_global_name: z.string().nullable().describe('The display name of the bot user, if set'),
	bot_discriminator: z.string().nullable().describe('The discriminator of the bot user, if any'),
	bot_is_public: z.boolean().describe('Whether the bot is publicly joinable'),
	bot_require_code_grant: z.boolean().describe('Whether an OAuth2 code grant is required for this bot'),
	oauth2_redirect_uris: z
		.array(z.string())
		.max(MAX_APPLICATION_REDIRECT_URIS)
		.describe('Registered OAuth2 redirect URIs'),
	has_client_secret: z.boolean().describe('Whether a hashed client secret is stored for this application'),
	has_bot_token: z.boolean().describe('Whether a hashed bot token is stored for this application'),
	bot_token_preview: z.string().nullable().describe('The preview (last few characters) of the bot token, if any'),
	bot_token_created_at: z.iso.datetime().nullable().describe('ISO 8601 timestamp when the bot token was created'),
	client_secret_created_at: z.iso
		.datetime()
		.nullable()
		.describe('ISO 8601 timestamp when the client secret was created'),
	version: Int32Type.describe('The optimistic locking version of the application record'),
});

export type ApplicationAdminResponse = z.infer<typeof ApplicationAdminResponse>;

export const LookupApplicationRequest = z.object({
	application_id: SnowflakeType.describe('ID of the application to look up'),
});

export type LookupApplicationRequest = z.infer<typeof LookupApplicationRequest>;

export const LookupApplicationResponse = z.object({
	application: ApplicationAdminResponse.nullable(),
});

export type LookupApplicationResponse = z.infer<typeof LookupApplicationResponse>;

export const ListUserApplicationsRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user whose applications to list'),
});

export type ListUserApplicationsRequest = z.infer<typeof ListUserApplicationsRequest>;

export const ListUserApplicationsResponse = z.object({
	applications: z.array(ApplicationAdminResponse),
});

export type ListUserApplicationsResponse = z.infer<typeof ListUserApplicationsResponse>;

export const ListGuildApplicationsRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild whose installed bot applications to list'),
});

export type ListGuildApplicationsRequest = z.infer<typeof ListGuildApplicationsRequest>;

export const ListGuildApplicationsResponse = z.object({
	applications: z.array(ApplicationAdminResponse),
});

export type ListGuildApplicationsResponse = z.infer<typeof ListGuildApplicationsResponse>;

export const TransferApplicationOwnershipRequest = z.object({
	application_id: SnowflakeType.describe('ID of the application to transfer'),
	new_owner_id: SnowflakeType.describe('ID of the user to transfer ownership to'),
});

export type TransferApplicationOwnershipRequest = z.infer<typeof TransferApplicationOwnershipRequest>;

export const ApplicationUpdateResponse = z.object({
	application: ApplicationAdminResponse,
});

export type ApplicationUpdateResponse = z.infer<typeof ApplicationUpdateResponse>;
