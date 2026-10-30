// SPDX-License-Identifier: AGPL-3.0-or-later

import {QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {createStringType, SnowflakeStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

function areServerCoordinatesPaired(
	latitude: number | null | undefined,
	longitude: number | null | undefined,
): boolean {
	const hasLatitude = latitude !== undefined;
	const hasLongitude = longitude !== undefined;
	if (hasLatitude !== hasLongitude) {
		return false;
	}
	if (!hasLatitude && !hasLongitude) {
		return true;
	}
	const latitudeIsNull = latitude === null;
	const longitudeIsNull = longitude === null;
	return latitudeIsNull === longitudeIsNull;
}

const SoftConnectionLimitType = z.number().int().min(1).max(2147483647).nullable();

export const VoiceInternalEndpointType = z
	.url({protocol: /^https?$/})
	.max(512)
	.describe('URL the API uses for the control API and health checks of this server, such as http://voice-fra:7880');

const VoiceServerNotesType = z
	.string()
	.trim()
	.max(2000)
	.nullable()
	.transform((value) => value || null);

export const VoiceServerHealthResponse = z.object({
	status: z.enum(['up', 'down', 'unknown']).describe('up, down, or unknown before the first check'),
	checked_at: z.string().nullable().describe('ISO 8601 timestamp of the last check, or null before the first'),
	error: z.string().nullable().describe('Status code or short transport error of the last failed check'),
});

export type VoiceServerHealthResponse = z.infer<typeof VoiceServerHealthResponse>;

export const VoiceRegionAdminResponse = z.object({
	id: z.string().describe('Unique identifier for the voice region'),
	name: z.string().describe('Display name of the voice region'),
	emoji: z.string().describe('Emoji representing the region'),
	latitude: z.number().describe('Geographic latitude coordinate'),
	longitude: z.number().describe('Geographic longitude coordinate'),
	is_default: z.boolean().describe('Whether this is the default region'),
	vip_only: z.boolean().describe('Whether this region is restricted to VIP users'),
	required_guild_features: z.array(z.string()).max(100).describe('Guild features required to use this region'),
	allowed_guild_ids: z.array(SnowflakeStringType).max(1000).describe('Guild IDs explicitly allowed to use this region'),
	allowed_user_ids: z.array(SnowflakeStringType).max(1000).describe('User IDs explicitly allowed to use this region'),
	created_at: z.string().nullable().describe('ISO 8601 timestamp when the region was created'),
	updated_at: z.string().nullable().describe('ISO 8601 timestamp when the region was last updated'),
});

export type VoiceRegionAdminResponse = z.infer<typeof VoiceRegionAdminResponse>;

export const VoiceServerAdminResponse = z.object({
	region_id: z.string().describe('ID of the region this server belongs to'),
	server_id: z.string().describe('Unique identifier for the voice server'),
	endpoint: z.url().describe('Client signal WebSocket endpoint URL for the voice server'),
	latitude: z.number().nullable().describe('Optional geographic latitude override for this server'),
	longitude: z.number().nullable().describe('Optional geographic longitude override for this server'),
	is_active: z.boolean().describe('Whether the server is currently active'),
	soft_connection_limit: SoftConnectionLimitType.describe(
		'Connection count above which placement prefers another server, or null when the server has no limit',
	),
	notes: z.string().nullable().describe('Free-form admin notes about this server, or null when there are none'),
	vip_only: z.boolean().describe('Whether this server is restricted to VIP users'),
	required_guild_features: z.array(z.string()).max(100).describe('Guild features required to use this server'),
	allowed_guild_ids: z.array(SnowflakeStringType).max(1000).describe('Guild IDs explicitly allowed to use this server'),
	allowed_user_ids: z.array(SnowflakeStringType).max(1000).describe('User IDs explicitly allowed to use this server'),
	internal_endpoint: z
		.string()
		.nullable()
		.describe('URL the API uses for the control API and health checks, or null to use the endpoint'),
	draining: z.boolean().describe('Whether placement sends new calls elsewhere while calls on this server finish'),
	health: VoiceServerHealthResponse.describe('Health of the server as this API node last saw it'),
	duplicate_api_key: z.boolean().describe('Whether another server uses the same API key'),
	created_at: z.string().nullable().describe('ISO 8601 timestamp when the server was created'),
	updated_at: z.string().nullable().describe('ISO 8601 timestamp when the server was last updated'),
});

export type VoiceServerAdminResponse = z.infer<typeof VoiceServerAdminResponse>;

export const CreateVoiceRegionRequest = z.object({
	id: createStringType(1, 64).describe('Unique identifier for the voice region'),
	name: createStringType(1, 100).describe('Display name of the voice region'),
	emoji: createStringType(1, 64).describe('Emoji representing the region'),
	latitude: z.number().describe('Geographic latitude coordinate'),
	longitude: z.number().describe('Geographic longitude coordinate'),
	is_default: z.boolean().optional().default(false).describe('Whether this is the default region'),
	vip_only: z.boolean().optional().default(false).describe('Whether this region is restricted to VIP users'),
	required_guild_features: z
		.array(createStringType(1, 64))
		.max(100)
		.optional()
		.default([])
		.describe('Guild features required to use this region'),
	allowed_guild_ids: z
		.array(SnowflakeType)
		.max(1000)
		.optional()
		.default([])
		.describe('Guild IDs explicitly allowed to use this region'),
	allowed_user_ids: z
		.array(SnowflakeType)
		.max(1000)
		.optional()
		.default([])
		.describe('User IDs explicitly allowed to use this region'),
});

export type CreateVoiceRegionRequest = z.infer<typeof CreateVoiceRegionRequest>;

export const UpdateVoiceRegionRequest = z.object({
	id: createStringType(1, 64).describe('Unique identifier for the voice region'),
	name: createStringType(1, 100).optional().describe('Display name of the voice region'),
	emoji: createStringType(1, 64).optional().describe('Emoji representing the region'),
	latitude: z.number().optional().describe('Geographic latitude coordinate'),
	longitude: z.number().optional().describe('Geographic longitude coordinate'),
	is_default: z.boolean().optional().describe('Whether this is the default region'),
	vip_only: z.boolean().optional().describe('Whether this region is restricted to VIP users'),
	required_guild_features: z
		.array(createStringType(1, 64))
		.max(100)
		.optional()
		.describe('Guild features required to use this region'),
	allowed_guild_ids: z
		.array(SnowflakeType)
		.max(1000)
		.optional()
		.describe('Guild IDs explicitly allowed to use this region'),
	allowed_user_ids: z
		.array(SnowflakeType)
		.max(1000)
		.optional()
		.describe('User IDs explicitly allowed to use this region'),
});

export type UpdateVoiceRegionRequest = z.infer<typeof UpdateVoiceRegionRequest>;

export const UpdateVoiceRegionRequestBody = UpdateVoiceRegionRequest.omit({id: true});

export const DeleteVoiceRegionRequest = z.object({
	id: createStringType(1, 64).describe('ID of the voice region to delete'),
});

export type DeleteVoiceRegionRequest = z.infer<typeof DeleteVoiceRegionRequest>;

export const CreateVoiceServerRequestBody = z
	.object({
		server_id: createStringType(1, 64).describe('Unique identifier for the voice server'),
		endpoint: z.url().describe('Client signal WebSocket endpoint URL for the voice server'),
		api_key: createStringType(1, 256).describe('API key for authenticating with the voice server'),
		api_secret: createStringType(1, 256).describe('API secret for authenticating with the voice server'),
		latitude: z.number().nullable().optional().describe('Optional geographic latitude override for this server'),
		longitude: z.number().nullable().optional().describe('Optional geographic longitude override for this server'),
		is_active: z.boolean().optional().default(true).describe('Whether the server is currently active'),
		soft_connection_limit: SoftConnectionLimitType.optional()
			.default(null)
			.describe('Connection count above which placement prefers another server, or null for no limit'),
		notes: VoiceServerNotesType.optional().default(null).describe('Free-form admin notes about this server'),
		vip_only: z.boolean().optional().default(false).describe('Whether this server is restricted to VIP users'),
		required_guild_features: z
			.array(createStringType(1, 64))
			.max(100)
			.optional()
			.default([])
			.describe('Guild features required to use this server'),
		allowed_guild_ids: z
			.array(SnowflakeType)
			.max(1000)
			.optional()
			.default([])
			.describe('Guild IDs explicitly allowed to use this server'),
		allowed_user_ids: z
			.array(SnowflakeType)
			.max(1000)
			.optional()
			.default([])
			.describe('User IDs explicitly allowed to use this server'),
		internal_endpoint: VoiceInternalEndpointType.nullable().optional().default(null),
		draining: z
			.boolean()
			.optional()
			.default(false)
			.describe('Whether placement sends new calls elsewhere while calls on this server finish'),
	})
	.refine((data) => areServerCoordinatesPaired(data.latitude, data.longitude), {
		error: 'Latitude and longitude must both be provided or both be omitted',
		path: ['latitude'],
	});

export const CreateVoiceServerRequest = CreateVoiceServerRequestBody.safeExtend({
	region_id: createStringType(1, 64).describe('ID of the region this server belongs to'),
});

export type CreateVoiceServerRequest = z.infer<typeof CreateVoiceServerRequest>;

export const UpdateVoiceServerRequestBody = z
	.object({
		endpoint: z.url().optional().describe('Client signal WebSocket endpoint URL for the voice server'),
		api_key: createStringType(1, 256).optional().describe('API key for authenticating with the voice server'),
		api_secret: createStringType(1, 256).optional().describe('API secret for authenticating with the voice server'),
		latitude: z.number().nullable().optional().describe('Optional geographic latitude override for this server'),
		longitude: z.number().nullable().optional().describe('Optional geographic longitude override for this server'),
		is_active: z.boolean().optional().describe('Whether the server is currently active'),
		soft_connection_limit: SoftConnectionLimitType.optional().describe(
			'Connection count above which placement prefers another server, or null for no limit',
		),
		notes: VoiceServerNotesType.optional().describe('Free-form admin notes about this server, or null to clear them'),
		vip_only: z.boolean().optional().describe('Whether this server is restricted to VIP users'),
		required_guild_features: z
			.array(createStringType(1, 64))
			.max(100)
			.optional()
			.describe('Guild features required to use this server'),
		allowed_guild_ids: z
			.array(SnowflakeType)
			.max(1000)
			.optional()
			.describe('Guild IDs explicitly allowed to use this server'),
		allowed_user_ids: z
			.array(SnowflakeType)
			.max(1000)
			.optional()
			.describe('User IDs explicitly allowed to use this server'),
		internal_endpoint: VoiceInternalEndpointType.nullable().optional(),
		draining: z
			.boolean()
			.optional()
			.describe('Whether placement sends new calls elsewhere while calls on this server finish'),
	})
	.refine((data) => areServerCoordinatesPaired(data.latitude, data.longitude), {
		error: 'Latitude and longitude must both be provided or both be omitted',
		path: ['latitude'],
	});

export const UpdateVoiceServerRequest = UpdateVoiceServerRequestBody.safeExtend({
	region_id: createStringType(1, 64).describe('ID of the region this server belongs to'),
	server_id: createStringType(1, 64).describe('Unique identifier for the voice server'),
});

export type UpdateVoiceServerRequest = z.infer<typeof UpdateVoiceServerRequest>;

export const DeleteVoiceServerRequest = z.object({
	region_id: createStringType(1, 64).describe('ID of the region the server belongs to'),
	server_id: createStringType(1, 64).describe('ID of the voice server to delete'),
});

export type DeleteVoiceServerRequest = z.infer<typeof DeleteVoiceServerRequest>;

export const ListVoiceRegionsRequest = z.object({
	include_servers: z.boolean().optional().default(false).describe('Whether to include voice servers in the response'),
});

export type ListVoiceRegionsRequest = z.infer<typeof ListVoiceRegionsRequest>;

export const GetVoiceRegionRequest = z.object({
	id: createStringType(1, 64).describe('ID of the voice region to retrieve'),
	include_servers: z.boolean().optional().default(true).describe('Whether to include voice servers in the response'),
});

export type GetVoiceRegionRequest = z.infer<typeof GetVoiceRegionRequest>;

export const ListVoiceServersRequest = z.object({
	region_id: createStringType(1, 64).describe('ID of the region to list servers for'),
});

export type ListVoiceServersRequest = z.infer<typeof ListVoiceServersRequest>;

export const GetVoiceServerRequest = z.object({
	region_id: createStringType(1, 64).describe('ID of the region the server belongs to'),
	server_id: createStringType(1, 64).describe('ID of the voice server to retrieve'),
});

export type GetVoiceServerRequest = z.infer<typeof GetVoiceServerRequest>;

const VoiceRegionWithServersResponse = VoiceRegionAdminResponse.extend({
	servers: z.array(VoiceServerAdminResponse).optional().describe('Voice servers in this region'),
});

export const ListVoiceRegionsResponse = z.object({
	regions: z.array(VoiceRegionWithServersResponse).describe('List of voice regions'),
});

export type ListVoiceRegionsResponse = z.infer<typeof ListVoiceRegionsResponse>;

export const GetVoiceRegionResponse = z.object({
	region: VoiceRegionWithServersResponse.nullable().describe('Voice region details or null if not found'),
});

export type GetVoiceRegionResponse = z.infer<typeof GetVoiceRegionResponse>;

export const CreateVoiceRegionResponse = z.object({
	region: VoiceRegionAdminResponse.describe('Created voice region'),
});

export type CreateVoiceRegionResponse = z.infer<typeof CreateVoiceRegionResponse>;

export const UpdateVoiceRegionResponse = z.object({
	region: VoiceRegionAdminResponse.describe('Updated voice region'),
});

export type UpdateVoiceRegionResponse = z.infer<typeof UpdateVoiceRegionResponse>;

export const ListVoiceServersResponse = z.object({
	servers: z.array(VoiceServerAdminResponse).describe('List of voice servers'),
});

export type ListVoiceServersResponse = z.infer<typeof ListVoiceServersResponse>;

export const GetVoiceServerResponse = z.object({
	server: VoiceServerAdminResponse.nullable().describe('Voice server details or null if not found'),
});

export type GetVoiceServerResponse = z.infer<typeof GetVoiceServerResponse>;

export const CreateVoiceServerResponse = z.object({
	server: VoiceServerAdminResponse.describe('Created voice server'),
});

export type CreateVoiceServerResponse = z.infer<typeof CreateVoiceServerResponse>;

export const UpdateVoiceServerResponse = z.object({
	server: VoiceServerAdminResponse.describe('Updated voice server'),
});

export type UpdateVoiceServerResponse = z.infer<typeof UpdateVoiceServerResponse>;

export const DeleteVoiceResponse = z.object({
	success: z.boolean().describe('Whether the deletion was successful'),
});

export type DeleteVoiceResponse = z.infer<typeof DeleteVoiceResponse>;

export const VoiceRegionIdParam = z.object({
	region_id: createStringType(1, 64).describe('ID of the voice region'),
});

export type VoiceRegionIdParam = z.infer<typeof VoiceRegionIdParam>;

export const VoiceServerIdParam = z.object({
	region_id: createStringType(1, 64).describe('ID of the region the server belongs to'),
	server_id: createStringType(1, 64).describe('ID of the voice server'),
});

export type VoiceServerIdParam = z.infer<typeof VoiceServerIdParam>;

export const ListVoiceRegionsQuery = z.object({
	include_servers: QueryBooleanType.optional()
		.default(false)
		.describe('Whether to include voice servers in the response'),
});

export type ListVoiceRegionsQuery = z.infer<typeof ListVoiceRegionsQuery>;

export const GetVoiceRegionQuery = z.object({
	include_servers: QueryBooleanType.optional()
		.default(true)
		.describe('Whether to include voice servers in the response'),
});

export type GetVoiceRegionQuery = z.infer<typeof GetVoiceRegionQuery>;

const VoiceIdType = z
	.string()
	.min(1)
	.max(64)
	.regex(/^[a-z0-9][a-z0-9-]*$/, {error: 'Use lowercase letters, digits and dashes'});

export const VoiceJoinTokenStatus = z.enum(['pending', 'used', 'expired', 'revoked']);

export const CreateVoiceJoinTokenRequest = z.object({
	region_id: VoiceIdType.nullable()
		.optional()
		.default(null)
		.describe('Region for the new server, or null to let the API pick the nearest existing region'),
	region_name: createStringType(1, 100)
		.nullable()
		.optional()
		.default(null)
		.describe('Name for a new region with the id in region_id, created when the server joins'),
	region_emoji: createStringType(1, 64)
		.nullable()
		.optional()
		.default(null)
		.describe('Emoji for the new region in region_name'),
	region_latitude: z
		.number()
		.min(-90)
		.max(90)
		.nullable()
		.optional()
		.default(null)
		.describe('Latitude of the new region in region_name. Placement sends people to the nearest region'),
	region_longitude: z
		.number()
		.min(-180)
		.max(180)
		.nullable()
		.optional()
		.default(null)
		.describe('Longitude of the new region in region_name'),
	server_id: VoiceIdType.nullable().optional().default(null).describe('Id for the new server'),
	hostname: z
		.string()
		.trim()
		.max(253)
		.regex(/^[A-Za-z0-9.-]+$/)
		.nullable()
		.optional()
		.default(null)
		.describe('Hostname the server must use for its endpoint'),
	internal_endpoint: VoiceInternalEndpointType.nullable().optional().default(null),
	soft_connection_limit: SoftConnectionLimitType.optional().default(null),
	notes: VoiceServerNotesType.optional().default(null),
	vip_only: z.boolean().optional().default(false),
	required_guild_features: z.array(createStringType(1, 64)).max(100).optional().default([]),
	allowed_guild_ids: z.array(SnowflakeType).max(1000).optional().default([]),
	allowed_user_ids: z.array(SnowflakeType).max(1000).optional().default([]),
	expires_in_seconds: z
		.number()
		.int()
		.min(60)
		.max(86400)
		.optional()
		.default(3600)
		.describe('Seconds until the token stops working, from 60 to 86400'),
});

export type CreateVoiceJoinTokenRequest = z.infer<typeof CreateVoiceJoinTokenRequest>;

export const CreateVoiceJoinTokenResponse = z.object({
	token_id: SnowflakeStringType.describe('ID of the token'),
	token: z.string().describe('The token. It appears in this response only'),
	expires_at: z.string().describe('ISO 8601 timestamp when the token stops working'),
	command: z.string().describe('Command to run on the voice machine'),
});

export type CreateVoiceJoinTokenResponse = z.infer<typeof CreateVoiceJoinTokenResponse>;

export const VoiceJoinTokenAdminResponse = z.object({
	token_id: SnowflakeStringType.describe('ID of the token'),
	status: VoiceJoinTokenStatus.describe('pending, used, expired or revoked'),
	created_at: z.string().describe('ISO 8601 timestamp when the token was created'),
	expires_at: z.string().describe('ISO 8601 timestamp when the token stops working'),
	used_at: z.string().nullable().describe('ISO 8601 timestamp when a server joined with it'),
	used_server_id: z.string().nullable().describe('ID of the server that joined with it'),
	revoked_at: z.string().nullable().describe('ISO 8601 timestamp when it was revoked'),
	region_id: z.string().nullable().describe('Region preset, or null'),
	region_name: z.string().nullable().describe('Name of the new region it creates, or null'),
	server_id: z.string().nullable().describe('Server id preset, or null'),
	hostname: z.string().nullable().describe('Hostname preset, or null'),
	notes: z.string().nullable().describe('Notes for the new server, or null'),
});

export type VoiceJoinTokenAdminResponse = z.infer<typeof VoiceJoinTokenAdminResponse>;

export const ListVoiceJoinTokensResponse = z.object({
	tokens: z.array(VoiceJoinTokenAdminResponse).describe('Join tokens, newest first'),
});

export type ListVoiceJoinTokensResponse = z.infer<typeof ListVoiceJoinTokensResponse>;

export const VoiceJoinTokenIdParam = z.object({
	token_id: SnowflakeType.describe('ID of the join token'),
});

export const CreateVoiceServerTestTokenResponse = z.object({
	token: z.string().describe('LiveKit token for a hidden audio only participant, valid for 60 seconds'),
	endpoint: z.string().describe('Endpoint of the server'),
	room: z.string().describe('Name of the throwaway test room'),
});

export type CreateVoiceServerTestTokenResponse = z.infer<typeof CreateVoiceServerTestTokenResponse>;
