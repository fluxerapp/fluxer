// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	VoicePublicIpv4Type,
	VoicePublicIpv6Type,
	VoiceServerHealthResponse,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import {createStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const VoiceIdType = z
	.string()
	.min(1)
	.max(64)
	.regex(/^[a-z0-9][a-z0-9-]*$/, {error: 'Use lowercase letters, digits and dashes'});

const LatitudeType = z.number().min(-90).max(90);
const LongitudeType = z.number().min(-180).max(180);
const SoftLimitType = z.number().int().min(1).max(2147483647);

export const VoiceTurnServer = z.object({
	host: z.string().describe('Hostname or IPv4 address of the TURN server'),
	port: z.number().int().describe('Port of the TURN server'),
	protocol: z.enum(['udp', 'tcp', 'tls']).describe('Transport clients use to connect to the TURN server'),
	username: z.string().optional().describe('Fixed TURN username'),
	credential: z.string().optional().describe('Fixed TURN password'),
	secret: z.string().optional().describe('Shared secret LiveKit signs short-lived TURN credentials with'),
	ttl: z.number().int().optional().describe('Lifetime of the signed credentials in seconds'),
});

export type VoiceTurnServer = z.infer<typeof VoiceTurnServer>;

const TurnServersType = z
	.array(VoiceTurnServer)
	.describe('TURN servers of the instance, for the LiveKit configuration of the voice server');

export const VoiceJoinPreviewRequest = z.object({
	public_ipv4: z.ipv4().nullable().optional().describe('Public IPv4 address of the voice machine'),
	public_ipv6: z.ipv6().nullable().optional().describe('Public IPv6 address of the voice machine'),
	region_id: VoiceIdType.optional().describe('Existing region the operator asked for'),
	cpu_count: z.number().int().min(1).max(4096).optional().describe('CPU count of the voice machine'),
	memory_bytes: z.number().int().min(0).optional().describe('Memory of the voice machine in bytes'),
	rexctl_version: z.string().max(64).optional().describe('Version of rexctl on the voice machine'),
});

export type VoiceJoinPreviewRequest = z.infer<typeof VoiceJoinPreviewRequest>;

export const VoiceJoinPreviewResponse = z.object({
	server_id: z.string().describe('Suggested server id, or the preset of the token'),
	server_id_pinned: z.boolean().describe('Whether the token sets the server id, which the join then always uses'),
	region: z.object({
		id: z.string().describe('Region id'),
		name: z.string().describe('Region name'),
		distance_km: z.number().nullable().describe('Distance from the voice machine, or null without GeoIP data'),
		new: z.boolean().describe('Whether the region is created when the server joins'),
		source: z
			.enum(['token', 'requested', 'nearest', 'default'])
			.describe('Why Fluxer picked the region, from the token, region_id, the nearest region or the default region'),
	}),
	latitude: z.number().nullable().describe('Server latitude from GeoIP, rounded to 0.1 degrees'),
	longitude: z.number().nullable().describe('Server longitude from GeoIP, rounded to 0.1 degrees'),
	hostname: z.string().nullable().describe('Hostname preset of the token, or null'),
	suggested_hostname: z.string().describe('A hostname under the instance domain the operator may use'),
	soft_connection_limit: z.number().nullable().describe('Soft limit preset of the token, or null'),
	webhook_url: z.string().describe('URL the server sends LiveKit webhooks to'),
	api_code_version: z.number().int().describe('Version of the API server code'),
	release: z.string().describe('Fluxer release of the API, or an empty string for a development build'),
	expires_at: z.string().describe('ISO 8601 timestamp when the token stops working'),
	turn_servers: TurnServersType,
});

export type VoiceJoinPreviewResponse = z.infer<typeof VoiceJoinPreviewResponse>;

export const VoiceJoinRequest = z
	.object({
		server_id: VoiceIdType.optional().describe('Server id, ignored when the token has a preset'),
		region_id: VoiceIdType.optional().describe('Existing region, ignored when the token has a preset'),
		endpoint: z.url({protocol: /^wss$/}).max(512).describe('Public wss endpoint of the server'),
		api_key: createStringType(1, 256).describe('LiveKit API key of the server'),
		api_secret: createStringType(32, 256).describe('LiveKit API secret of the server'),
		latitude: LatitudeType.optional(),
		longitude: LongitudeType.optional(),
		soft_connection_limit: SoftLimitType.optional(),
		public_ipv4: z.ipv4().optional().describe('Public IPv4 address of the voice machine'),
		public_ipv6: z.ipv6().optional().describe('Public IPv6 address of the voice machine'),
	})
	.refine((data) => (data.latitude === undefined) === (data.longitude === undefined), {
		error: 'Latitude and longitude must both be provided or both be omitted',
		path: ['latitude'],
	});

export type VoiceJoinRequest = z.infer<typeof VoiceJoinRequest>;

export const VoiceJoinResponse = z.object({
	server_id: z.string().describe('ID of the new server'),
	region_id: z.string().describe('Region of the new server'),
	endpoint: z.string().describe('Endpoint of the new server'),
	webhook_url: z.string().describe('URL the server sends LiveKit webhooks to'),
	node_url: z.string().describe('URL of the node routes for this server'),
	turn_servers: TurnServersType,
});

export type VoiceJoinResponse = z.infer<typeof VoiceJoinResponse>;

export const VoiceNodeResponse = z.object({
	region_id: z.string().describe('Region of the server'),
	server_id: z.string().describe('ID of the server'),
	endpoint: z.string().describe('Endpoint of the server'),
	latitude: z.number().nullable().describe('Server latitude'),
	longitude: z.number().nullable().describe('Server longitude'),
	is_active: z.boolean().describe('Whether an admin keeps the server turned on'),
	draining: z.boolean().describe('Whether placement sends new calls elsewhere'),
	soft_connection_limit: z.number().nullable().describe('Soft connection limit, or null'),
	public_ipv4: z.string().nullable().describe('Public IPv4 address the server last reported, or null'),
	public_ipv6: z.string().nullable().describe('Public IPv6 address the server last reported, or null'),
	turn_servers: TurnServersType,
	health: VoiceServerHealthResponse.describe('Health of the server as this API node last saw it'),
});

export type VoiceNodeResponse = z.infer<typeof VoiceNodeResponse>;

export const UpdateVoiceNodeRequest = z
	.object({
		endpoint: z.url({protocol: /^wss$/}).max(512).optional().describe('New public wss endpoint'),
		draining: z.boolean().optional().describe('Whether placement sends new calls elsewhere'),
		latitude: LatitudeType.nullable().optional(),
		longitude: LongitudeType.nullable().optional(),
		soft_connection_limit: SoftLimitType.nullable().optional(),
		public_ipv4: z
			.union([VoicePublicIpv4Type, z.literal('')])
			.optional()
			.describe('Public IPv4 address, or an empty string for none'),
		public_ipv6: z
			.union([VoicePublicIpv6Type, z.literal('')])
			.optional()
			.describe('Public IPv6 address, or an empty string for none'),
	})
	.strict()
	.refine((data) => (data.latitude === undefined) === (data.longitude === undefined), {
		error: 'Latitude and longitude must both be provided or both be omitted',
		path: ['latitude'],
	});

export type UpdateVoiceNodeRequest = z.infer<typeof UpdateVoiceNodeRequest>;
