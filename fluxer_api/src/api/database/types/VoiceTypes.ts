// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';

export interface VoiceRegionRow {
	id: string;
	name: string;
	emoji: string;
	latitude: number;
	longitude: number;
	is_default: boolean | null;
	vip_only: boolean | null;
	required_guild_features: Set<string> | null;
	allowed_guild_ids: Set<bigint> | null;
	allowed_user_ids: Set<bigint> | null;
	created_at: Date | null;
	updated_at: Date | null;
}

export const VOICE_REGION_COLUMNS = [
	'id',
	'name',
	'emoji',
	'latitude',
	'longitude',
	'is_default',
	'vip_only',
	'required_guild_features',
	'allowed_guild_ids',
	'allowed_user_ids',
	'created_at',
	'updated_at',
] as const satisfies ReadonlyArray<keyof VoiceRegionRow>;

export interface VoiceServerRow {
	region_id: string;
	server_id: string;
	endpoint: string;
	api_key: string;
	api_secret: string;
	latitude: number | null;
	longitude: number | null;
	is_active: boolean | null;
	soft_connection_limit: number | null;
	notes: string | null;
	vip_only: boolean | null;
	required_guild_features: Set<string> | null;
	allowed_guild_ids: Set<bigint> | null;
	allowed_user_ids: Set<bigint> | null;
	internal_endpoint: string | null;
	draining: boolean | null;
	pinned_hostname: string | null;
	pinned_soft_limit: boolean | null;
	created_at: Date | null;
	updated_at: Date | null;
}

export const VOICE_SERVER_COLUMNS = [
	'region_id',
	'server_id',
	'endpoint',
	'api_key',
	'api_secret',
	'latitude',
	'longitude',
	'is_active',
	'soft_connection_limit',
	'notes',
	'vip_only',
	'required_guild_features',
	'allowed_guild_ids',
	'allowed_user_ids',
	'internal_endpoint',
	'draining',
	'pinned_hostname',
	'pinned_soft_limit',
	'created_at',
	'updated_at',
] as const satisfies ReadonlyArray<keyof VoiceServerRow>;

export interface VoiceJoinTokenRow {
	token_id: bigint;
	secret_hash: string;
	created_by: bigint;
	created_at: Date;
	expires_at: Date;
	claim_id: string | null;
	claimed_at: Date | null;
	used_at: Date | null;
	used_by_ip: string | null;
	used_server_id: string | null;
	revoked_at: Date | null;
	region_id: string | null;
	region_name: string | null;
	region_emoji: string | null;
	region_latitude: number | null;
	region_longitude: number | null;
	server_id: string | null;
	hostname: string | null;
	internal_endpoint: string | null;
	soft_connection_limit: number | null;
	notes: string | null;
	vip_only: boolean | null;
	required_guild_features: Set<string> | null;
	allowed_guild_ids: Set<bigint> | null;
	allowed_user_ids: Set<bigint> | null;
}

export const VOICE_JOIN_TOKEN_COLUMNS = [
	'token_id',
	'secret_hash',
	'created_by',
	'created_at',
	'expires_at',
	'claim_id',
	'claimed_at',
	'used_at',
	'used_by_ip',
	'used_server_id',
	'revoked_at',
	'region_id',
	'region_name',
	'region_emoji',
	'region_latitude',
	'region_longitude',
	'server_id',
	'hostname',
	'internal_endpoint',
	'soft_connection_limit',
	'notes',
	'vip_only',
	'required_guild_features',
	'allowed_guild_ids',
	'allowed_user_ids',
] as const satisfies ReadonlyArray<keyof VoiceJoinTokenRow>;

export interface VoiceP2pConnectionReportRow {
	user_id: UserID;
	report_id: bigint;
	reported_at: Date;
	channel_id: ChannelID;
	guild_id: GuildID | null;
	participant_count: number;
	outcome: string;
	local_candidate_type: string | null;
	remote_candidate_type: string | null;
	ip_family: string | null;
	protocol: string | null;
	setup_ms: number | null;
	ice_restarted: boolean;
	country: string | null;
	ip: string | null;
	client_platform: string | null;
	client_os: string | null;
}

export const VOICE_P2P_CONNECTION_REPORT_COLUMNS = [
	'user_id',
	'report_id',
	'reported_at',
	'channel_id',
	'guild_id',
	'participant_count',
	'outcome',
	'local_candidate_type',
	'remote_candidate_type',
	'ip_family',
	'protocol',
	'setup_ms',
	'ice_restarted',
	'country',
	'ip',
	'client_platform',
	'client_os',
] as const satisfies ReadonlyArray<keyof VoiceP2pConnectionReportRow>;
