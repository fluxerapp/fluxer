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
	vip_only: boolean | null;
	required_guild_features: Set<string> | null;
	allowed_guild_ids: Set<bigint> | null;
	allowed_user_ids: Set<bigint> | null;
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
	'vip_only',
	'required_guild_features',
	'allowed_guild_ids',
	'allowed_user_ids',
	'created_at',
	'updated_at',
] as const satisfies ReadonlyArray<keyof VoiceServerRow>;

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
