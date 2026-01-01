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
	is_active: boolean | null;
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
	'is_active',
	'vip_only',
	'required_guild_features',
	'allowed_guild_ids',
	'allowed_user_ids',
	'created_at',
	'updated_at',
] as const satisfies ReadonlyArray<keyof VoiceServerRow>;
