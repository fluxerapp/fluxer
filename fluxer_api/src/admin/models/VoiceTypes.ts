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

export const VoiceRegionAdminResponse = z.object({
	id: z.string(),
	name: z.string(),
	emoji: z.string(),
	latitude: z.number(),
	longitude: z.number(),
	is_default: z.boolean(),
	vip_only: z.boolean(),
	required_guild_features: z.array(z.string()),
	allowed_guild_ids: z.array(z.string()),
	allowed_user_ids: z.array(z.string()),
	created_at: z.string().nullable(),
	updated_at: z.string().nullable(),
});

export type VoiceRegionAdminResponse = z.infer<typeof VoiceRegionAdminResponse>;

export const VoiceServerAdminResponse = z.object({
	region_id: z.string(),
	server_id: z.string(),
	endpoint: z.url(),
	is_active: z.boolean(),
	vip_only: z.boolean(),
	required_guild_features: z.array(z.string()),
	allowed_guild_ids: z.array(z.string()),
	allowed_user_ids: z.array(z.string()),
	created_at: z.string().nullable(),
	updated_at: z.string().nullable(),
});

export type VoiceServerAdminResponse = z.infer<typeof VoiceServerAdminResponse>;

export const CreateVoiceRegionRequest = z.object({
	id: createStringType(1, 64),
	name: createStringType(1, 100),
	emoji: createStringType(1, 64),
	latitude: z.number(),
	longitude: z.number(),
	is_default: z.boolean().optional().default(false),
	vip_only: z.boolean().optional().default(false),
	required_guild_features: z.array(createStringType(1, 64)).optional().default([]),
	allowed_guild_ids: z.array(Int64Type).optional().default([]),
	allowed_user_ids: z.array(Int64Type).optional().default([]),
});

export type CreateVoiceRegionRequest = z.infer<typeof CreateVoiceRegionRequest>;

export const UpdateVoiceRegionRequest = z.object({
	id: createStringType(1, 64),
	name: createStringType(1, 100).optional(),
	emoji: createStringType(1, 64).optional(),
	latitude: z.number().optional(),
	longitude: z.number().optional(),
	is_default: z.boolean().optional(),
	vip_only: z.boolean().optional(),
	required_guild_features: z.array(createStringType(1, 64)).optional(),
	allowed_guild_ids: z.array(Int64Type).optional(),
	allowed_user_ids: z.array(Int64Type).optional(),
});

export type UpdateVoiceRegionRequest = z.infer<typeof UpdateVoiceRegionRequest>;

export const DeleteVoiceRegionRequest = z.object({
	id: createStringType(1, 64),
});

export type DeleteVoiceRegionRequest = z.infer<typeof DeleteVoiceRegionRequest>;

export const CreateVoiceServerRequest = z.object({
	region_id: createStringType(1, 64),
	server_id: createStringType(1, 64),
	endpoint: z.url(),
	api_key: createStringType(1, 256),
	api_secret: createStringType(1, 256),
	is_active: z.boolean().optional().default(true),
	vip_only: z.boolean().optional().default(false),
	required_guild_features: z.array(createStringType(1, 64)).optional().default([]),
	allowed_guild_ids: z.array(Int64Type).optional().default([]),
	allowed_user_ids: z.array(Int64Type).optional().default([]),
});

export type CreateVoiceServerRequest = z.infer<typeof CreateVoiceServerRequest>;

export const UpdateVoiceServerRequest = z.object({
	region_id: createStringType(1, 64),
	server_id: createStringType(1, 64),
	endpoint: z.url().optional(),
	api_key: createStringType(1, 256).optional(),
	api_secret: createStringType(1, 256).optional(),
	is_active: z.boolean().optional(),
	vip_only: z.boolean().optional(),
	required_guild_features: z.array(createStringType(1, 64)).optional(),
	allowed_guild_ids: z.array(Int64Type).optional(),
	allowed_user_ids: z.array(Int64Type).optional(),
});

export type UpdateVoiceServerRequest = z.infer<typeof UpdateVoiceServerRequest>;

export const DeleteVoiceServerRequest = z.object({
	region_id: createStringType(1, 64),
	server_id: createStringType(1, 64),
});

export type DeleteVoiceServerRequest = z.infer<typeof DeleteVoiceServerRequest>;

export const ListVoiceRegionsRequest = z.object({
	include_servers: z.boolean().optional().default(false),
});

export type ListVoiceRegionsRequest = z.infer<typeof ListVoiceRegionsRequest>;

export const GetVoiceRegionRequest = z.object({
	id: createStringType(1, 64),
	include_servers: z.boolean().optional().default(true),
});

export type GetVoiceRegionRequest = z.infer<typeof GetVoiceRegionRequest>;

export const ListVoiceServersRequest = z.object({
	region_id: createStringType(1, 64),
});

export type ListVoiceServersRequest = z.infer<typeof ListVoiceServersRequest>;

export const GetVoiceServerRequest = z.object({
	region_id: createStringType(1, 64),
	server_id: createStringType(1, 64),
});

export type GetVoiceServerRequest = z.infer<typeof GetVoiceServerRequest>;
