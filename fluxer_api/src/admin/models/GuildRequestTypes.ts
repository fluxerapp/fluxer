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

import {createStringType, Int64Type, VanityURLCodeType, z} from '~/Schema';

export const UpdateGuildFeaturesRequest = z.object({
	guild_id: Int64Type,
	add_features: z.array(createStringType(1, 64)).default([]),
	remove_features: z.array(createStringType(1, 64)).default([]),
});

export type UpdateGuildFeaturesRequest = z.infer<typeof UpdateGuildFeaturesRequest>;

export const ForceAddUserToGuildRequest = z.object({
	user_id: Int64Type,
	guild_id: Int64Type,
});

export interface ForceAddUserToGuildRequest {
	user_id: bigint;
	guild_id: bigint;
}

export const ClearGuildFieldsRequest = z.object({
	guild_id: Int64Type,
	fields: z.array(z.enum(['icon', 'banner', 'splash'])),
});

export type ClearGuildFieldsRequest = z.infer<typeof ClearGuildFieldsRequest>;

export const DeleteGuildRequest = z.object({
	guild_id: Int64Type,
});

export type DeleteGuildRequest = z.infer<typeof DeleteGuildRequest>;

export const UpdateGuildVanityRequest = z.object({
	guild_id: Int64Type,
	vanity_url_code: VanityURLCodeType.nullable(),
});

export type UpdateGuildVanityRequest = z.infer<typeof UpdateGuildVanityRequest>;

export const UpdateGuildNameRequest = z.object({
	guild_id: Int64Type,
	name: createStringType(1, 100),
});

export type UpdateGuildNameRequest = z.infer<typeof UpdateGuildNameRequest>;

export const UpdateGuildSettingsRequest = z.object({
	guild_id: Int64Type,
	verification_level: z.number().optional(),
	mfa_level: z.number().optional(),
	nsfw_level: z.number().optional(),
	explicit_content_filter: z.number().optional(),
	default_message_notifications: z.number().optional(),
	disabled_operations: z.number().optional(),
});

export type UpdateGuildSettingsRequest = z.infer<typeof UpdateGuildSettingsRequest>;

export const TransferGuildOwnershipRequest = z.object({
	guild_id: Int64Type,
	new_owner_id: Int64Type,
});

export type TransferGuildOwnershipRequest = z.infer<typeof TransferGuildOwnershipRequest>;

export const BulkUpdateGuildFeaturesRequest = z.object({
	guild_ids: z.array(Int64Type),
	add_features: z.array(createStringType(1, 64)).default([]),
	remove_features: z.array(createStringType(1, 64)).default([]),
});

export type BulkUpdateGuildFeaturesRequest = z.infer<typeof BulkUpdateGuildFeaturesRequest>;

export const BulkAddGuildMembersRequest = z.object({
	guild_id: Int64Type,
	user_ids: z.array(Int64Type),
});

export type BulkAddGuildMembersRequest = z.infer<typeof BulkAddGuildMembersRequest>;
