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

export const ListAuditLogsRequest = z.object({
	admin_user_id: Int64Type.optional(),
	target_type: createStringType(1, 64).optional(),
	target_id: Int64Type.optional(),
	limit: z.number().default(50),
	offset: z.number().default(0),
});

export type ListAuditLogsRequest = z.infer<typeof ListAuditLogsRequest>;

export const SearchAuditLogsRequest = z.object({
	query: createStringType(1, 1024).optional(),
	admin_user_id: Int64Type.optional(),
	target_type: createStringType(1, 64).optional(),
	target_id: Int64Type.optional(),
	action: createStringType(1, 64).optional(),
	sort_by: z.enum(['createdAt', 'relevance']).default('createdAt'),
	sort_order: z.enum(['asc', 'desc']).default('desc'),
	limit: z.number().default(50),
	offset: z.number().default(0),
});

export type SearchAuditLogsRequest = z.infer<typeof SearchAuditLogsRequest>;

export const SearchReportsRequest = z.object({
	query: createStringType(1, 1024).optional(),
	limit: z.number().default(50),
	offset: z.number().default(0),
	reporter_id: Int64Type.optional(),
	status: z.number().optional(),
	report_type: z.number().optional(),
	category: createStringType(1, 128).optional(),
	reported_user_id: Int64Type.optional(),
	reported_guild_id: Int64Type.optional(),
	reported_channel_id: Int64Type.optional(),
	guild_context_id: Int64Type.optional(),
	resolved_by_admin_id: Int64Type.optional(),
	sort_by: z.enum(['createdAt', 'reportedAt', 'resolvedAt']).default('reportedAt'),
	sort_order: z.enum(['asc', 'desc']).default('desc'),
});

export type SearchReportsRequest = z.infer<typeof SearchReportsRequest>;

export const RefreshSearchIndexRequest = z.object({
	index_type: z.enum(['guilds', 'users', 'reports', 'audit_logs', 'channel_messages', 'favorite_memes']),
	guild_id: Int64Type.optional(),
	user_id: Int64Type.optional(),
});

export type RefreshSearchIndexRequest = z.infer<typeof RefreshSearchIndexRequest>;

export const GetIndexRefreshStatusRequest = z.object({
	job_id: createStringType(1, 128),
});

export type GetIndexRefreshStatusRequest = z.infer<typeof GetIndexRefreshStatusRequest>;

export const PurgeGuildAssetsRequest = z.object({
	ids: z.array(createStringType(1, 64)),
});

export type PurgeGuildAssetsRequest = z.infer<typeof PurgeGuildAssetsRequest>;

export interface PurgeGuildAssetResult {
	id: string;
	asset_type: 'emoji' | 'sticker' | 'unknown';
	found_in_db: boolean;
	guild_id: string | null;
}

export interface PurgeGuildAssetError {
	id: string;
	error: string;
}

export interface PurgeGuildAssetsResponse {
	processed: Array<PurgeGuildAssetResult>;
	errors: Array<PurgeGuildAssetError>;
}

export interface GuildEmojiAsset {
	id: string;
	name: string;
	animated: boolean;
	creator_id: string;
	media_url: string;
}

export interface ListGuildEmojisResponse {
	guild_id: string;
	emojis: Array<GuildEmojiAsset>;
}

export interface GuildStickerAsset {
	id: string;
	name: string;
	format_type: number;
	creator_id: string;
	media_url: string;
}

export interface ListGuildStickersResponse {
	guild_id: string;
	stickers: Array<GuildStickerAsset>;
}
