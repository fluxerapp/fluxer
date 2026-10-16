// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';

export interface GuildDiscoveryRow {
	guild_id: GuildID;
	status: string;
	category_type: number;
	description: string;
	primary_language: string | null;
	custom_tags: Array<string> | null;
	applied_at: Date;
	reviewed_at: Date | null;
	reviewed_by: UserID | null;
	review_reason: string | null;
	removed_at: Date | null;
	removed_by: UserID | null;
	removal_reason: string | null;
}

export const GUILD_DISCOVERY_COLUMNS = [
	'guild_id',
	'status',
	'category_type',
	'description',
	'primary_language',
	'custom_tags',
	'applied_at',
	'reviewed_at',
	'reviewed_by',
	'review_reason',
	'removed_at',
	'removed_by',
	'removal_reason',
] as const satisfies ReadonlyArray<keyof GuildDiscoveryRow>;

export interface GuildDiscoveryByStatusRow {
	status: string;
	applied_at: Date;
	guild_id: GuildID;
}

export const GUILD_DISCOVERY_BY_STATUS_COLUMNS = ['status', 'applied_at', 'guild_id'] as const satisfies ReadonlyArray<
	keyof GuildDiscoveryByStatusRow
>;
