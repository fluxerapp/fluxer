// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';

type PerkStatus = 'available' | 'coming_soon' | 'beta';
type PerkType = 'boolean' | 'numeric' | 'text';

interface BasePerk {
	id: string;
	type: PerkType;
	status: PerkStatus;
	i18nKey: string;
}

export interface BooleanTierPerk extends BasePerk {
	type: 'boolean';
	restrictedValue: boolean;
	stockValue: boolean;
}

export interface NumericTierPerk extends BasePerk {
	type: 'numeric';
	restrictedValue: number;
	stockValue: number;
	limitKey?: LimitKey;
	unit?: 'count' | 'bytes' | 'characters';
}

export interface TextTierPerk extends BasePerk {
	type: 'text';
	restrictedValueI18nKey: string;
	stockValueI18nKey: string;
}

export type LimitTierPerk = BooleanTierPerk | NumericTierPerk | TextTierPerk;

export function isBooleanTierPerk(perk: LimitTierPerk): perk is BooleanTierPerk {
	return perk.type === 'boolean';
}

export function isNumericTierPerk(perk: LimitTierPerk): perk is NumericTierPerk {
	return perk.type === 'numeric';
}

export function isTextTierPerk(perk: LimitTierPerk): perk is TextTierPerk {
	return perk.type === 'text';
}

export const LIMIT_TIER_PERKS: ReadonlyArray<LimitTierPerk> = [
	{
		id: 'custom_discriminator',
		type: 'boolean',
		status: 'available',
		i18nKey: 'custom_4_digit_username_tag',
		restrictedValue: false,
		stockValue: true,
	},
	{
		id: 'per_guild_profiles',
		type: 'boolean',
		status: 'available',
		i18nKey: 'per_community_profiles',
		restrictedValue: false,
		stockValue: true,
	},
	{
		id: 'profile_badge',
		type: 'boolean',
		status: 'available',
		i18nKey: 'profile_badge',
		restrictedValue: false,
		stockValue: true,
	},
	{
		id: 'custom_video_backgrounds',
		type: 'numeric',
		status: 'beta',
		i18nKey: 'custom_video_backgrounds',
		restrictedValue: 1,
		stockValue: 15,
		limitKey: 'max_custom_backgrounds',
		unit: 'count',
	},
	{
		id: 'entrance_sounds',
		type: 'boolean',
		status: 'beta',
		i18nKey: 'entrance_sounds',
		restrictedValue: false,
		stockValue: true,
	},
	{
		id: 'max_guilds',
		type: 'numeric',
		status: 'available',
		i18nKey: 'communities',
		restrictedValue: 100,
		stockValue: 200,
		limitKey: 'max_guilds',
		unit: 'count',
	},
	{
		id: 'max_message_length',
		type: 'numeric',
		status: 'available',
		i18nKey: 'message_character_limit',
		restrictedValue: 2000,
		stockValue: 4000,
		limitKey: 'max_message_length',
		unit: 'characters',
	},
	{
		id: 'max_bookmarks',
		type: 'numeric',
		status: 'available',
		i18nKey: 'bookmarked_messages',
		restrictedValue: 50,
		stockValue: 300,
		limitKey: 'max_bookmarks',
		unit: 'count',
	},
	{
		id: 'max_attachment_file_size',
		type: 'numeric',
		status: 'available',
		i18nKey: 'file_upload_size',
		restrictedValue: 25 * 1024 * 1024,
		stockValue: 500 * 1024 * 1024,
		limitKey: 'max_attachment_file_size',
		unit: 'bytes',
	},
	{
		id: 'max_favorite_memes',
		type: 'numeric',
		status: 'beta',
		i18nKey: 'saved_media',
		restrictedValue: 50,
		stockValue: 500,
		limitKey: 'max_favorite_memes',
		unit: 'count',
	},
	{
		id: 'use_animated_emojis',
		type: 'boolean',
		status: 'available',
		i18nKey: 'use_animated_emojis',
		restrictedValue: true,
		stockValue: true,
	},
	{
		id: 'global_expressions',
		type: 'boolean',
		status: 'available',
		i18nKey: 'global_emoji_sticker_access',
		restrictedValue: false,
		stockValue: true,
	},
	{
		id: 'video_quality',
		type: 'text',
		status: 'available',
		i18nKey: 'video_quality',
		restrictedValueI18nKey: 'video_quality_restricted',
		stockValueI18nKey: 'video_quality_stock',
	},
	{
		id: 'animated_profile',
		type: 'boolean',
		status: 'available',
		i18nKey: 'animated_avatars_and_banners',
		restrictedValue: false,
		stockValue: true,
	},
	{
		id: 'early_access',
		type: 'boolean',
		status: 'available',
		i18nKey: 'early_access',
		restrictedValue: false,
		stockValue: true,
	},
	{
		id: 'custom_themes',
		type: 'boolean',
		status: 'available',
		i18nKey: 'custom_themes',
		restrictedValue: true,
		stockValue: true,
	},
] as const;
