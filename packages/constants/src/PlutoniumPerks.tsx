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

import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';

export type PerkStatus = 'available' | 'coming_soon' | 'beta';
export type PerkType = 'boolean' | 'numeric' | 'text';

interface BasePerk {
	id: string;
	type: PerkType;
	status: PerkStatus;
	i18nKey: string;
}

export interface BooleanPerk extends BasePerk {
	type: 'boolean';
	freeValue: boolean;
	plutoniumValue: boolean;
}

export interface NumericPerk extends BasePerk {
	type: 'numeric';
	freeValue: number;
	plutoniumValue: number;
	limitKey?: LimitKey;
	unit?: 'count' | 'bytes' | 'characters';
}

export interface TextPerk extends BasePerk {
	type: 'text';
	freeValueI18nKey: string;
	plutoniumValueI18nKey: string;
}

export type PlutoniumPerk = BooleanPerk | NumericPerk | TextPerk;

export function isBooleanPerk(perk: PlutoniumPerk): perk is BooleanPerk {
	return perk.type === 'boolean';
}

export function isNumericPerk(perk: PlutoniumPerk): perk is NumericPerk {
	return perk.type === 'numeric';
}

export function isTextPerk(perk: PlutoniumPerk): perk is TextPerk {
	return perk.type === 'text';
}

export const PLUTONIUM_PERKS: ReadonlyArray<PlutoniumPerk> = [
	{
		id: 'custom_discriminator',
		type: 'boolean',
		status: 'available',
		i18nKey: 'custom_4_digit_username_tag',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'per_guild_profiles',
		type: 'boolean',
		status: 'available',
		i18nKey: 'per_community_profiles',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'message_scheduling',
		type: 'boolean',
		status: 'coming_soon',
		i18nKey: 'message_scheduling',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'profile_badge',
		type: 'boolean',
		status: 'available',
		i18nKey: 'profile_badge',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'custom_video_backgrounds',
		type: 'numeric',
		status: 'beta',
		i18nKey: 'custom_video_backgrounds',
		freeValue: 1,
		plutoniumValue: 15,
		limitKey: 'max_custom_backgrounds',
		unit: 'count',
	},
	{
		id: 'entrance_sounds',
		type: 'boolean',
		status: 'beta',
		i18nKey: 'entrance_sounds',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'max_guilds',
		type: 'numeric',
		status: 'available',
		i18nKey: 'communities',
		freeValue: 100,
		plutoniumValue: 200,
		limitKey: 'max_guilds',
		unit: 'count',
	},
	{
		id: 'max_message_length',
		type: 'numeric',
		status: 'available',
		i18nKey: 'message_character_limit',
		freeValue: 2000,
		plutoniumValue: 4000,
		limitKey: 'max_message_length',
		unit: 'characters',
	},
	{
		id: 'max_bookmarks',
		type: 'numeric',
		status: 'available',
		i18nKey: 'bookmarked_messages',
		freeValue: 50,
		plutoniumValue: 300,
		limitKey: 'max_bookmarks',
		unit: 'count',
	},
	{
		id: 'max_attachment_file_size',
		type: 'numeric',
		status: 'available',
		i18nKey: 'file_upload_size',
		freeValue: 25 * 1024 * 1024,
		plutoniumValue: 500 * 1024 * 1024,
		limitKey: 'max_attachment_file_size',
		unit: 'bytes',
	},
	{
		id: 'emoji_sticker_packs',
		type: 'boolean',
		status: 'coming_soon',
		i18nKey: 'emoji_sticker_packs',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'max_favorite_memes',
		type: 'numeric',
		status: 'beta',
		i18nKey: 'saved_media',
		freeValue: 50,
		plutoniumValue: 500,
		limitKey: 'max_favorite_memes',
		unit: 'count',
	},
	{
		id: 'use_animated_emojis',
		type: 'boolean',
		status: 'available',
		i18nKey: 'use_animated_emojis',
		freeValue: true,
		plutoniumValue: true,
	},
	{
		id: 'global_expressions',
		type: 'boolean',
		status: 'available',
		i18nKey: 'global_emoji_sticker_access',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'video_quality',
		type: 'text',
		status: 'available',
		i18nKey: 'video_quality',
		freeValueI18nKey: 'video_quality_free',
		plutoniumValueI18nKey: 'video_quality_premium',
	},
	{
		id: 'animated_profile',
		type: 'boolean',
		status: 'available',
		i18nKey: 'animated_avatars_and_banners',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'early_access',
		type: 'boolean',
		status: 'available',
		i18nKey: 'early_access',
		freeValue: false,
		plutoniumValue: true,
	},
	{
		id: 'custom_themes',
		type: 'boolean',
		status: 'available',
		i18nKey: 'custom_themes',
		freeValue: true,
		plutoniumValue: true,
	},
] as const;

export function getPerksByStatus(status: PerkStatus): ReadonlyArray<PlutoniumPerk> {
	return PLUTONIUM_PERKS.filter((perk) => perk.status === status);
}

export function getAvailablePerks(): ReadonlyArray<PlutoniumPerk> {
	return getPerksByStatus('available');
}

export function getBetaPerks(): ReadonlyArray<PlutoniumPerk> {
	return getPerksByStatus('beta');
}

export function getComingSoonPerks(): ReadonlyArray<PlutoniumPerk> {
	return getPerksByStatus('coming_soon');
}
