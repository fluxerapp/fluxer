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

import {GuildSplashCardAlignment, type GuildSplashCardAlignmentValue} from '~/constants/Guild';
import type {GuildRow} from '~/database/CassandraTypes';
import type {ChannelID, GuildID, UserID, VanityURLCode} from '../BrandedTypes';

export class Guild {
	readonly id: GuildID;
	readonly ownerId: UserID;
	readonly name: string;
	readonly vanityUrlCode: VanityURLCode | null;
	readonly iconHash: string | null;
	readonly bannerHash: string | null;
	readonly bannerWidth: number | null;
	readonly bannerHeight: number | null;
	readonly splashHash: string | null;
	readonly splashWidth: number | null;
	readonly splashHeight: number | null;
	readonly splashCardAlignment: GuildSplashCardAlignmentValue;
	readonly embedSplashHash: string | null;
	readonly embedSplashWidth: number | null;
	readonly embedSplashHeight: number | null;
	readonly features: Set<string>;
	readonly verificationLevel: number;
	readonly mfaLevel: number;
	readonly nsfwLevel: number;
	readonly explicitContentFilter: number;
	readonly defaultMessageNotifications: number;
	readonly systemChannelId: ChannelID | null;
	readonly systemChannelFlags: number;
	readonly rulesChannelId: ChannelID | null;
	readonly afkChannelId: ChannelID | null;
	readonly afkTimeout: number;
	readonly disabledOperations: number;
	readonly memberCount: number;
	readonly auditLogsIndexedAt: Date | null;
	readonly version: number;

	constructor(row: GuildRow) {
		this.id = row.guild_id;
		this.ownerId = row.owner_id;
		this.name = row.name;
		this.vanityUrlCode = row.vanity_url_code ?? null;
		this.iconHash = row.icon_hash ?? null;
		this.bannerHash = row.banner_hash ?? null;
		this.bannerWidth = row.banner_width ?? null;
		this.bannerHeight = row.banner_height ?? null;
		this.splashHash = row.splash_hash ?? null;
		this.splashWidth = row.splash_width ?? null;
		this.splashHeight = row.splash_height ?? null;
		this.splashCardAlignment = row.splash_card_alignment ?? GuildSplashCardAlignment.CENTER;
		this.embedSplashHash = row.embed_splash_hash ?? null;
		this.embedSplashWidth = row.embed_splash_width ?? null;
		this.embedSplashHeight = row.embed_splash_height ?? null;
		this.features = row.features ?? new Set();
		this.verificationLevel = row.verification_level ?? 0;
		this.mfaLevel = row.mfa_level ?? 0;
		this.nsfwLevel = row.nsfw_level ?? 0;
		this.explicitContentFilter = row.explicit_content_filter ?? 0;
		this.defaultMessageNotifications = row.default_message_notifications ?? 0;
		this.systemChannelId = row.system_channel_id ?? null;
		this.systemChannelFlags = row.system_channel_flags ?? 0;
		this.rulesChannelId = row.rules_channel_id ?? null;
		this.afkChannelId = row.afk_channel_id ?? null;
		this.afkTimeout = row.afk_timeout ?? 0;
		this.disabledOperations = row.disabled_operations ?? 0;
		this.memberCount = row.member_count ?? 0;
		this.auditLogsIndexedAt = row.audit_logs_indexed_at ?? null;
		this.version = row.version;
	}

	toRow(): GuildRow {
		return {
			guild_id: this.id,
			owner_id: this.ownerId,
			name: this.name,
			vanity_url_code: this.vanityUrlCode,
			icon_hash: this.iconHash,
			banner_hash: this.bannerHash,
			banner_width: this.bannerWidth,
			banner_height: this.bannerHeight,
			splash_hash: this.splashHash,
			splash_width: this.splashWidth,
			splash_height: this.splashHeight,
			splash_card_alignment: this.splashCardAlignment,
			embed_splash_hash: this.embedSplashHash,
			embed_splash_width: this.embedSplashWidth,
			embed_splash_height: this.embedSplashHeight,
			features: this.features.size > 0 ? this.features : null,
			verification_level: this.verificationLevel,
			mfa_level: this.mfaLevel,
			nsfw_level: this.nsfwLevel,
			explicit_content_filter: this.explicitContentFilter,
			default_message_notifications: this.defaultMessageNotifications,
			system_channel_id: this.systemChannelId,
			system_channel_flags: this.systemChannelFlags,
			rules_channel_id: this.rulesChannelId,
			afk_channel_id: this.afkChannelId,
			afk_timeout: this.afkTimeout,
			disabled_operations: this.disabledOperations,
			member_count: this.memberCount,
			audit_logs_indexed_at: this.auditLogsIndexedAt,
			version: this.version,
		};
	}
}
