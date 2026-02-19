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

import type {Guild} from '@fluxer/api/src/models/Guild';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {SearchableGuild} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

export interface GuildDiscoveryContext {
	description: string | null;
	categoryId: number | null;
}

export function convertToSearchableGuild(guild: Guild, discovery?: GuildDiscoveryContext): SearchableGuild {
	const createdAt = Math.floor(snowflakeToDate(BigInt(guild.id)).getTime() / 1000);

	return {
		id: guild.id.toString(),
		ownerId: guild.ownerId.toString(),
		name: guild.name,
		vanityUrlCode: guild.vanityUrlCode,
		iconHash: guild.iconHash,
		bannerHash: guild.bannerHash,
		splashHash: guild.splashHash,
		features: Array.from(guild.features),
		verificationLevel: guild.verificationLevel,
		mfaLevel: guild.mfaLevel,
		nsfwLevel: guild.nsfwLevel,
		createdAt,
		discoveryDescription: discovery?.description ?? null,
		discoveryCategory: discovery?.categoryId ?? null,
		isDiscoverable: guild.features.has(GuildFeatures.DISCOVERABLE),
	};
}
