// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild} from '@app/api/models/Guild';
import {normalizeDiscoveryTag} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures, getEffectiveGuildVerificationLevel} from '@fluxer/constants/src/GuildConstants';
import type {SearchableGuild} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

export interface GuildDiscoveryContext {
	description: string | null;
	categoryId: number | null;
	primaryLanguage?: string | null;
	tags?: ReadonlyArray<string> | null;
	memberCount?: number;
}

function hasIndexableDiscoveryContext(
	discovery: GuildDiscoveryContext | undefined,
): discovery is GuildDiscoveryContext {
	return discovery?.description !== undefined && discovery.categoryId !== undefined;
}

export function convertToSearchableGuild(guild: Guild, discovery?: GuildDiscoveryContext): SearchableGuild {
	const createdAt = Math.floor(snowflakeToDate(BigInt(guild.id)).getTime() / 1000);
	const isDiscoverable = guild.features.has(GuildFeatures.DISCOVERABLE) && hasIndexableDiscoveryContext(discovery);
	const discoveryContext = isDiscoverable ? discovery : undefined;
	const tags = (discoveryContext?.tags ?? []).map((t) => normalizeDiscoveryTag(t)).filter((t) => t.length > 0);
	return {
		id: guild.id.toString(),
		ownerId: guild.ownerId.toString(),
		name: guild.name,
		vanityUrlCode: guild.vanityUrlCode,
		iconHash: guild.iconHash,
		bannerHash: guild.bannerHash,
		splashHash: guild.splashHash,
		features: Array.from(guild.features),
		verificationLevel: getEffectiveGuildVerificationLevel(guild.verificationLevel, isDiscoverable),
		mfaLevel: guild.mfaLevel,
		nsfwLevel: guild.nsfwLevel,
		createdAt,
		memberCount: discoveryContext?.memberCount ?? guild.memberCount,
		discoveryDescription: discoveryContext?.description ?? null,
		discoveryCategory: discoveryContext?.categoryId ?? null,
		discoveryPrimaryLanguage: discoveryContext?.primaryLanguage ?? null,
		discoveryTags: tags,
		isDiscoverable,
	};
}
