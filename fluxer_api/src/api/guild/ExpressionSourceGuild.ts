// SPDX-License-Identifier: AGPL-3.0-or-later

import {stripGuildIconForFeatures} from '@app/api/infrastructure/AssetEntitlementUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {
	GUILD_EXPRESSION_SOURCE_BADGE_FEATURES,
	type GuildExpressionSourceGuildResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

interface ExpressionSourceGuildLookup {
	loadGuild: () => Promise<GuildResponse>;
	isMember: () => Promise<boolean>;
}

export async function resolveExpressionSourceGuild({
	loadGuild,
	isMember,
}: ExpressionSourceGuildLookup): Promise<GuildExpressionSourceGuildResponse> {
	const guild = await loadGuild();
	const features = new Set(guild.features);
	if (!features.has(GuildFeatures.DISCOVERABLE) && !(await isMember())) {
		throw new UnknownGuildError();
	}
	return {
		id: guild.id,
		name: guild.name,
		icon: stripGuildIconForFeatures(guild.icon, features),
		features: GUILD_EXPRESSION_SOURCE_BADGE_FEATURES.filter((feature) => features.has(feature)),
	};
}
