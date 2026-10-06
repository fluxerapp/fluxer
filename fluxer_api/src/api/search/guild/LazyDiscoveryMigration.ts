// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IGuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import type {Guild} from '@app/api/models/Guild';
import type {GuildDiscoveryContext} from '@app/api/search/guild/GuildSearchSerializer';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';

export async function resolveDiscoveryContextForIndexing(
	guild: Guild,
	provided: GuildDiscoveryContext | undefined,
	discoveryRepository: IGuildDiscoveryRepository | undefined,
): Promise<GuildDiscoveryContext | undefined> {
	const isDiscoverable = guild.features.has(GuildFeatures.DISCOVERABLE);
	if (!isDiscoverable) return provided;
	const hasFullContext =
		provided != null &&
		provided.description !== undefined &&
		provided.categoryId !== undefined &&
		provided.primaryLanguage !== undefined &&
		provided.tags !== undefined;
	if (hasFullContext) return provided;
	if (discoveryRepository == null) return undefined;
	try {
		const row = await discoveryRepository.findByGuildId(guild.id);
		if (!row || row.status !== DiscoveryApplicationStatus.APPROVED) return undefined;
		return {
			description: provided?.description ?? row.description,
			categoryId: provided?.categoryId ?? row.category_type,
			primaryLanguage: provided?.primaryLanguage ?? row.primary_language ?? null,
			tags: provided?.tags ?? row.custom_tags ?? [],
			memberCount: provided?.memberCount,
		};
	} catch {
		return undefined;
	}
}
