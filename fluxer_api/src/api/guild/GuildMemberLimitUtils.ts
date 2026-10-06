// SPDX-License-Identifier: AGPL-3.0-or-later

import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MAX_GUILD_MEMBERS, MAX_GUILD_MEMBERS_VERY_LARGE_GUILD} from '@fluxer/constants/src/LimitConstants';
import {DEFAULT_FREE_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';

function toGuildFeatureSet(guildFeatures: Iterable<string> | null | undefined): Set<string> {
	const featureSet = new Set<string>();
	if (!guildFeatures) {
		return featureSet;
	}
	for (const feature of guildFeatures) {
		if (feature) {
			featureSet.add(feature);
		}
	}
	return featureSet;
}

function resolveDefaultMaxGuildMembers(guildFeatures: Iterable<string> | null | undefined): number {
	const featureSet = toGuildFeatureSet(guildFeatures);
	if (featureSet.has(GuildFeatures.VERY_LARGE_GUILD)) {
		return MAX_GUILD_MEMBERS_VERY_LARGE_GUILD;
	}
	return MAX_GUILD_MEMBERS;
}

export function resolveMaxGuildMembersLimit(params: {
	guildFeatures: Iterable<string> | null | undefined;
	snapshot: LimitConfigSnapshot | null | undefined;
}): number {
	const featureSet = toGuildFeatureSet(params.guildFeatures);
	const defaultLimit = resolveDefaultMaxGuildMembers(featureSet);
	if (!params.snapshot) {
		return defaultLimit;
	}
	const ctx = createLimitMatchContext({guildFeatures: featureSet});
	const resolved = resolveLimit(params.snapshot, ctx, 'max_guild_members', {
		evaluationContext: 'guild',
		baseLimits: {
			...DEFAULT_FREE_LIMITS,
			max_guild_members: defaultLimit,
		},
	});
	if (!Number.isFinite(resolved) || resolved < 0) {
		return defaultLimit;
	}
	return Math.floor(resolved);
}
