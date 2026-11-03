// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {
	type GuildVisitsConfig,
	GuildVisitsConfigSchema,
	guildVisitsUserActive,
} from '@fluxer/schema/src/domains/admin/GuildVisitsSchemas';
import {EXPERIMENT_BUCKET_RESOLUTION} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';

const DEFAULT_CONFIG: GuildVisitsConfig = GuildVisitsConfigSchema.parse({});

let cachedConfig: GuildVisitsConfig = DEFAULT_CONFIG;

export function setCachedGuildVisitsConfig(config: GuildVisitsConfig | null): void {
	cachedConfig = config ?? DEFAULT_CONFIG;
}

export function userGuildVisitsActive(userId: UserID | bigint | string): boolean {
	return guildVisitsUserActive(cachedConfig, userId.toString());
}

export function guildVisitsOffer(
	features: Iterable<string>,
	userId: UserID | bigint | string | null | undefined,
): {visits_enabled?: boolean} {
	let active: boolean;
	if (userId == null) {
		active =
			cachedConfig.enabled &&
			cachedConfig.excluded_user_ids.length === 0 &&
			cachedConfig.user_basis_points >= EXPERIMENT_BUCKET_RESOLUTION;
	} else {
		active = userGuildVisitsActive(userId);
	}
	if (!active) return {};
	const set = new Set(features);
	return {visits_enabled: set.has(GuildFeatures.VISITS_ENABLED) && !set.has(GuildFeatures.INVITES_DISABLED)};
}
