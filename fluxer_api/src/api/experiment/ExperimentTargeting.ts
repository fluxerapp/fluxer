// SPDX-License-Identifier: AGPL-3.0-or-later

import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {ExperimentTargeting} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';

interface TargetableExperimentConfig {
	readonly enabled: boolean;
	readonly included_guild_ids: ReadonlyArray<string>;
}

const NO_GUILDS: ReadonlySet<string> = new Set();

export const ANONYMOUS_EXPERIMENT_TARGETING: ExperimentTargeting = {
	memberGuildIds: NO_GUILDS,
	premium: false,
};

export async function resolveExperimentTargeting(
	user: User,
	configs: ReadonlyArray<TargetableExperimentConfig>,
): Promise<ExperimentTargeting> {
	const needsGuilds = configs.some((config) => config.enabled && config.included_guild_ids.length > 0);
	const memberGuildIds = needsGuilds
		? new Set((await getUserRepository().getUserGuildIds(user.id)).map((guildId) => guildId.toString()))
		: NO_GUILDS;
	return {memberGuildIds, premium: !user.isBot && user.isPremium()};
}
