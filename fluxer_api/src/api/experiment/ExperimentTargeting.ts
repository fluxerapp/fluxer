// SPDX-License-Identifier: AGPL-3.0-or-later

import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {ExperimentTargeting} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';

interface TargetableExperimentConfig {
	readonly enabled: boolean;
	readonly included_guild_ids: ReadonlyArray<string>;
}

const NO_GUILDS: ReadonlySet<string> = new Set();

export async function resolveExperimentTargeting(
	user: User,
	config: TargetableExperimentConfig,
): Promise<ExperimentTargeting> {
	const needsGuilds = config.enabled && config.included_guild_ids.length > 0;
	const memberGuildIds = needsGuilds
		? new Set((await getUserRepository().getUserGuildIds(user.id)).map((guildId) => guildId.toString()))
		: NO_GUILDS;
	return {memberGuildIds, premium: !user.isBot && user.isPremium()};
}
