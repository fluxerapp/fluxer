// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild} from '@app/api/models/Guild';

export const DISQUALIFIED_MUTUAL_GUILD_DM_ACCESS_GUILD_IDS = [
	1427764882469228556n,
	1427764813854588940n,
	1473086401113346066n,
] as const;

const DISQUALIFIED_MUTUAL_GUILD_DM_ACCESS_GUILD_ID_SET: ReadonlySet<bigint> = new Set(
	DISQUALIFIED_MUTUAL_GUILD_DM_ACCESS_GUILD_IDS,
);

export function guildQualifiesForMutualGuildDmAccess(guild: Guild): boolean {
	return !DISQUALIFIED_MUTUAL_GUILD_DM_ACCESS_GUILD_ID_SET.has(guild.id);
}

export function getMutualGuildsForDmAccess({
	userGuilds,
	targetGuilds,
}: {
	userGuilds: ReadonlyArray<Guild>;
	targetGuilds: ReadonlyArray<Guild>;
}): Array<Guild> {
	const userGuildIdSet: ReadonlySet<bigint> = new Set(userGuilds.map((guild) => guild.id));
	return targetGuilds.filter((guild) => userGuildIdSet.has(guild.id) && guildQualifiesForMutualGuildDmAccess(guild));
}

export function hasMutualGuildForDmAccess(params: {
	userGuilds: ReadonlyArray<Guild>;
	targetGuilds: ReadonlyArray<Guild>;
}): boolean {
	return getMutualGuildsForDmAccess(params).length > 0;
}
