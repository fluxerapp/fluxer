// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseSnapshotEntity, parseSnapshotGuildMemberKey} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {StateSnapshotEntries} from '@app/features/gateway/snapshot/SnapshotTypes';

export class BackgroundGatewayReadyIdentityMismatchError extends Error {
	constructor(expectedUserId: string, receivedUserId: string) {
		super(`Background gateway READY user mismatch: expected ${expectedUserId}, received ${receivedUserId}`);
		this.name = 'BackgroundGatewayReadyIdentityMismatchError';
	}
}

export interface BackgroundSnapshotReadySeed {
	readonly user: {readonly id: string};
	readonly guilds: ReadonlyArray<unknown>;
	readonly private_channels: ReadonlyArray<unknown>;
	readonly relationships: ReadonlyArray<unknown>;
	readonly user_guild_settings: ReadonlyArray<unknown>;
	readonly read_states: ReadonlyArray<{readonly id: string; readonly mention_count: number}>;
}

export function buildBackgroundSnapshotReadySeed(
	entries: StateSnapshotEntries,
	userId: string,
	mentionCounts: ReadonlyMap<string, number>,
): BackgroundSnapshotReadySeed {
	const users = parseSnapshotEntity(entries, 'user');
	if (!Array.from(users.values()).some((user) => user.id === userId)) {
		throw new BackgroundGatewayReadyIdentityMismatchError(userId, 'missing');
	}
	const channelsByGuild = new Map<string, Array<unknown>>();
	const privateChannels: Array<unknown> = [];
	for (const channel of parseSnapshotEntity(entries, 'channel').values()) {
		const guildId = channel.guild_id;
		if (typeof guildId !== 'string') {
			privateChannels.push(channel);
			continue;
		}
		const channels = channelsByGuild.get(guildId);
		if (channels === undefined) {
			channelsByGuild.set(guildId, [channel]);
		} else {
			channels.push(channel);
		}
	}
	const selfMembersByGuild = new Map<string, Array<unknown>>();
	for (const [key, member] of parseSnapshotEntity(entries, 'guild_member')) {
		if (member.user.id !== userId) {
			continue;
		}
		const guildId = parseSnapshotGuildMemberKey(key).guildId;
		selfMembersByGuild.set(guildId, [member]);
	}
	const guilds = Array.from(parseSnapshotEntity(entries, 'guild').values(), (guild) => ({
		id: guild.id,
		properties: guild.properties,
		member_count: guild.member_count,
		channels: channelsByGuild.get(guild.id) ?? [],
		members: selfMembersByGuild.get(guild.id) ?? [],
	}));
	const userGuildSettings = Array.from(parseSnapshotEntity(entries, 'user_guild_settings').values());
	const readStates = Array.from(mentionCounts, ([channelId, mentionCount]) => ({
		id: channelId,
		mention_count: mentionCount,
	}));
	return {
		user: {id: userId},
		guilds,
		private_channels: privateChannels,
		relationships: Array.from(parseSnapshotEntity(entries, 'relationship').values()),
		user_guild_settings: userGuildSettings,
		read_states: readStates,
	};
}
