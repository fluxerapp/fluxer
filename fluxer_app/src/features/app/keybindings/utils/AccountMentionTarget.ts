// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AccountEngineAccountActivity} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import {compare as compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeUtils';

interface OtherAccountMentionTarget {
	readonly accountKey: string;
	readonly guildId: string | null;
	readonly channelId: string;
	readonly messageId: string | null;
}

interface RankedTarget {
	readonly target: OtherAccountMentionTarget;
	readonly at: number;
	readonly id: string;
}

function isNewer(candidate: RankedTarget, current: RankedTarget | null): boolean {
	if (current === null) return true;
	if (candidate.at !== current.at) return candidate.at > current.at;
	return compareSnowflakes(candidate.id, current.id) > 0;
}

function parseTime(value: string): number {
	const parsed = Date.parse(value);
	return Number.isNaN(parsed) ? 0 : parsed;
}

export function pickNewestOtherAccountMention(
	activities: ReadonlyArray<AccountEngineAccountActivity>,
	currentAccountKey: string | null,
): OtherAccountMentionTarget | null {
	let newestMention: RankedTarget | null = null;
	let newestChannel: RankedTarget | null = null;
	for (const activity of activities) {
		if (activity.accountKey === currentAccountKey) continue;
		const mentionedChannelIds = new Set<string>();
		for (const channel of activity.channels) {
			if (channel.mentionCount <= 0) continue;
			mentionedChannelIds.add(channel.channelId);
			const candidate: RankedTarget = {
				target: {
					accountKey: activity.accountKey,
					guildId: channel.guildId,
					channelId: channel.channelId,
					messageId: null,
				},
				at: 0,
				id: channel.lastMessageId ?? '0',
			};
			if (isNewer(candidate, newestChannel)) newestChannel = candidate;
		}
		for (const mention of activity.mentions) {
			if (!mentionedChannelIds.has(mention.channelId)) continue;
			const candidate: RankedTarget = {
				target: {
					accountKey: activity.accountKey,
					guildId: mention.guildId,
					channelId: mention.channelId,
					messageId: mention.messageId,
				},
				at: parseTime(mention.timestamp),
				id: mention.messageId,
			};
			if (isNewer(candidate, newestMention)) newestMention = candidate;
		}
	}
	return newestMention?.target ?? newestChannel?.target ?? null;
}
