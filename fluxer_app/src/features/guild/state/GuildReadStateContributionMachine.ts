// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';

export interface GuildReadStateContributionInput {
	isEligibleTextChannel: boolean;
	isPrivate: boolean;
	unreadBadgesLevel: number | null;
	isMutedForUnread: boolean;
	hasUnread: boolean;
	mentionCount: number;
}

export interface GuildReadStateContribution {
	mentionAllowed: boolean;
	unreadAllowed: boolean;
	mentionCount: number;
}

export function resolveGuildReadStateContribution(input: GuildReadStateContributionInput): GuildReadStateContribution {
	const ineligible = !input.isPrivate && !input.isEligibleTextChannel;
	const unreadAllowed =
		!ineligible &&
		input.hasUnread &&
		(input.isPrivate ||
			input.unreadBadgesLevel === MessageNotifications.ALL_MESSAGES ||
			(input.unreadBadgesLevel !== MessageNotifications.NO_MESSAGES &&
				input.unreadBadgesLevel !== MessageNotifications.ONLY_MENTIONS &&
				!input.isMutedForUnread));
	return {mentionAllowed: !ineligible && !(input.mentionCount <= 0), unreadAllowed, mentionCount: input.mentionCount};
}
