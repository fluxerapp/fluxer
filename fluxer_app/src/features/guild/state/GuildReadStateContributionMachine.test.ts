// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createGuildReadStateContributionSnapshot,
	type GuildReadStateContributionInput,
	resolveGuildReadStateContribution,
	selectGuildReadStateContribution,
	transitionGuildReadStateContributionSnapshot,
} from '@app/features/guild/state/GuildReadStateContributionMachine';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {describe, expect, it} from 'vitest';

function input(overrides: Partial<GuildReadStateContributionInput> = {}): GuildReadStateContributionInput {
	return {
		isEligibleTextChannel: true,
		isPrivate: false,
		unreadBadgesLevel: null,
		isMutedForUnread: false,
		hasUnread: true,
		mentionCount: 2,
		...overrides,
	};
}

describe('guildReadStateContributionMachine', () => {
	it('suppresses channels that cannot contribute to guild read state', () => {
		expect(resolveGuildReadStateContribution(input({isEligibleTextChannel: false}))).toMatchObject({
			mentionAllowed: false,
			unreadAllowed: false,
		});
	});

	it('keeps mention contribution when unread badges are disabled', () => {
		expect(
			resolveGuildReadStateContribution(input({unreadBadgesLevel: MessageNotifications.NO_MESSAGES})),
		).toMatchObject({
			mentionAllowed: true,
			unreadAllowed: false,
		});
	});

	it('applies unread badge policies before legacy mute behavior', () => {
		expect(
			resolveGuildReadStateContribution(input({unreadBadgesLevel: MessageNotifications.ONLY_MENTIONS})),
		).toMatchObject({
			mentionAllowed: true,
			unreadAllowed: false,
		});
		expect(
			resolveGuildReadStateContribution(input({unreadBadgesLevel: MessageNotifications.ALL_MESSAGES})),
		).toMatchObject({
			mentionAllowed: true,
			unreadAllowed: true,
		});
	});

	it('keeps legacy muted channels mention-only and unmuted channels fully contributing', () => {
		expect(resolveGuildReadStateContribution(input({isMutedForUnread: true}))).toMatchObject({
			mentionAllowed: true,
			unreadAllowed: false,
		});
		expect(resolveGuildReadStateContribution(input({isMutedForUnread: false}))).toMatchObject({
			mentionAllowed: true,
			unreadAllowed: true,
		});
	});

	it('always allows private unread contribution when unread exists', () => {
		expect(
			resolveGuildReadStateContribution(
				input({
					isEligibleTextChannel: false,
					isPrivate: true,
					unreadBadgesLevel: MessageNotifications.NO_MESSAGES,
					isMutedForUnread: true,
				}),
			),
		).toMatchObject({
			mentionAllowed: true,
			unreadAllowed: true,
		});
	});

	it('does not allow empty mentions or empty unread counts to contribute', () => {
		expect(resolveGuildReadStateContribution(input({mentionCount: 0, hasUnread: false}))).toEqual({
			mentionAllowed: false,
			unreadAllowed: false,
			mentionCount: 0,
		});
	});

	it('updates contribution policy from later channel input', () => {
		const mutedSnapshot = createGuildReadStateContributionSnapshot(input({isMutedForUnread: true}));
		expect(selectGuildReadStateContribution(mutedSnapshot)).toMatchObject({unreadAllowed: false});

		const unmutedSnapshot = transitionGuildReadStateContributionSnapshot(mutedSnapshot, {
			type: 'guildReadContribution.updated',
			input: input({isMutedForUnread: false}),
		});

		expect(selectGuildReadStateContribution(unmutedSnapshot)).toMatchObject({unreadAllowed: true});
	});
});
