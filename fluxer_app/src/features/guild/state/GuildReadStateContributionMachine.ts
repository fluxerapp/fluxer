// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

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

export type GuildReadStateContributionEvent = {
	type: 'guildReadContribution.updated';
	input: GuildReadStateContributionInput;
};

function isMentionAllowed(snapshot: GuildReadStateContributionSnapshot): boolean {
	if (snapshot.context.mentionCount <= 0) return false;
	switch (snapshot.value) {
		case 'ineligible':
			return false;
		default:
			return true;
	}
}

function isUnreadAllowed(snapshot: GuildReadStateContributionSnapshot): boolean {
	if (!snapshot.context.hasUnread) return false;
	switch (snapshot.value) {
		case 'private':
		case 'allMessages':
		case 'legacyUnmuted':
			return true;
		default:
			return false;
	}
}

export const guildReadStateContributionMachine = setup({
	types: {} as {
		context: GuildReadStateContributionInput;
		events: GuildReadStateContributionEvent;
		input: GuildReadStateContributionInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'guildReadContribution.updated') return {};
			return event.input;
		}),
	},
	guards: {
		ineligible: ({context}) => !context.isPrivate && !context.isEligibleTextChannel,
		privateChannel: ({context}) => context.isPrivate,
		noMessages: ({context}) => context.unreadBadgesLevel === MessageNotifications.NO_MESSAGES,
		onlyMentions: ({context}) => context.unreadBadgesLevel === MessageNotifications.ONLY_MENTIONS,
		allMessages: ({context}) => context.unreadBadgesLevel === MessageNotifications.ALL_MESSAGES,
		muted: ({context}) => context.isMutedForUnread,
	},
}).createMachine({
	id: 'guildReadStateContribution',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'ineligible', target: 'ineligible'},
				{guard: 'privateChannel', target: 'private'},
				{guard: 'noMessages', target: 'noMessages'},
				{guard: 'onlyMentions', target: 'onlyMentions'},
				{guard: 'allMessages', target: 'allMessages'},
				{guard: 'muted', target: 'legacyMuted'},
				{target: 'legacyUnmuted'},
			],
		},
		ineligible: {
			on: {'guildReadContribution.updated': {target: 'routing', actions: 'applyInput'}},
		},
		private: {
			on: {'guildReadContribution.updated': {target: 'routing', actions: 'applyInput'}},
		},
		noMessages: {
			on: {'guildReadContribution.updated': {target: 'routing', actions: 'applyInput'}},
		},
		onlyMentions: {
			on: {'guildReadContribution.updated': {target: 'routing', actions: 'applyInput'}},
		},
		allMessages: {
			on: {'guildReadContribution.updated': {target: 'routing', actions: 'applyInput'}},
		},
		legacyMuted: {
			on: {'guildReadContribution.updated': {target: 'routing', actions: 'applyInput'}},
		},
		legacyUnmuted: {
			on: {'guildReadContribution.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type GuildReadStateContributionSnapshot = SnapshotFrom<typeof guildReadStateContributionMachine>;

export function createGuildReadStateContributionSnapshot(
	input: GuildReadStateContributionInput,
): GuildReadStateContributionSnapshot {
	return initialTransition(guildReadStateContributionMachine, input)[0];
}

export function transitionGuildReadStateContributionSnapshot(
	snapshot: GuildReadStateContributionSnapshot,
	event: GuildReadStateContributionEvent,
): GuildReadStateContributionSnapshot {
	return transition(guildReadStateContributionMachine, snapshot, event)[0] as GuildReadStateContributionSnapshot;
}

export function selectGuildReadStateContribution(
	snapshot: GuildReadStateContributionSnapshot,
): GuildReadStateContribution {
	return {
		mentionAllowed: isMentionAllowed(snapshot),
		unreadAllowed: isUnreadAllowed(snapshot),
		mentionCount: snapshot.context.mentionCount,
	};
}

export function resolveGuildReadStateContribution(input: GuildReadStateContributionInput): GuildReadStateContribution {
	return selectGuildReadStateContribution(createGuildReadStateContributionSnapshot(input));
}
