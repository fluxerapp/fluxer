// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';
import type {ChannelUnreadState, ChannelUnreadStateInput} from './ChannelUnreadState';

interface ChannelUnreadMachineContext extends ChannelUnreadStateInput {}

export type ChannelUnreadMachineEvent = {
	type: 'channelUnread.updated';
	input: ChannelUnreadStateInput;
};

export type ChannelUnreadStateValue = 'disabled' | 'onlyMentions' | 'allMessages' | 'legacy';

function getUnreadStateValue(snapshot: ChannelUnreadSnapshot): ChannelUnreadStateValue {
	switch (snapshot.value) {
		case 'disabled':
			return 'disabled';
		case 'onlyMentions':
			return 'onlyMentions';
		case 'allMessages':
			return 'allMessages';
		default:
			return 'legacy';
	}
}

export const channelUnreadStateMachine = setup({
	types: {} as {
		context: ChannelUnreadMachineContext;
		events: ChannelUnreadMachineEvent;
		input: ChannelUnreadStateInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'channelUnread.updated') return {};
			return event.input;
		}),
	},
	guards: {
		unreadBadgesDisabled: ({context}) => context.unreadBadgesLevel === MessageNotifications.NO_MESSAGES,
		unreadBadgesOnlyMentions: ({context}) => context.unreadBadgesLevel === MessageNotifications.ONLY_MENTIONS,
		unreadBadgesAllMessages: ({context}) => context.unreadBadgesLevel === MessageNotifications.ALL_MESSAGES,
	},
}).createMachine({
	id: 'channelUnread',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'unreadBadgesDisabled', target: 'disabled'},
				{guard: 'unreadBadgesOnlyMentions', target: 'onlyMentions'},
				{guard: 'unreadBadgesAllMessages', target: 'allMessages'},
				{target: 'legacy'},
			],
		},
		disabled: {
			on: {'channelUnread.updated': {target: 'routing', actions: 'applyInput'}},
		},
		onlyMentions: {
			on: {'channelUnread.updated': {target: 'routing', actions: 'applyInput'}},
		},
		allMessages: {
			on: {'channelUnread.updated': {target: 'routing', actions: 'applyInput'}},
		},
		legacy: {
			on: {'channelUnread.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ChannelUnreadSnapshot = SnapshotFrom<typeof channelUnreadStateMachine>;

export function createChannelUnreadSnapshot(input: ChannelUnreadStateInput): ChannelUnreadSnapshot {
	return getInitialSnapshot(channelUnreadStateMachine, input);
}

export function transitionChannelUnreadSnapshot(
	snapshot: ChannelUnreadSnapshot,
	event: ChannelUnreadMachineEvent,
): ChannelUnreadSnapshot {
	return transition(channelUnreadStateMachine, snapshot, event)[0] as ChannelUnreadSnapshot;
}

export function selectChannelUnreadState(snapshot: ChannelUnreadSnapshot): ChannelUnreadState {
	const context = snapshot.context;
	const hasUnreadMessages = context.hasUnread;
	const rawHasMentions = context.mentionCount > 0;
	switch (getUnreadStateValue(snapshot)) {
		case 'disabled':
			return {
				hasUnreadMessages,
				hasMentions: false,
				isHighlight: false,
				shouldShowUnreadIndicator: false,
				isUnreadIndicatorMuted: false,
				hasVisibleUnread: false,
			};
		case 'onlyMentions':
			return {
				hasUnreadMessages,
				hasMentions: rawHasMentions,
				isHighlight: rawHasMentions,
				shouldShowUnreadIndicator: hasUnreadMessages,
				isUnreadIndicatorMuted: hasUnreadMessages,
				hasVisibleUnread: rawHasMentions || hasUnreadMessages,
			};
		case 'allMessages':
			return {
				hasUnreadMessages,
				hasMentions: rawHasMentions,
				isHighlight: rawHasMentions || hasUnreadMessages,
				shouldShowUnreadIndicator: hasUnreadMessages,
				isUnreadIndicatorMuted: false,
				hasVisibleUnread: rawHasMentions || hasUnreadMessages,
			};
		case 'legacy': {
			const shouldShowUnreadIndicator =
				hasUnreadMessages && (!context.isMuted || context.showFadedUnreadOnMutedChannels);
			const hasMentions = rawHasMentions;
			return {
				hasUnreadMessages,
				hasMentions,
				isHighlight: rawHasMentions || (hasUnreadMessages && !context.isMuted),
				shouldShowUnreadIndicator,
				isUnreadIndicatorMuted: shouldShowUnreadIndicator && context.isMuted,
				hasVisibleUnread: hasMentions || shouldShowUnreadIndicator,
			};
		}
	}
}

export function resolveChannelUnreadState(input: ChannelUnreadStateInput): ChannelUnreadState {
	return selectChannelUnreadState(createChannelUnreadSnapshot(input));
}
