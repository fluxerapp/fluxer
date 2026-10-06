// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface ReadStateMentionInput {
	authorBlocked: boolean;
	hasUserMention: boolean;
	hasEveryoneMention: boolean;
	hasRoleMention: boolean;
	isPrivate: boolean;
	isMuted: boolean;
}

export type ReadStateMentionReason = 'blocked' | 'user' | 'everyone' | 'role' | 'private' | 'none';

export type ReadStateMentionEvent = {
	type: 'readStateMention.updated';
	input: ReadStateMentionInput;
};

export interface ReadStateMentionModel {
	reason: ReadStateMentionReason;
	shouldMention: boolean;
}

function getMentionReason(snapshot: ReadStateMentionSnapshot): ReadStateMentionReason {
	switch (snapshot.value) {
		case 'blocked':
			return 'blocked';
		case 'user':
			return 'user';
		case 'everyone':
			return 'everyone';
		case 'role':
			return 'role';
		case 'private':
			return 'private';
		default:
			return 'none';
	}
}

export const readStateMentionMachine = setup({
	types: {} as {
		context: ReadStateMentionInput;
		events: ReadStateMentionEvent;
		input: ReadStateMentionInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'readStateMention.updated') return {};
			return event.input;
		}),
	},
	guards: {
		authorBlocked: ({context}) => context.authorBlocked,
		hasUserMention: ({context}) => context.hasUserMention,
		hasEveryoneMention: ({context}) => context.hasEveryoneMention,
		hasRoleMention: ({context}) => context.hasRoleMention,
		isUnmutedPrivateChannel: ({context}) => context.isPrivate && !context.isMuted,
	},
}).createMachine({
	id: 'readStateMention',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'authorBlocked', target: 'blocked'},
				{guard: 'hasUserMention', target: 'user'},
				{guard: 'hasEveryoneMention', target: 'everyone'},
				{guard: 'hasRoleMention', target: 'role'},
				{guard: 'isUnmutedPrivateChannel', target: 'private'},
				{target: 'none'},
			],
		},
		blocked: {
			on: {'readStateMention.updated': {target: 'routing', actions: 'applyInput'}},
		},
		user: {
			on: {'readStateMention.updated': {target: 'routing', actions: 'applyInput'}},
		},
		everyone: {
			on: {'readStateMention.updated': {target: 'routing', actions: 'applyInput'}},
		},
		role: {
			on: {'readStateMention.updated': {target: 'routing', actions: 'applyInput'}},
		},
		private: {
			on: {'readStateMention.updated': {target: 'routing', actions: 'applyInput'}},
		},
		none: {
			on: {'readStateMention.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ReadStateMentionSnapshot = SnapshotFrom<typeof readStateMentionMachine>;

export function createReadStateMentionSnapshot(input: ReadStateMentionInput): ReadStateMentionSnapshot {
	return initialTransition(readStateMentionMachine, input)[0];
}

export function transitionReadStateMentionSnapshot(
	snapshot: ReadStateMentionSnapshot,
	event: ReadStateMentionEvent,
): ReadStateMentionSnapshot {
	return transition(readStateMentionMachine, snapshot, event)[0] as ReadStateMentionSnapshot;
}

export function selectReadStateMentionModel(snapshot: ReadStateMentionSnapshot): ReadStateMentionModel {
	const reason = getMentionReason(snapshot);
	return {
		reason,
		shouldMention: reason !== 'blocked' && reason !== 'none',
	};
}

export function resolveReadStateMention(input: ReadStateMentionInput): ReadStateMentionModel {
	return selectReadStateMentionModel(createReadStateMentionSnapshot(input));
}
