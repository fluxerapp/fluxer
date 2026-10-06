// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type MentionType = '@everyone' | '@here' | 'role';

export interface MentionConfirmationInfo {
	mentionType: MentionType;
	memberCount: number;
	content: string;
	sourceContent: string;
	tts?: boolean;
	roleId?: string;
	roleName?: string;
}

interface MentionConfirmationContext {
	pending: MentionConfirmationInfo | null;
}

export type MentionConfirmationEvent =
	| {
			type: 'mentionConfirmation.requested';
			info: MentionConfirmationInfo;
			currentSourceContent: string;
	  }
	| {
			type: 'mentionConfirmation.composerChanged';
			sourceContent: string;
	  }
	| {
			type: 'mentionConfirmation.confirmed';
	  }
	| {
			type: 'mentionConfirmation.dismissed';
	  }
	| {
			type: 'mentionConfirmation.reset';
	  };

export interface MentionConfirmationModel {
	visible: boolean;
	pending: MentionConfirmationInfo | null;
}

export type MentionConfirmationSnapshot = SnapshotFrom<typeof mentionConfirmationStateMachine>;

function isCurrentRequest(event: MentionConfirmationEvent): boolean {
	return event.type === 'mentionConfirmation.requested' && event.info.sourceContent === event.currentSourceContent;
}

export const mentionConfirmationStateMachine = setup({
	types: {} as {
		context: MentionConfirmationContext;
		events: MentionConfirmationEvent;
	},
	guards: {
		isCurrentRequest: ({event}) => isCurrentRequest(event),
		isSameComposerContent: ({context, event}) =>
			event.type === 'mentionConfirmation.composerChanged' && context.pending?.sourceContent === event.sourceContent,
	},
	actions: {
		assignPending: assign(({event}) => {
			if (event.type !== 'mentionConfirmation.requested') {
				return {};
			}
			return {pending: event.info};
		}),
		clearPending: assign({pending: null}),
	},
}).createMachine({
	id: 'mentionConfirmation',
	context: () => ({
		pending: null,
	}),
	initial: 'idle',
	states: {
		idle: {
			on: {
				'mentionConfirmation.requested': {
					guard: 'isCurrentRequest',
					target: 'visible',
					actions: 'assignPending',
				},
				'mentionConfirmation.composerChanged': {},
				'mentionConfirmation.confirmed': {},
				'mentionConfirmation.dismissed': {},
				'mentionConfirmation.reset': {},
			},
		},
		visible: {
			on: {
				'mentionConfirmation.requested': {
					guard: 'isCurrentRequest',
					actions: 'assignPending',
				},
				'mentionConfirmation.composerChanged': [
					{
						guard: 'isSameComposerContent',
					},
					{
						target: 'idle',
						actions: 'clearPending',
					},
				],
				'mentionConfirmation.confirmed': {
					target: 'idle',
					actions: 'clearPending',
				},
				'mentionConfirmation.dismissed': {
					target: 'idle',
					actions: 'clearPending',
				},
				'mentionConfirmation.reset': {
					target: 'idle',
					actions: 'clearPending',
				},
			},
		},
	},
});

export function createMentionConfirmationSnapshot(): MentionConfirmationSnapshot {
	return initialTransition(mentionConfirmationStateMachine)[0];
}

export function transitionMentionConfirmationSnapshot(
	snapshot: MentionConfirmationSnapshot,
	event: MentionConfirmationEvent,
): MentionConfirmationSnapshot {
	return transition(mentionConfirmationStateMachine, snapshot, event)[0] as MentionConfirmationSnapshot;
}

export function selectMentionConfirmationModel(snapshot: MentionConfirmationSnapshot): MentionConfirmationModel {
	return {
		visible: snapshot.matches('visible'),
		pending: snapshot.context.pending,
	};
}
