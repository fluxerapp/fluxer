// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface AutoAckWindowConditions {
	channelId: string | null;
	isAtBottom: boolean;
	canAutoAck: boolean;
}

interface AutoAckWindowContext extends AutoAckWindowConditions {
	commands: Array<AutoAckWindowCommand>;
}

interface AutoAckWindowInput {
	conditions?: Partial<AutoAckWindowConditions>;
}

export type AutoAckWindowCommand =
	| {
			type: 'enable';
			channelId: string;
	  }
	| {
			type: 'disable';
			channelId: string;
	  };

export type AutoAckWindowEvent = {
	type: 'autoAck.conditionsChanged';
	conditions: AutoAckWindowConditions;
};

const DEFAULT_CONDITIONS: AutoAckWindowConditions = {
	channelId: null,
	isAtBottom: false,
	canAutoAck: false,
};

function createContext(input: AutoAckWindowInput = {}): AutoAckWindowContext {
	return {
		...DEFAULT_CONDITIONS,
		...input.conditions,
		commands: [],
	};
}

function shouldEnable(conditions: AutoAckWindowConditions): boolean {
	return conditions.channelId != null && conditions.isAtBottom && conditions.canAutoAck;
}

function getCommands(previous: AutoAckWindowContext, next: AutoAckWindowConditions): Array<AutoAckWindowCommand> {
	const commands: Array<AutoAckWindowCommand> = [];
	if (previous.channelId != null && previous.channelId !== next.channelId) {
		commands.push({type: 'disable', channelId: previous.channelId});
	}
	if (next.channelId == null) {
		return commands;
	}
	if (shouldEnable(next)) {
		commands.push({type: 'enable', channelId: next.channelId});
	} else {
		commands.push({type: 'disable', channelId: next.channelId});
	}
	return commands;
}

export const autoAckWindowStateMachine = setup({
	types: {} as {
		context: AutoAckWindowContext;
		events: AutoAckWindowEvent;
		input: AutoAckWindowInput;
	},
	actions: {
		applyConditions: assign(({context, event}) => {
			if (event.type !== 'autoAck.conditionsChanged') return {};
			return {
				...event.conditions,
				commands: getCommands(context, event.conditions),
			};
		}),
	},
	guards: {
		hasNoChannel: ({context}) => context.channelId == null,
		canEnable: ({context}) => shouldEnable(context),
	},
}).createMachine({
	id: 'autoAckWindow',
	context: ({input}) => createContext(input),
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'hasNoChannel', target: 'noChannel'},
				{guard: 'canEnable', target: 'enabled'},
				{target: 'disabled'},
			],
		},
		noChannel: {
			on: {'autoAck.conditionsChanged': {target: 'routing', actions: 'applyConditions'}},
		},
		enabled: {
			on: {'autoAck.conditionsChanged': {target: 'routing', actions: 'applyConditions'}},
		},
		disabled: {
			on: {'autoAck.conditionsChanged': {target: 'routing', actions: 'applyConditions'}},
		},
	},
});

export type AutoAckWindowSnapshot = SnapshotFrom<typeof autoAckWindowStateMachine>;

export function createAutoAckWindowSnapshot(input: AutoAckWindowInput = {}): AutoAckWindowSnapshot {
	return initialTransition(autoAckWindowStateMachine, input)[0];
}

export function transitionAutoAckWindowSnapshot(
	snapshot: AutoAckWindowSnapshot,
	event: AutoAckWindowEvent,
): AutoAckWindowSnapshot {
	return transition(autoAckWindowStateMachine, snapshot, event)[0] as AutoAckWindowSnapshot;
}

export function selectAutoAckWindowCommands(snapshot: AutoAckWindowSnapshot): Array<AutoAckWindowCommand> {
	return snapshot.context.commands;
}
