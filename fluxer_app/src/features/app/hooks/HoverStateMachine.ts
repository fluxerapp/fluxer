// SPDX-License-Identifier: AGPL-3.0-or-later

import {initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type HoverStateValue = 'idle' | 'hovering';

export type HoverStateEvent = {type: 'hover.enter'} | {type: 'hover.leave'};

export const hoverStateMachine = setup({
	types: {} as {
		events: HoverStateEvent;
	},
}).createMachine({
	id: 'hover',
	initial: 'idle',
	states: {
		idle: {
			on: {
				'hover.enter': {target: 'hovering'},
				'hover.leave': {target: 'idle'},
			},
		},
		hovering: {
			on: {
				'hover.enter': {target: 'hovering'},
				'hover.leave': {target: 'idle'},
			},
		},
	},
});

export type HoverStateSnapshot = SnapshotFrom<typeof hoverStateMachine>;

export function createHoverStateSnapshot(): HoverStateSnapshot {
	return initialTransition(hoverStateMachine)[0];
}

export function transitionHoverStateSnapshot(snapshot: HoverStateSnapshot, event: HoverStateEvent): HoverStateSnapshot {
	return transition(hoverStateMachine, snapshot, event)[0] as HoverStateSnapshot;
}

export function getHoverStateValue(snapshot: HoverStateSnapshot): HoverStateValue {
	return snapshot.value === 'hovering' ? 'hovering' : 'idle';
}

export function selectIsHovering(snapshot: HoverStateSnapshot): boolean {
	return getHoverStateValue(snapshot) === 'hovering';
}
