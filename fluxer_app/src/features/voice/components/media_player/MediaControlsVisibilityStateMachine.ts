// SPDX-License-Identifier: AGPL-3.0-or-later

import {getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export interface MediaControlsVisibilitySignals {
	disabled: boolean;
	isPlaying: boolean;
	isInteracting: boolean;
}

export type MediaControlsVisibilityStateValue = 'visible' | 'hidden';

export type MediaControlsVisibilityEvent =
	| {type: 'controls.show'}
	| {type: 'controls.hide'}
	| {type: 'controls.mouseMove'}
	| {type: 'controls.mouseEnter'}
	| {type: 'controls.mouseLeave'; signals: MediaControlsVisibilitySignals}
	| {type: 'controls.touchStart'; signals: MediaControlsVisibilitySignals};

export const mediaControlsVisibilityStateMachine = setup({
	types: {} as {
		events: MediaControlsVisibilityEvent;
	},
	guards: {
		shouldHideOnPointerLeave: ({event}) =>
			event.type === 'controls.mouseLeave' && event.signals.isPlaying && !event.signals.isInteracting,
		shouldToggleOnTouch: ({event}) =>
			event.type === 'controls.touchStart' && event.signals.isPlaying && !event.signals.isInteracting,
	},
}).createMachine({
	id: 'mediaControlsVisibility',
	initial: 'visible',
	states: {
		visible: {
			on: {
				'controls.show': {target: 'visible'},
				'controls.hide': {target: 'hidden'},
				'controls.mouseMove': {target: 'visible'},
				'controls.mouseEnter': {target: 'visible'},
				'controls.mouseLeave': [{target: 'hidden', guard: 'shouldHideOnPointerLeave'}, {target: 'visible'}],
				'controls.touchStart': [{target: 'hidden', guard: 'shouldToggleOnTouch'}, {target: 'visible'}],
			},
		},
		hidden: {
			on: {
				'controls.show': {target: 'visible'},
				'controls.hide': {target: 'hidden'},
				'controls.mouseMove': {target: 'visible'},
				'controls.mouseEnter': {target: 'visible'},
				'controls.mouseLeave': {target: 'hidden'},
				'controls.touchStart': {target: 'visible'},
			},
		},
	},
});

export type MediaControlsVisibilitySnapshot = SnapshotFrom<typeof mediaControlsVisibilityStateMachine>;

export function createMediaControlsVisibilitySnapshot(): MediaControlsVisibilitySnapshot {
	return getInitialSnapshot(mediaControlsVisibilityStateMachine);
}

export function transitionMediaControlsVisibilitySnapshot(
	snapshot: MediaControlsVisibilitySnapshot,
	event: MediaControlsVisibilityEvent,
): MediaControlsVisibilitySnapshot {
	const [nextSnapshot] = transition(mediaControlsVisibilityStateMachine, snapshot, event);
	return nextSnapshot;
}

export function getMediaControlsVisibilityValue(
	snapshot: MediaControlsVisibilitySnapshot,
): MediaControlsVisibilityStateValue {
	return typeof snapshot.value === 'string' ? (snapshot.value as MediaControlsVisibilityStateValue) : 'visible';
}

export function selectMediaControlsVisible(
	snapshot: MediaControlsVisibilitySnapshot,
	signals: MediaControlsVisibilitySignals,
): boolean {
	return (
		signals.disabled ||
		!signals.isPlaying ||
		signals.isInteracting ||
		getMediaControlsVisibilityValue(snapshot) === 'visible'
	);
}
