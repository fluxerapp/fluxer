// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type SteppedCarouselHeight = number | 'auto';

interface SteppedCarouselMachineInput {
	step: string;
	steps: ReadonlyArray<string>;
	direction?: number;
	focusOnStepChange?: boolean;
}

interface SteppedCarouselMachineContext {
	step: string;
	steps: ReadonlyArray<string>;
	direction: number;
	contentHeight: SteppedCarouselHeight;
	focusOnStepChange: boolean;
	focusRequestId: number;
}

export type SteppedCarouselMachineEvent =
	| {
			type: 'carousel.propsChanged';
			step: string;
			steps: ReadonlyArray<string>;
			direction?: number;
			focusOnStepChange: boolean;
	  }
	| {
			type: 'carousel.measured';
			offsetHeight: number;
			scrollHeight: number;
	  };

export interface SteppedCarouselModel {
	step: string;
	direction: number;
	contentHeight: SteppedCarouselHeight;
	focusRequestId: number;
}

function resolveDirection(steps: ReadonlyArray<string>, previousStep: string, nextStep: string): number {
	const currentIndex = steps.indexOf(nextStep);
	const previousIndex = steps.indexOf(previousStep);
	if (currentIndex === -1 || previousIndex === -1) {
		return 0;
	}
	return currentIndex - previousIndex;
}

export function resolveSteppedCarouselHeight({
	offsetHeight,
	scrollHeight,
}: {
	offsetHeight: number;
	scrollHeight: number;
}): SteppedCarouselHeight {
	const measuredHeight = Math.max(offsetHeight, scrollHeight);
	return measuredHeight > 0 ? measuredHeight : 'auto';
}

function createContext(input: SteppedCarouselMachineInput): SteppedCarouselMachineContext {
	return {
		step: input.step,
		steps: input.steps,
		direction: input.direction ?? 0,
		contentHeight: 'auto',
		focusOnStepChange: input.focusOnStepChange ?? false,
		focusRequestId: 0,
	};
}

function shouldRequestFocus(
	context: SteppedCarouselMachineContext,
	event: Extract<SteppedCarouselMachineEvent, {type: 'carousel.propsChanged'}>,
): boolean {
	if (!event.focusOnStepChange) {
		return false;
	}
	return context.step !== event.step || !context.focusOnStepChange;
}

export const steppedCarouselStateMachine = setup({
	types: {} as {
		context: SteppedCarouselMachineContext;
		events: SteppedCarouselMachineEvent;
		input: SteppedCarouselMachineInput;
	},
	actions: {
		applyProps: assign(({context, event}) => {
			if (event.type !== 'carousel.propsChanged') return {};
			const focusRequested = shouldRequestFocus(context, event);
			return {
				step: event.step,
				steps: event.steps,
				direction:
					event.direction ??
					(event.step === context.step ? context.direction : resolveDirection(event.steps, context.step, event.step)),
				focusOnStepChange: event.focusOnStepChange,
				focusRequestId: focusRequested ? context.focusRequestId + 1 : context.focusRequestId,
			};
		}),
		applyMeasurement: assign(({event}) => {
			if (event.type !== 'carousel.measured') return {};
			return {
				contentHeight: resolveSteppedCarouselHeight({
					offsetHeight: event.offsetHeight,
					scrollHeight: event.scrollHeight,
				}),
			};
		}),
	},
}).createMachine({
	id: 'steppedCarousel',
	context: ({input}) => createContext(input),
	initial: 'unmeasured',
	states: {
		unmeasured: {
			on: {
				'carousel.propsChanged': {actions: 'applyProps'},
				'carousel.measured': {target: 'ready', actions: 'applyMeasurement'},
			},
		},
		ready: {
			on: {
				'carousel.propsChanged': {actions: 'applyProps'},
				'carousel.measured': {actions: 'applyMeasurement'},
			},
		},
	},
});

export type SteppedCarouselSnapshot = SnapshotFrom<typeof steppedCarouselStateMachine>;
export type SteppedCarouselStateValue = 'unmeasured' | 'ready';

export function createSteppedCarouselSnapshot(input: SteppedCarouselMachineInput): SteppedCarouselSnapshot {
	return initialTransition(steppedCarouselStateMachine, input)[0];
}

export function transitionSteppedCarouselSnapshot(
	snapshot: SteppedCarouselSnapshot,
	event: SteppedCarouselMachineEvent,
): SteppedCarouselSnapshot {
	return transition(steppedCarouselStateMachine, snapshot, event)[0] as SteppedCarouselSnapshot;
}

export function getSteppedCarouselStateValue(snapshot: SteppedCarouselSnapshot): SteppedCarouselStateValue {
	return snapshot.value === 'ready' ? 'ready' : 'unmeasured';
}

export function selectSteppedCarouselModel(snapshot: SteppedCarouselSnapshot): SteppedCarouselModel {
	return {
		step: snapshot.context.step,
		direction: snapshot.context.direction,
		contentHeight: snapshot.context.contentHeight,
		focusRequestId: snapshot.context.focusRequestId,
	};
}
