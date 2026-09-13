// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createSteppedCarouselSnapshot,
	getSteppedCarouselStateValue,
	resolveSteppedCarouselHeight,
	type SteppedCarouselMachineEvent,
	type SteppedCarouselSnapshot,
	selectSteppedCarouselModel,
	transitionSteppedCarouselSnapshot,
} from '@app/features/ui/stepped_carousel/SteppedCarouselStateMachine';
import {describe, expect, it} from 'vitest';

const STEPS = ['account', 'credentials', 'mfa'] as const;

function transition(snapshot: SteppedCarouselSnapshot, event: SteppedCarouselMachineEvent): SteppedCarouselSnapshot {
	return transitionSteppedCarouselSnapshot(snapshot, event);
}

describe('steppedCarouselStateMachine', () => {
	it('starts unmeasured with automatic height and no transition direction', () => {
		const snapshot = createSteppedCarouselSnapshot({step: 'credentials', steps: STEPS});

		expect(getSteppedCarouselStateValue(snapshot)).toBe('unmeasured');
		expect(selectSteppedCarouselModel(snapshot)).toMatchObject({
			step: 'credentials',
			direction: 0,
			contentHeight: 'auto',
			focusRequestId: 0,
		});
	});

	it('resolves forward and backward directions from the configured step order', () => {
		let snapshot = createSteppedCarouselSnapshot({step: 'account', steps: STEPS});
		snapshot = transition(snapshot, {
			type: 'carousel.propsChanged',
			step: 'mfa',
			steps: STEPS,
			focusOnStepChange: false,
		});

		expect(selectSteppedCarouselModel(snapshot).direction).toBe(2);

		snapshot = transition(snapshot, {
			type: 'carousel.propsChanged',
			step: 'credentials',
			steps: STEPS,
			focusOnStepChange: false,
		});

		expect(selectSteppedCarouselModel(snapshot).direction).toBe(-1);
	});

	it('keeps the current direction when props change without a step change', () => {
		let snapshot = createSteppedCarouselSnapshot({step: 'account', steps: STEPS});
		snapshot = transition(snapshot, {
			type: 'carousel.propsChanged',
			step: 'mfa',
			steps: STEPS,
			focusOnStepChange: false,
		});
		snapshot = transition(snapshot, {
			type: 'carousel.propsChanged',
			step: 'mfa',
			steps: [...STEPS],
			focusOnStepChange: false,
		});

		expect(selectSteppedCarouselModel(snapshot).direction).toBe(2);
	});

	it('lets explicit direction override derived direction', () => {
		const snapshot = transition(createSteppedCarouselSnapshot({step: 'account', steps: STEPS}), {
			type: 'carousel.propsChanged',
			step: 'mfa',
			steps: STEPS,
			direction: -10,
			focusOnStepChange: false,
		});

		expect(selectSteppedCarouselModel(snapshot).direction).toBe(-10);
	});

	it('uses neutral direction when either step is not in the configured order', () => {
		const snapshot = transition(createSteppedCarouselSnapshot({step: 'missing', steps: STEPS}), {
			type: 'carousel.propsChanged',
			step: 'mfa',
			steps: STEPS,
			focusOnStepChange: false,
		});

		expect(selectSteppedCarouselModel(snapshot).direction).toBe(0);
	});

	it('measures visible content height from the largest available DOM height', () => {
		let snapshot = createSteppedCarouselSnapshot({step: 'credentials', steps: STEPS});
		snapshot = transition(snapshot, {type: 'carousel.measured', offsetHeight: 120, scrollHeight: 180});

		expect(getSteppedCarouselStateValue(snapshot)).toBe('ready');
		expect(selectSteppedCarouselModel(snapshot).contentHeight).toBe(180);

		snapshot = transition(snapshot, {type: 'carousel.measured', offsetHeight: 220, scrollHeight: 200});

		expect(selectSteppedCarouselModel(snapshot).contentHeight).toBe(220);
	});

	it('keeps automatic height instead of clipping content when the first measurement is zero', () => {
		const snapshot = transition(createSteppedCarouselSnapshot({step: 'credentials', steps: STEPS}), {
			type: 'carousel.measured',
			offsetHeight: 0,
			scrollHeight: 0,
		});

		expect(getSteppedCarouselStateValue(snapshot)).toBe('ready');
		expect(selectSteppedCarouselModel(snapshot).contentHeight).toBe('auto');
	});

	it('requests focus only after the mounted carousel changes step with focus enabled', () => {
		let snapshot = createSteppedCarouselSnapshot({
			step: 'account',
			steps: STEPS,
			focusOnStepChange: true,
		});

		snapshot = transition(snapshot, {
			type: 'carousel.propsChanged',
			step: 'account',
			steps: STEPS,
			focusOnStepChange: true,
		});

		expect(selectSteppedCarouselModel(snapshot).focusRequestId).toBe(0);

		snapshot = transition(snapshot, {
			type: 'carousel.propsChanged',
			step: 'credentials',
			steps: STEPS,
			focusOnStepChange: true,
		});

		expect(selectSteppedCarouselModel(snapshot).focusRequestId).toBe(1);

		snapshot = transition(snapshot, {
			type: 'carousel.propsChanged',
			step: 'mfa',
			steps: STEPS,
			focusOnStepChange: false,
		});

		expect(selectSteppedCarouselModel(snapshot).focusRequestId).toBe(1);
	});
});

describe('resolveSteppedCarouselHeight', () => {
	it('falls back to auto for non-rendered panes', () => {
		expect(resolveSteppedCarouselHeight({offsetHeight: 0, scrollHeight: 0})).toBe('auto');
	});

	it('uses scrollHeight when transform or layout timing makes offsetHeight smaller', () => {
		expect(resolveSteppedCarouselHeight({offsetHeight: 0, scrollHeight: 320})).toBe(320);
	});
});
