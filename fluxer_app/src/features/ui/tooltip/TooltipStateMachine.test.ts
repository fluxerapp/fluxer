// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createTooltipSnapshot,
	DEFAULT_TOOLTIP_ENVIRONMENT,
	selectTooltipModel,
	type TooltipEnvironment,
	type TooltipMachineEvent,
	type TooltipModel,
	type TooltipSnapshot,
	tooltipSnapshotsAreEquivalent,
	transitionTooltipSnapshot,
} from '@app/features/ui/tooltip/TooltipStateMachine';
import {describe, expect, it} from 'vitest';

function environment(overrides: Partial<TooltipEnvironment> = {}): TooltipEnvironment {
	return {...DEFAULT_TOOLTIP_ENVIRONMENT, ...overrides};
}

function transition(snapshot: TooltipSnapshot, event: TooltipMachineEvent): TooltipSnapshot {
	return transitionTooltipSnapshot(snapshot, event);
}

function expectModel(snapshot: TooltipSnapshot, expected: Partial<TooltipModel>) {
	expect(selectTooltipModel(snapshot)).toMatchObject(expected);
}

describe('tooltipStateMachine', () => {
	it('starts hidden without a visibility driver', () => {
		const snapshot = createTooltipSnapshot();

		expectModel(snapshot, {
			state: 'hidden',
			shouldRender: false,
			hasVisibilityDriver: false,
		});
	});

	it('opens immediately from hover when the environment allows tooltips', () => {
		const snapshot = transition(createTooltipSnapshot(), {
			type: 'tooltip.hoverChanged',
			hovered: true,
			delay: false,
		});

		expectModel(snapshot, {
			state: 'visible',
			hovered: true,
			delayPending: false,
			shouldRender: true,
		});
	});

	it('waits in the delayed state until the delay elapses', () => {
		let snapshot = transition(createTooltipSnapshot(), {
			type: 'tooltip.hoverChanged',
			hovered: true,
			delay: true,
		});

		expectModel(snapshot, {
			state: 'delayed',
			delayPending: true,
			shouldRender: false,
		});

		snapshot = transition(snapshot, {type: 'tooltip.delayElapsed'});

		expectModel(snapshot, {
			state: 'visible',
			delayPending: false,
			shouldRender: true,
		});
	});

	it('cancels a pending delayed hover when hover leaves', () => {
		let snapshot = transition(createTooltipSnapshot(), {
			type: 'tooltip.hoverChanged',
			hovered: true,
			delay: true,
		});

		snapshot = transition(snapshot, {
			type: 'tooltip.hoverChanged',
			hovered: false,
			delay: false,
		});

		expectModel(snapshot, {
			state: 'hidden',
			hovered: false,
			delayPending: false,
			shouldRender: false,
		});
	});

	it('opens from focus only when keyboard mode is enabled', () => {
		let snapshot = transition(createTooltipSnapshot(), {
			type: 'tooltip.focusChanged',
			focused: true,
			delay: false,
		});

		expectModel(snapshot, {
			state: 'hidden',
			focused: true,
			dismissed: true,
			shouldRender: false,
		});

		snapshot = transition(
			createTooltipSnapshot({
				keyboardModeEnabled: true,
			}),
			{
				type: 'tooltip.focusChanged',
				focused: true,
				delay: false,
			},
		);

		expectModel(snapshot, {
			state: 'visible',
			focused: true,
			dismissed: false,
			shouldRender: true,
		});
	});

	it('keeps a hovered tooltip open when keyboard focus leaves', () => {
		let snapshot = createTooltipSnapshot({keyboardModeEnabled: true});
		snapshot = transition(snapshot, {
			type: 'tooltip.hoverChanged',
			hovered: true,
			delay: false,
		});
		snapshot = transition(snapshot, {
			type: 'tooltip.focusChanged',
			focused: true,
			delay: false,
		});
		snapshot = transition(snapshot, {
			type: 'tooltip.focusChanged',
			focused: false,
			delay: false,
		});

		expectModel(snapshot, {
			state: 'visible',
			hovered: true,
			focused: false,
			shouldRender: true,
		});
	});

	it('dismisses until a fresh hover enters again', () => {
		let snapshot = transition(createTooltipSnapshot(), {
			type: 'tooltip.hoverChanged',
			hovered: true,
			delay: false,
		});
		snapshot = transition(snapshot, {type: 'tooltip.dismiss'});

		expectModel(snapshot, {
			state: 'hidden',
			hovered: true,
			dismissed: true,
			shouldRender: false,
		});

		snapshot = transition(snapshot, {
			type: 'tooltip.hoverChanged',
			hovered: false,
			delay: false,
		});
		snapshot = transition(snapshot, {
			type: 'tooltip.hoverChanged',
			hovered: true,
			delay: false,
		});

		expectModel(snapshot, {
			state: 'visible',
			hovered: true,
			dismissed: false,
			shouldRender: true,
		});
	});

	it('hides and suppresses re-opening when the environment becomes blocked', () => {
		let snapshot = transition(createTooltipSnapshot(), {
			type: 'tooltip.hoverChanged',
			hovered: true,
			delay: false,
		});

		snapshot = transition(snapshot, {
			type: 'tooltip.environmentChanged',
			environment: environment({contextMenuOpen: true}),
		});

		expectModel(snapshot, {
			state: 'hidden',
			hovered: true,
			dismissed: true,
			shouldRender: false,
		});

		snapshot = transition(snapshot, {
			type: 'tooltip.environmentChanged',
			environment: environment(),
		});

		expectModel(snapshot, {
			state: 'hidden',
			hovered: true,
			dismissed: true,
			shouldRender: false,
		});
	});

	it.each([
		['disabled', {disabled: true}],
		['mobile layout', {mobileEnabled: true}],
		['context menu', {contextMenuOpen: true}],
		['blocked hover controls', {hoverControlsEnabled: false}],
		['empty content', {hasRenderableContent: false}],
	] satisfies Array<
		[string, Partial<TooltipEnvironment>]
	>)('does not open while %s blocks the environment', (_name, override) => {
		const snapshot = transition(createTooltipSnapshot(override), {
			type: 'tooltip.hoverChanged',
			hovered: true,
			delay: false,
		});

		expectModel(snapshot, {
			state: 'hidden',
			hovered: true,
			shouldRender: false,
		});
	});

	it('hides a focus-only tooltip if keyboard mode turns off', () => {
		let snapshot = createTooltipSnapshot({keyboardModeEnabled: true});
		snapshot = transition(snapshot, {
			type: 'tooltip.focusChanged',
			focused: true,
			delay: false,
		});
		snapshot = transition(snapshot, {
			type: 'tooltip.environmentChanged',
			environment: environment({keyboardModeEnabled: false}),
		});

		expectModel(snapshot, {
			state: 'hidden',
			focused: true,
			dismissed: true,
			shouldRender: false,
		});
	});

	it('reports equivalent snapshots for no-op transitions', () => {
		const snapshot = createTooltipSnapshot();
		const nextSnapshot = transition(snapshot, {type: 'tooltip.delayElapsed'});

		expect(tooltipSnapshotsAreEquivalent(snapshot, nextSnapshot)).toBe(true);
	});
});
