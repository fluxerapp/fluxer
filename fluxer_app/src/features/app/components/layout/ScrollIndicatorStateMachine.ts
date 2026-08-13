// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export type ScrollIndicatorSeverity = 'mention' | 'unread';
export type ScrollIndicatorDirection = 'top' | 'bottom';

export interface ScrollIndicatorTargetMeasurement {
	id: string;
	severity: ScrollIndicatorSeverity;
	top: number;
	bottom: number;
	order: number;
}

export interface ScrollIndicatorEdgeCandidate extends ScrollIndicatorTargetMeasurement {
	direction: ScrollIndicatorDirection;
	distance: number;
}

export interface ActiveScrollIndicator {
	direction: ScrollIndicatorDirection;
	indicator: ScrollIndicatorEdgeCandidate;
}

export interface ActiveScrollIndicators {
	top: ActiveScrollIndicator | null;
	bottom: ActiveScrollIndicator | null;
}

export interface ScrollIndicatorMeasurement {
	scrollTop: number;
	viewportHeight: number;
	targets: ReadonlyArray<ScrollIndicatorTargetMeasurement>;
	preferredDirection?: ScrollIndicatorDirection | null;
}

interface ScrollIndicatorMachineInput {
	activeIndicators?: ActiveScrollIndicators;
}

interface ScrollIndicatorMachineContext {
	activeIndicators: ActiveScrollIndicators;
}

export type ScrollIndicatorMachineEvent =
	| {
			type: 'scrollIndicator.measured';
			measurement: ScrollIndicatorMeasurement;
	  }
	| {
			type: 'scrollIndicator.reset';
	  };

const severityOrder: Record<ScrollIndicatorSeverity, number> = {
	mention: 2,
	unread: 1,
};

const VISIBILITY_EPSILON = 0.5;

function isFiniteMeasurement(target: ScrollIndicatorTargetMeasurement): boolean {
	return (
		Number.isFinite(target.top) &&
		Number.isFinite(target.bottom) &&
		Number.isFinite(target.order) &&
		target.bottom >= target.top
	);
}

function isBetterEdgeCandidate(
	candidate: ScrollIndicatorEdgeCandidate,
	current: ScrollIndicatorEdgeCandidate | null,
): boolean {
	if (!current) return true;
	const candidateSeverity = severityOrder[candidate.severity];
	const currentSeverity = severityOrder[current.severity];
	if (candidateSeverity !== currentSeverity) return candidateSeverity > currentSeverity;
	if (candidate.distance !== current.distance) return candidate.distance < current.distance;
	return candidate.order < current.order;
}

export function resolveScrollIndicatorEdgeCandidates(measurement: ScrollIndicatorMeasurement): {
	topIndicator: ScrollIndicatorEdgeCandidate | null;
	bottomIndicator: ScrollIndicatorEdgeCandidate | null;
} {
	const viewportTop = Math.max(0, measurement.scrollTop);
	const viewportBottom = viewportTop + Math.max(0, measurement.viewportHeight);
	let topIndicator: ScrollIndicatorEdgeCandidate | null = null;
	let bottomIndicator: ScrollIndicatorEdgeCandidate | null = null;
	for (const target of measurement.targets) {
		if (!target.id || !isFiniteMeasurement(target)) continue;
		if (target.bottom <= viewportTop + VISIBILITY_EPSILON) {
			const candidate = {
				...target,
				direction: 'top' as const,
				distance: Math.max(0, viewportTop - target.bottom),
			};
			if (isBetterEdgeCandidate(candidate, topIndicator)) topIndicator = candidate;
		} else if (target.top >= viewportBottom - VISIBILITY_EPSILON) {
			const candidate = {
				...target,
				direction: 'bottom' as const,
				distance: Math.max(0, target.top - viewportBottom),
			};
			if (isBetterEdgeCandidate(candidate, bottomIndicator)) bottomIndicator = candidate;
		}
	}
	return {topIndicator, bottomIndicator};
}

export function pickActiveScrollIndicator(
	topIndicator: ScrollIndicatorEdgeCandidate | null,
	bottomIndicator: ScrollIndicatorEdgeCandidate | null,
	preferredDirection: ScrollIndicatorDirection | null,
	previousDirection: ScrollIndicatorDirection | null,
): ActiveScrollIndicator | null {
	if (!topIndicator && !bottomIndicator) return null;
	if (!topIndicator && bottomIndicator) {
		return {direction: 'bottom', indicator: bottomIndicator};
	}
	if (topIndicator && !bottomIndicator) {
		return {direction: 'top', indicator: topIndicator};
	}
	if (!topIndicator || !bottomIndicator) return null;
	const topSeverityRank = severityOrder[topIndicator.severity];
	const bottomSeverityRank = severityOrder[bottomIndicator.severity];
	if (topSeverityRank > bottomSeverityRank) {
		return {direction: 'top', indicator: topIndicator};
	}
	if (bottomSeverityRank > topSeverityRank) {
		return {direction: 'bottom', indicator: bottomIndicator};
	}
	if (topIndicator.distance < bottomIndicator.distance) {
		return {direction: 'top', indicator: topIndicator};
	}
	if (bottomIndicator.distance < topIndicator.distance) {
		return {direction: 'bottom', indicator: bottomIndicator};
	}
	if (preferredDirection === 'top') {
		return {direction: 'top', indicator: topIndicator};
	}
	if (preferredDirection === 'bottom') {
		return {direction: 'bottom', indicator: bottomIndicator};
	}
	if (previousDirection === 'top') {
		return {direction: 'top', indicator: topIndicator};
	}
	if (previousDirection === 'bottom') {
		return {direction: 'bottom', indicator: bottomIndicator};
	}
	return {direction: 'top', indicator: topIndicator};
}

export function resolveActiveScrollIndicator(
	measurement: ScrollIndicatorMeasurement,
	previousDirection: ScrollIndicatorDirection | null = null,
): ActiveScrollIndicator | null {
	const {topIndicator, bottomIndicator} = resolveScrollIndicatorEdgeCandidates(measurement);
	return pickActiveScrollIndicator(
		topIndicator,
		bottomIndicator,
		measurement.preferredDirection ?? null,
		previousDirection,
	);
}

export function resolveActiveScrollIndicators(measurement: ScrollIndicatorMeasurement): ActiveScrollIndicators {
	const {topIndicator, bottomIndicator} = resolveScrollIndicatorEdgeCandidates(measurement);
	return {
		top: topIndicator == null ? null : {direction: 'top', indicator: topIndicator},
		bottom: bottomIndicator == null ? null : {direction: 'bottom', indicator: bottomIndicator},
	};
}

const EMPTY_ACTIVE_SCROLL_INDICATORS: ActiveScrollIndicators = Object.freeze({top: null, bottom: null});

function areActiveScrollIndicatorsEqual(left: ActiveScrollIndicators, right: ActiveScrollIndicators): boolean {
	return (
		areActiveScrollIndicatorsEqualAtEdge(left.top, right.top) &&
		areActiveScrollIndicatorsEqualAtEdge(left.bottom, right.bottom)
	);
}

function areActiveScrollIndicatorsEqualAtEdge(
	left: ActiveScrollIndicator | null,
	right: ActiveScrollIndicator | null,
): boolean {
	if (left === right) return true;
	if (left == null || right == null) return false;
	return left.indicator.id === right.indicator.id && left.indicator.severity === right.indicator.severity;
}

export const scrollIndicatorStateMachine = setup({
	types: {} as {
		context: ScrollIndicatorMachineContext;
		events: ScrollIndicatorMachineEvent;
		input: ScrollIndicatorMachineInput;
	},
	guards: {
		hasActiveIndicator: ({context}) => context.activeIndicators.top != null || context.activeIndicators.bottom != null,
	},
	actions: {
		applyMeasurement: assign(({context, event}) => {
			if (event.type !== 'scrollIndicator.measured') return {};
			const activeIndicators = resolveActiveScrollIndicators(event.measurement);
			if (areActiveScrollIndicatorsEqual(context.activeIndicators, activeIndicators)) return {};
			return {activeIndicators};
		}),
		reset: assign(() => ({activeIndicators: EMPTY_ACTIVE_SCROLL_INDICATORS})),
	},
}).createMachine({
	id: 'scrollIndicator',
	context: ({input}) => ({
		activeIndicators: input.activeIndicators == null ? EMPTY_ACTIVE_SCROLL_INDICATORS : input.activeIndicators,
	}),
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'hasActiveIndicator', target: 'visible'}, {target: 'hidden'}],
		},
		hidden: {
			on: {
				'scrollIndicator.measured': {target: 'routing', actions: 'applyMeasurement'},
				'scrollIndicator.reset': {target: 'routing', actions: 'reset'},
			},
		},
		visible: {
			on: {
				'scrollIndicator.measured': {target: 'routing', actions: 'applyMeasurement'},
				'scrollIndicator.reset': {target: 'routing', actions: 'reset'},
			},
		},
	},
});

export type ScrollIndicatorMachineSnapshot = SnapshotFrom<typeof scrollIndicatorStateMachine>;
export type ScrollIndicatorStateValue = 'hidden' | 'visible';

export function createScrollIndicatorSnapshot(input: ScrollIndicatorMachineInput = {}): ScrollIndicatorMachineSnapshot {
	return getInitialSnapshot(scrollIndicatorStateMachine, input);
}

export function transitionScrollIndicatorSnapshot(
	snapshot: ScrollIndicatorMachineSnapshot,
	event: ScrollIndicatorMachineEvent,
): ScrollIndicatorMachineSnapshot {
	return transition(scrollIndicatorStateMachine, snapshot, event)[0] as ScrollIndicatorMachineSnapshot;
}

export function getScrollIndicatorStateValue(snapshot: ScrollIndicatorMachineSnapshot): ScrollIndicatorStateValue {
	return snapshot.value === 'visible' ? 'visible' : 'hidden';
}

export function selectActiveScrollIndicator(snapshot: ScrollIndicatorMachineSnapshot): ActiveScrollIndicator | null {
	const topIndicator = snapshot.context.activeIndicators.top;
	return topIndicator == null ? snapshot.context.activeIndicators.bottom : topIndicator;
}

export function selectActiveScrollIndicators(snapshot: ScrollIndicatorMachineSnapshot): ActiveScrollIndicators {
	return snapshot.context.activeIndicators;
}
