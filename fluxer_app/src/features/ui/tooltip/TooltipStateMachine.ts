// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface TooltipEnvironment {
	disabled: boolean;
	mobileEnabled: boolean;
	contextMenuOpen: boolean;
	keyboardModeEnabled: boolean;
	hoverControlsEnabled: boolean;
	hasRenderableContent: boolean;
}

interface TooltipMachineInput extends Partial<TooltipEnvironment> {
	hovered?: boolean;
	focused?: boolean;
	delayPending?: boolean;
	dismissed?: boolean;
}

interface TooltipMachineContext extends TooltipEnvironment {
	hovered: boolean;
	focused: boolean;
	delayPending: boolean;
	dismissed: boolean;
}

export type TooltipMachineEvent =
	| {
			type: 'tooltip.environmentChanged';
			environment: TooltipEnvironment;
	  }
	| {
			type: 'tooltip.hoverChanged';
			hovered: boolean;
			delay: boolean;
	  }
	| {
			type: 'tooltip.focusChanged';
			focused: boolean;
			delay: boolean;
	  }
	| {
			type: 'tooltip.delayElapsed';
	  }
	| {
			type: 'tooltip.dismiss';
	  };

export type TooltipStateValue = 'hidden' | 'delayed' | 'visible';

export interface TooltipModel {
	state: TooltipStateValue;
	hovered: boolean;
	focused: boolean;
	delayPending: boolean;
	dismissed: boolean;
	hasVisibilityDriver: boolean;
	shouldRender: boolean;
}

export const DEFAULT_TOOLTIP_ENVIRONMENT: TooltipEnvironment = {
	disabled: false,
	mobileEnabled: false,
	contextMenuOpen: false,
	keyboardModeEnabled: false,
	hoverControlsEnabled: true,
	hasRenderableContent: true,
};

function createContext(input: TooltipMachineInput = {}): TooltipMachineContext {
	const environment = {...DEFAULT_TOOLTIP_ENVIRONMENT, ...input};
	return {
		disabled: environment.disabled,
		mobileEnabled: environment.mobileEnabled,
		contextMenuOpen: environment.contextMenuOpen,
		keyboardModeEnabled: environment.keyboardModeEnabled,
		hoverControlsEnabled: environment.hoverControlsEnabled,
		hasRenderableContent: environment.hasRenderableContent,
		hovered: input.hovered ?? false,
		focused: input.focused ?? false,
		delayPending: input.delayPending ?? false,
		dismissed: input.dismissed ?? false,
	};
}

function environmentAllowsTooltip(environment: TooltipEnvironment): boolean {
	return (
		!environment.disabled &&
		!environment.mobileEnabled &&
		!environment.contextMenuOpen &&
		environment.hoverControlsEnabled &&
		environment.hasRenderableContent
	);
}

function hasVisibilityDriver(context: TooltipMachineContext): boolean {
	return context.hovered || (context.keyboardModeEnabled && context.focused);
}

function shouldShowTooltip(context: TooltipMachineContext): boolean {
	return environmentAllowsTooltip(context) && hasVisibilityDriver(context) && !context.dismissed;
}

function shouldSuppressForEnvironmentChange(context: TooltipMachineContext, environment: TooltipEnvironment): boolean {
	if (!environmentAllowsTooltip(environment)) return true;
	if (context.focused && !context.hovered && !environment.keyboardModeEnabled) return true;
	return false;
}

function getDismissedAfterHoverChange(context: TooltipMachineContext, hovered: boolean): boolean {
	switch (true) {
		case hovered && environmentAllowsTooltip(context):
			return false;
		case hovered:
			return context.dismissed;
		case context.focused:
			return context.dismissed;
		default:
			return false;
	}
}

function getDismissedAfterFocusChange(context: TooltipMachineContext, focused: boolean): boolean {
	switch (true) {
		case !focused && context.hovered:
			return context.dismissed;
		case !focused:
			return false;
		case !environmentAllowsTooltip(context):
			return context.dismissed;
		case context.hovered:
			return context.dismissed;
		default:
			return !context.keyboardModeEnabled;
	}
}

function getDelayPendingAfterHoverChange(
	context: TooltipMachineContext,
	event: Extract<TooltipMachineEvent, {type: 'tooltip.hoverChanged'}>,
): boolean {
	switch (true) {
		case event.hovered:
			return event.delay;
		case context.focused && context.keyboardModeEnabled:
			return context.delayPending;
		default:
			return false;
	}
}

function getDelayPendingAfterFocusChange(
	context: TooltipMachineContext,
	event: Extract<TooltipMachineEvent, {type: 'tooltip.focusChanged'}>,
): boolean {
	switch (true) {
		case event.focused && context.keyboardModeEnabled:
			return event.delay;
		case context.hovered:
			return context.delayPending;
		default:
			return false;
	}
}

const tooltipStateTransitions = {
	'tooltip.environmentChanged': {target: 'routing', actions: 'applyEnvironment'},
	'tooltip.hoverChanged': {target: 'routing', actions: 'applyHover'},
	'tooltip.focusChanged': {target: 'routing', actions: 'applyFocus'},
	'tooltip.delayElapsed': {target: 'routing', actions: 'completeDelay'},
	'tooltip.dismiss': {target: 'routing', actions: 'dismiss'},
} as const;

export const tooltipStateMachine = setup({
	types: {} as {
		context: TooltipMachineContext;
		events: TooltipMachineEvent;
		input: TooltipMachineInput;
	},
	actions: {
		applyEnvironment: assign(({context, event}) => {
			if (event.type !== 'tooltip.environmentChanged') return {};
			const suppressed = shouldSuppressForEnvironmentChange(context, event.environment);
			return {
				...event.environment,
				delayPending: suppressed ? false : context.delayPending,
				dismissed: context.dismissed || suppressed,
			};
		}),
		applyHover: assign(({context, event}) => {
			if (event.type !== 'tooltip.hoverChanged') return {};
			return {
				hovered: event.hovered,
				delayPending: getDelayPendingAfterHoverChange(context, event),
				dismissed: getDismissedAfterHoverChange(context, event.hovered),
			};
		}),
		applyFocus: assign(({context, event}) => {
			if (event.type !== 'tooltip.focusChanged') return {};
			return {
				focused: event.focused,
				delayPending: getDelayPendingAfterFocusChange(context, event),
				dismissed: getDismissedAfterFocusChange(context, event.focused),
			};
		}),
		completeDelay: assign({
			delayPending: false,
		}),
		dismiss: assign({
			delayPending: false,
			dismissed: true,
		}),
	},
	guards: {
		shouldBeVisible: ({context}) => shouldShowTooltip(context) && !context.delayPending,
		shouldBeDelayed: ({context}) => shouldShowTooltip(context) && context.delayPending,
	},
}).createMachine({
	id: 'tooltip',
	context: ({input}) => createContext(input),
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'shouldBeVisible', target: 'visible'},
				{guard: 'shouldBeDelayed', target: 'delayed'},
				{target: 'hidden'},
			],
		},
		hidden: {
			on: tooltipStateTransitions,
		},
		delayed: {
			on: tooltipStateTransitions,
		},
		visible: {
			on: tooltipStateTransitions,
		},
	},
});

export type TooltipSnapshot = SnapshotFrom<typeof tooltipStateMachine>;

export function createTooltipSnapshot(input: TooltipMachineInput = {}): TooltipSnapshot {
	return initialTransition(tooltipStateMachine, input)[0];
}

export function transitionTooltipSnapshot(snapshot: TooltipSnapshot, event: TooltipMachineEvent): TooltipSnapshot {
	return transition(tooltipStateMachine, snapshot, event)[0] as TooltipSnapshot;
}

export function getTooltipStateValue(snapshot: TooltipSnapshot): TooltipStateValue {
	switch (snapshot.value) {
		case 'visible':
			return 'visible';
		case 'delayed':
			return 'delayed';
		default:
			return 'hidden';
	}
}

export function selectTooltipModel(snapshot: TooltipSnapshot): TooltipModel {
	const state = getTooltipStateValue(snapshot);
	const hasDriver = hasVisibilityDriver(snapshot.context);
	return {
		state,
		hovered: snapshot.context.hovered,
		focused: snapshot.context.focused,
		delayPending: snapshot.context.delayPending,
		dismissed: snapshot.context.dismissed,
		hasVisibilityDriver: hasDriver,
		shouldRender: state === 'visible',
	};
}

export function tooltipSnapshotsAreEquivalent(left: TooltipSnapshot, right: TooltipSnapshot): boolean {
	return (
		getTooltipStateValue(left) === getTooltipStateValue(right) &&
		left.context.disabled === right.context.disabled &&
		left.context.mobileEnabled === right.context.mobileEnabled &&
		left.context.contextMenuOpen === right.context.contextMenuOpen &&
		left.context.keyboardModeEnabled === right.context.keyboardModeEnabled &&
		left.context.hoverControlsEnabled === right.context.hoverControlsEnabled &&
		left.context.hasRenderableContent === right.context.hasRenderableContent &&
		left.context.hovered === right.context.hovered &&
		left.context.focused === right.context.focused &&
		left.context.delayPending === right.context.delayPending &&
		left.context.dismissed === right.context.dismissed
	);
}
