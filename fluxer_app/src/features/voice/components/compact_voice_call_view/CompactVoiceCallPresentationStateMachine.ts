// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface CompactVoiceCallPresentationInput {
	readonly audioOnly?: boolean;
	readonly fillHeight?: boolean;
	readonly showAvatarFallback?: boolean;
	readonly hasRenderableCallMedia?: boolean;
}

interface CompactVoiceCallPresentationMachineContext {
	audioOnly: boolean;
	fillHeight: boolean;
	showAvatarFallback: boolean;
	hasRenderableCallMedia: boolean;
}

export type CompactVoiceCallPresentationMachineEvent = {
	type: 'presentation.update';
	input: CompactVoiceCallPresentationInput;
};

export type CompactVoiceCallPresentationStateValue =
	| 'compactCallLayout'
	| 'fullHeightCallLayout'
	| 'avatarFallback'
	| 'empty';

export interface CompactVoiceCallPresentationModel {
	readonly stateValue: CompactVoiceCallPresentationStateValue;
	readonly shouldRenderCallLayout: boolean;
	readonly useFullHeightCallLayout: boolean;
	readonly shouldShowAvatarFallback: boolean;
	readonly shouldForceFloatingHudVisible: boolean;
}

function normalizeCompactVoiceCallPresentationInput(
	input: CompactVoiceCallPresentationInput,
): CompactVoiceCallPresentationMachineContext {
	return {
		audioOnly: input.audioOnly ?? false,
		fillHeight: input.fillHeight ?? false,
		showAvatarFallback: input.showAvatarFallback ?? true,
		hasRenderableCallMedia: input.hasRenderableCallMedia ?? false,
	};
}

function shouldRenderCallLayout(context: CompactVoiceCallPresentationMachineContext): boolean {
	return !context.audioOnly && (context.hasRenderableCallMedia || context.fillHeight);
}

function shouldUseFullHeightCallLayout(context: CompactVoiceCallPresentationMachineContext): boolean {
	return context.fillHeight && shouldRenderCallLayout(context);
}

function shouldShowAvatarFallback(context: CompactVoiceCallPresentationMachineContext): boolean {
	return context.showAvatarFallback && !shouldRenderCallLayout(context);
}

export const compactVoiceCallPresentationStateMachine = setup({
	types: {} as {
		context: CompactVoiceCallPresentationMachineContext;
		events: CompactVoiceCallPresentationMachineEvent;
		input: CompactVoiceCallPresentationInput;
	},
	guards: {
		usesFullHeightCallLayout: ({context}) => shouldUseFullHeightCallLayout(context),
		usesCompactCallLayout: ({context}) => shouldRenderCallLayout(context),
		showsAvatarFallback: ({context}) => shouldShowAvatarFallback(context),
	},
	actions: {
		applyInput: assign(({event}) => normalizeCompactVoiceCallPresentationInput(event.input)),
	},
}).createMachine({
	id: 'compactVoiceCallPresentation',
	context: ({input}) => normalizeCompactVoiceCallPresentationInput(input),
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'usesFullHeightCallLayout', target: 'fullHeightCallLayout'},
				{guard: 'usesCompactCallLayout', target: 'compactCallLayout'},
				{guard: 'showsAvatarFallback', target: 'avatarFallback'},
				{target: 'empty'},
			],
		},
		fullHeightCallLayout: {
			on: {
				'presentation.update': {target: 'routing', actions: 'applyInput'},
			},
		},
		compactCallLayout: {
			on: {
				'presentation.update': {target: 'routing', actions: 'applyInput'},
			},
		},
		avatarFallback: {
			on: {
				'presentation.update': {target: 'routing', actions: 'applyInput'},
			},
		},
		empty: {
			on: {
				'presentation.update': {target: 'routing', actions: 'applyInput'},
			},
		},
	},
});

export type CompactVoiceCallPresentationMachineSnapshot = SnapshotFrom<typeof compactVoiceCallPresentationStateMachine>;

export function createCompactVoiceCallPresentationSnapshot(
	input: CompactVoiceCallPresentationInput,
): CompactVoiceCallPresentationMachineSnapshot {
	return initialTransition(compactVoiceCallPresentationStateMachine, input)[0];
}

export function transitionCompactVoiceCallPresentationSnapshot(
	snapshot: CompactVoiceCallPresentationMachineSnapshot,
	event: CompactVoiceCallPresentationMachineEvent,
): CompactVoiceCallPresentationMachineSnapshot {
	return transition(
		compactVoiceCallPresentationStateMachine,
		snapshot,
		event,
	)[0] as CompactVoiceCallPresentationMachineSnapshot;
}

export function getCompactVoiceCallPresentationStateValue(
	snapshot: CompactVoiceCallPresentationMachineSnapshot,
): CompactVoiceCallPresentationStateValue {
	if (snapshot.value === 'fullHeightCallLayout') return 'fullHeightCallLayout';
	if (snapshot.value === 'compactCallLayout') return 'compactCallLayout';
	if (snapshot.value === 'avatarFallback') return 'avatarFallback';
	return 'empty';
}

export function selectCompactVoiceCallPresentationModel(
	snapshot: CompactVoiceCallPresentationMachineSnapshot,
): CompactVoiceCallPresentationModel {
	const stateValue = getCompactVoiceCallPresentationStateValue(snapshot);
	const shouldRenderResolvedCallLayout = stateValue === 'compactCallLayout' || stateValue === 'fullHeightCallLayout';
	const shouldShowResolvedAvatarFallback = stateValue === 'avatarFallback';
	return {
		stateValue,
		shouldRenderCallLayout: shouldRenderResolvedCallLayout,
		useFullHeightCallLayout: stateValue === 'fullHeightCallLayout',
		shouldShowAvatarFallback: shouldShowResolvedAvatarFallback,
		shouldForceFloatingHudVisible: shouldShowResolvedAvatarFallback,
	};
}

export function resolveCompactVoiceCallPresentationModel(
	input: CompactVoiceCallPresentationInput,
): CompactVoiceCallPresentationModel {
	return selectCompactVoiceCallPresentationModel(createCompactVoiceCallPresentationSnapshot(input));
}
