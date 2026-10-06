// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type VoiceCallLayoutPresentationMode = 'grid' | 'focus';

export interface VoiceCallLayoutPresentationInput {
	readonly layoutMode: VoiceCallLayoutPresentationMode;
	readonly hasFocusMainTrack?: boolean;
	readonly secondaryFocusTrackCount?: number;
	readonly compact?: boolean;
	readonly isVoiceCallAppFullscreen?: boolean;
	readonly membersRowVisible?: boolean;
	readonly hasScreenShare?: boolean;
}

interface VoiceCallLayoutPresentationContext {
	layoutMode: VoiceCallLayoutPresentationMode;
	hasFocusMainTrack: boolean;
	secondaryFocusTrackCount: number;
	compact: boolean;
	isVoiceCallAppFullscreen: boolean;
	membersRowVisible: boolean;
	hasScreenShare: boolean;
	participantsGridExpanded: boolean;
}

export type VoiceCallLayoutPresentationMachineEvent =
	| {type: 'presentation.update'; input: VoiceCallLayoutPresentationInput}
	| {type: 'participants.toggle'}
	| {type: 'participants.collapse'};

export type VoiceCallLayoutPresentationStateValue =
	| 'gridLayout'
	| 'focusFallbackGrid'
	| 'focusLayout'
	| 'focusParticipantsExpanded';

export interface VoiceCallLayoutPresentationModel {
	readonly stateValue: VoiceCallLayoutPresentationStateValue;
	readonly mainLayout: VoiceCallLayoutPresentationMode;
	readonly hasSecondaryFocusTracks: boolean;
	readonly hasMiniGrid: boolean;
	readonly canShowParticipantsGridPanel: boolean;
	readonly participantsGridExpanded: boolean;
	readonly isParticipantsExpanded: boolean;
	readonly isFullscreenFocusLayout: boolean;
	readonly shouldRenderFocusFallbackGrid: boolean;
	readonly shouldRenderFocusLayout: boolean;
	readonly showMembersRow: boolean;
	readonly showMembersToggle: boolean;
	readonly shouldWrapScreenShareGrid: boolean;
	readonly focusScrollerOverflow: 'auto' | 'hidden';
}

function normalizeVoiceCallLayoutPresentationInput(
	input: VoiceCallLayoutPresentationInput,
): Omit<VoiceCallLayoutPresentationContext, 'participantsGridExpanded'> {
	return {
		layoutMode: input.layoutMode,
		hasFocusMainTrack: input.hasFocusMainTrack ?? false,
		secondaryFocusTrackCount: Math.max(0, input.secondaryFocusTrackCount ?? 0),
		compact: input.compact ?? false,
		isVoiceCallAppFullscreen: input.isVoiceCallAppFullscreen ?? false,
		membersRowVisible: input.membersRowVisible ?? false,
		hasScreenShare: input.hasScreenShare ?? false,
	};
}

function createVoiceCallLayoutPresentationContext(
	input: VoiceCallLayoutPresentationInput,
	participantsGridExpanded = false,
): VoiceCallLayoutPresentationContext {
	const normalizedInput = normalizeVoiceCallLayoutPresentationInput(input);
	const context = {
		...normalizedInput,
		participantsGridExpanded,
	};
	return canShowParticipantsGridPanel(context) ? context : {...context, participantsGridExpanded: false};
}

function applyVoiceCallLayoutPresentationInput(
	context: VoiceCallLayoutPresentationContext,
	input: VoiceCallLayoutPresentationInput,
): VoiceCallLayoutPresentationContext {
	return createVoiceCallLayoutPresentationContext(input, context.participantsGridExpanded);
}

function hasSecondaryFocusTracks(context: VoiceCallLayoutPresentationContext): boolean {
	return context.secondaryFocusTrackCount > 0;
}

function canShowParticipantsGridPanel(context: VoiceCallLayoutPresentationContext): boolean {
	return hasSecondaryFocusTracks(context) && context.compact && context.isVoiceCallAppFullscreen;
}

function usesGridLayout(context: VoiceCallLayoutPresentationContext): boolean {
	return context.layoutMode === 'grid';
}

function canRenderSecondaryFocusTracks(context: VoiceCallLayoutPresentationContext): boolean {
	if (canShowParticipantsGridPanel(context)) return true;
	if (!context.compact && context.isVoiceCallAppFullscreen) return true;
	return false;
}

function usesFocusFallbackGrid(context: VoiceCallLayoutPresentationContext): boolean {
	if (context.layoutMode !== 'focus') return false;
	if (context.hasFocusMainTrack) return false;
	if (!hasSecondaryFocusTracks(context)) return true;
	return !canRenderSecondaryFocusTracks(context);
}

function showsExpandedFocusParticipants(context: VoiceCallLayoutPresentationContext): boolean {
	return context.layoutMode === 'focus' && canShowParticipantsGridPanel(context) && context.participantsGridExpanded;
}

export const voiceCallLayoutPresentationStateMachine = setup({
	types: {} as {
		context: VoiceCallLayoutPresentationContext;
		events: VoiceCallLayoutPresentationMachineEvent;
		input: VoiceCallLayoutPresentationInput;
	},
	guards: {
		usesGridLayout: ({context}) => usesGridLayout(context),
		usesFocusFallbackGrid: ({context}) => usesFocusFallbackGrid(context),
		showsExpandedFocusParticipants: ({context}) => showsExpandedFocusParticipants(context),
	},
	actions: {
		applyInput: assign(({context, event}) => {
			if (event.type !== 'presentation.update') return {};
			return applyVoiceCallLayoutPresentationInput(context, event.input);
		}),
		toggleParticipantsGrid: assign(({context}) => ({
			participantsGridExpanded: canShowParticipantsGridPanel(context) ? !context.participantsGridExpanded : false,
		})),
		collapseParticipantsGrid: assign({participantsGridExpanded: false}),
	},
}).createMachine({
	id: 'voiceCallLayoutPresentation',
	context: ({input}) => createVoiceCallLayoutPresentationContext(input),
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'usesGridLayout', target: 'gridLayout'},
				{guard: 'usesFocusFallbackGrid', target: 'focusFallbackGrid'},
				{guard: 'showsExpandedFocusParticipants', target: 'focusParticipantsExpanded'},
				{target: 'focusLayout'},
			],
		},
		gridLayout: {
			on: {
				'presentation.update': {target: 'routing', actions: 'applyInput'},
				'participants.toggle': {target: 'routing', actions: 'toggleParticipantsGrid'},
				'participants.collapse': {target: 'routing', actions: 'collapseParticipantsGrid'},
			},
		},
		focusFallbackGrid: {
			on: {
				'presentation.update': {target: 'routing', actions: 'applyInput'},
				'participants.toggle': {target: 'routing', actions: 'toggleParticipantsGrid'},
				'participants.collapse': {target: 'routing', actions: 'collapseParticipantsGrid'},
			},
		},
		focusLayout: {
			on: {
				'presentation.update': {target: 'routing', actions: 'applyInput'},
				'participants.toggle': {target: 'routing', actions: 'toggleParticipantsGrid'},
				'participants.collapse': {target: 'routing', actions: 'collapseParticipantsGrid'},
			},
		},
		focusParticipantsExpanded: {
			on: {
				'presentation.update': {target: 'routing', actions: 'applyInput'},
				'participants.toggle': {target: 'routing', actions: 'toggleParticipantsGrid'},
				'participants.collapse': {target: 'routing', actions: 'collapseParticipantsGrid'},
			},
		},
	},
});

export type VoiceCallLayoutPresentationMachineSnapshot = SnapshotFrom<typeof voiceCallLayoutPresentationStateMachine>;

export function createVoiceCallLayoutPresentationSnapshot(
	input: VoiceCallLayoutPresentationInput,
): VoiceCallLayoutPresentationMachineSnapshot {
	return initialTransition(voiceCallLayoutPresentationStateMachine, input)[0];
}

export function transitionVoiceCallLayoutPresentationSnapshot(
	snapshot: VoiceCallLayoutPresentationMachineSnapshot,
	event: VoiceCallLayoutPresentationMachineEvent,
): VoiceCallLayoutPresentationMachineSnapshot {
	return transition(
		voiceCallLayoutPresentationStateMachine,
		snapshot,
		event,
	)[0] as VoiceCallLayoutPresentationMachineSnapshot;
}

export function getVoiceCallLayoutPresentationStateValue(
	snapshot: VoiceCallLayoutPresentationMachineSnapshot,
): VoiceCallLayoutPresentationStateValue {
	if (snapshot.value === 'gridLayout') return 'gridLayout';
	if (snapshot.value === 'focusFallbackGrid') return 'focusFallbackGrid';
	if (snapshot.value === 'focusParticipantsExpanded') return 'focusParticipantsExpanded';
	return 'focusLayout';
}

export function selectVoiceCallLayoutPresentationModel(
	snapshot: VoiceCallLayoutPresentationMachineSnapshot,
): VoiceCallLayoutPresentationModel {
	const stateValue = getVoiceCallLayoutPresentationStateValue(snapshot);
	const context = snapshot.context;
	const hasMiniGrid = hasSecondaryFocusTracks(context);
	const canShowParticipants = canShowParticipantsGridPanel(context);
	const isParticipantsExpanded = stateValue === 'focusParticipantsExpanded';
	const isFocusLayout = stateValue === 'focusLayout' || stateValue === 'focusParticipantsExpanded';
	const isFullscreenFocusLayout = !context.compact && context.isVoiceCallAppFullscreen;
	return {
		stateValue,
		mainLayout: stateValue === 'gridLayout' ? 'grid' : 'focus',
		hasSecondaryFocusTracks: hasMiniGrid,
		hasMiniGrid,
		canShowParticipantsGridPanel: canShowParticipants,
		participantsGridExpanded: context.participantsGridExpanded,
		isParticipantsExpanded,
		isFullscreenFocusLayout,
		shouldRenderFocusFallbackGrid: stateValue === 'focusFallbackGrid',
		shouldRenderFocusLayout: isFocusLayout,
		showMembersRow: isFocusLayout && isFullscreenFocusLayout && hasMiniGrid && context.membersRowVisible,
		showMembersToggle: isFocusLayout && isFullscreenFocusLayout && !isParticipantsExpanded && hasMiniGrid,
		shouldWrapScreenShareGrid: context.hasScreenShare,
		focusScrollerOverflow: isParticipantsExpanded ? 'auto' : 'hidden',
	};
}

export function resolveVoiceCallLayoutPresentationModel(
	input: VoiceCallLayoutPresentationInput,
): VoiceCallLayoutPresentationModel {
	return selectVoiceCallLayoutPresentationModel(createVoiceCallLayoutPresentationSnapshot(input));
}
