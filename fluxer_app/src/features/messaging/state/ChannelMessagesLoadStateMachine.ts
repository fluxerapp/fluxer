// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export interface ChannelMessagesLoadInput {
	isBefore: boolean;
	isAfter: boolean;
	hasJump: boolean;
	wasReady: boolean;
}

export type ChannelMessagesLoadMode = 'replace' | 'mergeBefore' | 'mergeAfter';

export interface ChannelMessagesLoadDecision {
	mode: ChannelMessagesLoadMode;
	prepend: boolean;
	trimTop: boolean;
	trimBottom: boolean;
	preserveHasMoreBefore: boolean;
	preserveHasMoreAfter: boolean;
}

export type ChannelMessagesLoadEvent = {
	type: 'channelMessagesLoad.updated';
	input: ChannelMessagesLoadInput;
};

function shouldReplaceVisibleWindow(context: ChannelMessagesLoadInput): boolean {
	if (context.hasJump) return true;
	if (!context.wasReady) return true;
	return !context.isBefore && !context.isAfter;
}

function getLoadMode(snapshot: ChannelMessagesLoadSnapshot): ChannelMessagesLoadMode {
	switch (snapshot.value) {
		case 'mergeBefore':
			return 'mergeBefore';
		case 'mergeAfter':
			return 'mergeAfter';
		default:
			return 'replace';
	}
}

function getLoadModeFromInput(input: ChannelMessagesLoadInput): ChannelMessagesLoadMode {
	if (shouldReplaceVisibleWindow(input)) return 'replace';
	if (input.isBefore) return 'mergeBefore';
	return 'mergeAfter';
}

function buildChannelMessagesLoadDecision(mode: ChannelMessagesLoadMode): ChannelMessagesLoadDecision {
	switch (mode) {
		case 'mergeBefore':
			return {
				mode,
				prepend: true,
				trimTop: false,
				trimBottom: true,
				preserveHasMoreBefore: false,
				preserveHasMoreAfter: true,
			};
		case 'mergeAfter':
			return {
				mode,
				prepend: false,
				trimTop: true,
				trimBottom: false,
				preserveHasMoreBefore: true,
				preserveHasMoreAfter: false,
			};
		case 'replace':
			return {
				mode,
				prepend: false,
				trimTop: false,
				trimBottom: false,
				preserveHasMoreBefore: false,
				preserveHasMoreAfter: false,
			};
	}
}

export const channelMessagesLoadMachine = setup({
	types: {} as {
		context: ChannelMessagesLoadInput;
		events: ChannelMessagesLoadEvent;
		input: ChannelMessagesLoadInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'channelMessagesLoad.updated') return {};
			return event.input;
		}),
	},
	guards: {
		shouldReplaceVisibleWindow: ({context}) => shouldReplaceVisibleWindow(context),
		isBeforePage: ({context}) => context.isBefore,
	},
}).createMachine({
	id: 'channelMessagesLoad',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'shouldReplaceVisibleWindow', target: 'replace'},
				{guard: 'isBeforePage', target: 'mergeBefore'},
				{target: 'mergeAfter'},
			],
		},
		replace: {
			on: {'channelMessagesLoad.updated': {target: 'routing', actions: 'applyInput'}},
		},
		mergeBefore: {
			on: {'channelMessagesLoad.updated': {target: 'routing', actions: 'applyInput'}},
		},
		mergeAfter: {
			on: {'channelMessagesLoad.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ChannelMessagesLoadSnapshot = SnapshotFrom<typeof channelMessagesLoadMachine>;

export function createChannelMessagesLoadSnapshot(input: ChannelMessagesLoadInput): ChannelMessagesLoadSnapshot {
	return getInitialSnapshot(channelMessagesLoadMachine, input);
}

export function transitionChannelMessagesLoadSnapshot(
	snapshot: ChannelMessagesLoadSnapshot,
	event: ChannelMessagesLoadEvent,
): ChannelMessagesLoadSnapshot {
	return transition(channelMessagesLoadMachine, snapshot, event)[0] as ChannelMessagesLoadSnapshot;
}

export function selectChannelMessagesLoadDecision(snapshot: ChannelMessagesLoadSnapshot): ChannelMessagesLoadDecision {
	return buildChannelMessagesLoadDecision(getLoadMode(snapshot));
}

export function resolveChannelMessagesLoadDecision(input: ChannelMessagesLoadInput): ChannelMessagesLoadDecision {
	return buildChannelMessagesLoadDecision(getLoadModeFromInput(input));
}

export interface ChannelMessagesWindowInput {
	ready: boolean;
	loading: boolean;
	failed: boolean;
	messageCount: number;
	hasMoreBefore: boolean;
	hasMoreAfter: boolean;
}

export type ChannelMessagesWindowStatus = {
	phase: ChannelMessagesWindowPhase;
	olderPageAvailable: boolean;
	newerPageAvailable: boolean;
	needsPage: boolean;
	retryVisible: boolean;
};

export type ChannelMessagesWindowPhase = 'placeholder' | 'retry' | 'stream';

export type ChannelMessagesWindowBar = 'none' | 'retry' | 'present';

export type ChannelMessagesWindowEvent = {
	type: 'channelMessagesWindow.updated';
	input: ChannelMessagesWindowInput;
};

function hasLoadedWindow(context: ChannelMessagesWindowInput): boolean {
	if (!context.ready) return false;
	if (context.messageCount > 0) return true;
	return !context.hasMoreBefore && !context.hasMoreAfter;
}

function getWindowPhase(snapshot: ChannelMessagesWindowSnapshot): ChannelMessagesWindowPhase {
	switch (snapshot.value) {
		case 'stream':
			return 'stream';
		case 'retry':
			return 'retry';
		default:
			return 'placeholder';
	}
}

function getWindowPhaseFromInput(input: ChannelMessagesWindowInput): ChannelMessagesWindowPhase {
	if (hasLoadedWindow(input)) return 'stream';
	if (input.failed) return 'retry';
	return 'placeholder';
}

function buildChannelMessagesWindowStatus(
	phase: ChannelMessagesWindowPhase,
	input: ChannelMessagesWindowInput,
): ChannelMessagesWindowStatus {
	return {
		phase,
		olderPageAvailable: input.hasMoreBefore,
		newerPageAvailable: input.hasMoreAfter,
		needsPage: phase === 'placeholder' && !input.loading,
		retryVisible: input.failed,
	};
}

export const channelMessagesWindowMachine = setup({
	types: {} as {
		context: ChannelMessagesWindowInput;
		events: ChannelMessagesWindowEvent;
		input: ChannelMessagesWindowInput;
	},
	actions: {
		applyInput: assign(({event}) => {
			if (event.type !== 'channelMessagesWindow.updated') return {};
			return event.input;
		}),
	},
	guards: {
		hasLoadedWindow: ({context}) => hasLoadedWindow(context),
		hasFailedLoad: ({context}) => context.failed,
	},
}).createMachine({
	id: 'channelMessagesWindow',
	context: ({input}) => input,
	initial: 'routing',
	states: {
		routing: {
			always: [
				{guard: 'hasLoadedWindow', target: 'stream'},
				{guard: 'hasFailedLoad', target: 'retry'},
				{target: 'placeholder'},
			],
		},
		stream: {
			on: {'channelMessagesWindow.updated': {target: 'routing', actions: 'applyInput'}},
		},
		retry: {
			on: {'channelMessagesWindow.updated': {target: 'routing', actions: 'applyInput'}},
		},
		placeholder: {
			on: {'channelMessagesWindow.updated': {target: 'routing', actions: 'applyInput'}},
		},
	},
});

export type ChannelMessagesWindowSnapshot = SnapshotFrom<typeof channelMessagesWindowMachine>;

export function createChannelMessagesWindowSnapshot(input: ChannelMessagesWindowInput): ChannelMessagesWindowSnapshot {
	return getInitialSnapshot(channelMessagesWindowMachine, input);
}

export function transitionChannelMessagesWindowSnapshot(
	snapshot: ChannelMessagesWindowSnapshot,
	event: ChannelMessagesWindowEvent,
): ChannelMessagesWindowSnapshot {
	return transition(channelMessagesWindowMachine, snapshot, event)[0] as ChannelMessagesWindowSnapshot;
}

export function selectChannelMessagesWindowStatus(
	snapshot: ChannelMessagesWindowSnapshot,
): ChannelMessagesWindowStatus {
	return buildChannelMessagesWindowStatus(getWindowPhase(snapshot), snapshot.context);
}

export function resolveChannelMessagesWindowStatus(input: ChannelMessagesWindowInput): ChannelMessagesWindowStatus {
	return buildChannelMessagesWindowStatus(getWindowPhaseFromInput(input), input);
}

export interface ChannelMessagesFillerMotionInput {
	reducedMotion: boolean;
	scrollManagerInitialized: boolean;
	ready: boolean;
}

export function selectChannelMessagesFillerVisible(input: ChannelMessagesFillerMotionInput): boolean {
	if (!input.reducedMotion) return true;
	return input.scrollManagerInitialized || input.ready;
}

export function selectChannelMessagesSpacerHeight(status: ChannelMessagesWindowStatus, fillerHeight: number): number {
	return status.olderPageAvailable || status.newerPageAvailable ? fillerHeight : 0;
}

export function selectChannelMessagesWindowBar(status: ChannelMessagesWindowStatus): ChannelMessagesWindowBar {
	if (status.phase === 'placeholder') return 'none';
	if (status.phase === 'retry' || status.retryVisible) return 'retry';
	return status.newerPageAvailable ? 'present' : 'none';
}
