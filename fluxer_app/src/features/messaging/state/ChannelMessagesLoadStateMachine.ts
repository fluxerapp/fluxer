// SPDX-License-Identifier: AGPL-3.0-or-later

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

function shouldReplaceVisibleWindow(context: ChannelMessagesLoadInput): boolean {
	if (context.hasJump) return true;
	if (!context.wasReady) return true;
	return !context.isBefore && !context.isAfter;
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

type ChannelMessagesWindowPhase = 'placeholder' | 'retry' | 'stream';

export type ChannelMessagesWindowBar = 'none' | 'retry' | 'present';

function hasLoadedWindow(context: ChannelMessagesWindowInput): boolean {
	if (!context.ready) return false;
	if (context.messageCount > 0) return true;
	return !context.hasMoreBefore && !context.hasMoreAfter;
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

export function selectChannelMessagesLoadRestoresTrust(input: {
	mode: ChannelMessagesLoadMode;
	isAfter: boolean;
	hasMoreAfter: boolean;
}): boolean {
	if (input.mode === 'replace') return true;
	return input.isAfter && !input.hasMoreAfter;
}

export function selectChannelMessagesWindowBar(status: ChannelMessagesWindowStatus): ChannelMessagesWindowBar {
	if (status.phase === 'placeholder') return 'none';
	if (status.phase === 'retry' || status.retryVisible) return 'retry';
	return status.newerPageAvailable ? 'present' : 'none';
}
