// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceMediaGraphStreamTileState} from '@app/features/voice/engine/VoiceMediaGraphTileState';
import {assign, getInitialSnapshot, setup, transition} from 'xstate';

export type VoiceParticipantTileScreenShareStateValue =
	| 'idle'
	| 'buffering'
	| 'watchFailed'
	| 'streamEnded'
	| 'watchPrompt';

export interface VoiceParticipantTileScreenShareSignals {
	graphTileState: VoiceMediaGraphStreamTileState;
	isScreenShare: boolean;
	isOwnScreenShare: boolean;
	isFocusedPlaceholderTile: boolean;
	isFocusPresentationTile: boolean;
	isTrackReference: boolean;
	cameraLocallyDisabled: boolean;
	isRepublishGracePending: boolean;
}

export interface VoiceParticipantTileCameraBufferingSignals {
	isScreenShare: boolean;
	isFocusedPlaceholderTile: boolean;
	cameraLocallyDisabled: boolean;
	isOwnCameraHidden: boolean;
	isCameraActive: boolean;
	hasVideo: boolean;
	hasRenderedVideoFrame: boolean;
}

export interface VoiceParticipantTileCameraActiveSignals {
	isCameraTile: boolean;
	isOwnContent: boolean;
	isCameraPublicationActive: boolean;
	isParticipantCameraActive: boolean;
	isLocalCameraRequested: boolean;
}

type VoiceParticipantTileEvent = {
	type: 'tile.evaluateScreenShare';
	signals: VoiceParticipantTileScreenShareSignals;
};

interface VoiceParticipantTileContext {
	signals: VoiceParticipantTileScreenShareSignals;
}

const DEFAULT_SIGNALS: VoiceParticipantTileScreenShareSignals = {
	graphTileState: 'idle',
	isScreenShare: false,
	isOwnScreenShare: false,
	isFocusedPlaceholderTile: false,
	isFocusPresentationTile: false,
	isTrackReference: false,
	cameraLocallyDisabled: false,
	isRepublishGracePending: false,
};

function isRemoteVisibleScreenShare(signals: VoiceParticipantTileScreenShareSignals): boolean {
	if (!signals.isScreenShare) return false;
	if (signals.isOwnScreenShare) return false;
	return !signals.isFocusedPlaceholderTile;
}

export function graphTileStateHoldsWatchIntent(graphTileState: VoiceMediaGraphStreamTileState): boolean {
	switch (graphTileState) {
		case 'watchDesired':
			return true;
		case 'publicationMissing':
			return true;
		case 'attaching':
			return true;
		case 'subscribedAwaitingFrame':
			return true;
		case 'rendering':
			return true;
		case 'failed':
			return true;
		case 'idle':
			return false;
	}
}

export function shouldShowWatchFailed(signals: VoiceParticipantTileScreenShareSignals): boolean {
	if (!isRemoteVisibleScreenShare(signals)) return false;
	return signals.graphTileState === 'failed';
}

export function shouldShowScreenShareBuffering(signals: VoiceParticipantTileScreenShareSignals): boolean {
	if (!isRemoteVisibleScreenShare(signals)) return false;
	if (signals.cameraLocallyDisabled) return false;
	switch (signals.graphTileState) {
		case 'watchDesired':
			return true;
		case 'attaching':
			return true;
		case 'subscribedAwaitingFrame':
			return true;
		case 'publicationMissing':
			if (signals.isRepublishGracePending) return true;
			return signals.isTrackReference;
		case 'rendering':
			return false;
		case 'failed':
			return false;
		case 'idle':
			return false;
	}
}

export type ScreenShareBufferingPresentation = 'spinner' | 'last-frame';

export interface ScreenShareBufferingPresentationSignals extends VoiceParticipantTileScreenShareSignals {
	hasRetainedLastFrame: boolean;
}

export function selectScreenShareBufferingPresentation(
	signals: ScreenShareBufferingPresentationSignals,
): ScreenShareBufferingPresentation | null {
	if (!shouldShowScreenShareBuffering(signals)) return null;
	if (signals.hasRetainedLastFrame) return 'last-frame';
	return 'spinner';
}

export function shouldShowStreamEnded(signals: VoiceParticipantTileScreenShareSignals): boolean {
	if (!isRemoteVisibleScreenShare(signals)) return false;
	if (signals.isFocusPresentationTile) return false;
	if (signals.graphTileState !== 'publicationMissing') return false;
	if (signals.isRepublishGracePending) return false;
	return !signals.isTrackReference;
}

export function shouldShowWatchPrompt(signals: VoiceParticipantTileScreenShareSignals): boolean {
	if (!isRemoteVisibleScreenShare(signals)) return false;
	if (graphTileStateHoldsWatchIntent(signals.graphTileState)) return false;
	if (!signals.isTrackReference) return false;
	if (signals.cameraLocallyDisabled) return false;
	return !signals.isFocusPresentationTile;
}

export function shouldShowCameraBuffering(signals: VoiceParticipantTileCameraBufferingSignals): boolean {
	if (signals.isScreenShare) return false;
	if (signals.isFocusedPlaceholderTile) return false;
	if (signals.cameraLocallyDisabled) return false;
	if (signals.isOwnCameraHidden) return false;
	return signals.isCameraActive && (!signals.hasVideo || !signals.hasRenderedVideoFrame);
}

export function selectVoiceParticipantTileCameraActive(signals: VoiceParticipantTileCameraActiveSignals): boolean {
	if (!signals.isCameraTile) return false;
	if (signals.isCameraPublicationActive) return true;
	if (signals.isParticipantCameraActive) return true;
	if (signals.isLocalCameraRequested) return true;
	return false;
}

export const voiceParticipantTileStateMachine = setup({
	types: {} as {
		context: VoiceParticipantTileContext;
		events: VoiceParticipantTileEvent;
	},
	actions: {
		assignSignals: assign(({event}) => ({signals: event.signals})),
	},
	guards: {
		shouldShowWatchFailed: ({event}) => shouldShowWatchFailed(event.signals),
		shouldShowScreenShareBuffering: ({event}) => shouldShowScreenShareBuffering(event.signals),
		shouldShowStreamEnded: ({event}) => shouldShowStreamEnded(event.signals),
		shouldShowWatchPrompt: ({event}) => shouldShowWatchPrompt(event.signals),
	},
}).createMachine({
	id: 'voiceParticipantTile',
	context: () => ({signals: DEFAULT_SIGNALS}),
	initial: 'idle',
	on: {
		'tile.evaluateScreenShare': [
			{target: '.watchFailed', guard: 'shouldShowWatchFailed', actions: 'assignSignals'},
			{target: '.buffering', guard: 'shouldShowScreenShareBuffering', actions: 'assignSignals'},
			{target: '.streamEnded', guard: 'shouldShowStreamEnded', actions: 'assignSignals'},
			{target: '.watchPrompt', guard: 'shouldShowWatchPrompt', actions: 'assignSignals'},
			{target: '.idle', actions: 'assignSignals'},
		],
	},
	states: {
		idle: {},
		buffering: {},
		watchFailed: {},
		streamEnded: {},
		watchPrompt: {},
	},
});

export function selectVoiceParticipantTileScreenShareState(
	signals: VoiceParticipantTileScreenShareSignals,
): VoiceParticipantTileScreenShareStateValue {
	const [snapshot] = transition(
		voiceParticipantTileStateMachine,
		getInitialSnapshot(voiceParticipantTileStateMachine),
		{
			type: 'tile.evaluateScreenShare',
			signals,
		},
	);
	return typeof snapshot.value === 'string' ? (snapshot.value as VoiceParticipantTileScreenShareStateValue) : 'idle';
}
