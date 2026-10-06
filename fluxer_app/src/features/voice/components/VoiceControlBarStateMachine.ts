// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, setup, transition} from 'xstate';

export type VoiceControlBarMuteLabel =
	| 'moderatorDeafened'
	| 'moderatorMuted'
	| 'permissionMuted'
	| 'pushToTalkHoldHint'
	| 'unmute'
	| 'mute';
export type VoiceControlBarDeafenLabel = 'moderatorDeafened' | 'undeafen' | 'deafen';
export type VoiceControlBarCameraLabel = 'noPermission' | 'limitReached' | 'userCapReached' | 'turnOff' | 'turnOn';
export type VoiceControlBarScreenShareLabel = 'noPermission' | 'end' | 'start';

export interface VoiceControlBarSignals {
	isDeafened: boolean;
	isGuildMuted: boolean;
	isGuildDeafened: boolean;
	isPermissionMuted: boolean;
	isPushToTalkEffective: boolean;
	effectiveMuted: boolean;
	canStream: boolean;
	isCameraEnabled: boolean;
	isCameraLimitReached: boolean;
	isCameraUserCapReached: boolean;
	isScreenShareEnabled: boolean;
	screenShareDisabled: boolean;
}

export interface VoiceControlButtonState<Label extends string> {
	disabled: boolean;
	pressed: boolean;
	label: Label;
}

export interface VoiceControlBarState {
	mute: VoiceControlButtonState<VoiceControlBarMuteLabel>;
	deafen: VoiceControlButtonState<VoiceControlBarDeafenLabel>;
	camera: VoiceControlButtonState<VoiceControlBarCameraLabel>;
	screenShare: VoiceControlButtonState<VoiceControlBarScreenShareLabel>;
}

type VoiceControlBarEvent = {type: 'controls.evaluate'; signals: VoiceControlBarSignals};

interface VoiceControlBarContext {
	signals: VoiceControlBarSignals;
	controls: VoiceControlBarState;
}

const DEFAULT_SIGNALS: VoiceControlBarSignals = {
	isDeafened: false,
	isGuildMuted: false,
	isGuildDeafened: false,
	isPermissionMuted: false,
	isPushToTalkEffective: false,
	effectiveMuted: false,
	canStream: true,
	isCameraEnabled: false,
	isCameraLimitReached: false,
	isCameraUserCapReached: false,
	isScreenShareEnabled: false,
	screenShareDisabled: false,
};

const DEFAULT_STATE: VoiceControlBarState = {
	mute: {disabled: false, pressed: false, label: 'mute'},
	deafen: {disabled: false, pressed: false, label: 'deafen'},
	camera: {disabled: false, pressed: false, label: 'turnOn'},
	screenShare: {disabled: false, pressed: false, label: 'start'},
};

function selectMuteLabel(signals: VoiceControlBarSignals): VoiceControlBarMuteLabel {
	if (signals.isGuildDeafened) return 'moderatorDeafened';
	if (signals.isGuildMuted) return 'moderatorMuted';
	if (signals.isPermissionMuted) return 'permissionMuted';
	if (signals.isPushToTalkEffective) return 'pushToTalkHoldHint';
	return signals.effectiveMuted ? 'unmute' : 'mute';
}

function selectDeafenLabel(signals: VoiceControlBarSignals): VoiceControlBarDeafenLabel {
	if (signals.isGuildDeafened) return 'moderatorDeafened';
	return signals.isDeafened ? 'undeafen' : 'deafen';
}

function selectCameraLabel(signals: VoiceControlBarSignals): VoiceControlBarCameraLabel {
	if (!signals.canStream && !signals.isCameraEnabled) return 'noPermission';
	if (signals.isCameraLimitReached) return 'limitReached';
	if (signals.isCameraUserCapReached && !signals.isCameraEnabled) return 'userCapReached';
	return signals.isCameraEnabled ? 'turnOff' : 'turnOn';
}

function selectScreenShareLabel(signals: VoiceControlBarSignals): VoiceControlBarScreenShareLabel {
	if (signals.screenShareDisabled) return 'noPermission';
	return signals.isScreenShareEnabled ? 'end' : 'start';
}

export function selectVoiceControlBarState(signals: VoiceControlBarSignals): VoiceControlBarState {
	const isMicLocked = signals.isGuildMuted || signals.isGuildDeafened || signals.isPermissionMuted;
	const isMuteToggleLocked = isMicLocked || signals.isPushToTalkEffective;
	return {
		mute: {
			disabled: isMuteToggleLocked,
			pressed: signals.effectiveMuted,
			label: selectMuteLabel(signals),
		},
		deafen: {
			disabled: signals.isGuildDeafened,
			pressed: signals.isDeafened || signals.isGuildDeafened,
			label: selectDeafenLabel(signals),
		},
		camera: {
			disabled:
				signals.isCameraLimitReached ||
				(signals.isCameraUserCapReached && !signals.isCameraEnabled) ||
				(!signals.canStream && !signals.isCameraEnabled),
			pressed: signals.isCameraEnabled,
			label: selectCameraLabel(signals),
		},
		screenShare: {
			disabled: signals.screenShareDisabled,
			pressed: signals.isScreenShareEnabled,
			label: selectScreenShareLabel(signals),
		},
	};
}

export const voiceControlBarStateMachine = setup({
	types: {} as {
		context: VoiceControlBarContext;
		events: VoiceControlBarEvent;
	},
	actions: {
		assignControls: assign(({event}) => ({
			signals: event.signals,
			controls: selectVoiceControlBarState(event.signals),
		})),
	},
}).createMachine({
	id: 'voiceControlBar',
	context: () => ({
		signals: DEFAULT_SIGNALS,
		controls: DEFAULT_STATE,
	}),
	initial: 'ready',
	on: {
		'controls.evaluate': {actions: 'assignControls'},
	},
	states: {
		ready: {},
	},
});

export function transitionVoiceControlBarState(signals: VoiceControlBarSignals): VoiceControlBarState {
	const [snapshot] = transition(voiceControlBarStateMachine, initialTransition(voiceControlBarStateMachine)[0], {
		type: 'controls.evaluate',
		signals,
	});
	return snapshot.context.controls;
}
