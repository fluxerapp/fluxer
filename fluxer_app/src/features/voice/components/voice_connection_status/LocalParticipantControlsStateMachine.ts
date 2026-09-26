// SPDX-License-Identifier: AGPL-3.0-or-later

import {initialTransition, setup, transition} from 'xstate';

export type LocalParticipantMediaControl = 'camera' | 'screenShare';
export type LocalParticipantMediaControlStateValue =
	| 'waiting'
	| 'permissionBlocked'
	| 'capBlocked'
	| 'active'
	| 'ready';
export type LocalParticipantCameraLabelKey =
	| 'waitingForConnection'
	| 'noCameraPermission'
	| 'cameraUserCapReached'
	| 'turnOffCamera'
	| 'turnOnCamera';
export type LocalParticipantScreenShareLabelKey =
	| 'waitingForConnection'
	| 'noScreenSharePermission'
	| 'configureOrEndScreenShare'
	| 'shareScreen';
export type LocalParticipantScreenShareClickAction = 'none' | 'openPicker' | 'openMenu';

export interface LocalParticipantControlSignals {
	isConnected: boolean;
	canStream: boolean;
	isCameraEnabled: boolean;
	isCameraUserCapReached: boolean;
	isScreenShareEnabled: boolean;
}

interface LocalParticipantMediaControlEvent {
	type: 'controls.evaluate';
	control: LocalParticipantMediaControl;
	signals: LocalParticipantControlSignals;
}

export interface LocalParticipantCameraControlState {
	value: LocalParticipantMediaControlStateValue;
	labelKey: LocalParticipantCameraLabelKey;
	disabled: boolean;
	canOpenSettings: boolean;
}

export interface LocalParticipantScreenShareControlState {
	value: LocalParticipantMediaControlStateValue;
	labelKey: LocalParticipantScreenShareLabelKey;
	disabled: boolean;
	clickAction: LocalParticipantScreenShareClickAction;
	canOpenMenu: boolean;
	canPreloadPicker: boolean;
}

export interface LocalParticipantControlsViewState {
	camera: LocalParticipantCameraControlState;
	screenShare: LocalParticipantScreenShareControlState;
}

const CAMERA_LABEL_BY_STATE: Record<LocalParticipantMediaControlStateValue, LocalParticipantCameraLabelKey> = {
	waiting: 'waitingForConnection',
	permissionBlocked: 'noCameraPermission',
	capBlocked: 'cameraUserCapReached',
	active: 'turnOffCamera',
	ready: 'turnOnCamera',
};

const SCREEN_SHARE_LABEL_BY_STATE: Record<LocalParticipantMediaControlStateValue, LocalParticipantScreenShareLabelKey> =
	{
		waiting: 'waitingForConnection',
		permissionBlocked: 'noScreenSharePermission',
		capBlocked: 'shareScreen',
		active: 'configureOrEndScreenShare',
		ready: 'shareScreen',
	};

function isControlEnabled(control: LocalParticipantMediaControl, signals: LocalParticipantControlSignals): boolean {
	return control === 'camera' ? signals.isCameraEnabled : signals.isScreenShareEnabled;
}

export const localParticipantMediaControlStateMachine = setup({
	types: {} as {
		events: LocalParticipantMediaControlEvent;
	},
	guards: {
		isDisconnected: ({event}) => !event.signals.isConnected,
		isPermissionBlocked: ({event}) => !event.signals.canStream && !isControlEnabled(event.control, event.signals),
		isCapBlocked: ({event}) =>
			event.control === 'camera' && event.signals.isCameraUserCapReached && !event.signals.isCameraEnabled,
		isActive: ({event}) => isControlEnabled(event.control, event.signals),
	},
}).createMachine({
	id: 'localParticipantMediaControl',
	initial: 'waiting',
	on: {
		'controls.evaluate': [
			{target: '.waiting', guard: 'isDisconnected'},
			{target: '.permissionBlocked', guard: 'isPermissionBlocked'},
			{target: '.capBlocked', guard: 'isCapBlocked'},
			{target: '.active', guard: 'isActive'},
			{target: '.ready'},
		],
	},
	states: {
		waiting: {},
		permissionBlocked: {},
		capBlocked: {},
		active: {},
		ready: {},
	},
});

function isDisabled(value: LocalParticipantMediaControlStateValue): boolean {
	return value === 'waiting' || value === 'permissionBlocked' || value === 'capBlocked';
}

export function selectLocalParticipantMediaControlState(
	control: LocalParticipantMediaControl,
	signals: LocalParticipantControlSignals,
): LocalParticipantMediaControlStateValue {
	const [snapshot] = transition(
		localParticipantMediaControlStateMachine,
		initialTransition(localParticipantMediaControlStateMachine)[0],
		{
			type: 'controls.evaluate',
			control,
			signals,
		},
	);
	return typeof snapshot.value === 'string' ? (snapshot.value as LocalParticipantMediaControlStateValue) : 'waiting';
}

export function selectLocalParticipantControlsViewState(
	signals: LocalParticipantControlSignals,
): LocalParticipantControlsViewState {
	const cameraValue = selectLocalParticipantMediaControlState('camera', signals);
	const screenShareValue = selectLocalParticipantMediaControlState('screenShare', signals);
	const cameraDisabled = isDisabled(cameraValue);
	const screenShareDisabled = isDisabled(screenShareValue);
	return {
		camera: {
			value: cameraValue,
			labelKey: CAMERA_LABEL_BY_STATE[cameraValue],
			disabled: cameraDisabled,
			canOpenSettings: !cameraDisabled,
		},
		screenShare: {
			value: screenShareValue,
			labelKey: SCREEN_SHARE_LABEL_BY_STATE[screenShareValue],
			disabled: screenShareDisabled,
			clickAction: screenShareValue === 'active' ? 'openMenu' : screenShareValue === 'ready' ? 'openPicker' : 'none',
			canOpenMenu: screenShareValue === 'active',
			canPreloadPicker: screenShareValue === 'ready',
		},
	};
}
