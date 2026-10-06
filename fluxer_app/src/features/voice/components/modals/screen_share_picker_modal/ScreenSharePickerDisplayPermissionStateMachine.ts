// SPDX-License-Identifier: AGPL-3.0-or-later

import type {NativePermissionResult} from '@app/features/permissions/system/utils/NativePermissions';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

interface ScreenSharePickerDisplayPermissionContext {
	settingsOpened: boolean;
}

export type ScreenSharePickerDisplayPermissionEvent =
	| {type: 'permission.check'}
	| {type: 'permission.result'; permission: NativePermissionResult}
	| {type: 'permission.settingsOpened'}
	| {type: 'permission.clear'};

export type ScreenSharePickerDisplayPermissionPrompt = 'none' | 'checking' | 'needs-permission' | 'restart-required';

function permissionAllowsDisplaySources(permission: NativePermissionResult): boolean {
	return permission === 'granted' || permission === 'unsupported';
}

export function screenRecordingPermissionAllowsPickerSources(permission: NativePermissionResult): boolean {
	return permissionAllowsDisplaySources(permission);
}

export const screenSharePickerDisplayPermissionStateMachine = setup({
	types: {} as {
		context: ScreenSharePickerDisplayPermissionContext;
		events: ScreenSharePickerDisplayPermissionEvent;
	},
	guards: {
		permissionAllowsDisplaySources: ({event}) =>
			event.type === 'permission.result' && permissionAllowsDisplaySources(event.permission),
		settingsAlreadyOpened: ({context}) => context.settingsOpened,
	},
	actions: {
		clear: assign(() => ({
			settingsOpened: false,
		})),
		markSettingsOpened: assign(() => ({
			settingsOpened: true,
		})),
	},
}).createMachine({
	id: 'screenSharePickerDisplayPermission',
	context: () => ({
		settingsOpened: false,
	}),
	initial: 'idle',
	states: {
		idle: {
			on: {
				'permission.check': {target: 'checking'},
				'permission.clear': {actions: 'clear'},
			},
		},
		checking: {
			on: {
				'permission.result': [
					{
						guard: 'settingsAlreadyOpened',
						target: 'restartRequired',
					},
					{
						guard: 'permissionAllowsDisplaySources',
						target: 'ready',
					},
					{target: 'blocked'},
				],
				'permission.clear': {target: 'idle', actions: 'clear'},
			},
		},
		ready: {
			on: {
				'permission.check': {target: 'checking'},
				'permission.settingsOpened': {target: 'restartRequired', actions: 'markSettingsOpened'},
				'permission.clear': {target: 'idle', actions: 'clear'},
			},
		},
		blocked: {
			on: {
				'permission.check': {target: 'checking'},
				'permission.result': [
					{
						guard: 'permissionAllowsDisplaySources',
						target: 'ready',
					},
					{target: 'blocked'},
				],
				'permission.settingsOpened': {target: 'restartRequired', actions: 'markSettingsOpened'},
				'permission.clear': {target: 'idle', actions: 'clear'},
			},
		},
		restartRequired: {
			on: {
				'permission.check': {target: 'checking'},
				'permission.result': {target: 'restartRequired'},
				'permission.settingsOpened': {actions: 'markSettingsOpened'},
				'permission.clear': {target: 'idle', actions: 'clear'},
			},
		},
	},
});

export type ScreenSharePickerDisplayPermissionSnapshot = SnapshotFrom<
	typeof screenSharePickerDisplayPermissionStateMachine
>;

export function createScreenSharePickerDisplayPermissionSnapshot(): ScreenSharePickerDisplayPermissionSnapshot {
	return initialTransition(screenSharePickerDisplayPermissionStateMachine)[0];
}

export function transitionScreenSharePickerDisplayPermissionSnapshot(
	snapshot: ScreenSharePickerDisplayPermissionSnapshot,
	event: ScreenSharePickerDisplayPermissionEvent,
): ScreenSharePickerDisplayPermissionSnapshot {
	return transition(
		screenSharePickerDisplayPermissionStateMachine,
		snapshot,
		event,
	)[0] as ScreenSharePickerDisplayPermissionSnapshot;
}

export function selectScreenSharePickerDisplayPermissionPrompt(
	snapshot: ScreenSharePickerDisplayPermissionSnapshot,
): ScreenSharePickerDisplayPermissionPrompt {
	if (snapshot.matches('checking')) return 'checking';
	if (snapshot.matches('blocked')) return 'needs-permission';
	if (snapshot.matches('restartRequired')) return 'restart-required';
	return 'none';
}
