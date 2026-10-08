// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PermissionKind} from '@app/features/permissions/system/utils/NativePermissions';
import {
	and,
	assign,
	getInitialSnapshot,
	type MachineSnapshot,
	type MetaObject,
	type NonReducibleUnknown,
	type StateSchema,
	setup,
	transition,
} from 'xstate';

export const DesktopClientIntroStage = Object.freeze({
	INACTIVE: 'inactive',
	WELCOME: 'welcome',
	PREFERENCES: 'preferences',
	PERMISSIONS: 'permissions',
	COMPLETE: 'complete',
} as const);

export type DesktopClientIntroStage = (typeof DesktopClientIntroStage)[keyof typeof DesktopClientIntroStage];

export interface DesktopClientIntroConditions {
	enabled: boolean;
	welcomeSeen: boolean;
	preferencesSeen: boolean;
	completed: boolean;
	permissionKinds: ReadonlyArray<PermissionKind>;
	skippedPermissions: ReadonlyArray<PermissionKind>;
}

interface DesktopClientIntroContext extends DesktopClientIntroConditions {
	permissionSteps: ReadonlyArray<PermissionKind>;
	permissionIndex: number;
}

interface SettledPermissions {
	settledPermissions: ReadonlyArray<PermissionKind>;
}

type DesktopClientIntroSyncEvent = {type: 'intro.sync'} & DesktopClientIntroConditions & SettledPermissions;

export type DesktopClientIntroMachineEvent =
	| DesktopClientIntroSyncEvent
	| {type: 'intro.continueWelcome'}
	| ({type: 'intro.completePreferences'} & SettledPermissions)
	| ({type: 'intro.advancePermission'} & SettledPermissions)
	| ({type: 'intro.skipPermission'} & SettledPermissions);

export interface DesktopClientIntroModel {
	stage: DesktopClientIntroStage;
	showWelcome: boolean;
	showPreferences: boolean;
	permission: PermissionKind | null;
	permissionIndex: number;
	permissionCount: number;
	isComplete: boolean;
}

function isSyncEvent(event: DesktopClientIntroMachineEvent): event is DesktopClientIntroSyncEvent {
	return event.type === 'intro.sync';
}

function pendingPermissions(
	conditions: Pick<DesktopClientIntroConditions, 'permissionKinds' | 'skippedPermissions'>,
	settled: ReadonlyArray<PermissionKind>,
): Array<PermissionKind> {
	return conditions.permissionKinds.filter(
		(kind) => !conditions.skippedPermissions.includes(kind) && !settled.includes(kind),
	);
}

function selectSyncedStage(event: DesktopClientIntroSyncEvent): DesktopClientIntroStage {
	if (!event.enabled) return DesktopClientIntroStage.INACTIVE;
	if (event.completed) return DesktopClientIntroStage.COMPLETE;
	if (!event.welcomeSeen) return DesktopClientIntroStage.WELCOME;
	if (!event.preferencesSeen) return DesktopClientIntroStage.PREFERENCES;
	if (pendingPermissions(event, event.settledPermissions).length > 0) return DesktopClientIntroStage.PERMISSIONS;
	return DesktopClientIntroStage.COMPLETE;
}

function nextPermissionIndex(context: DesktopClientIntroContext, settled: ReadonlyArray<PermissionKind>): number {
	return context.permissionSteps.findIndex((kind, index) => index > context.permissionIndex && !settled.includes(kind));
}

function settledOf(event: DesktopClientIntroMachineEvent): ReadonlyArray<PermissionKind> {
	return 'settledPermissions' in event ? event.settledPermissions : [];
}

const desktopClientIntroStateMachine = setup({
	types: {} as {
		context: DesktopClientIntroContext;
		events: DesktopClientIntroMachineEvent;
	},
	guards: {
		shouldEnterInactive: ({event}) =>
			isSyncEvent(event) && selectSyncedStage(event) === DesktopClientIntroStage.INACTIVE,
		shouldEnterComplete: ({event}) =>
			isSyncEvent(event) && selectSyncedStage(event) === DesktopClientIntroStage.COMPLETE,
		shouldEnterPermissions: ({event}) =>
			isSyncEvent(event) && selectSyncedStage(event) === DesktopClientIntroStage.PERMISSIONS,
		shouldEnterPreferences: ({event}) =>
			isSyncEvent(event) && selectSyncedStage(event) === DesktopClientIntroStage.PREFERENCES,
		hasPendingPermissions: ({context, event}) => pendingPermissions(context, settledOf(event)).length > 0,
		currentPermissionSettled: ({context, event}) => {
			const current = context.permissionSteps[context.permissionIndex];
			return current !== undefined && settledOf(event).includes(current);
		},
		hasNextPermission: ({context, event}) => nextPermissionIndex(context, settledOf(event)) !== -1,
	},
	actions: {
		syncConditions: assign(({context, event}) => {
			if (!isSyncEvent(event)) return context;
			const stage = selectSyncedStage(event);
			const completed = event.completed || stage === DesktopClientIntroStage.COMPLETE;
			const preferencesSeen = event.preferencesSeen || completed;
			return {
				enabled: event.enabled,
				welcomeSeen: event.welcomeSeen || preferencesSeen,
				preferencesSeen,
				completed,
				permissionKinds: event.permissionKinds,
				skippedPermissions: event.skippedPermissions,
				permissionSteps:
					stage === DesktopClientIntroStage.PERMISSIONS ? pendingPermissions(event, event.settledPermissions) : [],
				permissionIndex: 0,
			};
		}),
		rememberWelcome: assign({welcomeSeen: true}),
		startPermissions: assign(({context, event}) => ({
			welcomeSeen: true,
			preferencesSeen: true,
			permissionSteps: pendingPermissions(context, settledOf(event)),
			permissionIndex: 0,
		})),
		skipCurrentPermission: assign(({context}) => {
			const current = context.permissionSteps[context.permissionIndex];
			if (current === undefined || context.skippedPermissions.includes(current)) return {};
			return {skippedPermissions: [...context.skippedPermissions, current]};
		}),
		moveToNextPermission: assign(({context, event}) => ({
			permissionIndex: nextPermissionIndex(context, settledOf(event)),
		})),
		completeIntro: assign({welcomeSeen: true, preferencesSeen: true, completed: true}),
	},
}).createMachine({
	id: 'desktopClientIntro',
	context: {
		enabled: false,
		welcomeSeen: false,
		preferencesSeen: false,
		completed: false,
		permissionKinds: [],
		skippedPermissions: [],
		permissionSteps: [],
		permissionIndex: 0,
	},
	initial: 'inactive',
	on: {
		'intro.sync': [
			{guard: 'shouldEnterInactive', target: '.inactive', actions: 'syncConditions'},
			{guard: 'shouldEnterComplete', target: '.complete', actions: 'syncConditions'},
			{guard: 'shouldEnterPermissions', target: '.permissions', actions: 'syncConditions'},
			{guard: 'shouldEnterPreferences', target: '.preferences', actions: 'syncConditions'},
			{target: '.welcome', actions: 'syncConditions'},
		],
	},
	states: {
		inactive: {},
		welcome: {
			on: {
				'intro.continueWelcome': {target: 'preferences', actions: 'rememberWelcome'},
			},
		},
		preferences: {
			on: {
				'intro.completePreferences': [
					{guard: 'hasPendingPermissions', target: 'permissions', actions: 'startPermissions'},
					{target: 'complete', actions: 'completeIntro'},
				],
			},
		},
		permissions: {
			on: {
				'intro.advancePermission': [
					{guard: and(['currentPermissionSettled', 'hasNextPermission']), actions: 'moveToNextPermission'},
					{guard: 'currentPermissionSettled', target: 'complete', actions: 'completeIntro'},
				],
				'intro.skipPermission': [
					{guard: 'hasNextPermission', actions: ['skipCurrentPermission', 'moveToNextPermission']},
					{target: 'complete', actions: ['skipCurrentPermission', 'completeIntro']},
				],
			},
		},
		complete: {},
	},
});

export type DesktopClientIntroSnapshot = MachineSnapshot<
	DesktopClientIntroContext,
	DesktopClientIntroMachineEvent,
	Record<string, never>,
	DesktopClientIntroStage,
	string,
	NonReducibleUnknown,
	MetaObject,
	StateSchema
>;

export function createDesktopClientIntroSnapshot(
	conditions: DesktopClientIntroConditions,
	settledPermissions: ReadonlyArray<PermissionKind>,
): DesktopClientIntroSnapshot {
	const snapshot = getInitialSnapshot(desktopClientIntroStateMachine);
	return transitionDesktopClientIntroSnapshot(snapshot, {type: 'intro.sync', ...conditions, settledPermissions});
}

export function transitionDesktopClientIntroSnapshot(
	snapshot: DesktopClientIntroSnapshot,
	event: DesktopClientIntroMachineEvent,
): DesktopClientIntroSnapshot {
	return transition(desktopClientIntroStateMachine, snapshot, event)[0];
}

function stageOf(snapshot: DesktopClientIntroSnapshot): DesktopClientIntroStage {
	if (snapshot.matches('welcome')) return DesktopClientIntroStage.WELCOME;
	if (snapshot.matches('preferences')) return DesktopClientIntroStage.PREFERENCES;
	if (snapshot.matches('permissions')) return DesktopClientIntroStage.PERMISSIONS;
	if (snapshot.matches('complete')) return DesktopClientIntroStage.COMPLETE;
	return DesktopClientIntroStage.INACTIVE;
}

export function selectDesktopClientIntroModel(snapshot: DesktopClientIntroSnapshot): DesktopClientIntroModel {
	const stage = stageOf(snapshot);
	const {permissionSteps, permissionIndex} = snapshot.context;
	const showPermissions = stage === DesktopClientIntroStage.PERMISSIONS;
	return {
		stage,
		showWelcome: stage === DesktopClientIntroStage.WELCOME,
		showPreferences: stage === DesktopClientIntroStage.PREFERENCES,
		permission: showPermissions ? (permissionSteps[permissionIndex] ?? null) : null,
		permissionIndex: showPermissions ? permissionIndex : 0,
		permissionCount: showPermissions ? permissionSteps.length : 0,
		isComplete: stage === DesktopClientIntroStage.COMPLETE,
	};
}
