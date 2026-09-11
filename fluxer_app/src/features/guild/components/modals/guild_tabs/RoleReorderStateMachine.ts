// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface RoleReorderPoint {
	x: number;
	y: number;
}

export interface RoleReorderRect {
	top: number;
	bottom: number;
	left: number;
	right: number;
}

export interface RoleReorderAccess {
	canManageRoles: boolean;
	isGuildOwner: boolean;
}

export interface RoleReorderDragItem {
	id: string;
	isEveryone: boolean;
	isLocked: boolean;
}

export interface RoleReorderTargetRole {
	id: string;
	isEveryone: boolean;
	isLocked: boolean;
}

export interface RoleReorderTarget {
	role: RoleReorderTargetRole | null;
	isTerminal: boolean;
}

export type RoleReorderPosition = 'before' | 'after';

export interface RoleReorderDropResult {
	targetRoleId: string | null;
	position: RoleReorderPosition;
}

export interface RoleReorderIndicator {
	position: 'top' | 'bottom';
	isValid: boolean;
}

export interface RoleReorderIntent {
	indicator: RoleReorderIndicator;
	result: RoleReorderDropResult;
}

export type RoleReorderBlockedReason =
	| 'missing-manage-roles-permission'
	| 'source-is-everyone'
	| 'source-is-locked'
	| 'same-source-and-target'
	| 'target-before-locked-role'
	| 'top-target-requires-owner'
	| 'empty-target-rect';

export interface RoleReorderResolution {
	intent: RoleReorderIntent | null;
	indicator: RoleReorderIndicator | null;
	blockedReason: RoleReorderBlockedReason | null;
}

export interface RoleReorderMachineContext extends RoleReorderResolution {}

export type RoleReorderEvent =
	| {
			type: 'drag.hover';
			item: RoleReorderDragItem;
			target: RoleReorderTarget;
			access: RoleReorderAccess;
			clientOffset: RoleReorderPoint;
			targetRect: RoleReorderRect;
	  }
	| {type: 'drag.leave'}
	| {type: 'drag.drop'};

type VerticalZone = 'before' | 'after';

const initialRoleReorderMachineContext: RoleReorderMachineContext = {
	intent: null,
	indicator: null,
	blockedReason: null,
};

function getTargetHeight(rect: RoleReorderRect): number {
	return rect.bottom - rect.top;
}

function getVerticalZone(clientOffset: RoleReorderPoint, rect: RoleReorderRect): VerticalZone {
	const height = getTargetHeight(rect);
	const offsetY = Math.min(height, Math.max(0, clientOffset.y - rect.top));
	return offsetY < height / 2 ? 'before' : 'after';
}

function createIndicator(position: 'top' | 'bottom', isValid: boolean): RoleReorderIndicator {
	return {position, isValid};
}

function getSourceBlockedReason(item: RoleReorderDragItem, access: RoleReorderAccess): RoleReorderBlockedReason | null {
	if (!access.canManageRoles) return 'missing-manage-roles-permission';
	if (item.isEveryone) return 'source-is-everyone';
	if (item.isLocked && !access.isGuildOwner) return 'source-is-locked';
	return null;
}

function createDropResult(
	target: RoleReorderTarget,
	zone: VerticalZone,
): {indicatorPosition: 'top' | 'bottom'; result: RoleReorderDropResult} {
	if (target.role === null) {
		return {
			indicatorPosition: 'top',
			result: {
				targetRoleId: null,
				position: 'before',
			},
		};
	}
	if (target.isTerminal && !target.role.isEveryone && zone === 'after') {
		return {
			indicatorPosition: 'bottom',
			result: {
				targetRoleId: target.role.id,
				position: 'after',
			},
		};
	}
	return {
		indicatorPosition: 'top',
		result: {
			targetRoleId: target.role.id,
			position: 'before',
		},
	};
}

function getResultBlockedReason(
	item: RoleReorderDragItem,
	target: RoleReorderTarget,
	access: RoleReorderAccess,
	result: RoleReorderDropResult,
): RoleReorderBlockedReason | null {
	if (target.role === null && !access.isGuildOwner) return 'top-target-requires-owner';
	if (target.role && item.id === target.role.id) return 'same-source-and-target';
	if (
		target.role &&
		result.position === 'before' &&
		target.role.isLocked &&
		!target.role.isEveryone &&
		!access.isGuildOwner
	) {
		return 'target-before-locked-role';
	}
	return null;
}

export function getRoleDropBlockedReason(
	item: RoleReorderDragItem,
	target: RoleReorderTarget,
	access: RoleReorderAccess,
): RoleReorderBlockedReason | null {
	const sourceBlockedReason = getSourceBlockedReason(item, access);
	if (sourceBlockedReason) return sourceBlockedReason;
	if (target.role === null && !access.isGuildOwner) return 'top-target-requires-owner';
	return null;
}

export function canRoleDropOnTarget(
	item: RoleReorderDragItem,
	target: RoleReorderTarget,
	access: RoleReorderAccess,
): boolean {
	return getRoleDropBlockedReason(item, target, access) === null;
}

export function resolveRoleReorderHover(
	item: RoleReorderDragItem,
	target: RoleReorderTarget,
	access: RoleReorderAccess,
	clientOffset: RoleReorderPoint,
	targetRect: RoleReorderRect,
): RoleReorderResolution {
	if (getTargetHeight(targetRect) <= 0) {
		return {
			intent: null,
			indicator: null,
			blockedReason: 'empty-target-rect',
		};
	}
	const sourceBlockedReason = getSourceBlockedReason(item, access);
	const zone = getVerticalZone(clientOffset, targetRect);
	const {indicatorPosition, result} = createDropResult(target, zone);
	if (sourceBlockedReason) {
		return {
			intent: null,
			indicator: createIndicator(indicatorPosition, false),
			blockedReason: sourceBlockedReason,
		};
	}
	const resultBlockedReason = getResultBlockedReason(item, target, access, result);
	if (resultBlockedReason) {
		return {
			intent: null,
			indicator: createIndicator(indicatorPosition, false),
			blockedReason: resultBlockedReason,
		};
	}
	const indicator = createIndicator(indicatorPosition, true);
	return {
		intent: {
			indicator,
			result,
		},
		indicator,
		blockedReason: null,
	};
}

export const roleReorderStateMachine = setup({
	types: {} as {
		context: RoleReorderMachineContext;
		events: RoleReorderEvent;
	},
	guards: {
		hasIntent: ({context}) => context.intent !== null,
	},
	actions: {
		resolveHover: assign(({event}) => {
			if (event.type !== 'drag.hover') return initialRoleReorderMachineContext;
			return resolveRoleReorderHover(event.item, event.target, event.access, event.clientOffset, event.targetRect);
		}),
		clear: assign(() => initialRoleReorderMachineContext),
	},
}).createMachine({
	id: 'roleReorder',
	initial: 'idle',
	context: initialRoleReorderMachineContext,
	states: {
		idle: {
			on: {
				'drag.hover': {target: 'resolving', actions: 'resolveHover'},
				'drag.leave': {actions: 'clear'},
				'drag.drop': {actions: 'clear'},
			},
		},
		resolving: {
			always: [{target: 'targeting', guard: 'hasIntent'}, {target: 'blocked'}],
		},
		targeting: {
			on: {
				'drag.hover': {target: 'resolving', actions: 'resolveHover'},
				'drag.leave': {target: 'idle', actions: 'clear'},
				'drag.drop': {target: 'idle', actions: 'clear'},
			},
		},
		blocked: {
			on: {
				'drag.hover': {target: 'resolving', actions: 'resolveHover'},
				'drag.leave': {target: 'idle', actions: 'clear'},
				'drag.drop': {target: 'idle', actions: 'clear'},
			},
		},
	},
});

export type RoleReorderSnapshot = SnapshotFrom<typeof roleReorderStateMachine>;
export type RoleReorderStateValue = 'idle' | 'resolving' | 'targeting' | 'blocked';

export function createRoleReorderSnapshot(): RoleReorderSnapshot {
	return initialTransition(roleReorderStateMachine)[0];
}

export function transitionRoleReorderSnapshot(
	snapshot: RoleReorderSnapshot,
	event: RoleReorderEvent,
): RoleReorderSnapshot {
	return transition(roleReorderStateMachine, snapshot, event)[0] as RoleReorderSnapshot;
}

export function getRoleReorderStateValue(snapshot: RoleReorderSnapshot): RoleReorderStateValue {
	return snapshot.value as RoleReorderStateValue;
}

export function selectRoleReorderResolution(
	item: RoleReorderDragItem,
	target: RoleReorderTarget,
	access: RoleReorderAccess,
	clientOffset: RoleReorderPoint,
	targetRect: RoleReorderRect,
): RoleReorderResolution {
	const snapshot = transitionRoleReorderSnapshot(createRoleReorderSnapshot(), {
		type: 'drag.hover',
		item,
		target,
		access,
		clientOffset,
		targetRect,
	});
	return snapshot.context;
}

export function selectRoleReorderIntent(
	item: RoleReorderDragItem,
	target: RoleReorderTarget,
	access: RoleReorderAccess,
	clientOffset: RoleReorderPoint,
	targetRect: RoleReorderRect,
): RoleReorderIntent | null {
	return selectRoleReorderResolution(item, target, access, clientOffset, targetRect).intent;
}
