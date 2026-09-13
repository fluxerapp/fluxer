// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	canRoleDropOnTarget,
	createRoleReorderSnapshot,
	getRoleReorderStateValue,
	type RoleReorderAccess,
	type RoleReorderDragItem,
	type RoleReorderRect,
	type RoleReorderTarget,
	resolveRoleReorderHover,
	selectRoleReorderIntent,
	transitionRoleReorderSnapshot,
} from '@app/features/guild/components/modals/guild_tabs/RoleReorderStateMachine';
import {describe, expect, it} from 'vitest';

const rect: RoleReorderRect = {
	top: 20,
	bottom: 52,
	left: 0,
	right: 240,
};

const managerAccess: RoleReorderAccess = {
	canManageRoles: true,
	isGuildOwner: false,
};

const ownerAccess: RoleReorderAccess = {
	canManageRoles: true,
	isGuildOwner: true,
};

function point(y: number) {
	return {
		x: 120,
		y,
	};
}

function item(id: string, options: Partial<RoleReorderDragItem> = {}): RoleReorderDragItem {
	return {
		id,
		isEveryone: false,
		isLocked: false,
		...options,
	};
}

function target(
	id: string,
	options: {
		isEveryone?: boolean;
		isLocked?: boolean;
		isTerminal?: boolean;
	} = {},
): RoleReorderTarget {
	return {
		role: {
			id,
			isEveryone: options.isEveryone ?? false,
			isLocked: options.isLocked ?? false,
		},
		isTerminal: options.isTerminal ?? false,
	};
}

function topTarget(): RoleReorderTarget {
	return {
		role: null,
		isTerminal: false,
	};
}

describe('RoleReorderStateMachine', () => {
	it('starts idle and targets a valid role hover', () => {
		let snapshot = createRoleReorderSnapshot();
		expect(getRoleReorderStateValue(snapshot)).toBe('idle');
		snapshot = transitionRoleReorderSnapshot(snapshot, {
			type: 'drag.hover',
			item: item('source'),
			target: target('target'),
			access: managerAccess,
			clientOffset: point(22),
			targetRect: rect,
		});
		expect(getRoleReorderStateValue(snapshot)).toBe('targeting');
		expect(snapshot.context.intent).toEqual({
			indicator: {position: 'top', isValid: true},
			result: {
				targetRoleId: 'target',
				position: 'before',
			},
		});
	});

	it('clears targeting state on drop', () => {
		let snapshot = createRoleReorderSnapshot();
		snapshot = transitionRoleReorderSnapshot(snapshot, {
			type: 'drag.hover',
			item: item('source'),
			target: target('target'),
			access: managerAccess,
			clientOffset: point(22),
			targetRect: rect,
		});
		snapshot = transitionRoleReorderSnapshot(snapshot, {type: 'drag.drop'});
		expect(getRoleReorderStateValue(snapshot)).toBe('idle');
		expect(snapshot.context.intent).toBeNull();
		expect(snapshot.context.indicator).toBeNull();
	});

	it('uses one canonical before-target slot for every pixel of a non-terminal role', () => {
		const source = item('source');
		const destination = target('target');
		for (let y = rect.top; y <= rect.bottom; y++) {
			const intent = selectRoleReorderIntent(source, destination, managerAccess, point(y), rect);
			expect(intent, `expected intent for y=${y}`).toEqual({
				indicator: {position: 'top', isValid: true},
				result: {
					targetRoleId: 'target',
					position: 'before',
				},
			});
		}
	});

	it('allows the terminal non-everyone role to own the final after slot', () => {
		const source = item('source');
		const terminal = target('last-role', {isTerminal: true});
		expect(selectRoleReorderIntent(source, terminal, managerAccess, point(21), rect)).toEqual({
			indicator: {position: 'top', isValid: true},
			result: {
				targetRoleId: 'last-role',
				position: 'before',
			},
		});
		expect(selectRoleReorderIntent(source, terminal, managerAccess, point(51), rect)).toEqual({
			indicator: {position: 'bottom', isValid: true},
			result: {
				targetRoleId: 'last-role',
				position: 'after',
			},
		});
	});

	it('always maps @everyone to before-@everyone and never manufactures an after-everyone drop', () => {
		const source = item('source');
		const everyone = target('guild-1', {isEveryone: true, isTerminal: true});
		for (let y = rect.top; y <= rect.bottom; y++) {
			expect(selectRoleReorderIntent(source, everyone, managerAccess, point(y), rect)).toEqual({
				indicator: {position: 'top', isValid: true},
				result: {
					targetRoleId: 'guild-1',
					position: 'before',
				},
			});
		}
	});

	it('blocks impossible sources before producing reorder intents', () => {
		const destination = target('target');
		expect(canRoleDropOnTarget(item('source'), destination, {...managerAccess, canManageRoles: false})).toBe(false);
		expect(
			resolveRoleReorderHover(item('source'), destination, {...managerAccess, canManageRoles: false}, point(22), rect),
		).toMatchObject({
			intent: null,
			indicator: {position: 'top', isValid: false},
			blockedReason: 'missing-manage-roles-permission',
		});
		expect(
			resolveRoleReorderHover(item('guild-1', {isEveryone: true}), destination, managerAccess, point(22), rect),
		).toMatchObject({
			intent: null,
			indicator: {position: 'top', isValid: false},
			blockedReason: 'source-is-everyone',
		});
		expect(
			resolveRoleReorderHover(item('locked', {isLocked: true}), destination, managerAccess, point(22), rect),
		).toMatchObject({
			intent: null,
			indicator: {position: 'top', isValid: false},
			blockedReason: 'source-is-locked',
		});
	});

	it('blocks dropping a role onto itself', () => {
		let snapshot = createRoleReorderSnapshot();
		snapshot = transitionRoleReorderSnapshot(snapshot, {
			type: 'drag.hover',
			item: item('same'),
			target: target('same'),
			access: managerAccess,
			clientOffset: point(22),
			targetRect: rect,
		});
		expect(getRoleReorderStateValue(snapshot)).toBe('blocked');
		expect(snapshot.context.blockedReason).toBe('same-source-and-target');
		expect(snapshot.context.indicator).toEqual({position: 'top', isValid: false});
	});

	it('blocks inserting before a locked role for non-owners but allows owners', () => {
		const source = item('source');
		const lockedTarget = target('locked-target', {isLocked: true});
		expect(resolveRoleReorderHover(source, lockedTarget, managerAccess, point(22), rect)).toMatchObject({
			intent: null,
			indicator: {position: 'top', isValid: false},
			blockedReason: 'target-before-locked-role',
		});
		expect(selectRoleReorderIntent(source, lockedTarget, ownerAccess, point(22), rect)).toEqual({
			indicator: {position: 'top', isValid: true},
			result: {
				targetRoleId: 'locked-target',
				position: 'before',
			},
		});
	});

	it('only lets owners use an explicit top target', () => {
		const source = item('source');
		expect(resolveRoleReorderHover(source, topTarget(), managerAccess, point(22), rect)).toMatchObject({
			intent: null,
			indicator: {position: 'top', isValid: false},
			blockedReason: 'top-target-requires-owner',
		});
		expect(selectRoleReorderIntent(source, topTarget(), ownerAccess, point(22), rect)).toEqual({
			indicator: {position: 'top', isValid: true},
			result: {
				targetRoleId: null,
				position: 'before',
			},
		});
	});

	it('blocks zero-height target rects without manufacturing an indicator or result', () => {
		const emptyRect = {...rect, bottom: rect.top};
		expect(
			resolveRoleReorderHover(item('source'), target('target'), managerAccess, point(rect.top), emptyRect),
		).toEqual({
			intent: null,
			indicator: null,
			blockedReason: 'empty-target-rect',
		});
	});
});
