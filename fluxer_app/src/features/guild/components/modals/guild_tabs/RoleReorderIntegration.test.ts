// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRoleMovePreview} from '@app/features/guild/components/modals/guild_tabs/RoleMoveOperation';
import {
	type RoleReorderAccess,
	type RoleReorderDragItem,
	type RoleReorderRect,
	type RoleReorderTarget,
	selectRoleReorderResolution,
} from '@app/features/guild/components/modals/guild_tabs/RoleReorderStateMachine';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {describe, expect, it} from 'vitest';

const guildId = 'guild-1';
const rect: RoleReorderRect = {
	top: 10,
	bottom: 42,
	left: 0,
	right: 260,
};
const managerAccess: RoleReorderAccess = {
	canManageRoles: true,
	isGuildOwner: false,
};

function point(y: number) {
	return {
		x: 140,
		y,
	};
}

function role(id: string, position: number): GuildRole {
	return {
		id,
		guildId,
		position,
		get isEveryone() {
			return id === guildId;
		},
	} as GuildRole;
}

function roleIds(roles: ReadonlyArray<GuildRole>): Array<string> {
	return roles.map((role) => role.id);
}

function dragItem(role: GuildRole, isLocked: boolean): RoleReorderDragItem {
	return {
		id: role.id,
		isEveryone: role.isEveryone,
		isLocked,
	};
}

function target(role: GuildRole, isLocked: boolean, isTerminal: boolean): RoleReorderTarget {
	return {
		role: {
			id: role.id,
			isEveryone: role.isEveryone,
			isLocked,
		},
		isTerminal,
	};
}

describe('role reorder machine and move preview integration', () => {
	const roles = [
		role('locked-admin', 5),
		role('locked-manager', 4),
		role('alpha', 3),
		role('beta', 2),
		role('gamma', 1),
		role(guildId, 0),
	];
	const lockedRoleIds = new Set(['locked-admin', 'locked-manager']);
	const isRoleLocked = (role: GuildRole) => lockedRoleIds.has(role.id);
	const isRoleIdLocked = (roleId: string) => lockedRoleIds.has(roleId);

	it('moves a manageable role to the highest legal slot below locked roles from any pixel on that target row', () => {
		const source = roles[4]!;
		const firstManageableTarget = target(roles[2]!, false, false);
		for (let y = rect.top; y <= rect.bottom; y++) {
			const resolution = selectRoleReorderResolution(
				dragItem(source, false),
				firstManageableTarget,
				managerAccess,
				point(y),
				rect,
			);
			expect(resolution.intent, `expected intent for y=${y}`).toEqual({
				indicator: {position: 'top', isValid: true},
				result: {
					targetRoleId: 'alpha',
					position: 'before',
				},
			});
			const preview = createRoleMovePreview({
				roles,
				draggedRoleId: source.id,
				targetRoleId: resolution.intent!.result.targetRoleId,
				position: resolution.intent!.result.position,
				isRoleLocked,
			});
			expect(preview?.operation).toEqual({
				roleId: 'gamma',
				precedingRoleId: 'locked-manager',
			});
			expect(roleIds(preview?.order ?? [])).toEqual([
				'locked-admin',
				'locked-manager',
				'gamma',
				'alpha',
				'beta',
				guildId,
			]);
		}
	});

	it('keeps locked targets fixed by rejecting machine hovers that would insert before them', () => {
		const source = roles[4]!;
		const lockedTarget = target(roles[1]!, true, false);
		const resolution = selectRoleReorderResolution(
			dragItem(source, false),
			lockedTarget,
			managerAccess,
			point(41),
			rect,
		);
		expect(resolution).toMatchObject({
			intent: null,
			indicator: {position: 'top', isValid: false},
			blockedReason: 'target-before-locked-role',
		});
	});

	it('rejects machine intents for locked sources again in the move preview layer', () => {
		const lockedSource = roles[1]!;
		const manageableTarget = target(roles[2]!, false, false);
		const resolution = selectRoleReorderResolution(
			dragItem(lockedSource, true),
			manageableTarget,
			managerAccess,
			point(20),
			rect,
		);
		expect(resolution).toMatchObject({
			intent: null,
			blockedReason: 'source-is-locked',
		});
		expect(
			createRoleMovePreview({
				roles,
				draggedRoleId: lockedSource.id,
				targetRoleId: 'alpha',
				position: 'before',
				isRoleLocked,
			}),
		).toBeNull();
	});

	it('maps every machine-produced target for a manageable source into a preview that preserves locked prefix and @everyone', () => {
		const source = roles[4]!;
		for (let targetIndex = 0; targetIndex < roles.length; targetIndex++) {
			const destination = roles[targetIndex]!;
			const resolution = selectRoleReorderResolution(
				dragItem(source, false),
				target(destination, isRoleIdLocked(destination.id), targetIndex === roles.length - 1),
				managerAccess,
				point(targetIndex === roles.length - 1 ? 41 : 20),
				rect,
			);
			if (!resolution.intent) {
				expect(['target-before-locked-role', 'same-source-and-target']).toContain(resolution.blockedReason);
				continue;
			}
			const preview = createRoleMovePreview({
				roles,
				draggedRoleId: source.id,
				targetRoleId: resolution.intent.result.targetRoleId,
				position: resolution.intent.result.position,
				isRoleLocked,
			});
			if (!preview) {
				expect(destination.id).toBe(guildId);
				expect(resolution.intent.result).toEqual({
					targetRoleId: guildId,
					position: 'before',
				});
				continue;
			}
			expect(preview, `expected preview for target ${destination.id}`).not.toBeNull();
			expect(roleIds(preview.order).slice(0, 2)).toEqual(['locked-admin', 'locked-manager']);
			expect(roleIds(preview.order).at(-1)).toBe(guildId);
			expect(new Set(roleIds(preview.order))).toEqual(new Set(roleIds(roles)));
		}
	});

	it('supports terminal hoist-list after drops without using @everyone as a target', () => {
		const hoistedRoles = [role('alpha', 3), role('beta', 2), role('gamma', 1)];
		const source = hoistedRoles[0]!;
		const terminalTarget = target(hoistedRoles[2]!, false, true);
		const resolution = selectRoleReorderResolution(
			dragItem(source, false),
			terminalTarget,
			managerAccess,
			point(41),
			rect,
		);
		expect(resolution.intent).toEqual({
			indicator: {position: 'bottom', isValid: true},
			result: {
				targetRoleId: 'gamma',
				position: 'after',
			},
		});
		const preview = createRoleMovePreview({
			roles: hoistedRoles,
			draggedRoleId: source.id,
			targetRoleId: resolution.intent!.result.targetRoleId,
			position: resolution.intent!.result.position,
			isRoleLocked: () => false,
		});
		expect(roleIds(preview?.order ?? [])).toEqual(['beta', 'gamma', 'alpha']);
		expect(preview?.operation).toEqual({
			roleId: 'alpha',
			precedingRoleId: 'gamma',
		});
	});
});
