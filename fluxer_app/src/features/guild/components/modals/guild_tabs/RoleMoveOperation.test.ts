// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRoleMovePreview} from '@app/features/guild/components/modals/guild_tabs/RoleMoveOperation';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {describe, expect, it} from 'vitest';

const guildId = 'guild-1';

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

describe('createRoleMovePreview', () => {
	const roles = [
		role('locked-admin', 5),
		role('locked-manager', 4),
		role('alpha', 3),
		role('beta', 2),
		role(guildId, 0),
	];
	const lockedRoleIds = new Set(['locked-admin', 'locked-manager']);
	const isRoleLocked = (role: GuildRole) => lockedRoleIds.has(role.id);

	it('rejects moving @everyone or a locked source role', () => {
		expect(
			createRoleMovePreview({
				roles,
				draggedRoleId: guildId,
				targetRoleId: 'alpha',
				position: 'before',
				isRoleLocked,
			}),
		).toBeNull();
		expect(
			createRoleMovePreview({
				roles,
				draggedRoleId: 'locked-manager',
				targetRoleId: 'alpha',
				position: 'before',
				isRoleLocked,
			}),
		).toBeNull();
	});

	it('rejects moving a manageable role before locked roles', () => {
		expect(
			createRoleMovePreview({
				roles,
				draggedRoleId: 'beta',
				targetRoleId: 'locked-manager',
				position: 'before',
				isRoleLocked,
			}),
		).toBeNull();
	});

	it('allows moving manageable roles to the highest slot below the locked prefix', () => {
		const preview = createRoleMovePreview({
			roles,
			draggedRoleId: 'beta',
			targetRoleId: 'alpha',
			position: 'before',
			isRoleLocked,
		});
		expect(preview?.operation).toEqual({
			roleId: 'beta',
			precedingRoleId: 'locked-manager',
		});
		expect(roleIds(preview?.order ?? [])).toEqual(['locked-admin', 'locked-manager', 'beta', 'alpha', guildId]);
	});

	it('rejects after-@everyone moves and allows before-@everyone moves', () => {
		expect(
			createRoleMovePreview({
				roles,
				draggedRoleId: 'alpha',
				targetRoleId: guildId,
				position: 'after',
				isRoleLocked,
			}),
		).toBeNull();
		const preview = createRoleMovePreview({
			roles,
			draggedRoleId: 'alpha',
			targetRoleId: guildId,
			position: 'before',
			isRoleLocked,
		});
		expect(preview?.operation).toEqual({
			roleId: 'alpha',
			precedingRoleId: 'beta',
		});
		expect(roleIds(preview?.order ?? [])).toEqual(['locked-admin', 'locked-manager', 'beta', 'alpha', guildId]);
	});

	it('returns null for moves that keep the role in its current slot', () => {
		expect(
			createRoleMovePreview({
				roles,
				draggedRoleId: 'alpha',
				targetRoleId: 'beta',
				position: 'before',
				isRoleLocked,
			}),
		).toBeNull();
	});

	it('lets owners place a role at the absolute top when no role is locked', () => {
		const preview = createRoleMovePreview({
			roles,
			draggedRoleId: 'beta',
			targetRoleId: null,
			position: 'before',
			isRoleLocked: () => false,
		});
		expect(preview?.operation).toEqual({
			roleId: 'beta',
			precedingRoleId: null,
		});
		expect(roleIds(preview?.order ?? [])).toEqual(['beta', 'locked-admin', 'locked-manager', 'alpha', guildId]);
	});
});
