// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createRoleID, type RoleID} from '@app/api/BrandedTypes';
import type {GuildRoleRow} from '@app/api/database/types/GuildTypes';
import {computeMovedIds, getMemberListRoleOrderIds} from '@app/api/guild/services/role/RoleOrderAuditUtils';
import {GuildRole} from '@app/api/models/GuildRole';
import {describe, expect, test} from 'vitest';

const GUILD_ID = createGuildID(100n);
const EVERYONE_ROLE_ID = createRoleID(100n);

function createRole(overrides: Partial<GuildRoleRow> & Pick<GuildRoleRow, 'role_id'>): GuildRole {
	return new GuildRole({
		guild_id: GUILD_ID,
		name: 'Role',
		permissions: 0n,
		position: 1,
		hoist_position: null,
		color: 0,
		icon_hash: null,
		unicode_emoji: null,
		hoist: true,
		mentionable: false,
		version: 1,
		...overrides,
	});
}

describe('computeMovedIds', () => {
	test('drag up reports only the dragged id', () => {
		expect(computeMovedIds(['a', 'b', 'c', 'd'], ['d', 'a', 'b', 'c'])).toEqual(new Set(['d']));
	});

	test('drag down reports only the dragged id', () => {
		expect(computeMovedIds(['a', 'b', 'c', 'd'], ['b', 'c', 'd', 'a'])).toEqual(new Set(['a']));
	});

	test('drag into the middle reports only the dragged id', () => {
		expect(computeMovedIds(['a', 'b', 'c', 'd', 'e'], ['a', 'd', 'b', 'c', 'e'])).toEqual(new Set(['d']));
	});

	test('neighbour swap keeps the id that comes first after the swap', () => {
		expect(computeMovedIds(['a', 'b', 'c'], ['b', 'a', 'c'])).toEqual(new Set(['a']));
	});

	test('no change reports nothing', () => {
		expect(computeMovedIds(['a', 'b', 'c'], ['a', 'b', 'c'])).toEqual(new Set());
	});

	test('empty sequences report nothing', () => {
		expect(computeMovedIds([], [])).toEqual(new Set());
	});

	test('full reverse keeps only the first id after the reverse', () => {
		expect(computeMovedIds(['a', 'b', 'c', 'd'], ['d', 'c', 'b', 'a'])).toEqual(new Set(['c', 'b', 'a']));
	});

	test('ties between equally long subsequences go to the earliest index in after', () => {
		expect(computeMovedIds(['x', 'a', 'b'], ['a', 'x', 'b'])).toEqual(new Set(['x']));
		expect(computeMovedIds(['a', 'x', 'b'], ['x', 'a', 'b'])).toEqual(new Set(['a']));
		expect(computeMovedIds(['a', 'b', 'c', 'd'], ['c', 'd', 'a', 'b'])).toEqual(new Set(['a', 'b']));
	});

	test('repeated calls return the same ids in after order', () => {
		const before = ['a', 'b', 'c', 'd', 'e', 'f'];
		const after = ['f', 'd', 'a', 'e', 'b', 'c'];
		const first = [...computeMovedIds(before, after)];
		const second = [...computeMovedIds(before, after)];
		expect(first).toEqual(second);
		expect(first).toEqual(after.filter((id) => first.includes(id)));
	});

	test('ids only present in after are reported as moved', () => {
		expect(computeMovedIds(['a', 'b'], ['a', 'n', 'b'])).toEqual(new Set(['n']));
	});

	test('bigint ids compare by value', () => {
		const before = [createRoleID(1n), createRoleID(2n), createRoleID(3n)];
		const after = [createRoleID(3n), createRoleID(1n), createRoleID(2n)];
		const moved = computeMovedIds<RoleID>(before, after);
		expect(moved).toEqual(new Set([3n]));
		expect(moved.has(createRoleID(3n))).toBe(true);
	});
});

describe('getMemberListRoleOrderIds', () => {
	test('orders hoisted roles like fluxer_gateway guild_member_list_groups.erl:20-45 with the GuildRolesTab.tsx:133-141 id tie-break', () => {
		const roles = [
			createRole({role_id: EVERYONE_ROLE_ID, position: 0}),
			createRole({role_id: createRoleID(201n), position: 5}),
			createRole({role_id: createRoleID(202n), position: 4, hoist_position: 9}),
			createRole({role_id: createRoleID(203n), position: 3, hoist: false}),
			createRole({role_id: createRoleID(205n), position: 2}),
			createRole({role_id: createRoleID(204n), position: 1, hoist_position: 2}),
		];
		expect(getMemberListRoleOrderIds(roles, EVERYONE_ROLE_ID)).toEqual([202n, 201n, 204n, 205n]);
	});

	test('does not reorder the input array', () => {
		const roles = [
			createRole({role_id: createRoleID(201n), position: 1}),
			createRole({role_id: createRoleID(202n), position: 2}),
		];
		getMemberListRoleOrderIds(roles, EVERYONE_ROLE_ID);
		expect(roles.map((role) => role.id)).toEqual([201n, 202n]);
	});

	test('a first custom hoist order that moves one role reports only that role', () => {
		const before = [
			createRole({role_id: createRoleID(201n), position: 3}),
			createRole({role_id: createRoleID(202n), position: 2}),
			createRole({role_id: createRoleID(203n), position: 1}),
			createRole({role_id: createRoleID(204n), position: 4, hoist: false}),
		];
		const after = [
			createRole({role_id: createRoleID(201n), position: 3, hoist_position: 2}),
			createRole({role_id: createRoleID(202n), position: 2, hoist_position: 1}),
			createRole({role_id: createRoleID(203n), position: 1, hoist_position: 3}),
			createRole({role_id: createRoleID(204n), position: 4, hoist: false}),
		];
		expect(
			computeMovedIds(
				getMemberListRoleOrderIds(before, EVERYONE_ROLE_ID),
				getMemberListRoleOrderIds(after, EVERYONE_ROLE_ID),
			),
		).toEqual(new Set([203n]));
	});
});
