// SPDX-License-Identifier: AGPL-3.0-or-later

import {filterDisplayRoles} from '@fluxer/constants/src/RoleDisplayUtils';
import {expect, test} from 'vitest';

test('a hidden role is left out of what people see, and the member keeps it for permission checks', () => {
	const roles = [
		{id: '10', permissions: 1n << 3n},
		{id: '20', permissions: 1n << 10n},
	];
	const memberRoleIds = new Set(['10', '20']);
	const shown = filterDisplayRoles(
		roles.filter((role) => memberRoleIds.has(role.id)),
		['20'],
	);
	expect(shown.map((role) => role.id)).toEqual(['10']);
	expect([...memberRoleIds]).toEqual(['10', '20']);
	const permissions = roles
		.filter((role) => memberRoleIds.has(role.id))
		.reduce((granted, role) => granted | role.permissions, 0n);
	expect(permissions & (1n << 10n)).toBe(1n << 10n);
	expect(filterDisplayRoles(roles, [])).toEqual(roles);
});
