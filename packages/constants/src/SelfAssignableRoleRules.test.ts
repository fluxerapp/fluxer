// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	canUseSelfAssignableRoleSection,
	evaluateSelfAssignableRolePick,
} from '@fluxer/constants/src/SelfAssignableRoleRules';
import {describe, expect, it} from 'vitest';

describe('canUseSelfAssignableRoleSection', () => {
	it('lets everyone use a section without required roles', () => {
		expect(canUseSelfAssignableRoleSection([], new Set<string>())).toBe(true);
		expect(canUseSelfAssignableRoleSection([], null)).toBe(true);
	});

	it('needs one of the required roles otherwise', () => {
		expect(canUseSelfAssignableRoleSection(['a', 'b'], new Set(['b']))).toBe(true);
		expect(canUseSelfAssignableRoleSection(['a', 'b'], new Set(['c']))).toBe(false);
		expect(canUseSelfAssignableRoleSection(['a'], undefined)).toBe(false);
	});
});

describe('evaluateSelfAssignableRolePick', () => {
	const pick = (maxRoles: number | null, held: Array<string>, roleId = 'c') =>
		evaluateSelfAssignableRolePick({
			sectionRoleIds: ['a', 'b', 'c'],
			maxRoles,
			roleId,
			hasRole: (id) => held.includes(id),
		});

	it('swaps out the other held roles in a one-role section', () => {
		expect(pick(1, ['a'])).toEqual({allowed: true, replacedRoleIds: ['a']});
		expect(pick(1, [])).toEqual({allowed: true, replacedRoleIds: []});
	});

	it('refuses a new role once the member holds the limit', () => {
		expect(pick(2, ['a'])).toEqual({allowed: true, replacedRoleIds: []});
		expect(pick(2, ['a', 'b'])).toEqual({allowed: false, maxRoles: 2});
		expect(pick(null, ['a', 'b'])).toEqual({allowed: true, replacedRoleIds: []});
	});

	it('never refuses a role the member already holds', () => {
		expect(pick(1, ['a', 'b'], 'a')).toEqual({allowed: true, replacedRoleIds: ['b']});
		expect(pick(2, ['a', 'b', 'c'], 'c')).toEqual({allowed: true, replacedRoleIds: []});
	});
});
