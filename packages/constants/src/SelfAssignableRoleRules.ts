// SPDX-License-Identifier: AGPL-3.0-or-later

export function canUseSelfAssignableRoleSection<RoleId>(
	requiredRoleIds: ReadonlyArray<RoleId>,
	memberRoleIds: {has(roleId: RoleId): boolean} | null | undefined,
): boolean {
	return requiredRoleIds.length === 0 || requiredRoleIds.some((roleId) => memberRoleIds?.has(roleId) === true);
}

export type SelfAssignableRolePick<RoleId> =
	| {readonly allowed: true; readonly replacedRoleIds: ReadonlyArray<RoleId>}
	| {readonly allowed: false; readonly maxRoles: number};

export function evaluateSelfAssignableRolePick<RoleId>(input: {
	readonly sectionRoleIds: ReadonlyArray<RoleId>;
	readonly maxRoles: number | null;
	readonly roleId: RoleId;
	readonly hasRole: (roleId: RoleId) => boolean;
}): SelfAssignableRolePick<RoleId> {
	const heldOthers = input.sectionRoleIds.filter((id) => id !== input.roleId && input.hasRole(id));
	if (input.maxRoles === 1) return {allowed: true, replacedRoleIds: heldOthers};
	if (input.maxRoles != null && !input.hasRole(input.roleId) && heldOthers.length >= input.maxRoles) {
		return {allowed: false, maxRoles: input.maxRoles};
	}
	return {allowed: true, replacedRoleIds: []};
}
