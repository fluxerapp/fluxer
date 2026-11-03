// SPDX-License-Identifier: AGPL-3.0-or-later

export function isRoleHiddenFromDisplay(roleId: string, hiddenRoleIds: ReadonlyArray<string>): boolean {
	return hiddenRoleIds.includes(roleId);
}

export function filterDisplayRoles<T extends {readonly id: string}>(
	roles: ReadonlyArray<T>,
	hiddenRoleIds: ReadonlyArray<string>,
): Array<T> {
	return roles.filter((role) => !isRoleHiddenFromDisplay(role.id, hiddenRoleIds));
}
