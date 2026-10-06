// SPDX-License-Identifier: AGPL-3.0-or-later

import {ALL_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';

export function normalizeBotInvitePermissions(requestedPermissions: bigint): bigint {
	return requestedPermissions & ALL_PERMISSIONS;
}

function hasAdministratorPermission(permissions: bigint): boolean {
	return (permissions & Permissions.ADMINISTRATOR) === Permissions.ADMINISTRATOR;
}

function hasManageGuildPermission(permissions: bigint): boolean {
	return (permissions & Permissions.MANAGE_GUILD) === Permissions.MANAGE_GUILD;
}

export function canAuthorizeBotInvite({
	userPermissions,
	requestedPermissions,
}: {
	userPermissions: bigint;
	requestedPermissions?: bigint | null;
}): boolean {
	const normalizedRequestedPermissions = normalizeBotInvitePermissions(requestedPermissions ?? 0n);
	if (!hasAdministratorPermission(userPermissions) && !hasManageGuildPermission(userPermissions)) {
		return false;
	}
	if (normalizedRequestedPermissions === 0n || hasAdministratorPermission(userPermissions)) {
		return true;
	}
	return (normalizedRequestedPermissions & ~userPermissions) === 0n;
}
