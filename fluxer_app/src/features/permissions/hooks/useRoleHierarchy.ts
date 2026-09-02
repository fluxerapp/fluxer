// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild} from '@app/features/guild/models/Guild';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import Users from '@app/features/user/state/Users';
import {useCallback, useMemo} from 'react';

export function useRoleHierarchy(guild: Guild | null | undefined) {
	const currentUser = Users.currentUser;
	const currentUserHighestRole = useMemo(() => {
		if (!guild || !currentUser) return null;
		return PermissionUtils.getHighestRole(guild.toJSON(), currentUser.id);
	}, [guild, currentUser]);
	const canManageRole = useCallback(
		(role: {id: string; position: number; permissions: bigint}): boolean => {
			if (!guild || !currentUser) return false;
			if (guild.isOwner(currentUser.id)) return true;
			if (!currentUserHighestRole) return false;
			return PermissionUtils.isRoleHigher(guild.toJSON(), currentUser.id, currentUserHighestRole, role);
		},
		[guild, currentUser, currentUserHighestRole],
	);
	const canManageTarget = useCallback(
		(targetUserId: string): boolean => {
			if (!guild || !currentUser) return false;
			if (guild.isOwner(currentUser.id)) return true;
			if (guild.isOwner(targetUserId)) return false;
			return PermissionUtils.canManageTargetUser(guild.toJSON(), currentUser.id, currentUserHighestRole, targetUserId);
		},
		[guild, currentUser, currentUserHighestRole],
	);
	return {canManageRole, canManageTarget, currentUserHighestRole};
}
