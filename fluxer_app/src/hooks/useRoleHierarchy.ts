/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import type {GuildRecord} from '~/records/GuildRecord';
import UserStore from '~/stores/UserStore';
import * as PermissionUtils from '~/utils/PermissionUtils';

export function useRoleHierarchy(guild: GuildRecord | null | undefined) {
	const currentUser = UserStore.currentUser;

	const currentUserHighestRole = React.useMemo(() => {
		if (!guild || !currentUser) return null;
		return PermissionUtils.getHighestRole(guild.toJSON(), currentUser.id);
	}, [guild, currentUser]);

	const canManageRole = React.useCallback(
		(role: {id: string; position: number; permissions: bigint}): boolean => {
			if (!guild || !currentUser) return false;

			if (guild.isOwner(currentUser.id)) return true;

			if (!currentUserHighestRole) return false;

			return PermissionUtils.isRoleHigher(guild.toJSON(), currentUser.id, currentUserHighestRole, role);
		},
		[guild, currentUser, currentUserHighestRole],
	);

	const canManageTarget = React.useCallback(
		(targetUserId: string): boolean => {
			if (!guild || !currentUser) return false;

			if (guild.isOwner(currentUser.id)) return true;

			if (guild.isOwner(targetUserId)) return false;

			const targetHighestRole = PermissionUtils.getHighestRole(guild.toJSON(), targetUserId);

			if (!currentUserHighestRole) return false;
			if (!targetHighestRole) return true;

			return currentUserHighestRole.position > targetHighestRole.position;
		},
		[guild, currentUser, currentUserHighestRole],
	);

	return {canManageRole, canManageTarget, currentUserHighestRole};
}
