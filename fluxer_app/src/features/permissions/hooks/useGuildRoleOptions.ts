// SPDX-License-Identifier: AGPL-3.0-or-later

import {sortRolesByPosition} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import type {Guild} from '@app/features/guild/models/Guild';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {useRoleHierarchy} from '@app/features/permissions/hooks/useRoleHierarchy';
import {useMemo} from 'react';

export interface GuildRoleOption {
	readonly role: GuildRole;
	readonly canManage: boolean;
}

export function useGuildRoleOptions(guild: Guild | null | undefined): Array<GuildRoleOption> {
	const {canManageRole} = useRoleHierarchy(guild);
	return useMemo(() => {
		if (!guild) return [];
		return sortRolesByPosition(Object.values(guild.roles).filter((role) => !role.isEveryone)).map((role) => ({
			role,
			canManage: canManageRole({id: role.id, position: role.position, permissions: role.permissions}),
		}));
	}, [guild, canManageRole]);
}
