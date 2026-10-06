// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, RoleID} from '@app/api/BrandedTypes';
import type {GuildRoleRow} from '@app/api/database/types/GuildTypes';
import type {GuildRole} from '@app/api/models/GuildRole';

export abstract class IGuildRoleRepository {
	abstract getRole(roleId: RoleID, guildId: GuildID): Promise<GuildRole | null>;

	abstract listRoles(guildId: GuildID): Promise<Array<GuildRole>>;

	abstract listRolesByIds(roleIds: Array<RoleID>, guildId: GuildID): Promise<Array<GuildRole>>;

	abstract countRoles(guildId: GuildID): Promise<number>;

	abstract upsertRole(data: GuildRoleRow, oldData?: GuildRoleRow | null): Promise<GuildRole>;

	abstract deleteRole(guildId: GuildID, roleId: RoleID): Promise<void>;
}
