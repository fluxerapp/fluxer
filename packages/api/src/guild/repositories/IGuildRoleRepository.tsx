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

import type {GuildID, RoleID} from '@fluxer/api/src/BrandedTypes';
import type {GuildRoleRow} from '@fluxer/api/src/database/types/GuildTypes';
import type {GuildRole} from '@fluxer/api/src/models/GuildRole';

export abstract class IGuildRoleRepository {
	abstract getRole(roleId: RoleID, guildId: GuildID): Promise<GuildRole | null>;
	abstract listRoles(guildId: GuildID): Promise<Array<GuildRole>>;
	abstract listRolesByIds(roleIds: Array<RoleID>, guildId: GuildID): Promise<Array<GuildRole>>;
	abstract countRoles(guildId: GuildID): Promise<number>;
	abstract upsertRole(data: GuildRoleRow, oldData?: GuildRoleRow | null): Promise<GuildRole>;
	abstract deleteRole(guildId: GuildID, roleId: RoleID): Promise<void>;
}
