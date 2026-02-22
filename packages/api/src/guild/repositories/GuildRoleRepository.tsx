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
import {
	buildPatchFromData,
	deleteOneOrMany,
	executeVersionedUpdate,
	fetchMany,
	fetchOne,
} from '@fluxer/api/src/database/Cassandra';
import type {GuildRoleRow} from '@fluxer/api/src/database/types/GuildTypes';
import {GUILD_ROLE_COLUMNS} from '@fluxer/api/src/database/types/GuildTypes';
import {IGuildRoleRepository} from '@fluxer/api/src/guild/repositories/IGuildRoleRepository';
import {GuildRole} from '@fluxer/api/src/models/GuildRole';
import {GuildRoles} from '@fluxer/api/src/Tables';

const FETCH_GUILD_ROLE_BY_ID_QUERY = GuildRoles.selectCql({
	where: [GuildRoles.where.eq('guild_id'), GuildRoles.where.eq('role_id')],
	limit: 1,
});

const FETCH_GUILD_ROLES_BY_GUILD_ID_QUERY = GuildRoles.selectCql({
	where: GuildRoles.where.eq('guild_id'),
});

const FETCH_ROLES_BY_IDS_QUERY = GuildRoles.selectCql({
	where: [GuildRoles.where.eq('guild_id'), GuildRoles.where.in('role_id', 'role_ids')],
});

export class GuildRoleRepository extends IGuildRoleRepository {
	async getRole(roleId: RoleID, guildId: GuildID): Promise<GuildRole | null> {
		const role = await fetchOne<GuildRoleRow>(FETCH_GUILD_ROLE_BY_ID_QUERY, {
			guild_id: guildId,
			role_id: roleId,
		});
		return role ? new GuildRole(role) : null;
	}

	async listRoles(guildId: GuildID): Promise<Array<GuildRole>> {
		const roles = await fetchMany<GuildRoleRow>(FETCH_GUILD_ROLES_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return roles.map((role) => new GuildRole(role));
	}

	async listRolesByIds(roleIds: Array<RoleID>, guildId: GuildID): Promise<Array<GuildRole>> {
		if (roleIds.length === 0) return [];

		const roles = await fetchMany<GuildRoleRow>(FETCH_ROLES_BY_IDS_QUERY, {
			guild_id: guildId,
			role_ids: roleIds,
		});
		return roles.map((role) => new GuildRole(role));
	}

	async countRoles(guildId: GuildID): Promise<number> {
		const roles = await fetchMany<GuildRoleRow>(FETCH_GUILD_ROLES_BY_GUILD_ID_QUERY, {
			guild_id: guildId,
		});
		return roles.length;
	}

	async upsertRole(data: GuildRoleRow, oldData?: GuildRoleRow | null): Promise<GuildRole> {
		const guildId = data.guild_id;
		const roleId = data.role_id;

		const result = await executeVersionedUpdate<GuildRoleRow, 'guild_id' | 'role_id'>(
			async () =>
				fetchOne<GuildRoleRow>(FETCH_GUILD_ROLE_BY_ID_QUERY, {
					guild_id: guildId,
					role_id: roleId,
				}),
			(current) => ({
				pk: {guild_id: guildId, role_id: roleId},
				patch: buildPatchFromData(data, oldData ?? current, GUILD_ROLE_COLUMNS, ['guild_id', 'role_id']),
			}),
			GuildRoles,
			{initialData: oldData},
		);

		return new GuildRole({...data, version: result.finalVersion ?? 1});
	}

	async deleteRole(guildId: GuildID, roleId: RoleID): Promise<void> {
		await deleteOneOrMany(
			GuildRoles.deleteByPk({
				guild_id: guildId,
				role_id: roleId,
			}),
		);
	}
}
