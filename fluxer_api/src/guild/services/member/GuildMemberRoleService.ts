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

import type {GuildID, RoleID, UserID} from '~/BrandedTypes';
import {UnknownGuildMemberError} from '~/Errors';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IGuildRepository} from '../../IGuildRepository';
import type {GuildMemberAuthService} from './GuildMemberAuthService';
import type {GuildMemberValidationService} from './GuildMemberValidationService';

export class GuildMemberRoleService {
	constructor(
		private readonly guildRepository: IGuildRepository,
		private readonly gatewayService: IGatewayService,
		private readonly authService: GuildMemberAuthService,
		private readonly validationService: GuildMemberValidationService,
	) {}

	async addMemberRole(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		roleId: RoleID;
		requestCache: RequestCache;
	}): Promise<void> {
		const {userId, targetId, guildId, roleId} = params;
		const {guildData, canManageRoles} = await this.authService.getGuildAuthenticated({userId, guildId});

		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();

		await this.validationService.validateRoleAssignment({
			guildData,
			guildId,
			userId,
			targetId,
			roleId,
			canManageRoles,
		});

		if (targetMember.roleIds.has(roleId)) return;

		const updatedRoleIds = new Set(targetMember.roleIds);
		updatedRoleIds.add(roleId);

		const updatedMemberData = {
			...targetMember.toRow(),
			role_ids: updatedRoleIds,
			temporary: targetMember.isTemporary ? false : targetMember.isTemporary,
		};
		await this.guildRepository.upsertMember(updatedMemberData);

		if (targetMember.isTemporary) {
			await this.gatewayService.removeTemporaryGuild({userId: targetId, guildId});
		}
	}

	async removeMemberRole(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		roleId: RoleID;
		requestCache: RequestCache;
	}): Promise<void> {
		const {userId, targetId, guildId, roleId} = params;
		const {guildData, canManageRoles} = await this.authService.getGuildAuthenticated({userId, guildId});

		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();

		await this.validationService.validateRoleAssignment({
			guildData,
			guildId,
			userId,
			targetId,
			roleId,
			canManageRoles,
		});

		if (!targetMember.roleIds.has(roleId)) return;

		const updatedRoleIds = new Set(targetMember.roleIds);
		updatedRoleIds.delete(roleId);

		const updatedMemberData = {...targetMember.toRow(), role_ids: updatedRoleIds};
		await this.guildRepository.upsertMember(updatedMemberData);
	}
}
