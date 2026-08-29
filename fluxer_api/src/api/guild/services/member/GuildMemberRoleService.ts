// SPDX-License-Identifier: AGPL-3.0-or-later

import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import type {GuildID, RoleID, UserID} from '../../../BrandedTypes';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {IGuildRepositoryAggregate} from '../../repositories/IGuildRepositoryAggregate';
import type {GuildMemberAuthService} from './GuildMemberAuthService';
import type {GuildMemberValidationService} from './GuildMemberValidationService';

export class GuildMemberRoleService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly gatewayService: IGatewayService,
		private readonly authService: GuildMemberAuthService,
		private readonly validationService: GuildMemberValidationService,
	) {}

	async systemAddMemberRole(params: {targetId: UserID; guildId: GuildID; roleId: RoleID}): Promise<void> {
		const {targetId, guildId, roleId} = params;
		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();
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

	async addMemberRole(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		roleId: RoleID;
		requestCache: RequestCache;
	}): Promise<void> {
		const {userId, targetId, guildId, roleId} = params;
		const {guildData, hasPermission, canManageRoles} = await this.authService.getGuildAuthenticated({userId, guildId});
		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();
		await this.validationService.validateRoleAssignment({
			guildData,
			guildId,
			userId,
			targetId,
			roleId,
			hasPermission,
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
		const {guildData, hasPermission, canManageRoles} = await this.authService.getGuildAuthenticated({userId, guildId});
		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();
		await this.validationService.validateRoleAssignment({
			guildData,
			guildId,
			userId,
			targetId,
			roleId,
			hasPermission,
			canManageRoles,
		});
		if (!targetMember.roleIds.has(roleId)) return;
		const updatedRoleIds = new Set(targetMember.roleIds);
		updatedRoleIds.delete(roleId);
		const updatedMemberData = {...targetMember.toRow(), role_ids: updatedRoleIds};
		await this.guildRepository.upsertMember(updatedMemberData);
	}
}
