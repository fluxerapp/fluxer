// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, RoleID, UserID} from '@app/api/BrandedTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {getGuildAuth} from '@app/api/guild/services/GuildMfaEnforcement';
import type {GuildMemberValidationService} from '@app/api/guild/services/member/GuildMemberValidationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';

export class GuildMemberRoleService {
	private async applyAddMemberRole(
		targetMember: GuildMember,
		guildId: GuildID,
		targetId: UserID,
		roleId: RoleID,
	): Promise<void> {
		if (targetMember.roleIds.has(roleId)) return;
		const updatedRoleIds = new Set(targetMember.roleIds);
		updatedRoleIds.add(roleId);
		const updatedMemberData = {
			...targetMember.toRow(),
			role_ids: updatedRoleIds,
			temporary: false,
		};
		await this.guildRepository.upsertMember(updatedMemberData);
		if (targetMember.isTemporary) {
			await this.gatewayService.removeTemporaryGuild({userId: targetId, guildId});
		}
	}

	constructor(
		private readonly guildRepository: GuildRepository,
		private readonly gatewayService: IGatewayService,
		private readonly userRepository: UserRepository,
		private readonly validationService: GuildMemberValidationService,
	) {}

	async systemAddMemberRole(params: {targetId: UserID; guildId: GuildID; roleId: RoleID}): Promise<void> {
		const {targetId, guildId, roleId} = params;
		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();
		await this.applyAddMemberRole(targetMember, guildId, targetId, roleId);
	}

	async systemRemoveMemberRole(params: {targetId: UserID; guildId: GuildID; roleId: RoleID}): Promise<void> {
		const {targetId, guildId, roleId} = params;
		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();
		if (!targetMember.roleIds.has(roleId)) return;
		const updatedRoleIds = new Set(targetMember.roleIds);
		updatedRoleIds.delete(roleId);
		await this.guildRepository.upsertMember({...targetMember.toRow(), role_ids: updatedRoleIds});
	}

	async addMemberRole(params: {userId: UserID; targetId: UserID; guildId: GuildID; roleId: RoleID}): Promise<void> {
		const {userId, targetId, guildId, roleId} = params;
		const {guildData, hasPermission, canManageRoles} = await getGuildAuth(this.gatewayService, this.userRepository, {
			userId,
			guildId,
		});
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
		await this.applyAddMemberRole(targetMember, guildId, targetId, roleId);
	}

	async removeMemberRole(params: {userId: UserID; targetId: UserID; guildId: GuildID; roleId: RoleID}): Promise<void> {
		const {userId, targetId, guildId, roleId} = params;
		const {guildData, hasPermission, canManageRoles} = await getGuildAuth(this.gatewayService, this.userRepository, {
			userId,
			guildId,
		});
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
