// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, RoleID, UserID} from '@app/api/BrandedTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {getGuildAuth} from '@app/api/guild/services/GuildMfaEnforcement';
import {
	type GuildMemberRoleWriteLocks,
	withGuildMemberRoleWriteLock,
} from '@app/api/guild/services/member/GuildMemberRoleWriteLock';
import type {GuildMemberValidationService} from '@app/api/guild/services/member/GuildMemberValidationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';

export class GuildMemberRoleService {
	private async applyAddMemberRole(guildId: GuildID, targetId: UserID, roleId: RoleID): Promise<void> {
		const wasTemporary = await withGuildMemberRoleWriteLock(this.cacheService, guildId, targetId, async () => {
			const currentMember = await this.guildRepository.getMember(guildId, targetId);
			if (!currentMember) throw new UnknownGuildMemberError();
			if (currentMember.roleIds.has(roleId)) return false;
			const updatedRoleIds = new Set(currentMember.roleIds);
			updatedRoleIds.add(roleId);
			const previousRow = currentMember.toRow();
			await this.guildRepository.upsertMember(
				{...previousRow, role_ids: updatedRoleIds, temporary: false},
				previousRow,
			);
			return currentMember.isTemporary;
		});
		if (wasTemporary) {
			await this.gatewayService.removeTemporaryGuild({userId: targetId, guildId});
		}
	}

	private async applyRemoveMemberRole(guildId: GuildID, targetId: UserID, roleId: RoleID): Promise<void> {
		await withGuildMemberRoleWriteLock(this.cacheService, guildId, targetId, async () => {
			const currentMember = await this.guildRepository.getMember(guildId, targetId);
			if (!currentMember) throw new UnknownGuildMemberError();
			if (!currentMember.roleIds.has(roleId)) return;
			const updatedRoleIds = new Set(currentMember.roleIds);
			updatedRoleIds.delete(roleId);
			const previousRow = currentMember.toRow();
			await this.guildRepository.upsertMember({...previousRow, role_ids: updatedRoleIds}, previousRow);
		});
	}

	constructor(
		private readonly guildRepository: GuildRepository,
		private readonly gatewayService: IGatewayService,
		private readonly userRepository: UserRepository,
		private readonly validationService: GuildMemberValidationService,
		private readonly cacheService: GuildMemberRoleWriteLocks,
	) {}

	async systemAddMemberRole(params: {targetId: UserID; guildId: GuildID; roleId: RoleID}): Promise<void> {
		await this.applyAddMemberRole(params.guildId, params.targetId, params.roleId);
	}

	async systemRemoveMemberRole(params: {targetId: UserID; guildId: GuildID; roleId: RoleID}): Promise<void> {
		await this.applyRemoveMemberRole(params.guildId, params.targetId, params.roleId);
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
		await this.applyAddMemberRole(guildId, targetId, roleId);
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
		await this.applyRemoveMemberRole(guildId, targetId, roleId);
	}
}
