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
import {evaluateSelfAssignableRolePick} from '@fluxer/constants/src/SelfAssignableRoleRules';
import {MaxSelfAssignedRolesError} from '@fluxer/errors/src/domains/guild/MaxSelfAssignedRolesError';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';

export interface PickedRoleSection {
	readonly roleIds: ReadonlyArray<RoleID>;
	readonly maxRoles: number | null;
}

export class GuildMemberRoleService {
	private async applyAddMemberRole(
		guildId: GuildID,
		targetId: UserID,
		roleId: RoleID,
		options: {keepTemporary?: boolean; section?: PickedRoleSection} = {},
	): Promise<void> {
		const keepTemporary = options.keepTemporary === true;
		const endedTemporary = await withGuildMemberRoleWriteLock(this.cacheService, guildId, targetId, async () => {
			const currentMember = await this.guildRepository.getMember(guildId, targetId);
			if (!currentMember) throw new UnknownGuildMemberError();
			const pick = options.section
				? evaluateSelfAssignableRolePick({
						sectionRoleIds: options.section.roleIds,
						maxRoles: options.section.maxRoles,
						roleId,
						hasRole: (id) => currentMember.roleIds.has(id),
					})
				: null;
			if (pick && !pick.allowed) throw new MaxSelfAssignedRolesError(pick.maxRoles);
			const replaced = pick?.replacedRoleIds ?? [];
			const endsTemporary = !keepTemporary && currentMember.isTemporary;
			if (currentMember.roleIds.has(roleId) && replaced.length === 0 && !endsTemporary) return false;
			const updatedRoleIds = new Set(currentMember.roleIds);
			for (const id of replaced) updatedRoleIds.delete(id);
			updatedRoleIds.add(roleId);
			const previousRow = currentMember.toRow();
			await this.guildRepository.upsertMember(
				{...previousRow, role_ids: updatedRoleIds, temporary: keepTemporary ? previousRow.temporary : false},
				previousRow,
			);
			return endsTemporary;
		});
		if (endedTemporary) {
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

	async systemAddMemberRole(
		params: {targetId: UserID; guildId: GuildID; roleId: RoleID},
		options: {keepTemporary?: boolean; section?: PickedRoleSection} = {},
	): Promise<void> {
		await this.applyAddMemberRole(params.guildId, params.targetId, params.roleId, options);
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
