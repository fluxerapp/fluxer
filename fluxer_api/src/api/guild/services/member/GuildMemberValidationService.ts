// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {BannedFromGuildError} from '@fluxer/errors/src/domains/guild/BannedFromGuildError';
import {IpBannedFromGuildError} from '@fluxer/errors/src/domains/guild/IpBannedFromGuildError';
import {UnknownGuildRoleError} from '@fluxer/errors/src/domains/guild/UnknownGuildRoleError';
import {isSameIpDecisionMatch} from '@fluxer/ip_utils/src/IpAddress';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import type {GuildID, RoleID, UserID} from '../../../BrandedTypes';
import {guildIdToRoleId} from '../../../BrandedTypes';
import {Logger} from '../../../Logger';
import type {GuildMember} from '../../../models/GuildMember';
import {hasHighCgnatBlastRadiusRisk, isSingleIpBanCandidate} from '../../../risk/IpBanCgnatGuard';
import {isIpBanExempt} from '../../../risk/IpBanExemptions';
import type {IUserRepository} from '../../../user/IUserRepository';
import type {IGuildRepositoryAggregate} from '../../repositories/IGuildRepositoryAggregate';

function ensureNotEveryoneRole(roleId: RoleID, guildId: GuildID, path: string): void {
	if (roleId === guildIdToRoleId(guildId)) {
		throw InputValidationError.fromCode(path, ValidationErrorCodes.INVALID_ROLE_ID);
	}
}

export class GuildMemberValidationService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly userRepository: IUserRepository,
		private readonly ipInfoService: IpInfoService,
	) {}

	async validateAndGetRoleIds(params: {
		userId: UserID;
		guildId: GuildID;
		guildData: GuildResponse;
		targetId: UserID;
		targetMember: GuildMember;
		newRoles: Array<RoleID>;
		hasPermission: (permission: bigint) => Promise<boolean>;
		canManageRoles: (targetUserId: UserID, targetRoleId: RoleID) => Promise<boolean>;
	}): Promise<Array<RoleID>> {
		const {userId, guildId, guildData, targetId, targetMember, newRoles, hasPermission, canManageRoles} = params;
		for (const roleId of newRoles) {
			ensureNotEveryoneRole(roleId, guildId, 'roles');
		}
		const existingRoles = await this.guildRepository.listRolesByIds(newRoles, guildId);
		const existingRoleIds = existingRoles.map((role) => role.id);
		if (guildData && guildData.owner_id === userId.toString()) {
			return existingRoleIds;
		}
		if (!(await hasPermission(Permissions.MANAGE_ROLES))) {
			throw new MissingPermissionsError();
		}
		const currentRoles = targetMember.roleIds;
		const rolesToRemove = [...currentRoles].filter((roleId) => !existingRoleIds.includes(roleId));
		const rolesToAdd = existingRoleIds.filter((roleId) => !currentRoles.has(roleId));
		const rolesToCheck = [...rolesToAdd, ...rolesToRemove].filter((roleId) => roleId !== guildIdToRoleId(guildId));
		const checks = await Promise.all(
			rolesToCheck.map((roleId) => canManageRoles(targetId, roleId).then((ok) => ({roleId, ok}))),
		);
		const denied = checks.find((check) => !check.ok);
		if (denied) {
			throw new MissingPermissionsError();
		}
		return existingRoleIds;
	}

	async validateRoleAssignment(params: {
		guildData: GuildResponse;
		guildId: GuildID;
		userId: UserID;
		targetId: UserID;
		roleId: RoleID;
		canManageRoles: (targetUserId: UserID, targetRoleId: RoleID) => Promise<boolean>;
	}): Promise<void> {
		const {guildData, guildId, userId, targetId, roleId, canManageRoles} = params;
		ensureNotEveryoneRole(roleId, guildId, 'role_id');
		if (guildData && guildData.owner_id === userId.toString()) {
			const role = await this.guildRepository.getRole(roleId, guildId);
			if (!role || role.id === guildIdToRoleId(guildId)) {
				throw new UnknownGuildRoleError();
			}
		} else {
			if (!(await canManageRoles(targetId, roleId))) {
				throw new MissingPermissionsError();
			}
		}
	}

	async checkUserBanStatus({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<void> {
		const bans = await this.guildRepository.listBans(guildId);
		const user = await this.userRepository.findUnique(userId);
		const userIp = user?.lastActiveIp;
		for (const ban of bans) {
			if (ban.userId === userId) {
				throw new BannedFromGuildError();
			}
			if (isSameIpDecisionMatch(userIp, ban.ipAddress) && (await this.shouldEnforceIpBan(userIp, ban.ipAddress))) {
				throw new IpBannedFromGuildError();
			}
		}
	}

	private async shouldEnforceIpBan(
		userIp: string | null | undefined,
		bannedIp: string | null | undefined,
	): Promise<boolean> {
		if (isIpBanExempt(userIp)) {
			return false;
		}
		if (!userIp || !bannedIp || !isSingleIpBanCandidate(bannedIp)) {
			return true;
		}
		try {
			const highRisk = await hasHighCgnatBlastRadiusRisk(userIp, this.ipInfoService, {
				source: 'guild.member_ip_ban',
				reason: 'join_cgnat_guard',
			});
			if (highRisk) {
				Logger.warn(
					{userIp, bannedIp},
					'Skipping guild member IP ban match because IPInfo indicates high CGNAT blast-radius risk',
				);
			}
			return !highRisk;
		} catch (error) {
			Logger.warn({error, userIp, bannedIp}, 'IPInfo CGNAT guard failed while checking guild member IP ban');
			return true;
		}
	}
}
