// SPDX-License-Identifier: AGPL-3.0-or-later

import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildID, RoleID, UserID} from '../../../BrandedTypes';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {IUserRepository} from '../../../user/IUserRepository';
import {createGuildMfaEnforcer} from '../GuildMfaEnforcement';

interface GuildAuth {
	guildData: GuildResponse;
	checkPermission: (permission: bigint) => Promise<void>;
	checkTargetMember: (targetUserId: UserID) => Promise<void>;
	getMyPermissions: () => Promise<bigint>;
	hasPermission: (permission: bigint) => Promise<boolean>;
	canManageRoles: (targetUserId: UserID, targetRoleId: RoleID) => Promise<boolean>;
}

export class GuildMemberAuthService {
	constructor(
		private readonly gatewayService: IGatewayService,
		private readonly userRepository: IUserRepository,
	) {}

	async getGuildAuthenticated({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildAuth> {
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		if (!guildData) throw new MissingAccessError();
		const enforceGuildMfa = await createGuildMfaEnforcer({userRepository: this.userRepository, guildData, userId});
		const checkPermission = async (permission: bigint) => {
			const hasPermission = await this.gatewayService.checkPermission({guildId, userId, permission});
			if (!hasPermission) throw new MissingPermissionsError();
			enforceGuildMfa(permission);
		};
		const checkTargetMember = async (targetUserId: UserID) => {
			const canManage = await this.gatewayService.checkTargetMember({guildId, userId, targetUserId});
			if (!canManage) throw new MissingPermissionsError();
		};
		const getMyPermissions = async () => this.gatewayService.getUserPermissions({guildId, userId});
		const hasPermission = async (permission: bigint) => {
			const allowed = await this.gatewayService.checkPermission({guildId, userId, permission});
			if (allowed) enforceGuildMfa(permission);
			return allowed;
		};
		const canManageRoles = async (targetUserId: UserID, targetRoleId: RoleID) =>
			this.gatewayService.canManageRoles({guildId, userId, targetUserId, roleId: targetRoleId});
		return {
			guildData,
			checkPermission,
			checkTargetMember,
			getMyPermissions,
			hasPermission,
			canManageRoles,
		};
	}
}
