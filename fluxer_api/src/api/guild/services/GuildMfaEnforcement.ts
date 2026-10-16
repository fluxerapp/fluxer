// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, RoleID, UserID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import {MfaNotEnabledError} from '@fluxer/errors/src/domains/auth/MfaNotEnabledError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

const ELEVATED_MFA_PERMISSIONS =
	Permissions.KICK_MEMBERS |
	Permissions.BAN_MEMBERS |
	Permissions.ADMINISTRATOR |
	Permissions.MANAGE_CHANNELS |
	Permissions.MANAGE_GUILD |
	Permissions.MANAGE_MESSAGES |
	Permissions.MANAGE_ROLES |
	Permissions.MANAGE_WEBHOOKS |
	Permissions.MODERATE_MEMBERS |
	ThreadPermissionFlags.MANAGE_THREADS;

export async function createGuildMfaEnforcer(params: {
	userRepository: UserRepository;
	guildData: Pick<GuildResponse, 'mfa_level' | 'owner_id'>;
	userId: UserID;
}): Promise<(permission: bigint) => void> {
	const {userRepository, guildData, userId} = params;
	const requiresGuildMfa = guildData.mfa_level === GuildMFALevel.ELEVATED && guildData.owner_id !== userId.toString();
	let actorLacksMfa = false;
	if (requiresGuildMfa) {
		const actor = await userRepository.findUnique(userId);
		actorLacksMfa = !actor || actor.authenticatorTypes.size === 0;
	}
	return (permission: bigint) => {
		if (requiresGuildMfa && actorLacksMfa && (permission & ELEVATED_MFA_PERMISSIONS) !== 0n) {
			throw new MfaNotEnabledError();
		}
	};
}

export async function getGuildAuth(
	gatewayService: IGatewayService,
	userRepository: UserRepository,
	params: {userId: UserID; guildId: GuildID; guildData?: GuildResponse},
) {
	const {userId, guildId} = params;

	const guildData = params.guildData ?? (await gatewayService.getGuildData({guildId, userId}));
	if (!guildData) throw new MissingAccessError();
	const enforceGuildMfa = await createGuildMfaEnforcer({userRepository: userRepository, guildData, userId});
	const checkPermission = async (permission: bigint) => {
		const hasPermission = await gatewayService.checkPermission({guildId, userId, permission});
		if (!hasPermission) throw new MissingPermissionsError();
		enforceGuildMfa(permission);
	};
	const checkTargetMember = async (targetUserId: UserID) => {
		const canManage = await gatewayService.checkTargetMember({guildId, userId, targetUserId});
		if (!canManage) throw new MissingPermissionsError();
	};
	const getMyPermissions = async () => gatewayService.getUserPermissions({guildId, userId});
	const hasPermission = async (permission: bigint) => {
		const allowed = await gatewayService.checkPermission({guildId, userId, permission});
		if (allowed) enforceGuildMfa(permission);
		return allowed;
	};
	const canManageRoles = async (targetUserId: UserID, targetRoleId: RoleID) =>
		gatewayService.canManageRoles({guildId, userId, targetUserId, roleId: targetRoleId});
	return {
		enforceGuildMfa,
		guildData,
		checkPermission,
		checkTargetMember,
		getMyPermissions,
		hasPermission,
		canManageRoles,
	};
}

export async function requireGuildPermission(
	gatewayService: IGatewayService,
	userRepository: UserRepository,
	params: {guildId: GuildID; userId: UserID; permission: bigint; channelId?: ChannelID},
): Promise<void> {
	const allowed = await gatewayService.checkPermission(params);
	if (!allowed) throw new MissingPermissionsError();
	const guildData = await gatewayService.getGuildData({guildId: params.guildId, userId: params.userId});
	const enforceGuildMfa = await createGuildMfaEnforcer({userRepository, guildData, userId: params.userId});
	enforceGuildMfa(params.permission);
}
