// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createGuildID, createUserID, type UserID} from '@app/api/BrandedTypes';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import type {
	BanGuildMemberRequest,
	ForceAddUserToGuildRequest,
	KickGuildMemberRequest,
	UnbanGuildMemberRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import type {SuccessResponse} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

interface AdminGuildMembershipServiceDeps {
	userRepository: UserRepository;
	guildService: GuildService;
	auditService: AdminAuditService;
}

export class AdminGuildMembershipService {
	constructor(private readonly deps: AdminGuildMembershipServiceDeps) {}

	async forceAddUserToGuild({
		data,
		requestCache,
		adminUserId,
		auditLogReason,
		sendJoinMessage = true,
	}: {
		data: ForceAddUserToGuildRequest;
		requestCache: RequestCache;
		adminUserId: UserID;
		auditLogReason: string | null;
		sendJoinMessage?: boolean;
	}): Promise<SuccessResponse> {
		const {userRepository, guildService, auditService} = this.deps;
		const userId = createUserID(data.user_id);
		const guildId = createGuildID(data.guild_id);
		await userRepository.findUniqueAssert(userId);
		await guildService.members.addUserToGuild({
			userId,
			guildId,
			sendJoinMessage,
			skipBanCheck: true,
			skipAccountLimitCheck: true,
			joinSourceType: JoinSourceTypes.ADMIN_FORCE_ADD,
			requestCache,
			initiatorId: adminUserId,
		});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'user',
			targetId: BigInt(userId),
			action: 'force_add_to_guild',
			auditLogReason,
			metadata: new Map([['guild_id', String(guildId)]]),
		});
		return {success: true};
	}

	async banMember(data: BanGuildMemberRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildService, auditService} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const targetId = createUserID(data.user_id);
		await guildService.moderation.banMemberAsSystem({
			guildId,
			targetId,
			deleteMessageDays: data.delete_message_days,
			deleteMessageSeconds: data.delete_message_seconds,
			reason: data.reason,
			banDurationSeconds: data.ban_duration_seconds,
		});
		const metadata = new Map([
			['guild_id', guildId.toString()],
			['user_id', targetId.toString()],
			['delete_message_days', data.delete_message_days.toString()],
			['acl', AdminACLs.GUILD_BAN_MEMBER],
		]);
		if (data.reason) {
			metadata.set('reason', data.reason);
		}
		if (data.ban_duration_seconds != null) {
			metadata.set('ban_duration_seconds', data.ban_duration_seconds.toString());
		}
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild_member',
			targetId,
			action: 'ban_member',
			auditLogReason,
			metadata,
		});
	}

	async unbanMember(data: UnbanGuildMemberRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildService, auditService} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const targetId = createUserID(data.user_id);
		await guildService.moderation.unbanMemberAsSystem({guildId, targetId});
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild_member',
			targetId,
			action: 'unban_member',
			auditLogReason,
			metadata: new Map([
				['guild_id', guildId.toString()],
				['user_id', targetId.toString()],
				['acl', AdminACLs.GUILD_BAN_MEMBER],
			]),
		});
	}

	async kickMember(data: KickGuildMemberRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildService, auditService} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const targetId = createUserID(data.user_id);
		await guildService.members.removeMemberAsSystem({targetId, guildId});
		const metadata = new Map([
			['guild_id', guildId.toString()],
			['user_id', targetId.toString()],
			['acl', AdminACLs.GUILD_KICK_MEMBER],
		]);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild_member',
			targetId,
			action: 'kick_member',
			auditLogReason,
			metadata,
		});
	}
}
