// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createGuildID, createUserID, type UserID} from '@app/api/BrandedTypes';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	BanGuildMemberRequest,
	ForceAddUserToGuildRequest,
	KickGuildMemberRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import type {SuccessResponse} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

interface AdminGuildMembershipServiceDeps {
	userRepository: IUserRepository;
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
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		await guildService.members.addUserToGuild({
			skipRiskGate: true,
			userId,
			guildId,
			sendJoinMessage,
			skipBanCheck: true,
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
		await guildService.moderation.banMember(
			{
				userId: adminUserId,
				guildId,
				targetId,
				deleteMessageDays: data.delete_message_days,
				deleteMessageSeconds: data.delete_message_seconds,
				reason: data.reason ?? undefined,
				banDurationSeconds: data.ban_duration_seconds ?? undefined,
				skipGuildAuditLog: true,
			},
			auditLogReason,
		);
		const metadata = new Map([
			['guild_id', guildId.toString()],
			['user_id', targetId.toString()],
			['delete_message_days', data.delete_message_days.toString()],
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

	async kickMember(data: KickGuildMemberRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {guildService, auditService} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const targetId = createUserID(data.user_id);
		await guildService.members.removeMember(
			{
				userId: adminUserId,
				targetId,
				guildId,
			},
			auditLogReason,
		);
		const metadata = new Map([
			['guild_id', guildId.toString()],
			['user_id', targetId.toString()],
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
