// SPDX-License-Identifier: AGPL-3.0-or-later

import {JoinSourceTypes} from '@fluxer/constants/src/GuildConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	BanGuildMemberRequest,
	BulkAddGuildMembersRequest,
	ForceAddUserToGuildRequest,
	KickGuildMemberRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {createGuildID, createUserID, type UserID} from '../../../BrandedTypes';
import type {GuildService} from '../../../guild/services/GuildService';
import {createRequestCache, type RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {IUserRepository} from '../../../user/IUserRepository';
import type {AdminAuditService} from '../AdminAuditService';
import {BulkCancelledError, type BulkProgressHelpers} from '../BulkProgressHelpers';

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
	}: {
		data: ForceAddUserToGuildRequest;
		requestCache: RequestCache;
		adminUserId: UserID;
		auditLogReason: string | null;
	}) {
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
			sendJoinMessage: true,
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

	async bulkAddGuildMembers(
		data: BulkAddGuildMembersRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
		helpers?: BulkProgressHelpers,
	) {
		const {guildService, auditService} = this.deps;
		const successful: Array<string> = [];
		const failed: Array<{
			id: string;
			error: string;
		}> = [];
		const guildId = createGuildID(data.guild_id);
		const total = data.user_ids.length;
		await helpers?.reportProgress(0, total, `Adding ${total} members to guild ${guildId}`);
		let processed = 0;
		for (const userIdBigInt of data.user_ids) {
			if (helpers && (await helpers.shouldCancel())) throw new BulkCancelledError();
			try {
				const userId = createUserID(userIdBigInt);
				await guildService.members.addUserToGuild({
					skipRiskGate: true,
					userId,
					guildId,
					sendJoinMessage: false,
					skipBanCheck: true,
					joinSourceType: JoinSourceTypes.ADMIN_FORCE_ADD,
					requestCache: createRequestCache(),
					initiatorId: adminUserId,
				});
				successful.push(userId.toString());
			} catch (error) {
				failed.push({
					id: userIdBigInt.toString(),
					error: error instanceof Error ? error.message : 'Unknown error',
				});
			}
			processed++;
			if (helpers && processed % 25 === 0) {
				await helpers.reportProgress(processed, total, null);
			}
		}
		await helpers?.reportProgress(total, total, `+${successful.length} ok, ${failed.length} failed`);
		await auditService.createAuditLog({
			adminUserId,
			targetType: 'guild',
			targetId: BigInt(guildId),
			action: 'bulk_add_guild_members',
			auditLogReason,
			metadata: new Map([
				['guild_id', guildId.toString()],
				['user_count', data.user_ids.length.toString()],
			]),
		});
		return {
			successful,
			failed,
		};
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
