// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {isIpBanExempt} from '@app/api/ban/IpBanExemptions';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {mapGuildBansToResponse} from '@app/api/guild/GuildModel';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {assertNotBannedFromGuild} from '@app/api/guild/services/GuildBanChecks';
import {requireGuildPermission} from '@app/api/guild/services/GuildMfaEnforcement';
import {GuildMemberSearchIndexService} from '@app/api/guild/services/member/GuildMemberSearchIndexService';
import type {BanBy} from '@app/api/infrastructure/activity/Contract.generated';
import {emitGuildMemberBanned, emitGuildMemberUnbanned} from '@app/api/infrastructure/activity/ModerationEvents';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Guild} from '@app/api/models/Guild';
import type {GuildBan} from '@app/api/models/GuildBan';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {GuildBanResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

const SECONDS_PER_DAY = 86_400;
const GUILD_MODERATION_PERMISSIONS =
	Permissions.ADMINISTRATOR | Permissions.BAN_MEMBERS | Permissions.KICK_MEMBERS | Permissions.MANAGE_GUILD;

interface GuildBanOptions {
	deleteMessageDays?: number;
	deleteMessageSeconds?: number;
	reason?: string | null;
	banDurationSeconds?: number;
}

export class GuildModerationService {
	private readonly searchIndexService: GuildMemberSearchIndexService;

	constructor(
		private readonly guildRepository: GuildRepository,
		private readonly userRepository: UserRepository,
		private readonly gatewayService: IGatewayService,
		private readonly userCacheService: UserCacheService,
		private readonly workerService: IWorkerService<WorkerTaskName>,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {
		this.searchIndexService = new GuildMemberSearchIndexService();
	}

	private async checkModerationPermission(params: {
		guildId: GuildID;
		userId: UserID;
		permission: bigint;
	}): Promise<void> {
		await requireGuildPermission(this.gatewayService, this.userRepository, params);
	}

	async banMember(
		params: GuildBanOptions & {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, targetId} = params;
		await this.checkModerationPermission({guildId, userId, permission: Permissions.BAN_MEMBERS});
		if (userId === targetId) throw new UnknownGuildMemberError();
		const targetUser = await this.userRepository.findUnique(targetId);
		if (!targetUser) {
			throw new UnknownUserError();
		}
		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (targetMember) {
			const canManage = await this.gatewayService.checkTargetMember({guildId, userId, targetUserId: targetId});
			if (!canManage) throw new MissingPermissionsError();
		}
		await this.applyBan({
			...params,
			actorId: userId,
			by: 'moderator',
			targetUser,
			targetMember,
			guild: await this.guildRepository.findUnique(guildId),
			guildAuditLogReason: auditLogReason ?? (params.reason || null),
		});
	}

	async banMemberAsSystem(params: GuildBanOptions & {targetId: UserID; guildId: GuildID}): Promise<void> {
		const {guildId, targetId} = params;
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) throw new UnknownGuildError();
		if (targetId === SYSTEM_USER_ID) throw new UnknownGuildMemberError();
		if (guild.ownerId === targetId) throw new MissingPermissionsError();
		const targetUser = await this.userRepository.findUnique(targetId);
		if (!targetUser) {
			throw new UnknownUserError();
		}
		await this.applyBan({
			...params,
			actorId: SYSTEM_USER_ID,
			by: 'staff',
			targetUser,
			targetMember: await this.guildRepository.getMember(guildId, targetId),
			guild,
			guildAuditLogReason: params.reason || null,
		});
	}

	private async applyBan(
		params: GuildBanOptions & {
			targetId: UserID;
			guildId: GuildID;
			actorId: UserID;
			by: BanBy;
			targetUser: User;
			targetMember: GuildMember | null;
			guild: Guild | null;
			guildAuditLogReason: string | null;
		},
	): Promise<void> {
		const {
			actorId,
			guildId,
			targetId,
			deleteMessageDays,
			deleteMessageSeconds,
			reason,
			banDurationSeconds,
			by,
			targetUser,
			targetMember,
			guild: guildBeforeBan,
			guildAuditLogReason,
		} = params;
		const effectiveDeleteMessageSeconds =
			deleteMessageSeconds ?? (deleteMessageDays !== undefined ? deleteMessageDays * SECONDS_PER_DAY : undefined);
		if (effectiveDeleteMessageSeconds && effectiveDeleteMessageSeconds > 0) {
			await this.workerService.addJob('deleteUserMessagesInGuildByTime', {
				guildId: guildId.toString(),
				userId: targetId.toString(),
				seconds: effectiveDeleteMessageSeconds,
			});
		}
		const targetIp = isIpBanExempt(targetUser.lastActiveIp) ? null : targetUser.lastActiveIp || null;
		const targetEmail = targetUser.email?.toLowerCase() || null;
		let expiresAt: Date | null = null;
		if (banDurationSeconds && banDurationSeconds > 0) {
			expiresAt = new Date(Date.now() + banDurationSeconds * 1000);
		}
		const targetModerator = await this.isGuildModerator(guildId, targetId, guildBeforeBan, targetMember !== null);
		const ban = await this.guildRepository.upsertBan({
			guild_id: guildId,
			user_id: targetId,
			moderator_id: actorId,
			banned_at: new Date(),
			expires_at: expiresAt,
			reason: reason || null,
			ip: targetIp,
			email: targetEmail,
		});
		const metadata: Record<string, string> | undefined =
			effectiveDeleteMessageSeconds && effectiveDeleteMessageSeconds > 0
				? {delete_message_seconds: String(effectiveDeleteMessageSeconds)}
				: undefined;
		await this.guildAuditLogService.record({
			guildId,
			userId: actorId,
			action: AuditLogActionType.MEMBER_BAN_ADD,
			targetId: targetId,
			auditLogReason: guildAuditLogReason,
			metadata,
			changes: this.guildAuditLogService.computeChanges(null, this.serializeBanForAudit(ban)),
		});
		await emitGuildMemberBanned({
			guildId,
			userId: targetId,
			moderatorId: actorId,
			by,
			memberCount: guildBeforeBan?.memberCount ?? 0,
			targetModerator,
			bannedAt: ban.bannedAt,
			expiresAt: ban.expiresAt,
		});
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_BAN_ADD',
			data: {
				guild_id: guildId.toString(),
				user: {id: targetId.toString()},
			},
		});
		if (targetMember) {
			await this.guildRepository.deleteMember(guildId, targetId);
			const guild = await this.guildRepository.findUnique(guildId);
			if (guild) {
				await this.guildRepository.upsertPartial(
					guildId,
					{member_count: Math.max(0, guild.memberCount - 1)},
					guild.toRow(),
				);
			}
			await this.dispatchGuildMemberRemove({guildId, userId: targetId});
			await this.gatewayService.leaveGuild({userId: targetId, guildId});
			const guildForSearch = await this.guildRepository.findUnique(guildId);
			if (guildForSearch) {
				const includeDefault = guildForSearch.membersIndexedAt != null;
				if (includeDefault) {
					void this.searchIndexService.deleteMember(guildId, targetId, {includeDefault});
				}
			}
		}
	}

	async listBans(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildBanResponse>> {
		const {userId, guildId, requestCache} = params;
		await this.checkModerationPermission({guildId, userId, permission: Permissions.BAN_MEMBERS});
		const bans = await this.guildRepository.listBans(guildId);
		return await mapGuildBansToResponse(bans, this.userCacheService, requestCache);
	}

	async unbanMember(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, targetId} = params;
		await this.checkModerationPermission({guildId, userId, permission: Permissions.BAN_MEMBERS});
		await this.removeBan({guildId, targetId, actorId: userId, by: 'moderator'}, auditLogReason ?? null);
	}

	async unbanMemberAsSystem(params: {targetId: UserID; guildId: GuildID}): Promise<void> {
		const guild = await this.guildRepository.findUnique(params.guildId);
		if (!guild) throw new UnknownGuildError();
		await this.removeBan({...params, actorId: SYSTEM_USER_ID, by: 'staff'}, null);
	}

	private async removeBan(
		{guildId, targetId, actorId, by}: {guildId: GuildID; targetId: UserID; actorId: UserID; by: BanBy},
		auditLogReason: string | null,
	): Promise<void> {
		const ban = await this.guildRepository.getBan(guildId, targetId);
		if (!ban) {
			throw InputValidationError.fromCode('user_id', ValidationErrorCodes.USER_IS_NOT_BANNED);
		}
		await this.guildRepository.deleteBan(guildId, targetId);
		await emitGuildMemberUnbanned({guildId, userId: targetId, moderatorId: actorId, by});
		await this.guildAuditLogService.record({
			guildId,
			userId: actorId,
			action: AuditLogActionType.MEMBER_BAN_REMOVE,
			targetId: targetId,
			auditLogReason,
			changes: this.guildAuditLogService.computeChanges(this.serializeBanForAudit(ban), null),
		});
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_BAN_REMOVE',
			data: {
				guild_id: guildId.toString(),
				user: {id: targetId.toString()},
			},
		});
	}

	async checkUserBanStatus(params: {userId: UserID; guildId: GuildID}): Promise<void> {
		return assertNotBannedFromGuild(this.guildRepository, this.userRepository, params);
	}

	private async isGuildModerator(
		guildId: GuildID,
		targetId: UserID,
		guild: Guild | null,
		isMember: boolean,
	): Promise<boolean> {
		if (guild?.ownerId === targetId) return true;
		if (!isMember) return false;
		try {
			const permissions = await this.gatewayService.getUserPermissions({guildId, userId: targetId});
			return (permissions & GUILD_MODERATION_PERMISSIONS) !== 0n;
		} catch (error) {
			Logger.debug({error, guildId: guildId.toString()}, 'Could not read target permissions for a guild ban');
			return false;
		}
	}

	private serializeBanForAudit(ban: GuildBan): Record<string, unknown> {
		return {
			user_id: ban.userId.toString(),
			moderator_id: ban.moderatorId.toString(),
			banned_at: ban.bannedAt.toISOString(),
			expires_at: ban.expiresAt ? ban.expiresAt.toISOString() : null,
			reason: ban.reason ?? null,
		};
	}

	private async dispatchGuildMemberRemove({guildId, userId}: {guildId: GuildID; userId: UserID}): Promise<void> {
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_MEMBER_REMOVE',
			data: {user: {id: userId.toString()}},
		});
	}
}
