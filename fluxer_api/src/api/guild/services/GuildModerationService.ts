// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {BannedFromGuildError} from '@fluxer/errors/src/domains/guild/BannedFromGuildError';
import {IpBannedFromGuildError} from '@fluxer/errors/src/domains/guild/IpBannedFromGuildError';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {isSameIpDecisionMatch} from '@fluxer/ip_utils/src/IpAddress';
import type {GuildBanResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {GuildID, UserID} from '../../BrandedTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {GuildBan} from '../../models/GuildBan';
import {hasHighCgnatBlastRadiusRisk, isSingleIpBanCandidate} from '../../risk/IpBanCgnatGuard';
import type {IUserRepository} from '../../user/IUserRepository';
import type {WorkerTaskName} from '../../worker/WorkerLaneConfig';
import type {GuildAuditLogService} from '../GuildAuditLogService';
import type {GuildAuditLogChange} from '../GuildAuditLogTypes';
import {mapGuildBansToResponse} from '../GuildModel';
import type {IGuildRepositoryAggregate} from '../repositories/IGuildRepositoryAggregate';
import {GuildMemberSearchIndexService} from './member/GuildMemberSearchIndexService';

export class GuildModerationService {
	private readonly searchIndexService: GuildMemberSearchIndexService;

	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly userRepository: IUserRepository,
		private readonly gatewayService: IGatewayService,
		private readonly userCacheService: UserCacheService,
		private readonly workerService: IWorkerService<WorkerTaskName>,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly ipInfoService: IpInfoService,
	) {
		this.searchIndexService = new GuildMemberSearchIndexService();
	}

	async banMember(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
			deleteMessageDays?: number;
			reason?: string | null;
			banDurationSeconds?: number;
			skipGuildAuditLog?: boolean;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, targetId, deleteMessageDays, reason, banDurationSeconds, skipGuildAuditLog} = params;
		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.BAN_MEMBERS,
		});
		if (!hasPermission) throw new MissingPermissionsError();
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
		if (deleteMessageDays && deleteMessageDays > 0) {
			await this.workerService.addJob('deleteUserMessagesInGuildByTime', {
				guildId: guildId.toString(),
				userId: targetId.toString(),
				days: deleteMessageDays,
			});
		}
		const targetIp = targetUser.lastActiveIp || null;
		const targetEmail = targetUser.email?.toLowerCase() || null;
		let expiresAt: Date | null = null;
		if (banDurationSeconds && banDurationSeconds > 0) {
			expiresAt = new Date(Date.now() + banDurationSeconds * 1000);
		}
		const ban = await this.guildRepository.upsertBan({
			guild_id: guildId,
			user_id: targetId,
			moderator_id: userId,
			banned_at: new Date(),
			expires_at: expiresAt,
			reason: reason || null,
			ip: targetIp,
			email: targetEmail,
		});
		if (!skipGuildAuditLog) {
			const metadata: Record<string, string> | undefined =
				deleteMessageDays !== undefined ? {delete_member_days: deleteMessageDays.toString()} : undefined;
			await this.recordAuditLog({
				guildId,
				userId,
				action: AuditLogActionType.MEMBER_BAN_ADD,
				targetId: targetId,
				auditLogReason: auditLogReason ?? null,
				metadata,
				changes: this.guildAuditLogService.computeChanges(null, this.serializeBanForAudit(ban)),
			});
		}
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
		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.BAN_MEMBERS,
		});
		if (!hasPermission) throw new MissingPermissionsError();
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
		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.BAN_MEMBERS,
		});
		if (!hasPermission) throw new MissingPermissionsError();
		const ban = await this.guildRepository.getBan(guildId, targetId);
		if (!ban) {
			throw InputValidationError.fromCode('user_id', ValidationErrorCodes.USER_IS_NOT_BANNED);
		}
		await this.guildRepository.deleteBan(guildId, targetId);
		await this.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.MEMBER_BAN_REMOVE,
			targetId: targetId,
			auditLogReason: auditLogReason ?? null,
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
		const {userId, guildId} = params;
		const [bans, user] = await Promise.all([
			this.guildRepository.listBans(guildId),
			this.userRepository.findUnique(userId),
		]);
		const userIp = user?.lastActiveIp;
		const userEmail = user?.email?.toLowerCase();
		for (const ban of bans) {
			if (ban.userId === userId) throw new BannedFromGuildError();
			if (isSameIpDecisionMatch(userIp, ban.ipAddress) && (await this.shouldEnforceIpBan(userIp, ban.ipAddress))) {
				throw new IpBannedFromGuildError();
			}
		}
		if (userEmail) {
			const emailBan = await this.guildRepository.getBanByEmail(guildId, userEmail);
			if (emailBan) throw new BannedFromGuildError();
		}
	}

	private async shouldEnforceIpBan(
		userIp: string | null | undefined,
		bannedIp: string | null | undefined,
	): Promise<boolean> {
		if (!userIp || !bannedIp || !isSingleIpBanCandidate(bannedIp)) {
			return true;
		}
		try {
			const highRisk = await hasHighCgnatBlastRadiusRisk(userIp, this.ipInfoService, {
				source: 'guild.ip_ban',
				reason: 'join_cgnat_guard',
			});
			if (highRisk) {
				Logger.warn(
					{userIp, bannedIp},
					'Skipping guild IP ban match because IPInfo indicates high CGNAT blast-radius risk',
				);
			}
			return !highRisk;
		} catch (error) {
			Logger.warn({error, userIp, bannedIp}, 'IPInfo CGNAT guard failed while checking guild IP ban');
			return true;
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

	private async recordAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		action: AuditLogActionType;
		targetId?: UserID | string | null;
		auditLogReason?: string | null;
		metadata?: Map<string, string> | Record<string, string>;
		changes?: GuildAuditLogChange | null;
		createdAt?: Date;
	}): Promise<void> {
		const targetId =
			params.targetId === undefined || params.targetId === null
				? null
				: typeof params.targetId === 'string'
					? params.targetId
					: params.targetId.toString();
		try {
			const builder = this.guildAuditLogService
				.createBuilder(params.guildId, params.userId)
				.withAction(params.action, targetId)
				.withReason(params.auditLogReason ?? null);
			if (params.metadata) {
				builder.withMetadata(params.metadata);
			}
			if (params.changes) {
				builder.withChanges(params.changes);
			}
			if (params.createdAt) {
				builder.withCreatedAt(params.createdAt);
			}
			await builder.commit();
		} catch (error) {
			Logger.error(
				{
					error,
					guildId: params.guildId.toString(),
					userId: params.userId.toString(),
					action: params.action,
					targetId,
				},
				'Failed to record guild audit log',
			);
		}
	}
}
