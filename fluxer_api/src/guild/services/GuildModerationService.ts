/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {GuildID, UserID} from '~/BrandedTypes';
import {Permissions} from '~/Constants';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {BannedFromGuildError, InputValidationError, IpBannedFromGuildError, MissingPermissionsError} from '~/Errors';
import type {GuildBanResponse} from '~/guild/GuildModel';
import {mapGuildBansToResponse} from '~/guild/GuildModel';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {Logger} from '~/Logger';
import type {GuildBan} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IWorkerService} from '~/worker/IWorkerService';
import type {GuildAuditLogChange, GuildAuditLogService} from '../GuildAuditLogService';
import type {IGuildRepository} from '../IGuildRepository';

export class GuildModerationService {
	constructor(
		private readonly guildRepository: IGuildRepository,
		private readonly userRepository: IUserRepository,
		private readonly gatewayService: IGatewayService,
		private readonly userCacheService: UserCacheService,
		private readonly workerService: IWorkerService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {}

	async banMember(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
			deleteMessageDays?: number;
			reason?: string | null;
			banDurationSeconds?: number;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, targetId, deleteMessageDays, reason, banDurationSeconds} = params;

		const hasPermission = await this.gatewayService.checkPermission({
			guildId,
			userId,
			permission: Permissions.BAN_MEMBERS,
		});
		if (!hasPermission) throw new MissingPermissionsError();

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

		const targetUser = await this.userRepository.findUnique(targetId);
		const targetIp = targetUser?.lastActiveIp || null;

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
		});

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
				const guildRow = guild.toRow();
				await this.guildRepository.upsert({
					...guildRow,
					member_count: Math.max(0, guild.memberCount - 1),
				});
			}

			await this.dispatchGuildMemberRemove({guildId, userId: targetId});
			await this.gatewayService.leaveGuild({userId: targetId, guildId});
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
		params: {userId: UserID; targetId: UserID; guildId: GuildID},
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
			throw InputValidationError.create('user_id', 'User is not banned');
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

		const bans = await this.guildRepository.listBans(guildId);
		const user = await this.userRepository.findUnique(userId);
		const userIp = user?.lastActiveIp;

		for (const ban of bans) {
			if (ban.userId === userId) {
				throw new BannedFromGuildError();
			}
			if (userIp && ban.ipAddress && ban.ipAddress === userIp) {
				throw new IpBannedFromGuildError();
			}
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
