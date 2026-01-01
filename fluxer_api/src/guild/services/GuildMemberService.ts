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

import type {GuildID, RoleID, UserID} from '~/BrandedTypes';
import type {ChannelService} from '~/channel/services/ChannelService';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {UnknownGuildMemberError} from '~/Errors';
import type {GuildMemberResponse, GuildMemberUpdateRequest} from '~/guild/GuildModel';
import type {EntityAssetService} from '~/infrastructure/EntityAssetService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {GuildMember} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import type {GuildAuditLogService} from '../GuildAuditLogService';
import type {IGuildRepository} from '../IGuildRepository';
import {GuildMemberAuditService} from './member/GuildMemberAuditService';
import {GuildMemberAuthService} from './member/GuildMemberAuthService';
import {GuildMemberEventService} from './member/GuildMemberEventService';
import {GuildMemberOperationsService} from './member/GuildMemberOperationsService';
import {GuildMemberRoleService} from './member/GuildMemberRoleService';
import {GuildMemberValidationService} from './member/GuildMemberValidationService';

export class GuildMemberService {
	private readonly authService: GuildMemberAuthService;
	private readonly validationService: GuildMemberValidationService;
	private readonly auditService: GuildMemberAuditService;
	private readonly eventService: GuildMemberEventService;
	private readonly operationsService: GuildMemberOperationsService;
	private readonly roleService: GuildMemberRoleService;

	constructor(
		private readonly guildRepository: IGuildRepository,
		channelService: ChannelService,
		userCacheService: UserCacheService,
		gatewayService: IGatewayService,
		entityAssetService: EntityAssetService,
		userRepository: IUserRepository,
		rateLimitService: IRateLimitService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {
		this.authService = new GuildMemberAuthService(gatewayService);
		this.validationService = new GuildMemberValidationService(guildRepository, userRepository);
		this.auditService = new GuildMemberAuditService(guildAuditLogService);
		this.eventService = new GuildMemberEventService(gatewayService, userCacheService);
		this.operationsService = new GuildMemberOperationsService(
			guildRepository,
			channelService,
			userCacheService,
			gatewayService,
			entityAssetService,
			userRepository,
			rateLimitService,
			this.authService,
			this.validationService,
			this.guildAuditLogService,
		);
		this.roleService = new GuildMemberRoleService(
			guildRepository,
			gatewayService,
			this.authService,
			this.validationService,
		);
	}

	async getMembers(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildMemberResponse>> {
		return this.operationsService.getMembers(params);
	}

	async getMember(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<GuildMemberResponse> {
		return this.operationsService.getMember(params);
	}

	async updateMember(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
			data: GuildMemberUpdateRequest | Omit<GuildMemberUpdateRequest, 'roles'>;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<GuildMemberResponse> {
		const {userId, targetId, guildId, data, requestCache} = params;

		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();

		const previousSnapshot = this.auditService.serializeMemberForAudit(targetMember);

		const result = await this.operationsService.updateMember({
			userId,
			targetId,
			guildId,
			data,
			requestCache,
			auditLogReason,
		});

		const updatedMember = await this.guildRepository.getMember(guildId, targetId);
		if (!updatedMember) throw new UnknownGuildMemberError();

		await this.eventService.dispatchGuildMemberUpdate({guildId, member: updatedMember, requestCache});
		const timeoutMetadata = (() => {
			if (data.communication_disabled_until === undefined) {
				return undefined;
			}
			const metadata: Record<string, string> = {};
			if (data.communication_disabled_until !== null) {
				metadata.communication_disabled_until = data.communication_disabled_until;
			}
			const trimmedReason = data.timeout_reason?.trim();
			if (trimmedReason) {
				metadata.timeout_reason = trimmedReason;
			}
			return Object.keys(metadata).length > 0 ? metadata : undefined;
		})();
		await this.auditService.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.MEMBER_UPDATE,
			targetId: targetId,
			auditLogReason: auditLogReason ?? null,
			metadata: timeoutMetadata,
			changes: this.guildAuditLogService.computeChanges(
				previousSnapshot,
				this.auditService.serializeMemberForAudit(updatedMember),
			),
		});

		return result;
	}

	async addMemberRole(
		params: {userId: UserID; targetId: UserID; guildId: GuildID; roleId: RoleID; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, targetId, guildId, roleId, requestCache} = params;

		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();

		const previousSnapshot = this.auditService.serializeMemberForAudit(targetMember);

		await this.roleService.addMemberRole(params);

		const updatedMember = await this.guildRepository.getMember(guildId, targetId);
		if (updatedMember) {
			await this.eventService.dispatchGuildMemberUpdate({guildId, member: updatedMember, requestCache});
			await this.auditService.recordAuditLog({
				guildId,
				userId,
				action: AuditLogActionType.MEMBER_ROLE_UPDATE,
				targetId: targetId,
				auditLogReason: auditLogReason ?? null,
				metadata: {role_id: roleId.toString(), action: 'add'},
				changes: this.guildAuditLogService.computeChanges(
					previousSnapshot,
					this.auditService.serializeMemberForAudit(updatedMember),
				),
			});
		}
	}

	async removeMemberRole(
		params: {userId: UserID; targetId: UserID; guildId: GuildID; roleId: RoleID; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, targetId, guildId, roleId, requestCache} = params;

		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();

		const previousSnapshot = this.auditService.serializeMemberForAudit(targetMember);

		await this.roleService.removeMemberRole(params);

		const updatedMember = await this.guildRepository.getMember(guildId, targetId);
		if (updatedMember) {
			await this.eventService.dispatchGuildMemberUpdate({guildId, member: updatedMember, requestCache});
			await this.auditService.recordAuditLog({
				guildId,
				userId,
				action: AuditLogActionType.MEMBER_ROLE_UPDATE,
				targetId: targetId,
				auditLogReason: auditLogReason ?? null,
				metadata: {role_id: roleId.toString(), action: 'remove'},
				changes: this.guildAuditLogService.computeChanges(
					previousSnapshot,
					this.auditService.serializeMemberForAudit(updatedMember),
				),
			});
		}
	}

	async removeMember(
		params: {userId: UserID; targetId: UserID; guildId: GuildID},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, targetId, guildId} = params;

		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();

		await this.operationsService.removeMember(params);

		await this.eventService.dispatchGuildMemberRemove({guildId, userId: targetId});
		await this.auditService.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.MEMBER_KICK,
			targetId: targetId,
			auditLogReason: auditLogReason ?? null,
		});
	}

	async addUserToGuild(params: {
		userId: UserID;
		guildId: GuildID;
		sendJoinMessage?: boolean;
		skipGuildLimitCheck?: boolean;
		skipBanCheck?: boolean;
		isTemporary?: boolean;
		joinSourceType?: number;
		requestCache: RequestCache;
		initiatorId?: UserID;
	}): Promise<GuildMember> {
		return this.operationsService.addUserToGuild(params, this.eventService);
	}

	async leaveGuild(params: {userId: UserID; guildId: GuildID}, _auditLogReason?: string | null): Promise<void> {
		const {userId, guildId} = params;
		const member = await this.guildRepository.getMember(guildId, userId);
		if (!member) throw new UnknownGuildMemberError();

		await this.operationsService.leaveGuild(params);
		await this.eventService.dispatchGuildMemberRemove({guildId, userId});
	}
}
