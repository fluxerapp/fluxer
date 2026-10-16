// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, InviteCode, RoleID, UserID} from '@app/api/BrandedTypes';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {GuildMemberAuditService} from '@app/api/guild/services/member/GuildMemberAuditService';
import {GuildMemberEventService} from '@app/api/guild/services/member/GuildMemberEventService';
import {GuildMemberOperationsService} from '@app/api/guild/services/member/GuildMemberOperationsService';
import {GuildMemberRoleService} from '@app/api/guild/services/member/GuildMemberRoleService';
import {GuildMemberSearchIndexService} from '@app/api/guild/services/member/GuildMemberSearchIndexService';
import {GuildMemberValidationService} from '@app/api/guild/services/member/GuildMemberValidationService';
import type {EntityAssetService} from '@app/api/infrastructure/EntityAssetService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Guild} from '@app/api/models/Guild';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {JoinSourceType} from '@fluxer/constants/src/GuildConstants';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildMemberUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

export class GuildMemberService {
	private async applyRoleChange(params: {
		actorId: UserID;
		targetId: UserID;
		guildId: GuildID;
		roleId: RoleID;
		requestCache: RequestCache;
		auditLogReason?: string | null;
		mutate: () => Promise<void>;
	}): Promise<void> {
		const {actorId: userId, targetId, guildId, roleId, requestCache, auditLogReason, mutate} = params;
		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();
		const previousSnapshot = this.auditService.serializeMemberForAudit(targetMember);
		const role = await this.guildRepository.getRole(roleId, guildId);
		await mutate();
		const updatedMember = await this.guildRepository.getMember(guildId, targetId);
		if (updatedMember) {
			await this.eventService.dispatchGuildMemberUpdate({guildId, member: updatedMember, requestCache});
			await this.refreshMemberSearchIndex(updatedMember);
			await this.guildAuditLogService.record({
				guildId,
				userId,
				action: AuditLogActionType.MEMBER_ROLE_UPDATE,
				targetId: targetId,
				auditLogReason: auditLogReason ?? null,
				metadata: role ? {role_name: role.name} : undefined,
				changes: this.guildAuditLogService.computeChanges(
					previousSnapshot,
					this.auditService.serializeMemberForAudit(updatedMember),
				),
			});
		}
	}

	private async refreshMemberSearchIndex(member: GuildMember): Promise<void> {
		const user = await this.userRepository.findUnique(member.userId);
		if (!user) return;
		const guild = await this.guildRepository.findUnique(member.guildId);
		const options = this.getMemberSearchIndexOptions(guild);
		if (options) void this.searchIndexService.updateMember(member, user, options);
	}

	private readonly validationService: GuildMemberValidationService;
	private readonly auditService: GuildMemberAuditService;
	private readonly eventService: GuildMemberEventService;
	private readonly operationsService: GuildMemberOperationsService;
	private readonly roleService: GuildMemberRoleService;
	private readonly searchIndexService: GuildMemberSearchIndexService;
	private readonly userRepository: UserRepository;

	constructor(
		private readonly guildRepository: GuildRepository,
		channelService: ChannelService,
		userCacheService: UserCacheService,
		gatewayService: IGatewayService,
		entityAssetService: EntityAssetService,
		userRepository: UserRepository,
		rateLimitService: IRateLimitService,
		private readonly guildAuditLogService: GuildAuditLogService,
		limitConfigService: LimitConfigService,
	) {
		this.userRepository = userRepository;
		this.validationService = new GuildMemberValidationService(guildRepository);
		this.auditService = new GuildMemberAuditService();
		this.eventService = new GuildMemberEventService(gatewayService, userCacheService);
		this.searchIndexService = new GuildMemberSearchIndexService();
		this.operationsService = new GuildMemberOperationsService(
			guildRepository,
			channelService,
			userCacheService,
			gatewayService,
			entityAssetService,
			userRepository,
			rateLimitService,
			this.validationService,
			this.guildAuditLogService,
			limitConfigService,
			this.searchIndexService,
		);
		this.roleService = new GuildMemberRoleService(
			guildRepository,
			gatewayService,
			userRepository,
			this.validationService,
		);
	}

	private getMemberSearchIndexOptions(guild: Guild | null | undefined) {
		if (!guild) {
			return null;
		}
		const includeDefault = guild.membersIndexedAt != null;
		return includeDefault ? {includeDefault} : null;
	}

	async getMembers(params: {
		userId: UserID;
		guildId: GuildID;
		limit?: number;
		after?: UserID;
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
		await this.refreshMemberSearchIndex(updatedMember);
		await this.guildAuditLogService.record({
			guildId,
			userId,
			action: AuditLogActionType.MEMBER_UPDATE,
			targetId: targetId,
			auditLogReason:
				auditLogReason ??
				(data.communication_disabled_until !== undefined ? data.timeout_reason?.trim() || null : null),
			changes: this.guildAuditLogService.computeChanges(
				previousSnapshot,
				this.auditService.serializeMemberForAudit(updatedMember),
			),
		});
		return result;
	}

	async addMemberRole(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
			roleId: RoleID;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		await this.applyRoleChange({
			...params,
			actorId: params.userId,
			auditLogReason: auditLogReason,
			mutate: () => this.roleService.addMemberRole(params),
		});
	}

	async systemAddMemberRole(params: {
		targetId: UserID;
		guildId: GuildID;
		roleId: RoleID;
		initiatorId: UserID;
		requestCache: RequestCache;
	}): Promise<void> {
		await this.applyRoleChange({
			...params,
			actorId: params.initiatorId,
			auditLogReason: null,
			mutate: () => this.roleService.systemAddMemberRole(params),
		});
	}

	async removeMemberRole(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
			roleId: RoleID;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		await this.applyRoleChange({
			...params,
			actorId: params.userId,
			auditLogReason: auditLogReason,
			mutate: () => this.roleService.removeMemberRole(params),
		});
	}

	async removeMember(
		params: {
			userId: UserID;
			targetId: UserID;
			guildId: GuildID;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, targetId, guildId} = params;
		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();
		await this.operationsService.removeMember(params);
		const guild = await this.guildRepository.findUnique(guildId);
		const searchIndexOptions = this.getMemberSearchIndexOptions(guild);
		if (searchIndexOptions) {
			void this.searchIndexService.deleteMember(guildId, targetId, searchIndexOptions);
		}
		await this.eventService.dispatchGuildMemberRemove({guildId, userId: targetId});
		await this.guildAuditLogService.record({
			guildId,
			userId,
			action: AuditLogActionType.MEMBER_KICK,
			changes: null,
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
		skipAccountLimitCheck?: boolean;
		isTemporary?: boolean;
		joinSourceType?: JoinSourceType;
		sourceInviteCode?: InviteCode;
		inviterId?: UserID;
		requestCache: RequestCache;
		initiatorId?: UserID;
	}): Promise<GuildMember> {
		return this.operationsService.addUserToGuild(params, this.eventService);
	}

	async leaveGuild(
		params: {
			userId: UserID;
			guildId: GuildID;
		},
		_auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId} = params;
		const member = await this.guildRepository.getMember(guildId, userId);
		if (!member) throw new UnknownGuildMemberError();
		await this.operationsService.leaveGuild(params);
		const guild = await this.guildRepository.findUnique(guildId);
		const searchIndexOptions = this.getMemberSearchIndexOptions(guild);
		if (searchIndexOptions) {
			void this.searchIndexService.deleteMember(guildId, userId, searchIndexOptions);
		}
		await this.eventService.dispatchGuildMemberRemove({guildId, userId});
	}
}
