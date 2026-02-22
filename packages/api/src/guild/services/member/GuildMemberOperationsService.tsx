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

import type {GuildID, InviteCode, RoleID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createChannelID, createRoleID} from '@fluxer/api/src/BrandedTypes';
import type {ChannelService} from '@fluxer/api/src/channel/services/ChannelService';
import type {GuildMemberRow} from '@fluxer/api/src/database/types/GuildTypes';
import type {GuildAuditLogService} from '@fluxer/api/src/guild/GuildAuditLogService';
import type {GuildAuditLogChange} from '@fluxer/api/src/guild/GuildAuditLogTypes';
import {resolveMaxGuildMembersLimit} from '@fluxer/api/src/guild/GuildMemberLimitUtils';
import {mapGuildMemberToResponse} from '@fluxer/api/src/guild/GuildModel';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {GuildMemberAuthService} from '@fluxer/api/src/guild/services/member/GuildMemberAuthService';
import type {GuildMemberEventService} from '@fluxer/api/src/guild/services/member/GuildMemberEventService';
import type {GuildMemberSearchIndexService} from '@fluxer/api/src/guild/services/member/GuildMemberSearchIndexService';
import type {GuildMemberValidationService} from '@fluxer/api/src/guild/services/member/GuildMemberValidationService';
import type {EntityAssetService, PreparedAssetUpload} from '@fluxer/api/src/infrastructure/EntityAssetService';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import {getMetricsService} from '@fluxer/api/src/infrastructure/MetricsService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import {Logger} from '@fluxer/api/src/Logger';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {resolveLimitSafe} from '@fluxer/api/src/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@fluxer/api/src/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {GuildMember} from '@fluxer/api/src/models/GuildMember';
import type {User} from '@fluxer/api/src/models/User';
import type {UserGuildSettings} from '@fluxer/api/src/models/UserGuildSettings';
import type {UserSettings} from '@fluxer/api/src/models/UserSettings';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {mapUserGuildSettingsToResponse, mapUserSettingsToResponse} from '@fluxer/api/src/user/UserMappers';
import {removeGuildFromUserFolders} from '@fluxer/api/src/user/utils/GuildFolderUtils';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {JoinSourceTypes, SystemChannelFlags} from '@fluxer/constants/src/GuildConstants';
import {MAX_GUILDS_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {
	DEFAULT_GUILD_FOLDER_ICON,
	UNCATEGORIZED_FOLDER_ID,
	UserNotificationSettings,
} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {MaxGuildMembersError} from '@fluxer/errors/src/domains/guild/MaxGuildMembersError';
import {MaxGuildsError} from '@fluxer/errors/src/domains/guild/MaxGuildsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownGuildMemberError} from '@fluxer/errors/src/domains/guild/UnknownGuildMemberError';
import {UserNotInVoiceError} from '@fluxer/errors/src/domains/user/UserNotInVoiceError';
import type {IRateLimitService} from '@fluxer/rate_limit/src/IRateLimitService';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildMemberUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {ms} from 'itty-time';

interface MemberUpdateData {
	nick?: string | null;
	role_ids?: Set<RoleID>;
	avatar_hash?: string | null;
	banner_hash?: string | null;
	bio?: string | null;
	pronouns?: string | null;
	accent_color?: number | null;
	profile_flags?: number | null;
	mute?: boolean;
	deaf?: boolean;
	communication_disabled_until?: Date | null;
}

interface PreparedMemberAssets {
	avatar: PreparedAssetUpload | null;
	banner: PreparedAssetUpload | null;
}

interface VoiceAuditLogMetadataParams {
	newChannelId: bigint | null;
	previousChannelId: string | null;
}

function buildVoiceAuditLogMetadata(params: VoiceAuditLogMetadataParams): Record<string, string> | null {
	const channelId = params.newChannelId !== null ? params.newChannelId.toString() : (params.previousChannelId ?? null);
	if (!channelId) {
		return null;
	}
	return {
		channel_id: channelId,
		count: '1',
	};
}

export class GuildMemberOperationsService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly channelService: ChannelService,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly entityAssetService: EntityAssetService,
		private readonly userRepository: IUserRepository,
		private readonly rateLimitService: IRateLimitService,
		private readonly authService: GuildMemberAuthService,
		private readonly validationService: GuildMemberValidationService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly limitConfigService: LimitConfigService,
		private readonly searchIndexService?: GuildMemberSearchIndexService,
	) {}

	async getMembers(params: {
		userId: UserID;
		guildId: GuildID;
		limit?: number;
		after?: UserID;
		requestCache: RequestCache;
	}): Promise<Array<GuildMemberResponse>> {
		const {userId, guildId, limit = 1, after} = params;
		await this.authService.getGuildAuthenticated({userId, guildId});
		const cursorResult = await this.gatewayService.listGuildMembersCursor({
			guildId,
			limit,
			after,
		});

		return cursorResult.members;
	}

	private async recordVoiceAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		targetId: UserID;
		newChannelId: bigint | null;
		previousChannelId: string | null;
		connectionId: string | null;
		auditLogReason?: string | null;
	}): Promise<void> {
		const action = params.newChannelId === null ? AuditLogActionType.MEMBER_DISCONNECT : AuditLogActionType.MEMBER_MOVE;

		const previousSnapshot = params.previousChannelId !== null ? {channel_id: params.previousChannelId} : null;
		const nextSnapshot = params.newChannelId !== null ? {channel_id: params.newChannelId.toString()} : null;

		const voiceChanges = this.guildAuditLogService.computeChanges(previousSnapshot, nextSnapshot);
		const changes = voiceChanges.length > 0 ? voiceChanges : null;
		const metadata = buildVoiceAuditLogMetadata({
			newChannelId: params.newChannelId,
			previousChannelId: params.previousChannelId,
		});

		await this.recordGuildAuditLog({
			guildId: params.guildId,
			userId: params.userId,
			action,
			targetUserId: params.targetId,
			auditLogReason: params.auditLogReason,
			changes,
			metadata: metadata ?? undefined,
		});
	}

	private async recordGuildAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		action: AuditLogActionType;
		targetUserId: UserID;
		auditLogReason?: string | null;
		metadata?: Record<string, string>;
		changes?: GuildAuditLogChange | null;
	}): Promise<void> {
		const builder = this.guildAuditLogService
			.createBuilder(params.guildId, params.userId)
			.withAction(params.action, params.targetUserId.toString())
			.withReason(params.auditLogReason ?? null);

		if (params.metadata) {
			builder.withMetadata(params.metadata);
		}
		if (params.changes) {
			builder.withChanges(params.changes);
		}

		try {
			await builder.commit();
		} catch (error) {
			Logger.error(
				{
					error,
					guildId: params.guildId.toString(),
					userId: params.userId.toString(),
					action: params.action,
					targetId: params.targetUserId.toString(),
				},
				'Failed to record guild audit log',
			);
		}
	}

	private async fetchCurrentChannelId(guildId: GuildID, userId: UserID): Promise<string | null> {
		const voiceState = await this.gatewayService.getVoiceState({guildId, userId});
		return voiceState?.channel_id ?? null;
	}

	async getMember(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<GuildMemberResponse> {
		const {userId, targetId, guildId, requestCache} = params;
		await this.authService.getGuildAuthenticated({userId, guildId});
		const member = await this.guildRepository.getMember(guildId, targetId);
		if (!member) throw new UnknownGuildMemberError();
		return await mapGuildMemberToResponse(member, this.userCacheService, requestCache);
	}

	async updateMember(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		data: GuildMemberUpdateRequest | Omit<GuildMemberUpdateRequest, 'roles'>;
		requestCache: RequestCache;
		auditLogReason?: string | null;
	}): Promise<GuildMemberResponse> {
		const {userId, targetId, guildId, data, requestCache} = params;
		const {guildData, canManageRoles, hasPermission, checkTargetMember} = await this.authService.getGuildAuthenticated({
			userId,
			guildId,
		});

		const updateData: MemberUpdateData = {};

		if (data.nick !== undefined) {
			if (userId === targetId) {
				const canChangeNick = await hasPermission(Permissions.CHANGE_NICKNAME);
				if (!canChangeNick) throw new MissingPermissionsError();
			} else {
				const hasManageNicknames = await hasPermission(Permissions.MANAGE_NICKNAMES);
				if (!hasManageNicknames) throw new MissingPermissionsError();
				await checkTargetMember(targetId);
			}
		}

		if (data.communication_disabled_until !== undefined) {
			updateData.communication_disabled_until = await this.validateTimeout({
				userId,
				targetId,
				guildId,
				rawTimeout: data.communication_disabled_until,
				hasPermission,
				checkTargetMember,
			});
		}

		const targetMember = await this.guildRepository.getMember(guildId, targetId);
		if (!targetMember) throw new UnknownGuildMemberError();

		const targetUser = await this.userRepository.findUnique(targetId);
		if (!targetUser) {
			throw new UnknownGuildMemberError();
		}

		const preparedAssets: PreparedMemberAssets = {avatar: null, banner: null};

		if (data.nick !== undefined) {
			updateData.nick = data.nick;
		}

		if ('roles' in data && data.roles !== undefined) {
			const roleIds = await this.validationService.validateAndGetRoleIds({
				userId,
				guildId,
				guildData,
				targetId,
				targetMember,
				newRoles: Array.from(data.roles).map(createRoleID),
				hasPermission,
				canManageRoles,
			});
			updateData.role_ids = new Set(roleIds);
		}

		if (userId === targetId) {
			try {
				await this.updateSelfProfile({
					userId,
					targetId,
					guildId,
					targetUser,
					targetMember,
					data,
					updateData,
					preparedAssets,
				});
			} catch (error) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw error;
			}
		}

		await this.updateVoiceAndChannel({
			userId,
			targetId,
			guildId,
			targetMember,
			data,
			updateData,
			hasPermission,
			auditLogReason: params.auditLogReason,
		});

		const isAssigningRoles = updateData.role_ids !== undefined && updateData.role_ids.size > 0;
		const shouldRemoveTemporaryStatus = targetMember.isTemporary && isAssigningRoles;

		const updatedMemberData = this.buildMemberUpdateRow({targetMember, updateData, shouldRemoveTemporaryStatus});

		let updatedMember: GuildMember;
		try {
			updatedMember = await this.guildRepository.upsertMember(updatedMemberData);
		} catch (error) {
			await this.rollbackPreparedAssets(preparedAssets);
			throw error;
		}

		await this.commitPreparedAssets(preparedAssets);

		if (shouldRemoveTemporaryStatus) {
			await this.gatewayService.removeTemporaryGuild({userId: targetId, guildId});
		}

		return await mapGuildMemberToResponse(updatedMember, this.userCacheService, requestCache);
	}

	async removeMember(params: {userId: UserID; targetId: UserID; guildId: GuildID}): Promise<void> {
		let succeeded = false;
		try {
			const {userId, targetId, guildId} = params;
			const {guildData, checkTargetMember, checkPermission} = await this.authService.getGuildAuthenticated({
				userId,
				guildId,
			});
			await checkPermission(Permissions.KICK_MEMBERS);

			const targetMember = await this.guildRepository.getMember(guildId, targetId);
			if (!targetMember) throw new UnknownGuildMemberError();

			if (targetMember.userId === userId || guildData.owner_id === targetId.toString()) {
				throw new UnknownGuildMemberError();
			}

			await checkTargetMember(targetId);

			const guild = await this.guildRepository.findUnique(guildId);
			await this.guildRepository.deleteMember(guildId, targetId);

			if (guild) {
				const guildRow = guild.toRow();
				await this.guildRepository.upsert({
					...guildRow,
					member_count: Math.max(0, guild.memberCount - 1),
				});
			}

			await this.gatewayService.leaveGuild({userId: targetId, guildId});
			succeeded = true;
		} finally {
			const metric = succeeded ? 'guild.member.leave' : 'guild.member.leave.error';
			getMetricsService().counter({name: metric});
		}
	}

	async addUserToGuild(
		params: {
			userId: UserID;
			guildId: GuildID;
			sendJoinMessage?: boolean;
			skipGuildLimitCheck?: boolean;
			skipBanCheck?: boolean;
			isTemporary?: boolean;
			joinSourceType?: number;
			sourceInviteCode?: InviteCode;
			inviterId?: UserID;
			requestCache: RequestCache;
			initiatorId?: UserID;
		},
		eventService: GuildMemberEventService,
	): Promise<GuildMember> {
		let succeeded = false;
		try {
			const {
				userId,
				guildId,
				sendJoinMessage = true,
				skipGuildLimitCheck = false,
				skipBanCheck = false,
				isTemporary = false,
				joinSourceType = JoinSourceTypes.INSTANT_INVITE,
				sourceInviteCode = null,
				inviterId = null,
				requestCache,
			} = params;
			const initiatorId = params.initiatorId ?? userId;

			const guild = await this.guildRepository.findUnique(guildId);
			if (!guild) throw new UnknownGuildError();

			const existingMember = await this.guildRepository.getMember(guildId, userId);
			if (existingMember) return existingMember;

			const user = await this.userRepository.findUnique(userId);
			if (!user) throw new UnknownGuildError();

			if (!skipBanCheck) {
				await this.validationService.checkUserBanStatus({userId, guildId});
			}

			const userGuildsCount = await this.guildRepository.countUserGuilds(userId);
			if (!skipGuildLimitCheck) {
				await this.enforceGuildLimit(user, userGuildsCount);
			}

			const maxGuildMembers = resolveMaxGuildMembersLimit({
				guildFeatures: guild.features,
				snapshot: this.limitConfigService.getConfigSnapshot(),
			});
			if (guild.memberCount >= maxGuildMembers) {
				throw new MaxGuildMembersError(maxGuildMembers);
			}

			const guildMember = await this.guildRepository.upsertMember({
				guild_id: guildId,
				user_id: userId,
				joined_at: new Date(),
				nick: null,
				avatar_hash: null,
				banner_hash: null,
				bio: null,
				pronouns: null,
				accent_color: null,
				join_source_type: joinSourceType,
				source_invite_code: sourceInviteCode,
				inviter_id: inviterId,
				deaf: false,
				mute: false,
				communication_disabled_until: null,
				role_ids: null,
				is_premium_sanitized: null,
				temporary: isTemporary,
				profile_flags: null,
				version: 1,
			});

			const guildRow = guild.toRow();
			await this.guildRepository.upsert({
				...guildRow,
				member_count: guild.memberCount + 1,
			});

			const newMemberCount = guild.memberCount + 1;
			getMetricsService().gauge({
				name: 'guild.member_count',
				dimensions: {
					guild_id: guildId.toString(),
					guild_name: guild.name ?? 'unknown',
				},
				value: newMemberCount,
			});

			getMetricsService().gauge({
				name: 'user.guild_membership_count',
				dimensions: {
					user_id: userId.toString(),
					is_bot: user.isBot ? 'true' : 'false',
				},
				value: userGuildsCount + 1,
			});

			getMetricsService().counter({name: 'guild.member.join'});

			await this.applyJoinUserSettings({userId, guildId, user});

			await eventService.dispatchGuildMemberAdd({member: guildMember, requestCache});
			await this.gatewayService.joinGuild({userId, guildId});

			if (this.searchIndexService && guild.membersIndexedAt) {
				void this.searchIndexService.indexMember(guildMember, user);
			}

			if (sendJoinMessage && !(guild.systemChannelFlags & SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS)) {
				await this.channelService.sendJoinSystemMessage({guildId, userId, requestCache});
			}

			if (user.isBot) {
				await this.recordGuildAuditLog({
					guildId,
					userId: initiatorId,
					action: AuditLogActionType.BOT_ADD,
					targetUserId: userId,
					metadata: {
						temporary: isTemporary ? 'true' : 'false',
					},
				});
			}

			succeeded = true;
			return guildMember;
		} finally {
			if (!succeeded) {
				getMetricsService().counter({name: 'guild.member.join.error'});
			}
		}
	}

	async leaveGuild(params: {userId: UserID; guildId: GuildID}): Promise<void> {
		let succeeded = false;
		try {
			const {userId, guildId} = params;
			const guildData = await this.gatewayService.getGuildData({guildId, userId});
			if (!guildData) throw new UnknownGuildError();
			if (guildData.owner_id === userId.toString()) {
				throw InputValidationError.fromCode('guild_id', ValidationErrorCodes.CANNOT_LEAVE_GUILD_AS_OWNER);
			}

			const user = await this.userRepository.findUnique(userId);
			const guild = await this.guildRepository.findUnique(guildId);
			await this.guildRepository.deleteMember(guildId, userId);

			if (guild) {
				const guildRow = guild.toRow();
				const newMemberCount = Math.max(0, guild.memberCount - 1);
				await this.guildRepository.upsert({
					...guildRow,
					member_count: newMemberCount,
				});
				getMetricsService().gauge({
					name: 'guild.member_count',
					dimensions: {
						guild_id: guildId.toString(),
						guild_name: guild.name ?? 'unknown',
					},
					value: newMemberCount,
				});
			}

			if (user && !user.isBot) {
				await removeGuildFromUserFolders({
					userId,
					guildId,
					userRepository: this.userRepository,
					gatewayService: this.gatewayService,
				});
			}

			await this.gatewayService.leaveGuild({userId, guildId});
			const membershipCount = await this.guildRepository.countUserGuilds(userId);
			getMetricsService().gauge({
				name: 'user.guild_membership_count',
				dimensions: {
					user_id: userId.toString(),
					is_bot: user?.isBot ? 'true' : 'false',
				},
				value: membershipCount,
			});

			succeeded = true;
		} finally {
			const metric = succeeded ? 'guild.member.leave' : 'guild.member.leave.error';
			getMetricsService().counter({name: metric});
		}
	}

	private async validateTimeout(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		rawTimeout: string | null;
		hasPermission: (permission: bigint) => Promise<boolean>;
		checkTargetMember: (targetId: UserID) => Promise<void>;
	}): Promise<Date | null> {
		const {userId, targetId, guildId, rawTimeout, hasPermission, checkTargetMember} = params;

		if (userId === targetId) {
			throw new MissingPermissionsError();
		}

		const hasModerateMembers = await hasPermission(Permissions.MODERATE_MEMBERS);
		if (!hasModerateMembers) throw new MissingPermissionsError();

		const targetPermissions = await this.gatewayService.getUserPermissions({guildId, userId: targetId});
		if ((targetPermissions & Permissions.MODERATE_MEMBERS) === Permissions.MODERATE_MEMBERS) {
			throw new MissingPermissionsError();
		}

		await checkTargetMember(targetId);

		if (rawTimeout === null) {
			return null;
		}

		const parsedTimeout = new Date(rawTimeout);
		if (Number.isNaN(parsedTimeout.getTime())) {
			throw InputValidationError.fromCode('communication_disabled_until', ValidationErrorCodes.INVALID_TIMEOUT_VALUE);
		}

		const diffMs = parsedTimeout.getTime() - Date.now();
		if (diffMs > ms('1 year')) {
			throw InputValidationError.fromCode(
				'communication_disabled_until',
				ValidationErrorCodes.TIMEOUT_CANNOT_EXCEED_365_DAYS,
			);
		}

		return parsedTimeout;
	}

	private buildMemberUpdateRow(params: {
		targetMember: GuildMember;
		updateData: MemberUpdateData;
		shouldRemoveTemporaryStatus: boolean;
	}): GuildMemberRow {
		const {targetMember, updateData, shouldRemoveTemporaryStatus} = params;
		return {
			...targetMember.toRow(),
			nick: updateData.nick !== undefined ? updateData.nick : targetMember.nickname,
			role_ids: updateData.role_ids ?? targetMember.roleIds,
			avatar_hash: updateData.avatar_hash !== undefined ? updateData.avatar_hash : targetMember.avatarHash,
			banner_hash: updateData.banner_hash !== undefined ? updateData.banner_hash : targetMember.bannerHash,
			bio: updateData.bio !== undefined ? updateData.bio : targetMember.bio,
			pronouns: updateData.pronouns !== undefined ? updateData.pronouns : targetMember.pronouns,
			accent_color: updateData.accent_color !== undefined ? updateData.accent_color : targetMember.accentColor,
			profile_flags: updateData.profile_flags !== undefined ? updateData.profile_flags : targetMember.profileFlags,
			mute: updateData.mute !== undefined ? updateData.mute : targetMember.isMute,
			deaf: updateData.deaf !== undefined ? updateData.deaf : targetMember.isDeaf,
			communication_disabled_until:
				updateData.communication_disabled_until !== undefined
					? updateData.communication_disabled_until
					: targetMember.communicationDisabledUntil,
			temporary: shouldRemoveTemporaryStatus ? false : targetMember.isTemporary,
		};
	}

	private async enforceGuildLimit(user: User, currentGuildCount: number): Promise<void> {
		let maxGuilds = MAX_GUILDS_NON_PREMIUM;
		const ctx = createLimitMatchContext({user});
		maxGuilds = resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, 'max_guilds', maxGuilds);
		if (currentGuildCount >= maxGuilds) throw new MaxGuildsError(maxGuilds);
	}

	private async applyJoinUserSettings(params: {userId: UserID; guildId: GuildID; user: User}): Promise<void> {
		const {userId, guildId, user} = params;
		const userSettings = await this.userRepository.findSettings(userId);
		if (!userSettings) {
			return;
		}

		let needsUpdate = false;
		const settingsRow = userSettings.toRow();

		if (user.isBot && userSettings.botDefaultGuildsRestricted) {
			const updatedBotRestrictedGuilds = new Set(userSettings.botRestrictedGuilds);
			updatedBotRestrictedGuilds.add(guildId);
			settingsRow.bot_restricted_guilds = updatedBotRestrictedGuilds;
			needsUpdate = true;
		} else if (!user.isBot && userSettings.defaultGuildsRestricted) {
			const updatedRestrictedGuilds = new Set(userSettings.restrictedGuilds);
			updatedRestrictedGuilds.add(guildId);
			settingsRow.restricted_guilds = updatedRestrictedGuilds;
			needsUpdate = true;
		}

		if (!user.isBot) {
			const existingFolders = settingsRow.guild_folders ?? [];
			const uncategorizedIndex = existingFolders.findIndex((folder) => folder.folder_id === UNCATEGORIZED_FOLDER_ID);

			if (uncategorizedIndex !== -1) {
				const uncategorizedFolder = existingFolders[uncategorizedIndex];
				const updatedGuildIds = [guildId, ...(uncategorizedFolder.guild_ids ?? [])];
				existingFolders[uncategorizedIndex] = {
					...uncategorizedFolder,
					guild_ids: updatedGuildIds,
				};
			} else {
				existingFolders.push({
					folder_id: UNCATEGORIZED_FOLDER_ID,
					name: null,
					color: null,
					flags: 0,
					icon: DEFAULT_GUILD_FOLDER_ICON,
					guild_ids: [guildId],
				});
			}

			settingsRow.guild_folders = existingFolders;
			needsUpdate = true;
		}

		if (needsUpdate) {
			const updatedSettings = await this.userRepository.upsertSettings(settingsRow);
			await this.dispatchUserSettingsUpdate({userId, settings: updatedSettings});
		}

		if (!user.isBot && userSettings.defaultHideMutedChannels) {
			const existingGuildSettings = await this.userRepository.findGuildSettings(userId, guildId);
			const guildSettingsRow = existingGuildSettings
				? {...existingGuildSettings.toRow(), hide_muted_channels: true}
				: {
						user_id: userId,
						guild_id: guildId,
						message_notifications: UserNotificationSettings.INHERIT,
						muted: false,
						mute_config: null,
						mobile_push: true,
						suppress_everyone: false,
						suppress_roles: false,
						hide_muted_channels: true,
						channel_overrides: null,
						version: 1,
					};
			const updatedGuildSettings = await this.userRepository.upsertGuildSettings(guildSettingsRow);
			await this.dispatchUserGuildSettingsUpdate({userId, settings: updatedGuildSettings});
		}
	}

	private async updateSelfProfile(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		targetUser: User;
		targetMember: GuildMember;
		data: GuildMemberUpdateRequest | Omit<GuildMemberUpdateRequest, 'roles'>;
		updateData: MemberUpdateData;
		preparedAssets: PreparedMemberAssets;
	}): Promise<void> {
		const {targetId, guildId, targetUser, targetMember, data, updateData, preparedAssets} = params;

		const ctx = createLimitMatchContext({user: targetUser});
		const hasGuildProfileCustomization = resolveLimitSafe(
			this.limitConfigService.getConfigSnapshot(),
			ctx,
			'feature_per_guild_profiles',
			0,
		);

		if (hasGuildProfileCustomization === 0) {
			if (data.avatar !== undefined) {
				data.avatar = undefined;
			}
			if (data.banner !== undefined) {
				data.banner = undefined;
			}
			if (data.bio !== undefined) {
				data.bio = undefined;
			}
			if (data.accent_color !== undefined) {
				data.accent_color = undefined;
			}
		}

		if (data.profile_flags !== undefined) {
			updateData.profile_flags = data.profile_flags;
		}

		if (data.avatar !== undefined) {
			const avatarRateLimit = await this.rateLimitService.checkLimit({
				identifier: `guild_avatar_change:${guildId}:${targetId}`,
				maxAttempts: 25,
				windowMs: ms('30 minutes'),
			});

			if (!avatarRateLimit.allowed) {
				const minutes = Math.ceil((avatarRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.fromCode('avatar', ValidationErrorCodes.AVATAR_CHANGED_TOO_MANY_TIMES, {minutes});
			}

			const prepared = await this.entityAssetService.prepareAssetUpload({
				assetType: 'avatar',
				entityType: 'guild_member',
				entityId: targetId,
				guildId,
				previousHash: targetMember.avatarHash,
				base64Image: data.avatar,
				errorPath: 'avatar',
			});

			preparedAssets.avatar = prepared;
			if (prepared.newHash !== targetMember.avatarHash) {
				updateData.avatar_hash = prepared.newHash;
			}
		}

		if (data.banner !== undefined) {
			const bannerRateLimit = await this.rateLimitService.checkLimit({
				identifier: `guild_banner_change:${guildId}:${targetId}`,
				maxAttempts: 25,
				windowMs: ms('30 minutes'),
			});

			if (!bannerRateLimit.allowed) {
				const minutes = Math.ceil((bannerRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.fromCode('banner', ValidationErrorCodes.BANNER_CHANGED_TOO_MANY_TIMES, {minutes});
			}

			const prepared = await this.entityAssetService.prepareAssetUpload({
				assetType: 'banner',
				entityType: 'guild_member',
				entityId: targetId,
				guildId,
				previousHash: targetMember.bannerHash,
				base64Image: data.banner,
				errorPath: 'banner',
			});

			preparedAssets.banner = prepared;
			if (prepared.newHash !== targetMember.bannerHash) {
				updateData.banner_hash = prepared.newHash;
			}
		}

		if (data.bio !== undefined) {
			if (data.bio !== targetMember.bio) {
				const bioRateLimit = await this.rateLimitService.checkLimit({
					identifier: `guild_bio_change:${guildId}:${targetId}`,
					maxAttempts: 25,
					windowMs: ms('30 minutes'),
				});

				if (!bioRateLimit.allowed) {
					const minutes = Math.ceil((bioRateLimit.retryAfter || 0) / 60);
					throw InputValidationError.fromCode('bio', ValidationErrorCodes.BIO_CHANGED_TOO_MANY_TIMES, {minutes});
				}

				updateData.bio = data.bio;
			}
		}

		if (data.accent_color !== undefined) {
			if (data.accent_color !== targetMember.accentColor) {
				const accentColorRateLimit = await this.rateLimitService.checkLimit({
					identifier: `guild_accent_color_change:${guildId}:${targetId}`,
					maxAttempts: 25,
					windowMs: ms('30 minutes'),
				});

				if (!accentColorRateLimit.allowed) {
					const minutes = Math.ceil((accentColorRateLimit.retryAfter || 0) / 60);
					throw InputValidationError.fromCode(
						'accent_color',
						ValidationErrorCodes.ACCENT_COLOR_CHANGED_TOO_MANY_TIMES,
						{minutes},
					);
				}

				updateData.accent_color = data.accent_color;
			}
		}

		if (data.pronouns !== undefined) {
			if (data.pronouns !== targetMember.pronouns) {
				const pronounsRateLimit = await this.rateLimitService.checkLimit({
					identifier: `guild_pronouns_change:${guildId}:${targetId}`,
					maxAttempts: 25,
					windowMs: ms('30 minutes'),
				});

				if (!pronounsRateLimit.allowed) {
					const minutes = Math.ceil((pronounsRateLimit.retryAfter || 0) / 60);
					throw InputValidationError.fromCode('pronouns', ValidationErrorCodes.PRONOUNS_CHANGED_TOO_MANY_TIMES, {
						minutes,
					});
				}

				updateData.pronouns = data.pronouns;
			}
		}
	}

	private async updateVoiceAndChannel(params: {
		userId: UserID;
		targetId: UserID;
		guildId: GuildID;
		targetMember: GuildMember;
		data: GuildMemberUpdateRequest | Omit<GuildMemberUpdateRequest, 'roles'>;
		updateData: MemberUpdateData;
		hasPermission: (permission: bigint) => Promise<boolean>;
		auditLogReason?: string | null;
	}): Promise<void> {
		const {userId, targetId, guildId, targetMember, data, updateData, hasPermission, auditLogReason} = params;

		if (data.mute !== undefined || data.deaf !== undefined || data.channel_id !== undefined) {
			if (data.mute !== undefined || data.deaf !== undefined) {
				if (!(await hasPermission(Permissions.MUTE_MEMBERS))) {
					throw new MissingPermissionsError();
				}
			}

			if (data.channel_id !== undefined) {
				if (!(await hasPermission(Permissions.MOVE_MEMBERS))) {
					throw new MissingPermissionsError();
				}

				const previousChannelId = await this.fetchCurrentChannelId(guildId, targetId);

				const result = await this.gatewayService.moveMember({
					guildId,
					moderatorId: userId,
					userId: targetId,
					channelId: data.channel_id !== null ? createChannelID(data.channel_id) : null,
					connectionId: data.connection_id ?? null,
				});

				if (result.error) {
					switch (result.error) {
						case 'user_not_in_voice':
						case 'connection_not_found':
							throw new UserNotInVoiceError();
						case 'channel_not_found':
							throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.CHANNEL_DOES_NOT_EXIST);
						case 'channel_not_voice':
							throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.CHANNEL_MUST_BE_VOICE);
						case 'target_missing_connect':
						case 'moderator_missing_connect':
							throw new MissingPermissionsError();
						default:
							throw new UserNotInVoiceError();
					}
				} else {
					await this.recordVoiceAuditLog({
						guildId,
						userId,
						targetId,
						newChannelId: data.channel_id,
						previousChannelId,
						connectionId: data.connection_id ?? null,
						auditLogReason,
					});
				}
			}

			if (data.mute !== undefined || data.deaf !== undefined) {
				try {
					await this.gatewayService.updateMemberVoice({
						guildId,
						userId: targetId,
						mute: data.mute ?? targetMember.isMute,
						deaf: data.deaf ?? targetMember.isDeaf,
					});

					if (data.mute !== undefined) {
						updateData.mute = data.mute;
					}
					if (data.deaf !== undefined) {
						updateData.deaf = data.deaf;
					}
				} catch (error) {
					Logger.error({error, userId: targetId, guildId}, 'Failed to get user voice state, user not in voice');
					throw new UserNotInVoiceError();
				}
			}
		}
	}

	private async dispatchUserSettingsUpdate({
		userId,
		settings,
	}: {
		userId: UserID;
		settings: UserSettings;
	}): Promise<void> {
		const guildIds = await this.userRepository.getUserGuildIds(userId);
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'USER_SETTINGS_UPDATE',
			data: mapUserSettingsToResponse({settings, memberGuildIds: guildIds}),
		});
	}

	private async dispatchUserGuildSettingsUpdate({
		userId,
		settings,
	}: {
		userId: UserID;
		settings: UserGuildSettings;
	}): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'USER_GUILD_SETTINGS_UPDATE',
			data: mapUserGuildSettingsToResponse(settings),
		});
	}

	private async rollbackPreparedAssets(preparedAssets: PreparedMemberAssets): Promise<void> {
		const rollbackPromises: Array<Promise<void>> = [];

		if (preparedAssets.avatar) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(preparedAssets.avatar));
		}
		if (preparedAssets.banner) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(preparedAssets.banner));
		}

		await Promise.all(rollbackPromises);
	}

	private async commitPreparedAssets(preparedAssets: PreparedMemberAssets): Promise<void> {
		const commitPromises: Array<Promise<void>> = [];

		if (preparedAssets.avatar) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: preparedAssets.avatar}));
		}
		if (preparedAssets.banner) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: preparedAssets.banner}));
		}

		await Promise.all(commitPromises);
	}
}
