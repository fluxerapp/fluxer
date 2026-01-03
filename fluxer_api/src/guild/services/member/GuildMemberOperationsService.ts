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
import {createChannelID, createRoleID} from '~/BrandedTypes';
import {MAX_GUILDS_NON_PREMIUM, MAX_GUILDS_PREMIUM, Permissions, SystemChannelFlags} from '~/Constants';
import type {ChannelService} from '~/channel/services/ChannelService';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {JoinSourceTypes} from '~/constants/Guild';
import {
	InputValidationError,
	MaxGuildsError,
	MissingPermissionsError,
	UnknownGuildError,
	UnknownGuildMemberError,
	UserNotInVoiceError,
} from '~/Errors';
import type {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {GuildAuditLogChange} from '~/guild/GuildAuditLogTypes';
import type {GuildMemberResponse, GuildMemberUpdateRequest} from '~/guild/GuildModel';
import {mapGuildMembersToResponse, mapGuildMemberToResponse} from '~/guild/GuildModel';
import type {EntityAssetService, PreparedAssetUpload} from '~/infrastructure/EntityAssetService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {Logger} from '~/Logger';
import type {GuildMember, User, UserSettings} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserSettingsToResponse} from '~/user/UserMappers';
import type {IGuildRepository} from '../../IGuildRepository';
import type {GuildMemberAuthService} from './GuildMemberAuthService';
import type {GuildMemberEventService} from './GuildMemberEventService';
import type {GuildMemberValidationService} from './GuildMemberValidationService';

const MAX_TIMEOUT_DURATION_MS = 365 * 24 * 60 * 60 * 1000;

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
		private readonly guildRepository: IGuildRepository,
		private readonly channelService: ChannelService,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly entityAssetService: EntityAssetService,
		private readonly userRepository: IUserRepository,
		private readonly rateLimitService: IRateLimitService,
		private readonly authService: GuildMemberAuthService,
		private readonly validationService: GuildMemberValidationService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {}

	async getMembers(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildMemberResponse>> {
		const {userId, guildId, requestCache} = params;
		await this.authService.getGuildAuthenticated({userId, guildId});
		const members = await this.guildRepository.listMembers(guildId);
		return await mapGuildMembersToResponse(members, this.userCacheService, requestCache);
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
		try {
			const voiceState = await this.gatewayService.getVoiceState({guildId, userId});
			return voiceState?.channel_id ?? null;
		} catch (error) {
			Logger.warn(
				{error, guildId: guildId.toString(), userId: userId.toString()},
				'Failed to load voice state for audit log',
			);
			return null;
		}
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
			if (userId === targetId) {
				throw new MissingPermissionsError();
			}
			const hasModerateMembers = await hasPermission(Permissions.MODERATE_MEMBERS);
			if (!hasModerateMembers) throw new MissingPermissionsError();
			await checkTargetMember(targetId);

			const targetPermissions = await this.gatewayService.getUserPermissions({guildId, userId: targetId});
			if ((targetPermissions & Permissions.MODERATE_MEMBERS) === Permissions.MODERATE_MEMBERS) {
				throw new MissingPermissionsError();
			}

			const parsedTimeout =
				data.communication_disabled_until !== null ? new Date(data.communication_disabled_until) : null;

			if (parsedTimeout !== null && Number.isNaN(parsedTimeout.getTime())) {
				throw InputValidationError.create('communication_disabled_until', 'Invalid timeout value');
			}

			if (parsedTimeout !== null) {
				const diffMs = parsedTimeout.getTime() - Date.now();
				if (diffMs > MAX_TIMEOUT_DURATION_MS) {
					throw InputValidationError.create(
						'communication_disabled_until',
						'Timeout cannot be longer than 365 days from now.',
					);
				}
			}

			updateData.communication_disabled_until = parsedTimeout ?? null;
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

		const updatedMemberData = {
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

			await this.guildRepository.deleteMember(guildId, targetId);

			const guild = await this.guildRepository.findUnique(guildId);
			if (guild) {
				const guildRow = guild.toRow();
				await this.guildRepository.upsert({
					...guildRow,
					member_count: Math.max(0, guild.memberCount - 1),
				});
			}

			await this.gatewayService.leaveGuild({userId: targetId, guildId});

			getMetricsService().counter({name: 'guild.member.leave'});
		} catch (error) {
			getMetricsService().counter({name: 'guild.member.leave.error'});
			throw error;
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
			requestCache: RequestCache;
			initiatorId?: UserID;
		},
		eventService: GuildMemberEventService,
	): Promise<GuildMember> {
		try {
			const {
				userId,
				guildId,
				sendJoinMessage = true,
				skipGuildLimitCheck = false,
				skipBanCheck = false,
				isTemporary = false,
				joinSourceType = JoinSourceTypes.INVITE,
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
				const maxGuilds = user.isPremium() ? MAX_GUILDS_PREMIUM : MAX_GUILDS_NON_PREMIUM;
				if (userGuildsCount >= maxGuilds) throw new MaxGuildsError(maxGuilds);
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
				source_invite_code: null,
				inviter_id: null,
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

			if (user && !user.isBot) {
				const userSettings = await this.userRepository.findSettings(userId);
				if (userSettings?.defaultGuildsRestricted) {
					const updatedRestrictedGuilds = new Set(userSettings.restrictedGuilds);
					updatedRestrictedGuilds.add(guildId);
					const updatedRowData = {...userSettings.toRow(), restrictedGuilds: updatedRestrictedGuilds};
					const updatedSettings = await this.userRepository.upsertSettings(updatedRowData);
					await this.dispatchUserSettingsUpdate({userId, settings: updatedSettings});
				}
			}

			await eventService.dispatchGuildMemberAdd({member: guildMember, requestCache});
			await this.gatewayService.joinGuild({userId, guildId});

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

			return guildMember;
		} catch (error) {
			getMetricsService().counter({name: 'guild.member.join.error'});
			throw error;
		}
	}

	async leaveGuild(params: {userId: UserID; guildId: GuildID}): Promise<void> {
		try {
			const {userId, guildId} = params;
			const guildData = await this.gatewayService.getGuildData({guildId, userId});
			if (!guildData) throw new UnknownGuildError();
			if (guildData.owner_id === userId.toString()) {
				throw InputValidationError.create(
					'guild_id',
					'Cannot leave guild as owner. Transfer ownership or delete the guild instead.',
				);
			}

			await this.guildRepository.deleteMember(guildId, userId);

			const guild = await this.guildRepository.findUnique(guildId);
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

			await this.gatewayService.leaveGuild({userId, guildId});
			const membershipCount = await this.guildRepository.countUserGuilds(userId);
			const user = await this.userRepository.findUnique(userId);
			getMetricsService().gauge({
				name: 'user.guild_membership_count',
				dimensions: {
					user_id: userId.toString(),
					is_bot: user?.isBot ? 'true' : 'false',
				},
				value: membershipCount,
			});

			getMetricsService().counter({name: 'guild.member.leave'});
		} catch (error) {
			getMetricsService().counter({name: 'guild.member.leave.error'});
			throw error;
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

		if (!targetUser.isPremium()) {
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
				windowMs: 30 * 60 * 1000,
			});

			if (!avatarRateLimit.allowed) {
				const minutes = Math.ceil((avatarRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.create(
					'avatar',
					`You've changed your avatar too many times recently. Please try again in ${minutes} minutes.`,
				);
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
				windowMs: 30 * 60 * 1000,
			});

			if (!bannerRateLimit.allowed) {
				const minutes = Math.ceil((bannerRateLimit.retryAfter || 0) / 60);
				throw InputValidationError.create(
					'banner',
					`You've changed your banner too many times recently. Please try again in ${minutes} minutes.`,
				);
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
					windowMs: 30 * 60 * 1000,
				});

				if (!bioRateLimit.allowed) {
					const minutes = Math.ceil((bioRateLimit.retryAfter || 0) / 60);
					throw InputValidationError.create(
						'bio',
						`You've changed your bio too many times recently. Please try again in ${minutes} minutes.`,
					);
				}

				updateData.bio = data.bio;
			}
		}

		if (data.accent_color !== undefined) {
			if (data.accent_color !== targetMember.accentColor) {
				const accentColorRateLimit = await this.rateLimitService.checkLimit({
					identifier: `guild_accent_color_change:${guildId}:${targetId}`,
					maxAttempts: 25,
					windowMs: 30 * 60 * 1000,
				});

				if (!accentColorRateLimit.allowed) {
					const minutes = Math.ceil((accentColorRateLimit.retryAfter || 0) / 60);
					throw InputValidationError.create(
						'accent_color',
						`You've changed your accent color too many times recently. Please try again in ${minutes} minutes.`,
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
					windowMs: 30 * 60 * 1000,
				});

				if (!pronounsRateLimit.allowed) {
					const minutes = Math.ceil((pronounsRateLimit.retryAfter || 0) / 60);
					throw InputValidationError.create(
						'pronouns',
						`You've changed your pronouns too many times recently. Please try again in ${minutes} minutes.`,
					);
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
							throw new UserNotInVoiceError();
						case 'channel_not_found':
							throw InputValidationError.create('channel_id', 'Channel does not exist');
						case 'channel_not_voice':
							throw InputValidationError.create('channel_id', 'Channel must be a voice channel');
						case 'moderator_missing_connect':
							throw new MissingPermissionsError();
						case 'target_missing_connect':
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
				} catch (_error) {
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
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'USER_SETTINGS_UPDATE',
			data: mapUserSettingsToResponse({settings}),
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

		await Promise.allSettled(rollbackPromises);
	}

	private async commitPreparedAssets(preparedAssets: PreparedMemberAssets): Promise<void> {
		const commitPromises: Array<Promise<void>> = [];

		if (preparedAssets.avatar) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: preparedAssets.avatar}));
		}
		if (preparedAssets.banner) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: preparedAssets.banner}));
		}

		await Promise.allSettled(commitPromises);
	}
}
