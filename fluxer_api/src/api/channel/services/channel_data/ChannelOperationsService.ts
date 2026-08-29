// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {
	ALL_PERMISSIONS,
	ChannelTypes,
	GUILD_TEXT_BASED_CHANNEL_TYPES,
	Permissions,
} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel, GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MAX_CHANNELS_PER_CATEGORY} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {MaxCategoryChannelsError} from '@fluxer/errors/src/domains/channel/MaxCategoryChannelsError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {CannotExecuteOnDmError} from '@fluxer/errors/src/domains/core/CannotExecuteOnDmError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import {ChannelNameType} from '@fluxer/schema/src/primitives/ChannelValidators';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {ChannelID, GuildID, RoleID, UserID} from '../../../BrandedTypes';
import {createChannelID, createGuildID, createRoleID, createUserID} from '../../../BrandedTypes';
import type {GuildAuditLogService} from '../../../guild/GuildAuditLogService';
import {mapGuildToGuildResponse} from '../../../guild/GuildModel';
import type {IGuildRepositoryAggregate} from '../../../guild/repositories/IGuildRepositoryAggregate';
import {ChannelHelpers} from '../../../guild/services/channel/ChannelHelpers';
import {contentModerationService} from '../../../infrastructure/ContentModerationService';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {ILiveKitService} from '../../../infrastructure/ILiveKitService';
import type {IVoiceRoomStore} from '../../../infrastructure/IVoiceRoomStore';
import type {IInviteRepository} from '../../../invite/IInviteRepository';
import {Logger} from '../../../Logger';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import {createLimitMatchContext} from '../../../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../../models/Channel';
import {ChannelPermissionOverwrite} from '../../../models/ChannelPermissionOverwrite';
import {deleteChannelMessageSearchDocuments} from '../../../search/MessageSearchIndexCleanup';
import type {IUserRepository} from '../../../user/IUserRepository';
import {serializeChannelForAudit} from '../../../utils/AuditSerializationUtils';
import {applyProtectedOverwriteBits} from '../../../utils/featureUtils';
import type {VoiceAvailabilityService} from '../../../voice/VoiceAvailabilityService';
import type {VoiceRegionAvailability} from '../../../voice/VoiceModel';
import type {IWebhookRepository} from '../../../webhook/IWebhookRepository';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {ChannelAuthService} from './ChannelAuthService';
import type {ChannelUtilsService} from './ChannelUtilsService';

export interface ChannelUpdateData {
	name?: string;
	topic?: string | null;
	url?: string | null;
	parent_id?: bigint | null;
	bitrate?: number | null;
	user_limit?: number | null;
	voice_connection_limit?: number | null;
	nsfw?: boolean;
	nsfw_override?: boolean | null;
	content_warning_level?: number;
	content_warning_text?: string | null;
	rate_limit_per_user?: number;
	permission_overwrites?: Array<{
		id: bigint;
		type: number;
		allow?: bigint;
		deny?: bigint;
	}> | null;
	rtc_region?: string | null;
	icon?: string | null;
	owner_id?: bigint | null;
	nicks?: Record<string, string | null> | null;
}

export class ChannelOperationsService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		private gatewayService: IGatewayService,
		private channelAuthService: ChannelAuthService,
		private channelUtilsService: ChannelUtilsService,
		private voiceRoomStore: IVoiceRoomStore,
		private liveKitService: ILiveKitService,
		private voiceAvailabilityService: VoiceAvailabilityService | null,
		private readonly guildAuditLogService: GuildAuditLogService,
		private inviteRepository: IInviteRepository,
		private webhookRepository: IWebhookRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private limitConfigService: LimitConfigService,
		private rateLimitService: IRateLimitService,
	) {}

	async getChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<Channel> {
		const {channel} = await this.channelAuthService.getChannelAuthenticated({userId, channelId});
		return channel;
	}

	async getPublicChannelData(channelId: ChannelID) {
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();
		return channel;
	}

	async getChannelMemberCount(channelId: ChannelID): Promise<number> {
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();
		return channel.recipientIds.size;
	}

	async getChannelSystem(channelId: ChannelID): Promise<Channel | null> {
		return await this.channelRepository.channelData.findUnique(channelId);
	}

	async editChannel({
		userId,
		channelId,
		data,
		clientFeatures,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		data: ChannelUpdateData;
		clientFeatures: ReadonlySet<string>;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const {channel, guild, checkPermission} = await this.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
		});
		if (channel.type === ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeError();
		}
		if (!guild) throw new MissingPermissionsError();
		await checkPermission(Permissions.MANAGE_CHANNELS);
		const guildIdValue = createGuildID(BigInt(guild.id));
		contentModerationService.scanText(data.name ?? null, {
			userId,
			guildId: guildIdValue,
			channelId,
			messageId: null,
			surface: 'profile_field',
		});
		contentModerationService.scanText(data.topic ?? null, {
			userId,
			guildId: guildIdValue,
			channelId,
			messageId: null,
			surface: 'profile_field',
		});
		let channelName = data.name ?? channel.name;
		if (data.name !== undefined && channel.type === ChannelTypes.GUILD_TEXT) {
			const hasFlexibleNamesEnabled = guild.features?.includes(GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES) ?? false;
			if (!hasFlexibleNamesEnabled) {
				channelName = ChannelNameType.parse(data.name);
			}
		}
		if (data.rtc_region !== undefined && channel.type === ChannelTypes.GUILD_VOICE) {
			await checkPermission(Permissions.UPDATE_RTC_REGION);
			if (data.rtc_region !== null) {
				if (this.voiceAvailabilityService !== null) {
					const guildId = createGuildID(BigInt(guild.id));
					const availableRegions = this.voiceAvailabilityService.getAvailableRegions({
						requestingUserId: userId,
						guildId,
						guildFeatures: new Set(guild.features ?? []),
					});
					const regionAllowed = availableRegions.some((region) => region.id === data.rtc_region && region.isAccessible);
					if (!regionAllowed) {
						throw InputValidationError.fromCode('rtc_region', ValidationErrorCodes.INVALID_OR_RESTRICTED_RTC_REGION, {
							region: data.rtc_region ?? 'unknown',
						});
					}
				} else {
					const availableRegions = this.liveKitService.getRegionMetadata().map((region) => region.id);
					if (availableRegions.length > 0 && !availableRegions.includes(data.rtc_region)) {
						throw InputValidationError.fromCode('rtc_region', ValidationErrorCodes.INVALID_RTC_REGION, {
							region: data.rtc_region ?? 'unknown',
							availableRegions: availableRegions.join(', '),
						});
					}
				}
			}
		}
		const previousPermissionOverwrites = channel.permissionOverwrites;
		let permissionOverwrites = channel.permissionOverwrites;
		if (data.permission_overwrites !== undefined) {
			const guildId = createGuildID(BigInt(guild.id));
			await checkPermission(Permissions.MANAGE_ROLES);
			const isOwner = guild.owner_id === userId.toString();
			const channelPermissions = await this.gatewayService.getUserPermissions({
				guildId,
				userId,
				channelId: channel.id,
			});
			if (!isOwner) {
				for (const overwrite of data.permission_overwrites ?? []) {
					const allowPerms = (overwrite.allow ? BigInt(overwrite.allow) : 0n) & ALL_PERMISSIONS;
					if ((allowPerms & ~channelPermissions) !== 0n) {
						throw new MissingPermissionsError();
					}
				}
				const nextDeny = new Map<RoleID | UserID, bigint>();
				for (const overwrite of data.permission_overwrites ?? []) {
					const targetKey = overwrite.type === 0 ? createRoleID(overwrite.id) : createUserID(overwrite.id);
					nextDeny.set(targetKey, (overwrite.deny ? BigInt(overwrite.deny) : 0n) & ALL_PERMISSIONS);
				}
				for (const [targetId, existing] of previousPermissionOverwrites ?? []) {
					const removedDeny = existing.deny & ~(nextDeny.get(targetId) ?? 0n);
					if ((removedDeny & ~channelPermissions) !== 0n) {
						throw new MissingPermissionsError();
					}
				}
			}
			permissionOverwrites = new Map();
			for (const overwrite of data.permission_overwrites ?? []) {
				const targetId = overwrite.type === 0 ? createRoleID(overwrite.id) : createUserID(overwrite.id);
				const existing = previousPermissionOverwrites?.get(targetId);
				const protectedBits = applyProtectedOverwriteBits(
					{
						allow: (overwrite.allow ? BigInt(overwrite.allow) : 0n) & ALL_PERMISSIONS,
						deny: (overwrite.deny ? BigInt(overwrite.deny) : 0n) & ALL_PERMISSIONS,
					},
					{
						allow: existing?.allow ?? 0n,
						deny: existing?.deny ?? 0n,
					},
					clientFeatures,
				);
				permissionOverwrites.set(
					targetId,
					new ChannelPermissionOverwrite({
						type: overwrite.type,
						allow_: protectedBits.allow,
						deny_: protectedBits.deny,
					}),
				);
			}
		}
		const requestedParentId =
			data.parent_id !== undefined ? (data.parent_id ? createChannelID(data.parent_id) : null) : channel.parentId;
		if (data.parent_id !== undefined) {
			await this.validateParentCategory({
				guildId: guildIdValue,
				channel,
				parentId: requestedParentId,
				validateCapacity: requestedParentId !== null && requestedParentId !== (channel.parentId ?? null),
			});
		}
		const updatedChannelData = {
			...channel.toRow(),
			name: channelName,
			topic: data.topic !== undefined ? data.topic : channel.topic,
			url: data.url !== undefined && channel.type === ChannelTypes.GUILD_LINK ? data.url : channel.url,
			parent_id: requestedParentId,
			bitrate: data.bitrate !== undefined && channel.type === ChannelTypes.GUILD_VOICE ? data.bitrate : channel.bitrate,
			user_limit:
				data.user_limit !== undefined && channel.type === ChannelTypes.GUILD_VOICE
					? data.user_limit
					: channel.userLimit,
			voice_connection_limit:
				data.voice_connection_limit !== undefined && channel.type === ChannelTypes.GUILD_VOICE
					? data.voice_connection_limit
					: channel.voiceConnectionLimit,
			rate_limit_per_user:
				data.rate_limit_per_user !== undefined && GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)
					? data.rate_limit_per_user
					: channel.rateLimitPerUser,
			nsfw: resolveNsfwOverrideWrite(channel, data),
			content_warning_level: resolveContentWarningLevelWrite(channel, data),
			content_warning_text: resolveContentWarningTextWrite(channel, data),
			rtc_region:
				data.rtc_region !== undefined && channel.type === ChannelTypes.GUILD_VOICE
					? data.rtc_region
					: channel.rtcRegion,
			permission_overwrites: new Map(
				Array.from(permissionOverwrites.entries()).map(([targetId, overwrite]) => [
					targetId,
					overwrite.toPermissionOverwrite(),
				]),
			),
		};
		const updatedChannel = await this.channelRepository.channelData.upsert(updatedChannelData);
		if (
			data.rate_limit_per_user !== undefined &&
			GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type) &&
			data.rate_limit_per_user !== channel.rateLimitPerUser
		) {
			try {
				await this.rateLimitService.clearLimitsByIdentifierPrefix(`slowmode:${channelId}:`);
			} catch (error) {
				Logger.error(
					{error, channelId: channelId.toString()},
					'Failed to clear slowmode rate-limit state on channel edit',
				);
			}
		}
		await this.channelUtilsService.dispatchChannelUpdate({channel: updatedChannel, requestCache});
		if (channel.type === ChannelTypes.GUILD_CATEGORY && data.permission_overwrites !== undefined && guild) {
			await this.propagatePermissionsToSyncedChildren({
				categoryChannel: updatedChannel,
				previousPermissionOverwrites,
				guildId: createGuildID(BigInt(guild.id)),
				requestCache,
			});
		}
		if (
			data.rtc_region !== undefined &&
			channel.type === ChannelTypes.GUILD_VOICE &&
			data.rtc_region !== channel.rtcRegion &&
			this.voiceRoomStore
		) {
			await this.handleRtcRegionSwitch({
				guildId: createGuildID(BigInt(guild.id)),
				channelId,
			});
		}
		const beforeSnapshot = serializeChannelForAudit(channel);
		const afterSnapshot = serializeChannelForAudit(updatedChannel);
		const changes = this.guildAuditLogService.computeChanges(beforeSnapshot, afterSnapshot);
		if (changes.length > 0) {
			const builder = this.guildAuditLogService
				.createBuilder(guildIdValue, userId)
				.withAction(AuditLogActionType.CHANNEL_UPDATE, channel.id.toString())
				.withReason(null)
				.withMetadata({
					type: updatedChannel.type.toString(),
				})
				.withChanges(changes);
			try {
				await builder.commit();
			} catch (error) {
				Logger.error(
					{
						error,
						guildId: guildIdValue.toString(),
						userId: userId.toString(),
						action: AuditLogActionType.CHANNEL_UPDATE,
						targetId: channel.id.toString(),
					},
					'Failed to record guild audit log',
				);
			}
			if (data.name !== undefined && channel.name !== updatedChannel.name) {
			}
		}
		if (data.permission_overwrites !== undefined) {
			await this.guildAuditLogService.recordPermissionOverwriteDiff({
				guildId: guildIdValue,
				userId,
				channelId: updatedChannel.id,
				previous: previousPermissionOverwrites,
				next: updatedChannel.permissionOverwrites,
			});
		}
		return updatedChannel;
	}

	async deleteChannel({
		userId,
		channelId,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		requestCache: RequestCache;
	}): Promise<void> {
		const {channel, guild, checkPermission} = await this.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			skipNsfwValidation: true,
		});
		if (this.channelAuthService.isPersonalNotesChannel({userId, channelId})) {
			throw new CannotExecuteOnDmError();
		}
		if (guild) {
			await checkPermission(Permissions.MANAGE_CHANNELS);
			const guildId = createGuildID(BigInt(guild.id));
			if (channel.type === ChannelTypes.GUILD_CATEGORY) {
				const guildChannels = await this.channelRepository.channelData.listGuildChannels(guildId);
				const childChannels = guildChannels.filter((ch: Channel) => ch.parentId === channelId);
				for (const childChannel of childChannels) {
					const updatedChild = await this.channelRepository.channelData.upsert({
						...childChannel.toRow(),
						parent_id: null,
					});
					await this.channelUtilsService.dispatchChannelUpdate({channel: updatedChild, requestCache});
				}
			}
			const [channelInvites, channelWebhooks] = await Promise.all([
				this.inviteRepository.listChannelInvites(channelId),
				this.webhookRepository.listByChannel(channelId),
			]);
			await Promise.all([
				...channelInvites.map((invite) => this.inviteRepository.delete(invite.code)),
				...channelWebhooks.map((webhook) => this.webhookRepository.delete(webhook.id)),
			]);
			await this.channelUtilsService.purgeChannelAttachments(channel);
			await this.channelRepository.messages.deleteAllChannelMessages(channelId);
			await deleteChannelMessageSearchDocuments(channelId, {context: {source: 'channel_delete'}});
			await this.channelUtilsService.dispatchChannelDelete({channel, requestCache});
			const guildIdValue = createGuildID(BigInt(guild.id));
			const changes = this.guildAuditLogService.computeChanges(ChannelHelpers.serializeChannelForAudit(channel), null);
			const builder = this.guildAuditLogService
				.createBuilder(guildIdValue, userId)
				.withAction(AuditLogActionType.CHANNEL_DELETE, channel.id.toString())
				.withReason(null)
				.withMetadata({
					type: channel.type.toString(),
				})
				.withChanges(changes);
			try {
				await builder.commit();
			} catch (error) {
				Logger.error(
					{
						error,
						guildId: guildIdValue.toString(),
						userId: userId.toString(),
						action: AuditLogActionType.CHANNEL_DELETE,
						targetId: channel.id.toString(),
					},
					'Failed to record guild audit log',
				);
			}
			await this.channelRepository.channelData.delete(channelId, guildId);
			const guildModel = await this.guildRepository.findUnique(guildId);
			if (guildModel) {
				const guildRow = guildModel.toRow();
				const patch: Partial<typeof guildRow> = {};
				if (guildRow.system_channel_id === channelId) patch.system_channel_id = null;
				if (guildRow.rules_channel_id === channelId) patch.rules_channel_id = null;
				if (guildRow.afk_channel_id === channelId) patch.afk_channel_id = null;
				if (Object.keys(patch).length > 0) {
					const updatedGuild = await this.guildRepository.upsertPartial(guildId, patch, guildRow);
					await this.gatewayService.dispatchGuild({
						guildId,
						event: 'GUILD_UPDATE',
						data: mapGuildToGuildResponse(updatedGuild),
					});
				}
			}
		} else {
			await this.userRepository.closeDmForUser(userId, channelId);
			await this.channelUtilsService.dispatchDmChannelDelete({channel, userId, requestCache});
		}
	}

	async getAvailableRtcRegions({
		userId,
		channelId,
	}: {
		userId: UserID;
		channelId: ChannelID;
	}): Promise<Array<VoiceRegionAvailability>> {
		if (this.voiceAvailabilityService === null) {
			return [];
		}
		const {channel, guild} = await this.channelAuthService.getChannelAuthenticated({userId, channelId});
		if (channel.type !== ChannelTypes.GUILD_VOICE) {
			throw new InvalidChannelTypeError();
		}
		if (!guild) {
			return [];
		}
		const guildId = createGuildID(BigInt(guild.id));
		const regions = this.voiceAvailabilityService.getAvailableRegions({
			requestingUserId: userId,
			guildId,
			guildFeatures: new Set(guild.features ?? []),
		});
		const accessibleRegions = regions.filter((region) => region.isAccessible);
		return accessibleRegions.sort((a, b) => a.name.localeCompare(b.name));
	}

	private async propagatePermissionsToSyncedChildren({
		categoryChannel,
		previousPermissionOverwrites,
		guildId,
		requestCache,
	}: {
		categoryChannel: Channel;
		previousPermissionOverwrites: Map<RoleID | UserID, ChannelPermissionOverwrite>;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<void> {
		const guildChannels = await this.channelRepository.channelData.listGuildChannels(guildId);
		const childChannels = guildChannels.filter((ch: Channel) => ch.parentId === categoryChannel.id);
		const syncedChannels: Array<Channel> = [];
		for (const child of childChannels) {
			if (this.arePermissionsEqual(child.permissionOverwrites, previousPermissionOverwrites)) {
				syncedChannels.push(child);
			}
		}
		if (syncedChannels.length > 0) {
			await Promise.all(
				syncedChannels.map(async (child) => {
					const updatedChild = await this.channelRepository.channelData.upsert({
						...child.toRow(),
						permission_overwrites: new Map(
							Array.from(categoryChannel.permissionOverwrites.entries()).map(([targetId, overwrite]) => [
								targetId,
								overwrite.toPermissionOverwrite(),
							]),
						),
					});
					await this.channelUtilsService.dispatchChannelUpdate({channel: updatedChild, requestCache});
				}),
			);
		}
	}

	private arePermissionsEqual(
		perms1: Map<RoleID | UserID, ChannelPermissionOverwrite>,
		perms2: Map<RoleID | UserID, ChannelPermissionOverwrite>,
	): boolean {
		if (perms1.size !== perms2.size) return false;
		for (const [targetId, overwrite1] of perms1.entries()) {
			const overwrite2 = perms2.get(targetId);
			if (!overwrite2) return false;
			if (
				overwrite1.type !== overwrite2.type ||
				overwrite1.allow !== overwrite2.allow ||
				overwrite1.deny !== overwrite2.deny
			) {
				return false;
			}
		}
		return true;
	}

	private async handleRtcRegionSwitch({guildId, channelId}: {guildId: GuildID; channelId: ChannelID}): Promise<void> {
		if (!this.voiceRoomStore) {
			Logger.warn('[ChannelOperationsService] VoiceRoomStore not available, skipping region switch');
			return;
		}
		await this.voiceRoomStore.deleteRoomServer(guildId, channelId);
		await this.gatewayService.switchVoiceRegion({guildId, channelId});
	}

	private async ensureCategoryHasCapacity(params: {guildId: GuildID; categoryId: ChannelID}): Promise<void> {
		const count = await this.gatewayService.getCategoryChannelCount(params);
		let maxChannels = MAX_CHANNELS_PER_CATEGORY;
		const guild = await this.guildRepository.findUnique(params.guildId);
		const ctx = createLimitMatchContext({user: null, guildFeatures: guild?.features ?? null});
		const resolved = resolveLimit(this.limitConfigService.getConfigSnapshot(), ctx, 'max_channels_per_category', {
			evaluationContext: 'guild',
		});
		if (Number.isFinite(resolved) && resolved >= 0) {
			maxChannels = Math.floor(resolved);
		}
		if (count >= maxChannels) {
			throw new MaxCategoryChannelsError(maxChannels);
		}
	}

	private async validateParentCategory(params: {
		guildId: GuildID;
		channel: Channel;
		parentId: ChannelID | null;
		validateCapacity: boolean;
	}): Promise<void> {
		if (params.parentId === null) {
			return;
		}
		if (params.channel.type === ChannelTypes.GUILD_CATEGORY) {
			throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.CATEGORIES_CANNOT_HAVE_PARENTS);
		}
		const guildChannels = await this.channelRepository.channelData.listGuildChannels(params.guildId);
		const parentChannel = guildChannels.find((channel) => channel.id === params.parentId);
		if (!parentChannel) {
			throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.INVALID_PARENT_CHANNEL);
		}
		if (parentChannel.type !== ChannelTypes.GUILD_CATEGORY) {
			throw InputValidationError.fromCode('parent_id', ValidationErrorCodes.PARENT_MUST_BE_CATEGORY);
		}
		if (params.validateCapacity) {
			await this.ensureCategoryHasCapacity({guildId: params.guildId, categoryId: params.parentId});
		}
	}

	async setChannelPermissionOverwrite(params: {
		userId: UserID;
		channelId: ChannelID;
		overwriteId: bigint;
		overwrite: {
			type: number;
			allow_: bigint;
			deny_: bigint;
		};
		clientFeatures: ReadonlySet<string>;
		requestCache: RequestCache;
	}): Promise<void> {
		const channel = await this.channelRepository.channelData.findUnique(params.channelId);
		if (!channel || !channel.guildId) throw new UnknownChannelError();
		const canManageRoles = await this.gatewayService.checkPermission({
			guildId: channel.guildId,
			userId: params.userId,
			channelId: channel.id,
			permission: Permissions.MANAGE_ROLES,
		});
		if (!canManageRoles) throw new MissingPermissionsError();
		const userPermissions = await this.gatewayService.getUserPermissions({
			guildId: channel.guildId,
			userId: params.userId,
			channelId: channel.id,
		});
		const targetId = params.overwrite.type === 0 ? createRoleID(params.overwriteId) : createUserID(params.overwriteId);
		const existing = channel.permissionOverwrites?.get(targetId);
		const protectedBits = applyProtectedOverwriteBits(
			{
				allow: params.overwrite.allow_ & ALL_PERMISSIONS,
				deny: params.overwrite.deny_ & ALL_PERMISSIONS,
			},
			{
				allow: existing?.allow ?? 0n,
				deny: existing?.deny ?? 0n,
			},
			params.clientFeatures,
		);
		const sanitizedAllow = protectedBits.allow;
		const sanitizedDeny = protectedBits.deny;
		const hasAdministrator = (userPermissions & Permissions.ADMINISTRATOR) !== 0n;
		if (!hasAdministrator && (sanitizedAllow & ~userPermissions) !== 0n) throw new MissingPermissionsError();
		const removedDeny = (existing?.deny ?? 0n) & ~sanitizedDeny;
		if (!hasAdministrator && (removedDeny & ~userPermissions) !== 0n) throw new MissingPermissionsError();
		const previousPermissionOverwrites = channel.permissionOverwrites;
		const overwrites = new Map(channel.permissionOverwrites ?? []);
		overwrites.set(
			targetId,
			new ChannelPermissionOverwrite({
				type: params.overwrite.type,
				allow_: sanitizedAllow,
				deny_: sanitizedDeny,
			}),
		);
		const updated = await this.channelRepository.channelData.upsert({
			...channel.toRow(),
			permission_overwrites: new Map(
				Array.from(overwrites.entries()).map(([id, ow]) => [id, ow.toPermissionOverwrite()]),
			),
		});
		await this.channelUtilsService.dispatchChannelUpdate({channel: updated, requestCache: params.requestCache});
		if (channel.type === ChannelTypes.GUILD_CATEGORY) {
			await this.propagatePermissionsToSyncedChildren({
				categoryChannel: updated,
				previousPermissionOverwrites,
				guildId: channel.guildId,
				requestCache: params.requestCache,
			});
		}
		const previousSnapshot = existing
			? {
					id: params.overwriteId.toString(),
					type: existing.type.toString(),
					allow: existing.allow.toString(),
					deny: existing.deny.toString(),
				}
			: null;
		const nextSnapshot = {
			id: params.overwriteId.toString(),
			type: params.overwrite.type.toString(),
			allow: sanitizedAllow.toString(),
			deny: sanitizedDeny.toString(),
		};
		const changes = this.guildAuditLogService.computeChanges(previousSnapshot, nextSnapshot);
		if (changes.length > 0) {
			const action = existing
				? AuditLogActionType.CHANNEL_OVERWRITE_UPDATE
				: AuditLogActionType.CHANNEL_OVERWRITE_CREATE;
			const builder = this.guildAuditLogService
				.createBuilder(channel.guildId, params.userId)
				.withAction(action, params.overwriteId.toString())
				.withReason(null)
				.withMetadata({
					id: params.overwriteId.toString(),
					type: params.overwrite.type.toString(),
					channel_id: channel.id.toString(),
				})
				.withChanges(changes);
			try {
				await builder.commit();
			} catch (error) {
				Logger.error(
					{
						error,
						guildId: channel.guildId.toString(),
						userId: params.userId.toString(),
						action,
						targetId: params.overwriteId.toString(),
					},
					'Failed to record guild audit log',
				);
			}
		}
	}

	async deleteChannelPermissionOverwrite(params: {
		userId: UserID;
		channelId: ChannelID;
		overwriteId: bigint;
		requestCache: RequestCache;
	}): Promise<void> {
		const channel = await this.channelRepository.channelData.findUnique(params.channelId);
		if (!channel || !channel.guildId) throw new UnknownChannelError();
		const canManageRoles = await this.gatewayService.checkPermission({
			guildId: channel.guildId,
			userId: params.userId,
			channelId: channel.id,
			permission: Permissions.MANAGE_ROLES,
		});
		if (!canManageRoles) throw new MissingPermissionsError();
		const previousPermissionOverwrites = channel.permissionOverwrites;
		const overwrites = new Map(channel.permissionOverwrites ?? []);
		const removedRole = overwrites.get(createRoleID(params.overwriteId));
		const removedUser = overwrites.get(createUserID(params.overwriteId));
		const removed = removedRole ?? removedUser;
		if (removed) {
			const userPermissions = await this.gatewayService.getUserPermissions({
				guildId: channel.guildId,
				userId: params.userId,
				channelId: channel.id,
			});
			const hasAdministrator = (userPermissions & Permissions.ADMINISTRATOR) !== 0n;
			if (!hasAdministrator && (removed.deny & ~userPermissions) !== 0n) throw new MissingPermissionsError();
		}
		overwrites.delete(createRoleID(params.overwriteId));
		overwrites.delete(createUserID(params.overwriteId));
		const updated = await this.channelRepository.channelData.upsert({
			...channel.toRow(),
			permission_overwrites: new Map(
				Array.from(overwrites.entries()).map(([id, ow]) => [id, ow.toPermissionOverwrite()]),
			),
		});
		await this.channelUtilsService.dispatchChannelUpdate({channel: updated, requestCache: params.requestCache});
		if (channel.type === ChannelTypes.GUILD_CATEGORY) {
			await this.propagatePermissionsToSyncedChildren({
				categoryChannel: updated,
				previousPermissionOverwrites,
				guildId: channel.guildId,
				requestCache: params.requestCache,
			});
		}
		if (removed) {
			const previousSnapshot = {
				id: params.overwriteId.toString(),
				type: removed.type.toString(),
				allow: removed.allow.toString(),
				deny: removed.deny.toString(),
			};
			const changes = this.guildAuditLogService.computeChanges(previousSnapshot, null);
			const builder = this.guildAuditLogService
				.createBuilder(channel.guildId, params.userId)
				.withAction(AuditLogActionType.CHANNEL_OVERWRITE_DELETE, params.overwriteId.toString())
				.withReason(null)
				.withMetadata({
					id: params.overwriteId.toString(),
					type: removed.type.toString(),
					channel_id: channel.id.toString(),
				})
				.withChanges(changes);
			try {
				await builder.commit();
			} catch (error) {
				Logger.error(
					{
						error,
						guildId: channel.guildId.toString(),
						userId: params.userId.toString(),
						action: AuditLogActionType.CHANNEL_OVERWRITE_DELETE,
						targetId: params.overwriteId.toString(),
					},
					'Failed to record guild audit log',
				);
			}
		}
	}
}

function isWritableGuildChannel(type: number): boolean {
	return (
		type === ChannelTypes.GUILD_TEXT ||
		type === ChannelTypes.GUILD_VOICE ||
		type === ChannelTypes.GUILD_LINK ||
		type === ChannelTypes.GUILD_CATEGORY
	);
}

function resolveNsfwOverrideWrite(channel: Channel, data: ChannelUpdateData): boolean | null {
	if (!isWritableGuildChannel(channel.type)) {
		return channel.nsfwOverride;
	}
	if (data.nsfw_override !== undefined) {
		return data.nsfw_override;
	}
	if (data.nsfw !== undefined) {
		return data.nsfw === true ? true : null;
	}
	return channel.nsfwOverride;
}

function resolveContentWarningLevelWrite(channel: Channel, data: ChannelUpdateData): number {
	if (!isWritableGuildChannel(channel.type)) {
		return channel.contentWarningLevel;
	}
	if (data.content_warning_level === undefined) {
		return channel.contentWarningLevel;
	}
	return data.content_warning_level === ContentWarningLevel.CONTENT_WARNING
		? ContentWarningLevel.CONTENT_WARNING
		: ContentWarningLevel.INHERIT;
}

function resolveContentWarningTextWrite(channel: Channel, data: ChannelUpdateData): string | null {
	if (!isWritableGuildChannel(channel.type)) {
		return channel.contentWarningText;
	}
	if (data.content_warning_text === undefined) {
		return channel.contentWarningText;
	}
	const trimmed = data.content_warning_text == null ? null : data.content_warning_text.trim();
	return trimmed && trimmed.length > 0 ? trimmed : null;
}
