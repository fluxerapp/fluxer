// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, RoleID, UserID} from '@app/api/BrandedTypes';
import {createChannelID, createGuildID, createRoleID, createUserID} from '@app/api/BrandedTypes';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import {
	enqueueChannelFollowerRemoval,
	scheduleDeletedChannelFollowerRemoval,
	withChannelFollowLock,
} from '@app/api/channel/services/ChannelFollowers';
import type {ChannelAuthService} from '@app/api/channel/services/channel_data/ChannelAuthService';
import type {ChannelUtilsService} from '@app/api/channel/services/channel_data/ChannelUtilsService';
import {dispatchThreadEvents} from '@app/api/channel/services/thread/ThreadDispatch';
import {
	loadConvertibleParentThreads,
	PARENT_CONVERSION_LOCK_TTL_SECONDS,
	retypedThreadEvents,
	retypeParentThreads,
} from '@app/api/channel/services/thread/ThreadParentConversion';
import {
	buildThreadParentPatch,
	loadThreadParentConfig,
	serializeThreadParentForAudit,
	type ThreadParentSettingsInput,
} from '@app/api/channel/services/thread/ThreadParentSettings';
import {enqueueDeleteChannelThreads} from '@app/api/channel/threads/ThreadJobs';
import {
	everEnabled,
	guildActive,
	isTainted,
	THREAD_FEATURE_CHANNEL_TYPES,
	type ThreadViewer,
	viewerActive,
} from '@app/api/experiment/ChannelThreadsGate';
import {resolveExperimentTargeting} from '@app/api/experiment/ExperimentTargeting';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import {mapGuildToGuildResponse} from '@app/api/guild/GuildModel';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import {ChannelHelpers} from '@app/api/guild/services/channel/ChannelHelpers';
import {createGuildMfaEnforcer} from '@app/api/guild/services/GuildMfaEnforcement';
import {hasThreadPermissionBits, resolveProtectedBitActor} from '@app/api/guild/services/ThreadPermissionBits';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import type {IInviteRepository} from '@app/api/invite/IInviteRepository';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {Channel} from '@app/api/models/Channel';
import {ChannelPermissionOverwrite} from '@app/api/models/ChannelPermissionOverwrite';
import type {ThreadState} from '@app/api/models/ThreadState';
import {deleteChannelMessageSearchDocuments} from '@app/api/search/MessageSearchIndexCleanup';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {serializeChannelForAudit} from '@app/api/utils/AuditSerializationUtils';
import {applyProtectedOverwriteBits, permissionWriteMask, protectedThreadBits} from '@app/api/utils/featureUtils';
import {overwriteGrantedBits} from '@app/api/utils/PermissionUtils';
import type {VoiceAvailabilityService} from '@app/api/voice/VoiceAvailabilityService';
import type {VoiceRegionAvailability} from '@app/api/voice/VoiceModel';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {
	ANNOUNCEMENT_CONVERTIBLE_CHANNEL_TYPES,
	ChannelTypes,
	GUILD_TEXT_BASED_CHANNEL_TYPES,
	Permissions,
	WebhookTypes,
} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel, clampVoiceChannelBitrate, GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {MAX_CHANNELS_PER_CATEGORY} from '@fluxer/constants/src/LimitConstants';
import {THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {withImplicitThreadBits} from '@fluxer/constants/src/ThreadPermissionUtils';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ChannelHasFollowedChannelsError} from '@fluxer/errors/src/domains/channel/ChannelHasFollowedChannelsError';
import {ChannelTypeConversionNotSupportedError} from '@fluxer/errors/src/domains/channel/ChannelTypeConversionNotSupportedError';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {MaxCategoryChannelsError} from '@fluxer/errors/src/domains/channel/MaxCategoryChannelsError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {CannotExecuteOnDmError} from '@fluxer/errors/src/domains/core/CannotExecuteOnDmError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import {resolveVoiceP2pAssignment} from '@fluxer/schema/src/domains/admin/VoiceP2pSchemas';
import {ChannelNameType} from '@fluxer/schema/src/primitives/ChannelValidators';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

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
	rtc_p2p?: boolean;
	icon?: string | null;
	owner_id?: bigint | null;
	nicks?: Record<string, string | null> | null;
}

function assertOverwriteTarget(channel: Channel, viewer: ThreadViewer | undefined): void {
	if (!THREAD_FEATURE_CHANNEL_TYPES.has(channel.type)) return;
	const active = viewer !== undefined && channel.guildId !== null && viewerActive(viewer, channel.guildId);
	if (!active) throw new UnknownChannelError();
	if (channel.isThread()) throw new InvalidChannelTypeError();
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
		private cacheService: ICacheService,
		private snowflakeService: ISnowflakeService,
	) {}

	private async assertVoiceP2pAssigned(userId: UserID, countryCode: string | null): Promise<void> {
		const config = await getInstanceConfigRepository().getVoiceP2pConfig();
		const user = config.enabled ? await this.userRepository.findUnique(userId) : null;
		if (
			user === null ||
			!resolveVoiceP2pAssignment(
				config,
				userId.toString(),
				await resolveExperimentTargeting(user, countryCode, [config]),
			).enabled
		) {
			throw InputValidationError.fromCode('rtc_p2p', ValidationErrorCodes.GUILD_FEATURE_NOT_TOGGLEABLE);
		}
	}

	async getChannel({
		userId,
		viewer,
		channelId,
		skipNsfwValidation,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		skipNsfwValidation?: boolean;
	}): Promise<Channel> {
		const {channel} = await this.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
			skipNsfwValidation,
		});
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
		viewer,
		channelId,
		data,
		clientFeatures,
		requestCache,
		auditLogReason,
		typeConversion,
		threadParent,
		countryCode,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		data: ChannelUpdateData;
		clientFeatures: ReadonlySet<string>;
		requestCache: RequestCache;
		auditLogReason: string | null;
		typeConversion?: ChannelTypeConversion | null;
		threadParent?: ThreadParentSettingsInput | null;
		countryCode?: string | null;
	}): Promise<Channel> {
		const {channel, guild, checkPermission} = await this.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
			skipNsfwValidation: true,
		});
		if (channel.type === ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeError();
		}
		if (!guild) throw new MissingPermissionsError();
		await checkPermission(Permissions.MANAGE_CHANNELS);
		const nextType = resolveNextChannelType(channel, typeConversion ?? null);
		const guildIdValue = createGuildID(BigInt(guild.id));
		const parentConfig = await loadThreadParentConfig(this.channelRepository.threads, channel);
		const parentPatch =
			threadParent && guildActive(guildIdValue)
				? await buildThreadParentPatch({
						channelType: channel.type,
						guildId: guildIdValue,
						input: threadParent,
						current: parentConfig,
						guildRepository: this.guildRepository,
						generateId: () => this.snowflakeService.generate(),
					})
				: null;
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
		if (data.name !== undefined && (isTextNamedChannelType(channel.type) || channel.isThreadOnly())) {
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
		if (data.rtc_p2p === true && !channel.rtcP2p && channel.type === ChannelTypes.GUILD_VOICE) {
			await this.assertVoiceP2pAssigned(userId, countryCode ?? null);
		}
		const previousPermissionOverwrites = channel.permissionOverwrites;
		let permissionOverwrites = channel.permissionOverwrites;
		if (data.permission_overwrites !== undefined) {
			const guildId = createGuildID(BigInt(guild.id));
			await checkPermission(Permissions.MANAGE_ROLES);
			const isOwner = guild.owner_id === userId.toString();
			const gatewayPermissions = await this.gatewayService.getUserPermissions({
				guildId,
				userId,
				channelId: channel.id,
			});
			const actor = await resolveProtectedBitActor({
				guildId,
				userId,
				clientFeatures,
				viewer,
				isBot: async () => viewer.kind === 'user' && viewer.bot,
			});
			const writeMask = permissionWriteMask(actor);
			const channelPermissions = actor.threadBits ? withImplicitThreadBits(gatewayPermissions) : gatewayPermissions;
			permissionOverwrites = new Map();
			for (const overwrite of data.permission_overwrites ?? []) {
				const targetId = overwrite.type === 0 ? createRoleID(overwrite.id) : createUserID(overwrite.id);
				const existing = previousPermissionOverwrites?.get(targetId);
				const protectedBits = applyProtectedOverwriteBits(
					{
						allow: (overwrite.allow ? BigInt(overwrite.allow) : 0n) & writeMask,
						deny: (overwrite.deny ? BigInt(overwrite.deny) : 0n) & writeMask,
					},
					{
						allow: existing?.allow ?? 0n,
						deny: existing?.deny ?? 0n,
					},
					actor,
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
			const keptBits = protectedThreadBits(actor);
			if (keptBits !== 0n) {
				for (const [targetId, previous] of previousPermissionOverwrites ?? []) {
					if (permissionOverwrites.has(targetId)) continue;
					const allow = previous.allow & keptBits;
					const deny = previous.deny & keptBits;
					if (allow === 0n && deny === 0n) continue;
					permissionOverwrites.set(
						targetId,
						new ChannelPermissionOverwrite({type: previous.type, allow_: allow, deny_: deny}),
					);
				}
			}
			if (!isOwner) {
				const targetIds = new Set([...(previousPermissionOverwrites?.keys() ?? []), ...permissionOverwrites.keys()]);
				for (const targetId of targetIds) {
					const grantedBits = overwriteGrantedBits(
						previousPermissionOverwrites?.get(targetId),
						permissionOverwrites.get(targetId),
					);
					if ((grantedBits & ~keptBits & ~channelPermissions) !== 0n) {
						throw new MissingPermissionsError();
					}
				}
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
		let nextBitrate = channel.bitrate;
		if (data.bitrate !== undefined && channel.type === ChannelTypes.GUILD_VOICE) {
			nextBitrate = data.bitrate === null ? null : clampVoiceChannelBitrate(data.bitrate, guild.features ?? []);
		}
		const updatedChannelData = {
			...channel.toRow(),
			type: nextType,
			name: channelName,
			topic: data.topic !== undefined ? data.topic : channel.topic,
			url: data.url !== undefined && channel.type === ChannelTypes.GUILD_LINK ? data.url : channel.url,
			parent_id: requestedParentId,
			bitrate: nextBitrate,
			user_limit:
				data.user_limit !== undefined && channel.type === ChannelTypes.GUILD_VOICE
					? data.user_limit
					: channel.userLimit,
			voice_connection_limit:
				data.voice_connection_limit !== undefined && channel.type === ChannelTypes.GUILD_VOICE
					? data.voice_connection_limit
					: channel.voiceConnectionLimit,
			rate_limit_per_user:
				data.rate_limit_per_user !== undefined && acceptsRateLimit(channel.type)
					? data.rate_limit_per_user
					: channel.rateLimitPerUser,
			nsfw: resolveNsfwOverrideWrite(channel, data),
			content_warning_level: resolveContentWarningLevelWrite(channel, data),
			content_warning_text: resolveContentWarningTextWrite(channel, data),
			rtc_region:
				data.rtc_region !== undefined && channel.type === ChannelTypes.GUILD_VOICE
					? data.rtc_region
					: channel.rtcRegion,
			rtc_p2p: data.rtc_p2p !== undefined && channel.type === ChannelTypes.GUILD_VOICE ? data.rtc_p2p : channel.rtcP2p,
			permission_overwrites: new Map(
				Array.from(permissionOverwrites.entries()).map(([targetId, overwrite]) => [
					targetId,
					overwrite.toPermissionOverwrite(),
				]),
			),
		};
		let retypedThreads: Array<ThreadState> = [];
		const toAnnouncement =
			nextType === ChannelTypes.GUILD_ANNOUNCEMENT && channel.type !== ChannelTypes.GUILD_ANNOUNCEMENT;
		const updatedChannel =
			toAnnouncement || (nextType !== channel.type && everEnabled())
				? await withChannelFollowLock(
						this.cacheService,
						channelId,
						async () => {
							if (toAnnouncement) {
								const webhooks = await this.webhookRepository.listByChannel(channelId);
								if (webhooks.some((webhook) => webhook.type === WebhookTypes.CHANNEL_FOLLOWER)) {
									throw new ChannelHasFollowedChannelsError();
								}
							}
							const threads = await loadConvertibleParentThreads(
								this.channelRepository,
								guildIdValue,
								channel,
								nextType,
							);
							if (threads.length === 0) return this.channelRepository.channelData.upsert(updatedChannelData);
							const {parent, active} = await retypeParentThreads(this.channelRepository, threads, nextType, () =>
								this.channelRepository.channelData.upsert(updatedChannelData),
							);
							retypedThreads = active;
							return parent;
						},
						everEnabled() ? PARENT_CONVERSION_LOCK_TTL_SECONDS : undefined,
					)
				: await this.channelRepository.channelData.upsert(updatedChannelData);
		if (channel.type === ChannelTypes.GUILD_ANNOUNCEMENT && nextType !== ChannelTypes.GUILD_ANNOUNCEMENT) {
			await enqueueChannelFollowerRemoval({sourceChannelId: channelId, reason: 'converted'});
		}
		if (parentPatch) await this.channelRepository.threads.patchParentConfig(guildIdValue, channel.id, parentPatch);
		const nextParentConfig = parentPatch
			? await loadThreadParentConfig(this.channelRepository.threads, updatedChannel)
			: parentConfig;
		if (
			data.rate_limit_per_user !== undefined &&
			acceptsRateLimit(channel.type) &&
			data.rate_limit_per_user !== channel.rateLimitPerUser
		) {
			try {
				await this.rateLimitService.clearLimitsByIdentifierPrefix(`slowmode:${channelId}:`);
				if (everEnabled() && (await isTainted(guildIdValue))) {
					await this.rateLimitService.clearLimitsByIdentifierPrefix(`slowmode-thread:${channelId}:`);
				}
			} catch (error) {
				Logger.error(
					{error, channelId: channelId.toString()},
					'Failed to clear slowmode rate-limit state on channel edit',
				);
			}
		}
		await this.channelUtilsService.dispatchChannelUpdate({channel: updatedChannel, requestCache});
		await dispatchThreadEvents(
			this.gatewayService,
			guildIdValue,
			await retypedThreadEvents(this.channelRepository, updatedChannel, retypedThreads),
		);
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
		const beforeSnapshot = {
			...serializeChannelForAudit(channel),
			...serializeThreadParentForAudit(channel.type, parentConfig),
		};
		const afterSnapshot = {
			...serializeChannelForAudit(updatedChannel),
			...serializeThreadParentForAudit(updatedChannel.type, nextParentConfig),
		};
		const changes = this.guildAuditLogService.computeChanges(beforeSnapshot, afterSnapshot);
		if (changes.length > 0) {
			const builder = this.guildAuditLogService
				.createBuilder(guildIdValue, userId)
				.withAction(AuditLogActionType.CHANNEL_UPDATE, channel.id.toString())
				.withReason(auditLogReason)
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
		}
		if (data.permission_overwrites !== undefined) {
			await this.guildAuditLogService.recordPermissionOverwriteDiff({
				guildId: guildIdValue,
				userId,
				channelId: updatedChannel.id,
				previous: previousPermissionOverwrites,
				next: updatedChannel.permissionOverwrites,
				reason: auditLogReason,
			});
		}
		return updatedChannel;
	}

	async deleteChannel({
		userId,
		viewer,
		channelId,
		requestCache,
		auditLogReason,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<void> {
		const {channel, guild, checkPermission} = await this.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
			skipNsfwValidation: true,
		});
		if (this.channelAuthService.isPersonalNotesChannel({userId, channelId})) {
			throw new CannotExecuteOnDmError();
		}
		if (guild) {
			await checkPermission(Permissions.MANAGE_CHANNELS);
			const guildId = createGuildID(BigInt(guild.id));
			if (channel.type === ChannelTypes.GUILD_CATEGORY) {
				const guildChannels = await this.channelRepository.channelData.listGuildChannels(guildId, 'maintenance');
				const childChannels = guildChannels.filter((ch: Channel) => ch.parentId === channelId);
				for (const childChannel of childChannels) {
					const updatedChild = await this.channelRepository.channelData.upsert({
						...childChannel.toRow(),
						parent_id: null,
					});
					if (THREAD_ONLY_CHANNEL_TYPES.has(updatedChild.type) && !guildActive(guildId)) continue;
					await this.channelUtilsService.dispatchChannelUpdate({channel: updatedChild, requestCache});
				}
			}
			await scheduleDeletedChannelFollowerRemoval({
				channel,
				crossposts: this.channelRepository.crossposts,
				copyMode: 'source_deleted',
			});
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
				.withReason(auditLogReason)
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
			await this.channelRepository.channelData.delete(channelId, guildId, channel.type);
			if (channel.isThreadParent() && everEnabled() && (await isTainted(guildId, {fresh: true}))) {
				await enqueueDeleteChannelThreads(guildId, channelId);
				await this.channelRepository.threads.deleteParentConfig(guildId, channelId);
			}
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
		viewer,
		channelId,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
	}): Promise<Array<VoiceRegionAvailability>> {
		if (this.voiceAvailabilityService === null) {
			return [];
		}
		const {channel, guild} = await this.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
			skipNsfwValidation: true,
		});
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
		const guildChannels = await this.channelRepository.channelData.listGuildChannels(guildId, 'maintenance');
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
					if (THREAD_ONLY_CHANNEL_TYPES.has(updatedChild.type) && !guildActive(guildId)) return;
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
		const guildChannels = await this.channelRepository.channelData.listGuildChannels(params.guildId, 'enrolled');
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

	private async checkOverwritePermission(params: {
		guildId: GuildID;
		userId: UserID;
		channelId: ChannelID;
	}): Promise<void> {
		const canManageRoles = await this.gatewayService.checkPermission({
			guildId: params.guildId,
			userId: params.userId,
			channelId: params.channelId,
			permission: Permissions.MANAGE_ROLES,
		});
		if (!canManageRoles) throw new MissingPermissionsError();
		const guildData = await this.gatewayService.getGuildData({guildId: params.guildId, userId: params.userId});
		const enforceGuildMfa = await createGuildMfaEnforcer({
			userRepository: this.userRepository,
			guildData,
			userId: params.userId,
		});
		enforceGuildMfa(Permissions.MANAGE_ROLES);
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
		viewer?: ThreadViewer;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<void> {
		const channel = await this.channelRepository.channelData.findUnique(params.channelId);
		if (!channel?.guildId) throw new UnknownChannelError();
		assertOverwriteTarget(channel, params.viewer);
		await this.checkOverwritePermission({guildId: channel.guildId, userId: params.userId, channelId: channel.id});
		const userPermissions = await this.gatewayService.getUserPermissions({
			guildId: channel.guildId,
			userId: params.userId,
			channelId: channel.id,
		});
		const targetId = params.overwrite.type === 0 ? createRoleID(params.overwriteId) : createUserID(params.overwriteId);
		const existing = channel.permissionOverwrites?.get(targetId);
		const actor = await resolveProtectedBitActor({
			guildId: channel.guildId,
			userId: params.userId,
			clientFeatures: params.clientFeatures,
			viewer: params.viewer,
			isBot: async () => (await this.userRepository.findUnique(params.userId))?.isBot ?? false,
		});
		const writeMask = permissionWriteMask(actor);
		const protectedBits = applyProtectedOverwriteBits(
			{
				allow: params.overwrite.allow_ & writeMask,
				deny: params.overwrite.deny_ & writeMask,
			},
			{
				allow: existing?.allow ?? 0n,
				deny: existing?.deny ?? 0n,
			},
			actor,
		);
		const sanitizedAllow = protectedBits.allow;
		const sanitizedDeny = protectedBits.deny;
		const hasAdministrator = (userPermissions & Permissions.ADMINISTRATOR) !== 0n;
		const grantedBits = overwriteGrantedBits(existing, {allow: sanitizedAllow, deny: sanitizedDeny});
		const effectivePermissions = actor.threadBits ? withImplicitThreadBits(userPermissions) : userPermissions;
		if (!hasAdministrator && (grantedBits & ~effectivePermissions) !== 0n) throw new MissingPermissionsError();
		const previousPermissionOverwrites = channel.permissionOverwrites;
		const nextOverwrite = new ChannelPermissionOverwrite({
			type: params.overwrite.type,
			allow_: sanitizedAllow,
			deny_: sanitizedDeny,
		});
		const overwrites = new Map(channel.permissionOverwrites ?? []);
		overwrites.set(targetId, nextOverwrite);
		const updated = await this.channelRepository.channelData.upsert(
			{
				...channel.toRow(),
				permission_overwrites: new Map(
					Array.from(overwrites.entries()).map(([id, ow]) => [id, ow.toPermissionOverwrite()]),
				),
			},
			channel.toRow(),
		);
		await this.channelUtilsService.dispatchChannelUpdate({channel: updated, requestCache: params.requestCache});
		if (channel.type === ChannelTypes.GUILD_CATEGORY) {
			await this.propagatePermissionsToSyncedChildren({
				categoryChannel: updated,
				previousPermissionOverwrites,
				guildId: channel.guildId,
				requestCache: params.requestCache,
			});
		}
		await this.guildAuditLogService.recordPermissionOverwriteDiff({
			guildId: channel.guildId,
			userId: params.userId,
			channelId: channel.id,
			previous: existing ? new Map([[targetId, existing]]) : null,
			next: new Map([[targetId, nextOverwrite]]),
			reason: params.auditLogReason,
		});
	}

	async deleteChannelPermissionOverwrite(params: {
		userId: UserID;
		channelId: ChannelID;
		overwriteId: bigint;
		clientFeatures?: ReadonlySet<string>;
		viewer?: ThreadViewer;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<void> {
		const channel = await this.channelRepository.channelData.findUnique(params.channelId);
		if (!channel?.guildId) throw new UnknownChannelError();
		assertOverwriteTarget(channel, params.viewer);
		await this.checkOverwritePermission({guildId: channel.guildId, userId: params.userId, channelId: channel.id});
		const previousPermissionOverwrites = channel.permissionOverwrites;
		const overwrites = new Map(channel.permissionOverwrites ?? []);
		const removedRole = overwrites.get(createRoleID(params.overwriteId));
		const removedUser = overwrites.get(createUserID(params.overwriteId));
		const removed = removedRole ?? removedUser;
		const kept =
			removed && (hasThreadPermissionBits(removed.allow) || hasThreadPermissionBits(removed.deny))
				? protectedThreadBits(
						await resolveProtectedBitActor({
							guildId: channel.guildId,
							userId: params.userId,
							clientFeatures: params.clientFeatures ?? new Set(),
							viewer: params.viewer,
							isBot: async () => (await this.userRepository.findUnique(params.userId))?.isBot ?? false,
						}),
					)
				: 0n;
		if (removed) {
			const userPermissions = await this.gatewayService.getUserPermissions({
				guildId: channel.guildId,
				userId: params.userId,
				channelId: channel.id,
			});
			const hasAdministrator = (userPermissions & Permissions.ADMINISTRATOR) !== 0n;
			if (!hasAdministrator && (removed.deny & ~kept & ~userPermissions) !== 0n) throw new MissingPermissionsError();
		}
		overwrites.delete(createRoleID(params.overwriteId));
		overwrites.delete(createUserID(params.overwriteId));
		if (removed && kept !== 0n) {
			const removedTargetId = removed.type === 0 ? createRoleID(params.overwriteId) : createUserID(params.overwriteId);
			overwrites.set(
				removedTargetId,
				new ChannelPermissionOverwrite({
					type: removed.type,
					allow_: removed.allow & kept,
					deny_: removed.deny & kept,
				}),
			);
		}
		const updated = await this.channelRepository.channelData.upsert(
			{
				...channel.toRow(),
				permission_overwrites: new Map(
					Array.from(overwrites.entries()).map(([id, ow]) => [id, ow.toPermissionOverwrite()]),
				),
			},
			channel.toRow(),
		);
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
			const removedTargetId = removed.type === 0 ? createRoleID(params.overwriteId) : createUserID(params.overwriteId);
			await this.guildAuditLogService.recordPermissionOverwriteDiff({
				guildId: channel.guildId,
				userId: params.userId,
				channelId: channel.id,
				previous: new Map([[removedTargetId, removed]]),
				next: null,
				reason: params.auditLogReason,
			});
		}
	}
}

export interface ChannelTypeConversion {
	from: number;
	to: number;
}

function resolveNextChannelType(channel: Channel, typeConversion: ChannelTypeConversion | null): number {
	if (typeConversion === null || typeConversion.to === channel.type) {
		return channel.type;
	}
	if (
		typeConversion.from !== channel.type ||
		!ANNOUNCEMENT_CONVERTIBLE_CHANNEL_TYPES.has(channel.type) ||
		!ANNOUNCEMENT_CONVERTIBLE_CHANNEL_TYPES.has(typeConversion.to)
	) {
		throw new ChannelTypeConversionNotSupportedError();
	}
	return typeConversion.to;
}

function isTextNamedChannelType(type: number): boolean {
	return type === ChannelTypes.GUILD_TEXT || type === ChannelTypes.GUILD_ANNOUNCEMENT;
}

function isWritableGuildChannel(type: number): boolean {
	return (
		type === ChannelTypes.GUILD_TEXT ||
		type === ChannelTypes.GUILD_ANNOUNCEMENT ||
		type === ChannelTypes.GUILD_VOICE ||
		type === ChannelTypes.GUILD_LINK ||
		type === ChannelTypes.GUILD_CATEGORY ||
		THREAD_ONLY_CHANNEL_TYPES.has(type)
	);
}

function acceptsRateLimit(type: number): boolean {
	return GUILD_TEXT_BASED_CHANNEL_TYPES.has(type) || THREAD_ONLY_CHANNEL_TYPES.has(type);
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
