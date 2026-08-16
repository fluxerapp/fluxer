// SPDX-License-Identifier: AGPL-3.0-or-later

import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ALL_PERMISSIONS, ChannelTypes, DEFAULT_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {DiscoveryApplicationStatus} from '@fluxer/constants/src/DiscoveryConstants';
import {
	ContentWarningLevel,
	GuildFeatures,
	GuildNSFWLevel,
	GuildSplashCardAlignment,
	GuildVerificationLevel,
	JoinSourceTypes,
	SystemChannelFlags,
} from '@fluxer/constants/src/GuildConstants';
import {
	MAX_GUILD_CHANNELS,
	MAX_GUILD_ROLES,
	VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
} from '@fluxer/constants/src/LimitConstants';
import {DEFAULT_GUILD_FOLDER_ICON} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {BotsCannotCreateGuildsError} from '@fluxer/errors/src/domains/guild/BotsCannotCreateGuildsError';
import {GuildTemplateInvalidError} from '@fluxer/errors/src/domains/guild/GuildTemplateInvalidError';
import {MaxGuildsError} from '@fluxer/errors/src/domains/guild/MaxGuildsError';
import {UnclaimedAccountCannotCreateGuildsError} from '@fluxer/errors/src/domains/guild/UnclaimedAccountCannotCreateGuildsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {GuildCreateRequest, GuildUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {GuildPartialResponse, GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {
	TemplateChannel,
	TemplateRole,
	TemplateSerializedGuild,
} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import type {ChannelID, GuildID, RoleID, UserID} from '../../../BrandedTypes';
import {createChannelID, createGuildID, createRoleID, guildIdToRoleId} from '../../../BrandedTypes';
import type {IChannelRepository} from '../../../channel/IChannelRepository';
import type {ChannelService} from '../../../channel/services/ChannelService';
import {BatchBuilder} from '../../../database/CassandraQueryExecution';
import type {PermissionOverwrite} from '../../../database/types/ChannelTypes';
import type {GuildRow} from '../../../database/types/GuildTypes';
import {contentModerationService} from '../../../infrastructure/ContentModerationService';
import type {EntityAssetService, PreparedAssetUpload} from '../../../infrastructure/EntityAssetService';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {InviteRepository} from '../../../invite/InviteRepository';
import {Logger} from '../../../Logger';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import {Guild} from '../../../models/Guild';
import type {User} from '../../../models/User';
import {getGuildSearchService} from '../../../SearchFactory';
import type {GuildDiscoveryContext} from '../../../search/guild/GuildSearchSerializer';
import {deleteGuildMessageSearchDocuments} from '../../../search/MessageSearchIndexCleanup';
import {Channels, ChannelsByGuild, GuildMembers, GuildMembersByUserId, GuildRoles, Guilds} from '../../../Tables';
import type {IUserRepository} from '../../../user/IUserRepository';
import {mapUserSettingsToResponse} from '../../../user/UserMappers';
import {addGuildToUncategorizedFolder, removeGuildFromUserFolders} from '../../../user/utils/GuildFolderUtils';
import type {IWebhookRepository} from '../../../webhook/IWebhookRepository';
import {mapGuildToGuildResponse, mapGuildToPartialResponse} from '../../GuildModel';
import type {IGuildDiscoveryRepository} from '../../repositories/GuildDiscoveryRepository';
import type {IGuildRepositoryAggregate} from '../../repositories/IGuildRepositoryAggregate';
import type {GuildDataHelpers} from './GuildDataHelpers';

const DEFAULT_TEXT_CATEGORY_NAME = 'Text Channels';
const DEFAULT_VOICE_CATEGORY_NAME = 'Voice Channels';
const DEFAULT_TEXT_CHANNEL_NAME = 'general';
const DEFAULT_VOICE_CHANNEL_NAME = 'General';

interface PreparedGuildAssets {
	icon: PreparedAssetUpload | null;
	banner: PreparedAssetUpload | null;
	splash: PreparedAssetUpload | null;
	embed_splash: PreparedAssetUpload | null;
}

interface TemplateGuildSettings {
	verificationLevel: number;
	explicitContentFilter: number;
	defaultMessageNotifications: number;
	systemChannelFlags: number;
	afkTimeout: number;
}

interface MappedTemplateChannel {
	channel: TemplateChannel;
	channelId: ChannelID;
	fluxerType: number;
}

const BASE_GUILD_FEATURES: ReadonlyArray<string> = [
	GuildFeatures.ANIMATED_ICON,
	GuildFeatures.ANIMATED_BANNER,
	GuildFeatures.BANNER,
	GuildFeatures.INVITE_SPLASH,
];
const USER_TOGGLEABLE_GUILD_FEATURES: ReadonlySet<string> = new Set([
	GuildFeatures.INVITES_DISABLED,
	GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES,
	GuildFeatures.DETACHED_BANNER,
	GuildFeatures.CLONE_EMOJI_DISABLED,
	GuildFeatures.CLONE_STICKER_DISABLED,
	GuildFeatures.HIDE_OWNER_CROWN,
]);
const SUPPORTED_SYSTEM_CHANNEL_FLAGS = SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS;

function setsEqual(a: ReadonlySet<string>, b: ReadonlySet<string>): boolean {
	if (a === b) return true;
	if (a.size !== b.size) return false;
	for (const value of a) {
		if (!b.has(value)) return false;
	}
	return true;
}

const TEMPLATE_AFK_TIMEOUT_MIN_SECONDS = 60;
const TEMPLATE_AFK_TIMEOUT_MAX_SECONDS = 3600;
const THE_OTHER_PLATFORM_GUILD_ANNOUNCEMENT_CHANNEL_TYPE = 5;
const THE_OTHER_PLATFORM_GUILD_STAGE_VOICE_CHANNEL_TYPE = 13;

export class GuildOperationsService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly channelRepository: IChannelRepository,
		private readonly inviteRepository: InviteRepository,
		private readonly channelService: ChannelService,
		private readonly gatewayService: IGatewayService,
		private readonly entityAssetService: EntityAssetService,
		private readonly userRepository: IUserRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly webhookRepository: IWebhookRepository,
		private readonly helpers: GuildDataHelpers,
		private readonly limitConfigService: LimitConfigService,
		private readonly discoveryRepository: IGuildDiscoveryRepository,
	) {}

	async getGuild({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildResponse> {
		try {
			const guild = await this.gatewayService.getGuildData({guildId, userId});
			if (!guild) throw new UnknownGuildError();
			return guild;
		} catch (error) {
			if (this.isGuildAccessError(error)) {
				if (await this.guildExists(guildId)) {
					throw new AccessDeniedError();
				}
				throw new UnknownGuildError();
			}
			throw error;
		}
	}

	async getUserGuilds(
		userId: UserID,
		options?: {
			before?: GuildID;
			after?: GuildID;
			limit?: number;
			withCounts?: boolean;
		},
	): Promise<Array<GuildResponse>> {
		let guilds = await this.guildRepository.listUserGuilds(userId);
		guilds.sort((a, b) => (a.id < b.id ? -1 : 1));
		if (options?.after) {
			const index = guilds.findIndex((g) => g.id === options.after);
			if (index !== -1) {
				guilds = guilds.slice(index + 1);
			}
		} else if (options?.before) {
			const index = guilds.findIndex((g) => g.id === options.before);
			if (index !== -1) {
				guilds = guilds.slice(0, index);
			}
		}
		const limit = options?.limit ?? 200;
		guilds = guilds.slice(0, limit);
		const guildIds = guilds.map((g) => g.id);
		let permissionsMap = new Map<GuildID, bigint>();
		try {
			permissionsMap = await this.gatewayService.getUserPermissionsBatch({guildIds, userId});
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), guildCount: guildIds.length, error},
				'[GuildOperationsService] Failed to fetch guild permissions batch for list_guilds; returning without permissions',
			);
		}
		const responses = guilds.map((guild) => {
			const permissions = permissionsMap.get(guild.id);
			if (permissions == null) {
				return mapGuildToGuildResponse(guild);
			}
			return mapGuildToGuildResponse(guild, {permissions});
		});
		if (!options?.withCounts) {
			return responses;
		}
		const guildsWithCounts: Array<GuildResponse> = [];
		const countBatchSize = 25;
		for (let index = 0; index < guilds.length; index += countBatchSize) {
			const guildChunk = guilds.slice(index, index + countBatchSize);
			const responseChunk = responses.slice(index, index + countBatchSize);
			const batchResults = await Promise.all(
				guildChunk.map(async (guild, chunkIndex) => {
					const baseResponse = responseChunk[chunkIndex] ?? mapGuildToGuildResponse(guild);
					try {
						const counts = await this.gatewayService.getGuildCounts(guild.id);
						return {
							...baseResponse,
							approximate_member_count: counts.memberCount,
							approximate_presence_count: counts.presenceCount,
						};
					} catch (error) {
						Logger.warn(
							{guildId: guild.id.toString(), userId: userId.toString(), error},
							'[GuildOperationsService] Failed to fetch guild counts for list_guilds; returning without counts for guild',
						);
						return baseResponse;
					}
				}),
			);
			guildsWithCounts.push(...batchResults);
		}
		return guildsWithCounts;
	}

	async getPublicGuildData(guildId: GuildID): Promise<GuildPartialResponse> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) throw new UnknownGuildError();
		return mapGuildToPartialResponse(guild);
	}

	async getGuildSystem(guildId: GuildID): Promise<Guild> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) throw new UnknownGuildError();
		return guild;
	}

	async createGuild(
		params: {
			user: User;
			data: GuildCreateRequest;
			locale?: string | null;
		},
		_auditLogReason?: string | null,
	): Promise<GuildResponse> {
		const {user, data} = params;
		if (user.isBot) {
			throw new BotsCannotCreateGuildsError();
		}
		if (user.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotCreateGuildsError();
		}
		const currentGuildCount = await this.guildRepository.countUserGuilds(user.id);
		const ctx = createLimitMatchContext({user});
		const maxGuilds = resolveLimitSafe(
			this.limitConfigService.getConfigSnapshot(),
			ctx,
			'max_guilds',
			DEFAULT_STOCK_LIMITS.max_guilds,
		);
		if (currentGuildCount >= maxGuilds) throw new MaxGuildsError(maxGuilds);
		const guildId = createGuildID(await this.snowflakeService.generate());
		contentModerationService.scanText(data.name, {
			userId: user.id,
			guildId,
			channelId: null,
			messageId: null,
			surface: 'profile_field',
		});
		let preparedIcon: PreparedAssetUpload | null = null;
		if (data.icon) {
			preparedIcon = await this.entityAssetService.prepareAssetUpload({
				assetType: 'icon',
				entityType: 'guild',
				entityId: guildId,
				previousHash: null,
				base64Image: data.icon,
				errorPath: 'icon',
			});
		}
		const iconKey = preparedIcon?.newHash ?? null;
		const shouldUseEmptyFeatures = data.empty_features ?? false;
		const featuresSet = shouldUseEmptyFeatures ? new Set<string>() : new Set(BASE_GUILD_FEATURES);
		const templateSettings = this.sanitiseTemplateGuildSettings(data.template);
		const batch = new BatchBuilder();
		let systemChannelId: ChannelID;
		if (data.template) {
			const templateBatch = new BatchBuilder();
			const templateResult = await this.buildTemplateEntities(guildId, data.template, templateBatch);
			systemChannelId = templateResult.systemChannelId;
			await templateBatch.executeChunked(50);
		} else {
			const defaultResult = await this.buildDefaultEntities(guildId, batch);
			systemChannelId = defaultResult.systemChannelId;
		}
		const guildData: GuildRow = {
			guild_id: guildId,
			owner_id: user.id,
			name: data.name,
			vanity_url_code: null,
			icon_hash: iconKey,
			banner_hash: null,
			banner_width: null,
			banner_height: null,
			splash_hash: null,
			splash_width: null,
			splash_height: null,
			splash_card_alignment: GuildSplashCardAlignment.CENTER,
			embed_splash_hash: null,
			embed_splash_width: null,
			embed_splash_height: null,
			features: featuresSet,
			verification_level: templateSettings.verificationLevel,
			mfa_level: 0,
			nsfw_level: 0,
			nsfw: false,
			content_warning_level: null,
			content_warning_text: null,
			explicit_content_filter: templateSettings.explicitContentFilter,
			default_message_notifications: templateSettings.defaultMessageNotifications,
			system_channel_id: systemChannelId,
			system_channel_flags: templateSettings.systemChannelFlags,
			rules_channel_id: null,
			afk_channel_id: null,
			afk_timeout: templateSettings.afkTimeout,
			disabled_operations: 0,
			member_count: 1,
			audit_logs_indexed_at: null,
			members_indexed_at: null,
			message_history_cutoff: null,
			version: 1,
		};
		batch.addPrepared(Guilds.insert(guildData));
		batch.addPrepared(
			GuildMembers.insert({
				guild_id: guildId,
				user_id: user.id,
				joined_at: new Date(),
				nick: null,
				avatar_hash: null,
				banner_hash: null,
				bio: null,
				pronouns: null,
				accent_color: null,
				join_source_type: JoinSourceTypes.CREATOR,
				source_invite_code: null,
				inviter_id: null,
				deaf: false,
				mute: false,
				communication_disabled_until: null,
				role_ids: null,
				is_premium_sanitized: null,
				temporary: false,
				profile_flags: null,
				mention_flags: null,
				version: 1,
			}),
		);
		batch.addPrepared(GuildMembersByUserId.insert({user_id: user.id, guild_id: guildId}));
		await batch.execute();
		const guild = new Guild(guildData);
		await this.gatewayService.startGuild(guildId);
		await this.gatewayService.joinGuild({userId: user.id, guildId});
		if (!user.isBot) {
			const userSettings = await this.userRepository.findSettings(user.id);
			if (userSettings) {
				const settingsRow = userSettings.toRow();
				const {folders: nextFolders, modified} = addGuildToUncategorizedFolder({
					folders: settingsRow.guild_folders ?? [],
					guildId,
					defaultIcon: DEFAULT_GUILD_FOLDER_ICON,
				});
				if (modified) {
					settingsRow.guild_folders = nextFolders;
					const updatedSettings = await this.userRepository.upsertSettings(settingsRow);
					await this.gatewayService.dispatchPresence({
						userId: user.id,
						event: 'USER_SETTINGS_UPDATE',
						data: mapUserSettingsToResponse({settings: updatedSettings}),
					});
				}
			}
		}
		const guildSearchService = getGuildSearchService();
		if (guildSearchService) {
			await guildSearchService.indexGuild(guild).catch((error) => {
				Logger.error({guildId: guild.id, error}, 'Failed to index guild in search');
			});
		}
		return mapGuildToGuildResponse(guild);
	}

	async updateGuild(
		params: {
			userId: UserID;
			guildId: GuildID;
			data: GuildUpdateRequest;
			requestCache: RequestCache;
		},
		auditLogReason?: string | null,
	): Promise<{
		guild: GuildResponse;
		previousFeatures: Set<string>;
		updatedFeatures: Set<string>;
	}> {
		const {userId, guildId, data} = params;
		const {checkPermission, guildData} = await this.helpers.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_GUILD);
		const currentGuild = await this.guildRepository.findUnique(guildId);
		if (!currentGuild) throw new UnknownGuildError();
		const previousSnapshot = this.helpers.serializeGuildForAudit(currentGuild);
		const previousFeatures = new Set(currentGuild.features);
		contentModerationService.scanText(data.name ?? null, {
			userId,
			guildId,
			channelId: null,
			messageId: null,
			surface: 'profile_field',
		});
		if (
			data.verification_level !== undefined &&
			data.verification_level < GuildVerificationLevel.LOW &&
			currentGuild.features.has(GuildFeatures.DISCOVERABLE)
		) {
			throw InputValidationError.fromCode(
				'verification_level',
				ValidationErrorCodes.DISCOVERABLE_GUILD_VERIFICATION_LEVEL_TOO_LOW,
			);
		}
		const isMfaLevelChange = data.mfa_level !== undefined && data.mfa_level !== currentGuild.mfaLevel;
		if (isMfaLevelChange) {
			const isOwner = guildData.owner_id === userId.toString();
			if (!isOwner) {
				throw new MissingPermissionsError();
			}
			const owner = await this.userRepository.findUniqueAssert(userId);
			if (owner.authenticatorTypes.size === 0) {
				throw InputValidationError.fromCode(
					'mfa_level',
					ValidationErrorCodes.MUST_ENABLE_2FA_BEFORE_REQUIRING_FOR_MODS,
				);
			}
		}
		const updatedFeatures = this.computeUpdatedFeatures(previousFeatures, data.features);
		const featuresChanged = !setsEqual(previousFeatures, updatedFeatures);
		const preparedAssets: PreparedGuildAssets = {icon: null, banner: null, splash: null, embed_splash: null};
		const patch: Partial<GuildRow> = {};
		if (data.name !== undefined) {
			patch.name = data.name;
		}
		if (data.icon !== undefined) {
			preparedAssets.icon = await this.entityAssetService.prepareAssetUpload({
				assetType: 'icon',
				entityType: 'guild',
				entityId: guildId,
				previousHash: currentGuild.iconHash,
				base64Image: data.icon,
				errorPath: 'icon',
			});
			patch.icon_hash = preparedAssets.icon.newHash;
		}
		if (data.banner !== undefined) {
			if (data.banner && !currentGuild.features.has(GuildFeatures.BANNER)) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw InputValidationError.fromCode('banner', ValidationErrorCodes.GUILD_BANNER_REQUIRES_FEATURE);
			}
			if (data.banner === null) {
				patch.banner_hash = null;
				patch.banner_width = null;
				patch.banner_height = null;
			} else {
				try {
					preparedAssets.banner = await this.entityAssetService.prepareAssetUpload({
						assetType: 'banner',
						entityType: 'guild',
						entityId: guildId,
						previousHash: currentGuild.bannerHash,
						base64Image: data.banner,
						errorPath: 'banner',
					});
					if (preparedAssets.banner.isAnimated && !currentGuild.features.has(GuildFeatures.ANIMATED_BANNER)) {
						await this.rollbackPreparedAssets(preparedAssets);
						throw InputValidationError.fromCode('banner', ValidationErrorCodes.ANIMATED_GUILD_BANNER_REQUIRES_FEATURE);
					}
					patch.banner_hash = preparedAssets.banner.newHash;
					patch.banner_height =
						preparedAssets.banner.newHash === currentGuild.bannerHash && currentGuild.bannerHeight != null
							? currentGuild.bannerHeight
							: (preparedAssets.banner.height ?? null);
					patch.banner_width =
						preparedAssets.banner.newHash === currentGuild.bannerHash && currentGuild.bannerWidth != null
							? currentGuild.bannerWidth
							: (preparedAssets.banner.width ?? null);
				} catch (error) {
					await this.rollbackPreparedAssets(preparedAssets);
					throw error;
				}
			}
		}
		if (data.splash !== undefined) {
			if (data.splash && !currentGuild.features.has(GuildFeatures.INVITE_SPLASH)) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw InputValidationError.fromCode('splash', ValidationErrorCodes.INVITE_SPLASH_REQUIRES_FEATURE);
			}
			if (data.splash === null) {
				patch.splash_hash = null;
				patch.splash_width = null;
				patch.splash_height = null;
			} else {
				try {
					preparedAssets.splash = await this.entityAssetService.prepareAssetUpload({
						assetType: 'splash',
						entityType: 'guild',
						entityId: guildId,
						previousHash: currentGuild.splashHash,
						base64Image: data.splash,
						errorPath: 'splash',
					});
					patch.splash_hash = preparedAssets.splash.newHash;
					patch.splash_height =
						preparedAssets.splash.newHash === currentGuild.splashHash && currentGuild.splashHeight != null
							? currentGuild.splashHeight
							: (preparedAssets.splash.height ?? null);
					patch.splash_width =
						preparedAssets.splash.newHash === currentGuild.splashHash && currentGuild.splashWidth != null
							? currentGuild.splashWidth
							: (preparedAssets.splash.width ?? null);
				} catch (error) {
					await this.rollbackPreparedAssets(preparedAssets);
					throw error;
				}
			}
		}
		if (data.embed_splash !== undefined) {
			if (data.embed_splash && !currentGuild.features.has(GuildFeatures.INVITE_SPLASH)) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw InputValidationError.fromCode('embed_splash', ValidationErrorCodes.EMBED_SPLASH_REQUIRES_FEATURE);
			}
			if (data.embed_splash === null) {
				patch.embed_splash_hash = null;
				patch.embed_splash_width = null;
				patch.embed_splash_height = null;
			} else {
				try {
					preparedAssets.embed_splash = await this.entityAssetService.prepareAssetUpload({
						assetType: 'embed_splash',
						entityType: 'guild',
						entityId: guildId,
						previousHash: currentGuild.embedSplashHash,
						base64Image: data.embed_splash,
						errorPath: 'embed_splash',
					});
					patch.embed_splash_hash = preparedAssets.embed_splash.newHash;
					patch.embed_splash_height =
						preparedAssets.embed_splash.newHash === currentGuild.embedSplashHash &&
						currentGuild.embedSplashHeight != null
							? currentGuild.embedSplashHeight
							: (preparedAssets.embed_splash.height ?? null);
					patch.embed_splash_width =
						preparedAssets.embed_splash.newHash === currentGuild.embedSplashHash &&
						currentGuild.embedSplashWidth != null
							? currentGuild.embedSplashWidth
							: (preparedAssets.embed_splash.width ?? null);
				} catch (error) {
					await this.rollbackPreparedAssets(preparedAssets);
					throw error;
				}
			}
		}
		if (data.splash_card_alignment !== undefined) {
			patch.splash_card_alignment = data.splash_card_alignment;
		}
		if (data.afk_channel_id !== undefined) {
			if (data.afk_channel_id) {
				const afkChannelId = createChannelID(data.afk_channel_id);
				const afkChannel = await this.channelRepository.findUnique(afkChannelId);
				if (!afkChannel || afkChannel.guildId !== guildId) {
					throw InputValidationError.fromCode('afk_channel_id', ValidationErrorCodes.AFK_CHANNEL_MUST_BE_IN_GUILD);
				}
				if (afkChannel.type !== ChannelTypes.GUILD_VOICE) {
					throw InputValidationError.fromCode('afk_channel_id', ValidationErrorCodes.AFK_CHANNEL_MUST_BE_VOICE);
				}
				patch.afk_channel_id = afkChannelId;
			} else {
				patch.afk_channel_id = null;
			}
		}
		if (data.afk_timeout !== undefined) {
			patch.afk_timeout = data.afk_timeout;
		}
		if (data.system_channel_id !== undefined) {
			if (data.system_channel_id) {
				const systemChannelId = createChannelID(data.system_channel_id);
				const systemChannel = await this.channelRepository.findUnique(systemChannelId);
				if (!systemChannel || systemChannel.guildId !== guildId) {
					throw InputValidationError.fromCode(
						'system_channel_id',
						ValidationErrorCodes.SYSTEM_CHANNEL_MUST_BE_IN_GUILD,
					);
				}
				if (systemChannel.type !== ChannelTypes.GUILD_TEXT) {
					throw InputValidationError.fromCode('system_channel_id', ValidationErrorCodes.SYSTEM_CHANNEL_MUST_BE_TEXT);
				}
				patch.system_channel_id = systemChannelId;
			} else {
				patch.system_channel_id = null;
			}
		}
		if (data.system_channel_flags !== undefined) {
			patch.system_channel_flags = data.system_channel_flags & SUPPORTED_SYSTEM_CHANNEL_FLAGS;
		}
		if (data.default_message_notifications !== undefined) {
			patch.default_message_notifications = data.default_message_notifications;
		}
		if (data.verification_level !== undefined) {
			patch.verification_level = data.verification_level;
		}
		if (data.mfa_level !== undefined) {
			patch.mfa_level = data.mfa_level;
		}
		let nextNsfw: boolean | undefined;
		if (data.nsfw !== undefined) {
			nextNsfw = data.nsfw;
		} else if (data.nsfw_level !== undefined) {
			nextNsfw = data.nsfw_level === GuildNSFWLevel.AGE_RESTRICTED;
			if (
				nextNsfw === true &&
				data.content_warning_level === undefined &&
				(currentGuild.contentWarningLevel ?? 0) === ContentWarningLevel.INHERIT
			) {
				patch.content_warning_level = ContentWarningLevel.CONTENT_WARNING;
			}
		}
		if (nextNsfw !== undefined) {
			patch.nsfw = nextNsfw;
			patch.nsfw_level = nextNsfw ? GuildNSFWLevel.AGE_RESTRICTED : GuildNSFWLevel.SAFE;
		}
		if (data.content_warning_level !== undefined) {
			patch.content_warning_level =
				data.content_warning_level === ContentWarningLevel.CONTENT_WARNING
					? ContentWarningLevel.CONTENT_WARNING
					: ContentWarningLevel.INHERIT;
		}
		if (data.content_warning_text !== undefined) {
			const trimmed = data.content_warning_text == null ? null : data.content_warning_text.trim();
			patch.content_warning_text = trimmed && trimmed.length > 0 ? trimmed : null;
		}
		if (data.explicit_content_filter !== undefined) {
			patch.explicit_content_filter = data.explicit_content_filter;
		}
		if (data.message_history_cutoff !== undefined) {
			if (data.message_history_cutoff === null) {
				patch.message_history_cutoff = null;
			} else {
				const cutoffDate = new Date(data.message_history_cutoff);
				const guildCreationTimestamp = extractTimestamp(guildId.toString());
				if (cutoffDate.getTime() < guildCreationTimestamp) {
					await this.rollbackPreparedAssets(preparedAssets);
					throw InputValidationError.fromCode(
						'message_history_cutoff',
						ValidationErrorCodes.MESSAGE_HISTORY_CUTOFF_BEFORE_GUILD_CREATION,
					);
				}
				if (cutoffDate.getTime() > Date.now()) {
					await this.rollbackPreparedAssets(preparedAssets);
					throw InputValidationError.fromCode(
						'message_history_cutoff',
						ValidationErrorCodes.MESSAGE_HISTORY_CUTOFF_IN_FUTURE,
					);
				}
				patch.message_history_cutoff = cutoffDate;
			}
		}
		if (featuresChanged) {
			patch.features = updatedFeatures;
		}
		let updatedGuild: Guild;
		if (Object.keys(patch).length === 0) {
			updatedGuild = currentGuild;
		} else {
			try {
				updatedGuild = await this.guildRepository.upsertPartial(guildId, patch, currentGuild.toRow());
			} catch (error) {
				await this.rollbackPreparedAssets(preparedAssets);
				Logger.error({error, guildId}, 'Guild update failed, rolled back asset uploads');
				throw error;
			}
		}
		try {
			await this.commitPreparedAssets(preparedAssets);
		} catch (error) {
			Logger.error({error, guildId}, 'Failed to commit asset changes after successful guild update');
		}
		await this.helpers.dispatchGuildUpdate(updatedGuild);
		const guildSearchService = getGuildSearchService();
		if (guildSearchService) {
			let discoveryContext: GuildDiscoveryContext | undefined;
			if (updatedGuild.features.has(GuildFeatures.DISCOVERABLE)) {
				const discoveryRow = await this.discoveryRepository.findByGuildId(updatedGuild.id).catch(() => null);
				if (discoveryRow?.status === DiscoveryApplicationStatus.APPROVED) {
					discoveryContext = {
						description: discoveryRow.description,
						categoryId: discoveryRow.category_type,
						primaryLanguage: discoveryRow.primary_language ?? null,
						tags: discoveryRow.custom_tags ?? [],
					};
				}
			}
			await guildSearchService.updateGuild(updatedGuild, discoveryContext).catch((error) => {
				Logger.error({guildId: updatedGuild.id, error}, 'Failed to update guild in search');
			});
		}
		const auditLogChanges = this.helpers.computeGuildChanges(previousSnapshot, updatedGuild);
		if (auditLogChanges.length > 0) {
			await this.helpers.recordAuditLog({
				guildId,
				userId,
				action: AuditLogActionType.GUILD_UPDATE,
				targetId: guildId,
				auditLogReason: auditLogReason ?? null,
				metadata: {name: updatedGuild.name},
				changes: auditLogChanges,
			});
		}
		if (data.name !== undefined && currentGuild.name !== updatedGuild.name) {
		}
		if (data.icon !== undefined && currentGuild.iconHash !== updatedGuild.iconHash) {
		}
		if (data.banner !== undefined && currentGuild.bannerHash !== updatedGuild.bannerHash) {
		}
		return {
			guild: mapGuildToGuildResponse(updatedGuild),
			previousFeatures,
			updatedFeatures: new Set(updatedGuild.features),
		};
	}

	private computeUpdatedFeatures(
		previousFeatures: ReadonlySet<string>,
		desiredFeatures: ReadonlyArray<string> | undefined,
	): Set<string> {
		if (!desiredFeatures) {
			return new Set(previousFeatures);
		}
		const desired = new Set(desiredFeatures);
		const next = new Set(previousFeatures);
		for (const feature of desired) {
			if (previousFeatures.has(feature)) continue;
			if (!USER_TOGGLEABLE_GUILD_FEATURES.has(feature)) {
				throw InputValidationError.fromCode('features', ValidationErrorCodes.GUILD_FEATURE_NOT_TOGGLEABLE, {
					feature,
				});
			}
			next.add(feature);
		}
		for (const feature of previousFeatures) {
			if (desired.has(feature)) continue;
			if (!USER_TOGGLEABLE_GUILD_FEATURES.has(feature)) continue;
			next.delete(feature);
		}
		return next;
	}

	private async rollbackPreparedAssets(assets: PreparedGuildAssets): Promise<void> {
		const rollbackPromises: Array<Promise<void>> = [];
		if (assets.icon) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(assets.icon));
		}
		if (assets.banner) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(assets.banner));
		}
		if (assets.splash) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(assets.splash));
		}
		if (assets.embed_splash) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(assets.embed_splash));
		}
		await Promise.all(rollbackPromises);
	}

	private async commitPreparedAssets(assets: PreparedGuildAssets): Promise<void> {
		const commitPromises: Array<Promise<void>> = [];
		if (assets.icon) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: assets.icon, deferDeletion: true}));
		}
		if (assets.banner) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: assets.banner, deferDeletion: true}));
		}
		if (assets.splash) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: assets.splash, deferDeletion: true}));
		}
		if (assets.embed_splash) {
			commitPromises.push(
				this.entityAssetService.commitAssetChange({prepared: assets.embed_splash, deferDeletion: true}),
			);
		}
		await Promise.all(commitPromises);
	}

	async deleteGuild(
		params: {
			user: User;
			guildId: GuildID;
		},
		_auditLogReason?: string | null,
	): Promise<void> {
		const {user, guildId} = params;
		const {guildData} = await this.helpers.getGuildAuthenticated({userId: user.id, guildId});
		if (!guildData || guildData.owner_id !== user.id.toString()) {
			throw new MissingPermissionsError();
		}
		await this.performGuildDeletion(guildId);
	}

	async deleteGuildById(guildId: GuildID): Promise<void> {
		await this.performGuildDeletion(guildId);
	}

	private async performGuildDeletion(guildId: GuildID): Promise<void> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		const members = await this.guildRepository.listMembers(guildId);
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_DELETE',
			data: {id: guildId.toString()},
		});
		await Promise.all(
			members.map(async (member) => {
				await this.gatewayService.leaveGuild({userId: member.userId, guildId});
			}),
		);
		await Promise.all(members.map((member) => this.userRepository.deleteGuildSettings(member.userId, guildId)));
		await Promise.all(
			members.map(async (member) => {
				const user = await this.userRepository.findUnique(member.userId);
				if (user && !user.isBot) {
					await removeGuildFromUserFolders({
						userId: member.userId,
						guildId,
						userRepository: this.userRepository,
						gatewayService: this.gatewayService,
					});
				}
			}),
		);
		const invites = await this.inviteRepository.listGuildInvites(guildId);
		await Promise.all(invites.map((invite) => this.inviteRepository.delete(invite.code)));
		const webhooks = await this.webhookRepository.listByGuild(guildId);
		await Promise.all(webhooks.map((webhook) => this.webhookRepository.delete(webhook.id)));
		const channels = await this.channelRepository.listGuildChannels(guildId);
		await Promise.all(channels.map((channel) => this.channelRepository.deleteAllChannelMessages(channel.id)));
		await deleteGuildMessageSearchDocuments(guildId, {context: {source: 'guild_delete'}});
		await Promise.all(channels.map((channel) => this.channelService.attachments.purgeChannelAttachments(channel)));
		const discoveryRow = await this.discoveryRepository.findByGuildId(guildId);
		if (discoveryRow) {
			await this.discoveryRepository.deleteByGuildId(guildId, discoveryRow.status, discoveryRow.applied_at);
		}
		await this.guildRepository.delete(guildId, guild.ownerId);
		await this.gatewayService.stopGuild(guildId);
		const guildSearchService = getGuildSearchService();
		if (guildSearchService) {
			await guildSearchService.deleteGuild(guildId).catch((error) => {
				Logger.error({guildId, error}, 'Failed to delete guild from search');
			});
		}
	}

	private async buildDefaultEntities(
		guildId: GuildID,
		batch: BatchBuilder,
	): Promise<{
		systemChannelId: ChannelID;
	}> {
		const textCategoryId = createChannelID(await this.snowflakeService.generate());
		const voiceCategoryId = createChannelID(await this.snowflakeService.generate());
		const generalChannelId = createChannelID(await this.snowflakeService.generate());
		const generalVoiceId = createChannelID(await this.snowflakeService.generate());
		const addChannel = (
			channelId: ChannelID,
			type: number,
			name: string,
			parentId: ChannelID | null,
			position: number,
			bitrate: number | null = null,
		) => {
			batch.addPrepared(
				Channels.insert({
					channel_id: channelId,
					guild_id: guildId,
					type,
					name,
					topic: null,
					icon_hash: null,
					url: null,
					parent_id: parentId,
					position,
					owner_id: null,
					recipient_ids: null,
					nsfw: false,
					content_warning_level: null,
					content_warning_text: null,
					rate_limit_per_user: 0,
					bitrate,
					user_limit: bitrate !== null ? 0 : null,
					voice_connection_limit: bitrate !== null ? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT : null,
					rtc_region: null,
					last_message_id: null,
					last_pin_timestamp: null,
					permission_overwrites: null,
					nicks: null,
					soft_deleted: false,
					indexed_at: null,
					version: 1,
				}),
			);
			batch.addPrepared(
				ChannelsByGuild.upsertAll({
					guild_id: guildId,
					channel_id: channelId,
				}),
			);
		};
		addChannel(textCategoryId, ChannelTypes.GUILD_CATEGORY, DEFAULT_TEXT_CATEGORY_NAME, null, 0);
		addChannel(voiceCategoryId, ChannelTypes.GUILD_CATEGORY, DEFAULT_VOICE_CATEGORY_NAME, null, 1);
		addChannel(generalChannelId, ChannelTypes.GUILD_TEXT, DEFAULT_TEXT_CHANNEL_NAME, textCategoryId, 0);
		addChannel(generalVoiceId, ChannelTypes.GUILD_VOICE, DEFAULT_VOICE_CHANNEL_NAME, voiceCategoryId, 0, 64000);
		batch.addPrepared(
			GuildRoles.insert({
				guild_id: guildId,
				role_id: guildIdToRoleId(guildId),
				name: '@everyone',
				permissions: DEFAULT_PERMISSIONS,
				position: 0,
				hoist_position: null,
				color: 0,
				icon_hash: null,
				unicode_emoji: null,
				hoist: false,
				mentionable: false,
				version: 1,
			}),
		);
		return {systemChannelId: generalChannelId};
	}

	private async buildTemplateEntities(
		guildId: GuildID,
		template: TemplateSerializedGuild,
		batch: BatchBuilder,
	): Promise<{
		systemChannelId: ChannelID;
	}> {
		if (template.channels.length > MAX_GUILD_CHANNELS) {
			throw new GuildTemplateInvalidError();
		}
		if (template.roles.length > MAX_GUILD_ROLES) {
			throw new GuildTemplateInvalidError();
		}
		const roleIdMap = new Map<string, RoleID>();
		const channelIdMap = new Map<string, ChannelID>();
		const channelTypeMap = new Map<string, number>();
		const everyoneRole = this.findTemplateEveryoneRole(template.roles);
		const everyoneRoleKey = everyoneRole ? this.getTemplateEntityKey(everyoneRole.id) : null;
		const everyoneRoleId = guildIdToRoleId(guildId);
		roleIdMap.set('0', everyoneRoleId);
		if (everyoneRoleKey) {
			roleIdMap.set(everyoneRoleKey, everyoneRoleId);
		}
		const seenRoleKeys = new Set<string>();
		for (const role of template.roles) {
			const roleKey = this.getTemplateEntityKey(role.id);
			if (seenRoleKeys.has(roleKey)) {
				throw new GuildTemplateInvalidError();
			}
			seenRoleKeys.add(roleKey);
			if (roleKey === '0' || roleKey === everyoneRoleKey) continue;
			roleIdMap.set(roleKey, createRoleID(await this.snowflakeService.generate()));
		}
		const seenChannelKeys = new Set<string>();
		const mappedChannels: Array<MappedTemplateChannel> = [];
		const skippedOtherPlatformChannelTypes = new Set<number>();
		for (const channel of template.channels) {
			const channelKey = this.getTemplateEntityKey(channel.id);
			if (seenChannelKeys.has(channelKey)) {
				throw new GuildTemplateInvalidError();
			}
			seenChannelKeys.add(channelKey);
			const fluxerType = this.mapOtherPlatformTemplateChannelTypeToFluxer(channel.type);
			if (fluxerType == null) {
				skippedOtherPlatformChannelTypes.add(channel.type);
				continue;
			}
			const channelId = createChannelID(await this.snowflakeService.generate());
			channelIdMap.set(channelKey, channelId);
			channelTypeMap.set(channelKey, fluxerType);
			mappedChannels.push({channel, channelId, fluxerType});
		}
		if (skippedOtherPlatformChannelTypes.size > 0) {
			Logger.warn(
				{
					guildId: guildId.toString(),
					unsupportedOtherPlatformChannelTypes: Array.from(skippedOtherPlatformChannelTypes),
				},
				'[GuildOperationsService] Template import skipped unsupported channel types',
			);
		}
		const everyonePermissions =
			this.parseTemplatePermissionBitfield(everyoneRole?.permissions_new ?? everyoneRole?.permissions) ||
			DEFAULT_PERMISSIONS;
		batch.addPrepared(
			GuildRoles.insert({
				guild_id: guildId,
				role_id: everyoneRoleId,
				name: '@everyone',
				permissions: everyonePermissions,
				position: 0,
				hoist_position: null,
				color: 0,
				icon_hash: null,
				unicode_emoji: null,
				hoist: false,
				mentionable: false,
				version: 1,
			}),
		);
		let nextRolePosition = 1;
		for (const role of template.roles) {
			const roleKey = this.getTemplateEntityKey(role.id);
			if (roleKey === '0' || roleKey === everyoneRoleKey) continue;
			const roleId = roleIdMap.get(roleKey);
			if (!roleId) continue;
			batch.addPrepared(
				GuildRoles.insert({
					guild_id: guildId,
					role_id: roleId,
					name: role.name,
					permissions: this.parseTemplatePermissionBitfield(role.permissions_new ?? role.permissions),
					position: nextRolePosition,
					hoist_position: null,
					color: role.color ?? 0,
					icon_hash: null,
					unicode_emoji: role.unicode_emoji ?? null,
					hoist: role.hoist ?? false,
					mentionable: role.mentionable ?? false,
					version: 1,
				}),
			);
			nextRolePosition += 1;
		}
		let firstTextChannelId: ChannelID | null = null;
		for (const mappedChannel of mappedChannels) {
			const {channel, channelId, fluxerType} = mappedChannel;
			const parentKey = channel.parent_id != null ? this.getTemplateEntityKey(channel.parent_id) : null;
			const parentType = parentKey != null ? channelTypeMap.get(parentKey) : null;
			const parentId =
				parentKey != null && parentType === ChannelTypes.GUILD_CATEGORY ? (channelIdMap.get(parentKey) ?? null) : null;
			const isVoice = fluxerType === ChannelTypes.GUILD_VOICE;
			let permissionOverwrites: Map<RoleID | UserID, PermissionOverwrite> | null = null;
			if (channel.permission_overwrites && channel.permission_overwrites.length > 0) {
				permissionOverwrites = new Map();
				for (const overwrite of channel.permission_overwrites) {
					if (overwrite.type !== 0) continue;
					const mappedRoleId = roleIdMap.get(this.getTemplateEntityKey(overwrite.id));
					if (!mappedRoleId) continue;
					const allow = this.parseTemplatePermissionBitfield(overwrite.allow);
					const deny = this.parseTemplatePermissionBitfield(overwrite.deny);
					permissionOverwrites.set(mappedRoleId, {
						type: overwrite.type,
						allow_: allow,
						deny_: deny,
					});
				}
				if (permissionOverwrites.size === 0) {
					permissionOverwrites = null;
				}
			}
			if (fluxerType === ChannelTypes.GUILD_TEXT && !firstTextChannelId) {
				firstTextChannelId = channelId;
			}
			batch.addPrepared(
				Channels.insert({
					channel_id: channelId,
					guild_id: guildId,
					type: fluxerType,
					name: channel.name,
					topic: channel.topic ?? null,
					icon_hash: null,
					url: null,
					parent_id: parentId,
					position: channel.position,
					owner_id: null,
					recipient_ids: null,
					nsfw: channel.nsfw ?? false,
					content_warning_level: null,
					content_warning_text: null,
					rate_limit_per_user: channel.rate_limit_per_user ?? 0,
					bitrate: isVoice ? (channel.bitrate ?? 64000) : null,
					user_limit: isVoice ? (channel.user_limit ?? 0) : null,
					voice_connection_limit: isVoice
						? (channel.voice_connection_limit ?? VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT)
						: null,
					rtc_region: null,
					last_message_id: null,
					last_pin_timestamp: null,
					permission_overwrites: permissionOverwrites,
					nicks: null,
					soft_deleted: false,
					indexed_at: null,
					version: 1,
				}),
			);
			batch.addPrepared(
				ChannelsByGuild.upsertAll({
					guild_id: guildId,
					channel_id: channelId,
				}),
			);
		}
		let systemChannelId: ChannelID | null = null;
		if (template.system_channel_id != null) {
			const templateSystemChannelKey = this.getTemplateEntityKey(template.system_channel_id);
			const templateSystemChannelType = channelTypeMap.get(templateSystemChannelKey);
			if (templateSystemChannelType === ChannelTypes.GUILD_TEXT) {
				systemChannelId = channelIdMap.get(templateSystemChannelKey) ?? null;
			}
		}
		if (!systemChannelId && firstTextChannelId) {
			systemChannelId = firstTextChannelId;
		}
		if (!systemChannelId) {
			systemChannelId = createChannelID(await this.snowflakeService.generate());
			batch.addPrepared(
				Channels.insert({
					channel_id: systemChannelId,
					guild_id: guildId,
					type: ChannelTypes.GUILD_TEXT,
					name: DEFAULT_TEXT_CHANNEL_NAME,
					topic: null,
					icon_hash: null,
					url: null,
					parent_id: null,
					position: 0,
					owner_id: null,
					recipient_ids: null,
					nsfw: false,
					content_warning_level: null,
					content_warning_text: null,
					rate_limit_per_user: 0,
					bitrate: null,
					user_limit: null,
					voice_connection_limit: null,
					rtc_region: null,
					last_message_id: null,
					last_pin_timestamp: null,
					permission_overwrites: null,
					nicks: null,
					soft_deleted: false,
					indexed_at: null,
					version: 1,
				}),
			);
			batch.addPrepared(
				ChannelsByGuild.upsertAll({
					guild_id: guildId,
					channel_id: systemChannelId,
				}),
			);
		}
		return {systemChannelId};
	}

	private parseTemplatePermissionBitfield(value: string | number | undefined): bigint {
		const valueToParse = value ?? '0';
		try {
			return BigInt(valueToParse) & ALL_PERMISSIONS;
		} catch {
			throw new GuildTemplateInvalidError();
		}
	}

	private getTemplateEntityKey(id: number | string): string {
		return String(id);
	}

	private findTemplateEveryoneRole(roles: Array<TemplateRole>): TemplateRole | null {
		const namedEveryoneRole = roles.find((role) => role.name === '@everyone');
		if (namedEveryoneRole) {
			return namedEveryoneRole;
		}
		return roles.find((role) => this.getTemplateEntityKey(role.id) === '0') ?? null;
	}

	private mapOtherPlatformTemplateChannelTypeToFluxer(channelType: number): number | null {
		if (
			channelType === ChannelTypes.GUILD_TEXT ||
			channelType === ChannelTypes.GUILD_VOICE ||
			channelType === ChannelTypes.GUILD_CATEGORY
		) {
			return channelType;
		}
		if (channelType === THE_OTHER_PLATFORM_GUILD_ANNOUNCEMENT_CHANNEL_TYPE) {
			return ChannelTypes.GUILD_TEXT;
		}
		if (channelType === THE_OTHER_PLATFORM_GUILD_STAGE_VOICE_CHANNEL_TYPE) {
			return ChannelTypes.GUILD_VOICE;
		}
		return null;
	}

	private sanitiseTemplateGuildSettings(template?: TemplateSerializedGuild): TemplateGuildSettings {
		return {
			verificationLevel: this.clampTemplateSetting(template?.verification_level, 0, 4, 0),
			explicitContentFilter: this.clampTemplateSetting(template?.explicit_content_filter, 0, 2, 0),
			defaultMessageNotifications: this.clampTemplateSetting(template?.default_message_notifications, 0, 1, 0),
			systemChannelFlags: (template?.system_channel_flags ?? 0) & SUPPORTED_SYSTEM_CHANNEL_FLAGS,
			afkTimeout: this.clampTemplateSetting(
				template?.afk_timeout,
				TEMPLATE_AFK_TIMEOUT_MIN_SECONDS,
				TEMPLATE_AFK_TIMEOUT_MAX_SECONDS,
				300,
			),
		};
	}

	private clampTemplateSetting(value: number | undefined, min: number, max: number, fallback: number): number {
		if (value == null || Number.isNaN(value)) {
			return fallback;
		}
		const integerValue = Math.trunc(value);
		if (integerValue < min) return min;
		if (integerValue > max) return max;
		return integerValue;
	}

	private isGuildAccessError(error: unknown): boolean {
		return error instanceof UnknownGuildError;
	}

	private async guildExists(guildId: GuildID): Promise<boolean> {
		const guild = await this.guildRepository.findUnique(guildId);
		return guild !== null;
	}
}
