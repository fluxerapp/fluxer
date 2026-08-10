// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_GUILD_FOLDER_ICON,
	GroupDmAddPermissionFlags,
	IncomingCallFlags,
	RelationshipTypes,
	SensitiveMediaFilterLevel,
	UNCATEGORIZED_FOLDER_ID,
	UserFlags,
	UserNotificationSettings,
	VOICE_ACTIVITY_SHARING_COOLDOWN_MS,
} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {ValidationError} from '@fluxer/errors/src/ValidationError';
import {
	decodeSyncedPreferences,
	encodedSyncedPreferencesByteLength,
	encodeSyncedPreferences,
	SYNCED_PREFERENCES_MAX_BYTES,
	type SyncedPreferences,
} from '@fluxer/schema/src/domains/user/SyncedPreferencesCodec';
import type {
	UserGuildSettingsUpdateRequest,
	UserSettingsUpdateRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createUserID,
	type GuildID,
	type UserID,
} from '../../BrandedTypes';
import type {ChannelOverride, UserGuildSettingsRow} from '../../database/types/UserTypes';
import type {IGuildRepositoryAggregate} from '../../guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {User} from '../../models/User';
import type {UserGuildSettings} from '../../models/UserGuildSettings';
import type {UserSettings} from '../../models/UserSettings';
import {isUserAdult} from '../../utils/AgeUtils';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import type {IUserSettingsRepository} from '../repositories/IUserSettingsRepository';
import {getCachedUserPartialResponse} from '../UserCacheHelpers';
import {mapRelationshipToResponse} from '../UserMappers';
import {dedupeGuildFolders} from '../utils/GuildFolderUtils';
import {CustomStatusValidator} from './CustomStatusValidator';
import type {UserAccountUpdatePropagator} from './UserAccountUpdatePropagator';

interface UserAccountSettingsServiceDeps {
	userAccountRepository: IUserAccountRepository;
	userSettingsRepository: IUserSettingsRepository;
	userRelationshipRepository: IUserRelationshipRepository;
	updatePropagator: UserAccountUpdatePropagator;
	gatewayService: IGatewayService;
	userCacheService: UserCacheService;
	guildRepository: IGuildRepositoryAggregate;
	limitConfigService: LimitConfigService;
}

export class UserAccountSettingsService {
	private readonly customStatusValidator: CustomStatusValidator;

	constructor(private readonly deps: UserAccountSettingsServiceDeps) {
		this.customStatusValidator = new CustomStatusValidator(
			this.deps.userAccountRepository,
			this.deps.guildRepository,
			this.deps.limitConfigService,
		);
	}

	async findSettings(userId: UserID): Promise<UserSettings> {
		const userSettings = await this.deps.userSettingsRepository.findSettings(userId);
		if (!userSettings) throw new UnknownUserError();
		return userSettings;
	}

	async updateSettings(params: {
		userId: UserID;
		data: UserSettingsUpdateRequest;
		dateOfBirth?: string | null;
		flags?: bigint;
	}): Promise<UserSettings> {
		const {userId, data, dateOfBirth, flags = 0n} = params;
		const currentSettings = await this.deps.userSettingsRepository.findSettings(userId);
		if (!currentSettings) {
			throw new UnknownUserError();
		}
		const updatedRowData = {...currentSettings.toRow(), user_id: userId};
		const localeChanged = data.locale !== undefined && data.locale !== currentSettings.locale;
		const isStaffUser = (flags & UserFlags.STAFF) === UserFlags.STAFF;
		if (data.status !== undefined) updatedRowData.status = data.status;
		if (data.status_resets_at !== undefined) updatedRowData.status_resets_at = data.status_resets_at;
		if (data.status_resets_to !== undefined) updatedRowData.status_resets_to = data.status_resets_to;
		if (data.theme !== undefined) {
			if (data.theme !== currentSettings.theme) {
			}
			updatedRowData.theme = data.theme;
		}
		if (data.locale !== undefined) updatedRowData.locale = data.locale;
		if (data.custom_status !== undefined) {
			if (data.custom_status === null) {
				updatedRowData.custom_status = null;
			} else {
				const validated = await this.customStatusValidator.validate(userId, data.custom_status);
				updatedRowData.custom_status = {
					text: validated.text,
					expires_at: validated.expiresAt,
					emoji_id: validated.emojiId,
					emoji_name: validated.emojiName,
					emoji_animated: validated.emojiAnimated,
				};
			}
		}
		if (data.flags !== undefined) updatedRowData.friend_source_flags = data.flags;
		if (data.restricted_guilds !== undefined) {
			updatedRowData.restricted_guilds = data.restricted_guilds
				? new Set(data.restricted_guilds.map(createGuildID))
				: null;
		}
		if (data.bot_restricted_guilds !== undefined) {
			updatedRowData.bot_restricted_guilds = data.bot_restricted_guilds
				? new Set(data.bot_restricted_guilds.map(createGuildID))
				: null;
		}
		if (data.default_guilds_restricted !== undefined) {
			updatedRowData.default_guilds_restricted = data.default_guilds_restricted;
		}
		if (data.bot_default_guilds_restricted !== undefined) {
			updatedRowData.bot_default_guilds_restricted = data.bot_default_guilds_restricted;
		}
		if (data.inline_attachment_media !== undefined) {
			updatedRowData.inline_attachment_media = data.inline_attachment_media;
		}
		if (data.inline_embed_media !== undefined) updatedRowData.inline_embed_media = data.inline_embed_media;
		if (data.gif_auto_play !== undefined) updatedRowData.gif_auto_play = data.gif_auto_play;
		if (data.render_embeds !== undefined) updatedRowData.render_embeds = data.render_embeds;
		if (data.render_reactions !== undefined) updatedRowData.render_reactions = data.render_reactions;
		if (data.animate_emoji !== undefined) updatedRowData.animate_emoji = data.animate_emoji;
		if (data.animate_stickers !== undefined) updatedRowData.animate_stickers = data.animate_stickers;
		if (data.render_spoilers !== undefined) updatedRowData.render_spoilers = data.render_spoilers;
		if (data.message_display_compact !== undefined) {
			updatedRowData.message_display_compact = data.message_display_compact;
		}
		if (data.friend_source_flags !== undefined) {
			updatedRowData.friend_source_flags = data.friend_source_flags;
		}
		if (data.incoming_call_flags !== undefined) {
			updatedRowData.incoming_call_flags = this.normalizeIncomingCallFlags(data.incoming_call_flags);
		}
		if (data.group_dm_add_permission_flags !== undefined) {
			updatedRowData.group_dm_add_permission_flags = this.normalizeGroupDmAddPermissionFlags(
				data.group_dm_add_permission_flags,
			);
		}
		if (data.profile_privacy !== undefined) {
			updatedRowData.profile_privacy = data.profile_privacy;
		}
		if (data.guild_folders !== undefined) {
			const mappedFolders = data.guild_folders.map((folder) => ({
				folder_id: folder.id,
				name: folder.name ?? null,
				color: folder.color ?? 0x000000,
				flags: folder.flags ?? 0,
				icon: folder.icon ?? DEFAULT_GUILD_FOLDER_ICON,
				guild_ids: folder.guild_ids.map(createGuildID),
			}));
			const hasUncategorized = mappedFolders.some((folder) => folder.folder_id === UNCATEGORIZED_FOLDER_ID);
			if (!hasUncategorized) {
				mappedFolders.unshift({
					folder_id: UNCATEGORIZED_FOLDER_ID,
					name: null,
					color: 0x000000,
					flags: 0,
					icon: DEFAULT_GUILD_FOLDER_ICON,
					guild_ids: [],
				});
			}
			const {folders: dedupedFolders} = dedupeGuildFolders(mappedFolders);
			updatedRowData.guild_folders = dedupedFolders;
		}
		if (data.afk_timeout !== undefined) updatedRowData.afk_timeout = data.afk_timeout;
		if (data.time_format !== undefined) updatedRowData.time_format = data.time_format;
		if (data.developer_mode !== undefined) updatedRowData.developer_mode = data.developer_mode;
		if (data.trusted_domains !== undefined) {
			const domainsSet = new Set(data.trusted_domains);
			if (domainsSet.has('*') && domainsSet.size > 1) {
				throw ValidationError.fromPath(
					'trusted_domains',
					'INVALID_TRUSTED_DOMAINS',
					'Cannot combine wildcard (*) with specific domains',
				);
			}
			updatedRowData.trusted_domains = domainsSet.size > 0 ? domainsSet : null;
		}
		if (data.default_hide_muted_channels !== undefined) {
			updatedRowData.default_hide_muted_channels = data.default_hide_muted_channels;
		}
		const userIsAdult = isUserAdult(dateOfBirth);
		if (userIsAdult) {
			if (data.sensitive_content_friend_dm_filter !== undefined) {
				updatedRowData.sensitive_content_friend_dm_filter = data.sensitive_content_friend_dm_filter;
			}
			if (data.sensitive_content_non_friend_dm_filter !== undefined) {
				updatedRowData.sensitive_content_non_friend_dm_filter = data.sensitive_content_non_friend_dm_filter;
			}
			if (data.sensitive_content_guild_filter !== undefined) {
				updatedRowData.sensitive_content_guild_filter = data.sensitive_content_guild_filter;
			}
		} else {
			if (data.sensitive_content_friend_dm_filter !== undefined) {
				const allowed =
					data.sensitive_content_friend_dm_filter === SensitiveMediaFilterLevel.BLUR ||
					data.sensitive_content_friend_dm_filter === SensitiveMediaFilterLevel.BLOCK;
				if (!allowed) {
					throw ValidationError.fromPath(
						'sensitive_content_friend_dm_filter',
						'AGE_RESTRICTED',
						'Non-adult users can only set friend DM filter to blur or block',
					);
				}
				updatedRowData.sensitive_content_friend_dm_filter = data.sensitive_content_friend_dm_filter;
			}
			if (data.sensitive_content_non_friend_dm_filter !== undefined) {
				throw ValidationError.fromPath(
					'sensitive_content_non_friend_dm_filter',
					'AGE_RESTRICTED',
					'Non-adult users cannot modify the non-friend DM content filter',
				);
			}
			if (data.sensitive_content_guild_filter !== undefined) {
				throw ValidationError.fromPath(
					'sensitive_content_guild_filter',
					'AGE_RESTRICTED',
					'Non-adult users cannot modify the guild content filter',
				);
			}
		}
		if (isStaffUser) {
			if (data.suppress_unprivileged_self_mentions !== undefined) {
				updatedRowData.suppress_unprivileged_self_mentions = data.suppress_unprivileged_self_mentions;
			}
			if (data.suppress_unprivileged_self_mentions_bypass_user_ids !== undefined) {
				updatedRowData.suppress_unprivileged_self_mentions_bypass_user_ids = this.normalizeUserIdSet(
					data.suppress_unprivileged_self_mentions_bypass_user_ids,
				);
			}
			if (data.staff_dm_access_user_ids !== undefined) {
				updatedRowData.staff_dm_access_user_ids = this.normalizeUserIdSet(data.staff_dm_access_user_ids);
			}
		} else {
			updatedRowData.suppress_unprivileged_self_mentions = false;
			updatedRowData.suppress_unprivileged_self_mentions_bypass_user_ids = null;
			updatedRowData.staff_dm_access_user_ids = null;
		}
		if (data.synced_preferences !== undefined) {
			updatedRowData.synced_preferences = normalizeSyncedPreferencesSnapshot(data.synced_preferences);
		}
		await this.deps.userSettingsRepository.upsertSettings(updatedRowData);
		const updatedSettings = await this.findSettings(userId);
		await this.deps.updatePropagator.dispatchUserSettingsUpdate({userId, settings: updatedSettings});
		if (localeChanged) {
			const user = await this.deps.userAccountRepository.findUnique(userId);
			if (user) {
				const updatedUser = await this.deps.userAccountRepository.patchUpsert(
					userId,
					{locale: data.locale},
					user.toRow(),
				);
				await this.deps.updatePropagator.dispatchUserUpdate(updatedUser);
			}
		}
		return updatedSettings;
	}

	async findGuildSettings(userId: UserID, guildId: GuildID | null): Promise<UserGuildSettings | null> {
		return await this.deps.userSettingsRepository.findGuildSettings(userId, guildId);
	}

	async updateGuildSettings(params: {
		userId: UserID;
		guildId: GuildID | null;
		data: UserGuildSettingsUpdateRequest;
	}): Promise<UserGuildSettings> {
		const {userId, guildId, data} = params;
		const currentSettings = await this.deps.userSettingsRepository.findGuildSettings(userId, guildId);
		const resolvedGuildId = guildId ?? createGuildID(0n);
		const baseRow: UserGuildSettingsRow = currentSettings
			? {
					...currentSettings.toRow(),
					user_id: userId,
					guild_id: resolvedGuildId,
				}
			: {
					user_id: userId,
					guild_id: resolvedGuildId,
					message_notifications: UserNotificationSettings.INHERIT,
					muted: false,
					mute_config: null,
					mobile_push: true,
					suppress_everyone: false,
					suppress_roles: false,
					hide_muted_channels: false,
					channel_overrides: null,
					unread_badges: null,
					version: 1,
				};
		const updatedRowData: UserGuildSettingsRow = {...baseRow};
		if (data.message_notifications !== undefined) updatedRowData.message_notifications = data.message_notifications;
		if (data.muted !== undefined) updatedRowData.muted = data.muted;
		if (data.mute_config !== undefined) {
			updatedRowData.mute_config = data.mute_config
				? {
						end_time: data.mute_config.end_time ?? null,
						selected_time_window: data.mute_config.selected_time_window,
					}
				: null;
		}
		if (data.mobile_push !== undefined) updatedRowData.mobile_push = data.mobile_push;
		if (data.suppress_everyone !== undefined) updatedRowData.suppress_everyone = data.suppress_everyone;
		if (data.suppress_roles !== undefined) updatedRowData.suppress_roles = data.suppress_roles;
		if (data.hide_muted_channels !== undefined) updatedRowData.hide_muted_channels = data.hide_muted_channels;
		if (data.unread_badges !== undefined) updatedRowData.unread_badges = data.unread_badges ?? null;
		if (data.channel_overrides !== undefined) {
			if (data.channel_overrides) {
				const channelOverrides = new Map<ChannelID, ChannelOverride>();
				for (const [channelIdStr, override] of Object.entries(data.channel_overrides)) {
					const channelId = createChannelID(BigInt(channelIdStr));
					channelOverrides.set(channelId, {
						collapsed: override.collapsed,
						message_notifications: override.message_notifications,
						muted: override.muted,
						mute_config: override.mute_config
							? {
									end_time: override.mute_config.end_time ?? null,
									selected_time_window: override.mute_config.selected_time_window,
								}
							: null,
						unread_badges: override.unread_badges ?? null,
					});
				}
				updatedRowData.channel_overrides = channelOverrides.size > 0 ? channelOverrides : null;
			} else {
				updatedRowData.channel_overrides = null;
			}
		}
		const updatedSettings = await this.deps.userSettingsRepository.upsertGuildSettings(updatedRowData);
		await this.deps.updatePropagator.dispatchUserGuildSettingsUpdate({userId, settings: updatedSettings});
		return updatedSettings;
	}

	private normalizeIncomingCallFlags(flags: number): number {
		let normalizedFlags = flags;
		const modifierFlags = flags & IncomingCallFlags.SILENT_EVERYONE;
		if ((normalizedFlags & IncomingCallFlags.FRIENDS_ONLY) === IncomingCallFlags.FRIENDS_ONLY) {
			normalizedFlags = IncomingCallFlags.FRIENDS_ONLY | modifierFlags;
		}
		if ((normalizedFlags & IncomingCallFlags.NOBODY) === IncomingCallFlags.NOBODY) {
			normalizedFlags = IncomingCallFlags.NOBODY | modifierFlags;
		}
		return normalizedFlags;
	}

	private normalizeGroupDmAddPermissionFlags(flags: number): number {
		let normalizedFlags = flags;
		if ((normalizedFlags & GroupDmAddPermissionFlags.FRIENDS_ONLY) === GroupDmAddPermissionFlags.FRIENDS_ONLY) {
			normalizedFlags = GroupDmAddPermissionFlags.FRIENDS_ONLY;
		}
		if ((normalizedFlags & GroupDmAddPermissionFlags.NOBODY) === GroupDmAddPermissionFlags.NOBODY) {
			normalizedFlags = GroupDmAddPermissionFlags.NOBODY;
		}
		if ((normalizedFlags & GroupDmAddPermissionFlags.EVERYONE) === GroupDmAddPermissionFlags.EVERYONE) {
			normalizedFlags = GroupDmAddPermissionFlags.EVERYONE;
		}
		return normalizedFlags;
	}

	private normalizeUserIdSet(value: ReadonlyArray<bigint> | null | undefined): Set<UserID> | null {
		if (value == null || value.length === 0) {
			return null;
		}
		const normalized = new Set<UserID>();
		for (const userId of value) {
			normalized.add(createUserID(userId));
		}
		return normalized.size > 0 ? normalized : null;
	}

	async updateVoiceActivitySharingDefault(params: {
		userId: UserID;
		shareVoiceActivity: boolean;
		requestCache: RequestCache;
	}): Promise<User> {
		const {userId, shareVoiceActivity, requestCache} = params;
		const user = await this.deps.userAccountRepository.findUnique(userId);
		if (!user) throw new UnknownUserError();
		const lastChange = user.lastVoiceActivitySharingChangeAt;
		const now = new Date();
		if (lastChange != null) {
			const elapsedMs = now.getTime() - lastChange.getTime();
			if (elapsedMs < VOICE_ACTIVITY_SHARING_COOLDOWN_MS) {
				const retryAfterSeconds = Math.ceil((VOICE_ACTIVITY_SHARING_COOLDOWN_MS - elapsedMs) / 1000);
				throw InputValidationError.fromCode(
					'share_voice_activity',
					ValidationErrorCodes.VOICE_ACTIVITY_SHARING_ON_COOLDOWN,
					{retry_after: retryAfterSeconds},
				);
			}
		}
		const currentSettings = await this.deps.userSettingsRepository.findSettings(userId);
		if (!currentSettings) throw new UnknownUserError();
		const settingsUnchanged = currentSettings.defaultShareVoiceActivity === shareVoiceActivity;
		const updatedSettingsRow = {
			...currentSettings.toRow(),
			user_id: userId,
			default_share_voice_activity: shareVoiceActivity,
		};
		await this.deps.userSettingsRepository.upsertSettings(updatedSettingsRow);
		const updatedSettings = await this.findSettings(userId);
		const updatedFriendRows = await this.deps.userRelationshipRepository.bulkUpdateFriendShareVoiceActivity(
			userId,
			shareVoiceActivity,
		);
		const userPartialResolver = (id: UserID) =>
			getCachedUserPartialResponse({
				userId: id,
				userCacheService: this.deps.userCacheService,
				requestCache,
			});
		await Promise.all(
			updatedFriendRows.flatMap((friendRel) => {
				const dispatches: Array<Promise<unknown>> = [];
				dispatches.push(
					(async () => {
						const inverse = await this.deps.userRelationshipRepository.getRelationship(
							friendRel.targetUserId,
							userId,
							RelationshipTypes.FRIEND,
						);
						await this.deps.gatewayService.dispatchPresence({
							userId,
							event: 'RELATIONSHIP_UPDATE',
							data: await mapRelationshipToResponse({
								relationship: friendRel,
								userPartialResolver,
								inverseRelationshipResolver: async () => inverse,
							}),
						});
					})(),
				);
				dispatches.push(
					(async () => {
						const inverseForFriend = await this.deps.userRelationshipRepository.getRelationship(
							userId,
							friendRel.targetUserId,
							RelationshipTypes.FRIEND,
						);
						const friendOwnRow = await this.deps.userRelationshipRepository.getRelationship(
							friendRel.targetUserId,
							userId,
							RelationshipTypes.FRIEND,
						);
						if (friendOwnRow == null) return;
						await this.deps.gatewayService.dispatchPresence({
							userId: friendRel.targetUserId,
							event: 'RELATIONSHIP_UPDATE',
							data: await mapRelationshipToResponse({
								relationship: friendOwnRow,
								userPartialResolver,
								inverseRelationshipResolver: async () => inverseForFriend,
							}),
						});
					})(),
				);
				return dispatches;
			}),
		);
		const updatedUser = await this.deps.userAccountRepository.patchUpsert(
			userId,
			{last_voice_activity_sharing_change_at: now},
			user.toRow(),
		);
		const finalUser = updatedUser ?? user;
		await this.deps.updatePropagator.dispatchUserUpdate(finalUser);
		if (!settingsUnchanged) {
			await this.deps.updatePropagator.dispatchUserSettingsUpdate({userId, settings: updatedSettings});
		}
		return finalUser;
	}
}

function normalizeSyncedPreferencesSnapshot(value: string | null | undefined): string | null {
	if (value == null || value === '') return null;
	if (encodedSyncedPreferencesByteLength(value) > SYNCED_PREFERENCES_MAX_BYTES) {
		throw ValidationError.fromPath(
			'synced_preferences',
			'TOO_LARGE',
			`synced_preferences exceeds ${SYNCED_PREFERENCES_MAX_BYTES} bytes`,
		);
	}
	let decoded: SyncedPreferences;
	try {
		decoded = decodeSyncedPreferences(value);
	} catch (error) {
		throw ValidationError.fromPath(
			'synced_preferences',
			'INVALID_FORMAT',
			error instanceof Error ? error.message : 'invalid synced_preferences encoding',
		);
	}
	const canonical = encodeSyncedPreferences(decoded);
	return canonical === '' ? null : canonical;
}
