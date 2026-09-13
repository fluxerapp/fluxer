// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import {applicationIdToUserId} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import type {ApplicationRow} from '@app/api/database/types/OAuth2Types';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {DiscriminatorService} from '@app/api/infrastructure/DiscriminatorService';
import type {EntityAssetService, PreparedAssetUpload} from '@app/api/infrastructure/EntityAssetService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import {profileSubstringBlocklistCache} from '@app/api/middleware/ProfileSubstringBlocklistCache';
import type {Application} from '@app/api/models/Application';
import type {User} from '@app/api/models/User';
import {remapAuthorMessagesToDeletedUser} from '@app/api/oauth/ApplicationMessageAuthorAnonymization';
import type {BotAuthService} from '@app/api/oauth/BotAuthService';
import {generateOAuthTokenSecret} from '@app/api/oauth/OAuthTokenSecret';
import type {IApplicationRepository} from '@app/api/oauth/repositories/IApplicationRepository';
import {enforceFluxerTagChangeRateLimit} from '@app/api/user/FluxerTagChangeRateLimit';
import {hasPartialUserFieldsChanged, mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {runAllInOrder} from '@app/api/utils/ConcurrencyUtils';
import {hashPassword} from '@app/api/utils/PasswordUtils';
import {generateRandomUsername} from '@app/api/utils/UsernameGenerator';
import {deriveUsernameFromDisplayName} from '@app/api/utils/UsernameSuggestionUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
	ProfileFieldPrivacyFlags,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InternalServerError} from '@fluxer/errors/src/domains/core/InternalServerError';
import {BotUserNotFoundError} from '@fluxer/errors/src/domains/oauth/BotUserNotFoundError';
import {UnclaimedAccountCannotCreateApplicationsError} from '@fluxer/errors/src/domains/oauth/UnclaimedAccountCannotCreateApplicationsError';
import {UnknownApplicationError} from '@fluxer/errors/src/domains/oauth/UnknownApplicationError';
import type {BotProfileUpdateRequest} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';

interface ApplicationServiceDeps {
	discriminatorService: DiscriminatorService;
	channelRepository: IChannelRepository;
	applicationRepository: IApplicationRepository;
	botAuthService: BotAuthService;
	entityAssetService: EntityAssetService;
	userCacheService: UserCacheService;
}

const BOT_IMAGE_FIELDS = [
	{field: 'avatar', hash: 'avatar_hash', previousHash: 'avatarHash'},
	{field: 'banner', hash: 'banner_hash', previousHash: 'bannerHash'},
] as const;

export class ApplicationNotOwnedError extends ForbiddenError {
	constructor() {
		super({code: APIErrorCodes.APPLICATION_NOT_OWNED});
		this.name = 'ApplicationNotOwnedError';
	}
}

class BotUserGenerationError extends InternalServerError {
	constructor() {
		super({code: APIErrorCodes.BOT_USER_GENERATION_FAILED});
		this.name = 'BotUserGenerationError';
	}
}

export class ApplicationService {
	constructor(
		private readonly apiContext: ApiContext,
		public readonly deps: ApplicationServiceDeps,
	) {}

	private async generateBotUsername(applicationName: string): Promise<{
		username: string;
		discriminator: number;
	}> {
		const preferredUsername = deriveUsernameFromDisplayName(applicationName);
		if (preferredUsername) {
			const discResult = await this.deps.discriminatorService.generateDiscriminator({
				username: preferredUsername,
			});
			if (discResult.available && discResult.discriminator !== -1) {
				return {username: preferredUsername, discriminator: discResult.discriminator};
			}
		}
		Logger.info(
			{applicationName, preferredUsername: preferredUsername ?? null},
			'Application name did not yield a usable bot username, falling back to random username',
		);
		for (let attempts = 0; attempts < 100; attempts++) {
			const randomUsername = generateRandomUsername();
			const randomDiscResult = await this.deps.discriminatorService.generateDiscriminator({
				username: randomUsername,
			});
			if (randomDiscResult.available && randomDiscResult.discriminator !== -1) {
				return {username: randomUsername, discriminator: randomDiscResult.discriminator};
			}
		}
		throw new BotUserGenerationError();
	}

	async createApplication(args: {
		ownerUserId: UserID;
		name: string;
		redirectUris?: Array<string>;
		botPublic?: boolean;
		botRequireCodeGrant?: boolean;
	}): Promise<{
		application: Application;
		botUser: User;
		botToken: string;
		clientSecret: string;
	}> {
		const initialRedirectUris = args.redirectUris ?? [];
		const appModCtx = {
			userId: args.ownerUserId,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: 'app_asset' as const,
		};
		contentModerationService.scanText(args.name, appModCtx);
		for (const uri of initialRedirectUris) {
			contentModerationService.scanUrl(uri, {
				userId: args.ownerUserId,
				guildId: null,
				channelId: null,
				messageId: null,
				surface: 'oauth_redirect',
			});
		}
		const owner = await this.apiContext.services.users.findUniqueAssert(args.ownerUserId);
		const botIsPublic = args.botPublic ?? true;
		const botRequireCodeGrant = args.botRequireCodeGrant ?? false;
		if (owner.isUnclaimedAccount()) {
			throw new UnclaimedAccountCannotCreateApplicationsError();
		}
		const applicationId: ApplicationID = (await this.apiContext.services.snowflake.generate()) as ApplicationID;
		const botUserId = applicationIdToUserId(applicationId);
		const {username, discriminator} = await this.generateBotUsername(args.name);
		if (profileSubstringBlocklistCache.containsBannedSubstring('username', username)) {
			throw new ContentBlockedError();
		}
		Logger.info(
			{
				applicationId: applicationId.toString(),
				botUserId: botUserId.toString(),
				username,
				discriminator,
				applicationName: args.name,
			},
			'Creating application with bot user',
		);
		const botUserRow: UserRow = {
			user_id: botUserId,
			username,
			discriminator,
			global_name: null,
			bot: true,
			system: false,
			email: null,
			email_verified: null,
			email_bounced: null,
			password_hash: null,
			password_last_changed_at: null,
			totp_secret: null,
			authenticator_types: owner.authenticatorTypes ? new Set(owner.authenticatorTypes) : null,
			avatar_hash: null,
			avatar_color: null,
			banner_hash: null,
			banner_color: null,
			bio: null,
			pronouns: null,
			accent_color: null,
			timezone: null,
			timezone_privacy_flags: ProfileFieldPrivacyFlags.EVERYONE,
			date_of_birth: null,
			locale: null,
			flags: 0n,
			premium_type: null,
			premium_since: null,
			premium_until: null,
			premium_gift_extension_ends_at: null,
			premium_will_cancel: null,
			premium_billing_cycle: null,
			premium_lifetime_sequence: null,
			premium_grace_ends_at: null,
			stripe_subscription_id: null,
			stripe_customer_id: null,
			has_ever_purchased: null,
			suspicious_activity_flags: null,
			terms_agreed_at: null,
			privacy_agreed_at: null,
			last_active_at: null,
			last_active_ip: null,
			temp_banned_until: null,
			pending_deletion_at: null,
			pending_bulk_message_deletion_at: null,
			pending_bulk_message_deletion_channel_count: null,
			pending_bulk_message_deletion_message_count: null,
			deletion_reason_code: null,
			deletion_public_reason: null,
			deletion_audit_log_reason: null,
			acls: null,
			traits: null,
			first_refund_at: null,
			gift_inventory_server_seq: null,
			gift_inventory_client_seq: null,
			premium_onboarding_dismissed_at: null,
			mention_flags: null,
			last_voice_activity_sharing_change_at: null,
			version: 1,
		};
		const botUser = await this.apiContext.services.users.create(botUserRow);
		const {
			token: botToken,
			hash: botTokenHash,
			preview: botTokenPreview,
		} = await this.deps.botAuthService.generateBotToken(applicationId);
		const botTokenCreatedAt = new Date();
		const clientSecret = generateOAuthTokenSecret();
		const clientSecretHash = await hashPassword(clientSecret);
		const clientSecretCreatedAt = new Date();
		const applicationRow: ApplicationRow = {
			application_id: applicationId,
			owner_user_id: args.ownerUserId,
			name: args.name,
			bot_user_id: botUserId,
			bot_is_public: botIsPublic,
			bot_require_code_grant: botRequireCodeGrant,
			oauth2_redirect_uris: new Set<string>(initialRedirectUris),
			client_secret_hash: clientSecretHash,
			bot_token_hash: botTokenHash,
			bot_token_preview: botTokenPreview,
			bot_token_created_at: botTokenCreatedAt,
			client_secret_created_at: clientSecretCreatedAt,
		};
		const application = await this.deps.applicationRepository.upsertApplication(applicationRow);
		Logger.info(
			{applicationId: applicationId.toString(), botUserId: botUserId.toString()},
			'Successfully created application with bot user',
		);
		return {application, botUser, botToken, clientSecret};
	}

	async getApplication(applicationId: ApplicationID): Promise<Application | null> {
		return this.deps.applicationRepository.getApplication(applicationId);
	}

	async listApplicationsByOwner(ownerUserId: UserID): Promise<Array<Application>> {
		return this.deps.applicationRepository.listApplicationsByOwner(ownerUserId);
	}

	private async verifyOwnership(userId: UserID, applicationId: ApplicationID): Promise<Application> {
		const application = await this.deps.applicationRepository.getApplication(applicationId);
		if (!application) {
			throw new UnknownApplicationError();
		}
		if (application.ownerUserId !== userId) {
			throw new ApplicationNotOwnedError();
		}
		return application;
	}

	async updateApplication(args: {
		userId: UserID;
		applicationId: ApplicationID;
		name?: string;
		redirectUris?: Array<string>;
		botPublic?: boolean;
		botRequireCodeGrant?: boolean;
	}): Promise<Application> {
		const application = await this.verifyOwnership(args.userId, args.applicationId);
		contentModerationService.scanText(args.name ?? null, {
			userId: args.userId,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: 'app_asset',
		});
		if (args.redirectUris) {
			for (const uri of args.redirectUris) {
				contentModerationService.scanUrl(uri, {
					userId: args.userId,
					guildId: null,
					channelId: null,
					messageId: null,
					surface: 'oauth_redirect',
				});
			}
		}
		const updatedRow: ApplicationRow = {
			...application.toRow(),
			name: args.name ?? application.name,
			oauth2_redirect_uris: args.redirectUris ? new Set(args.redirectUris) : application.oauth2RedirectUris,
			bot_is_public: args.botPublic ?? application.botIsPublic,
			bot_require_code_grant: args.botRequireCodeGrant ?? application.botRequireCodeGrant,
		};
		return this.deps.applicationRepository.upsertApplication(updatedRow);
	}

	async deleteApplication(userId: UserID, applicationId: ApplicationID): Promise<void> {
		const application = await this.verifyOwnership(userId, applicationId);
		if (application.hasBotUser()) {
			const botUserId = application.getBotUserId()!;
			const replacementAuthorId = await remapAuthorMessagesToDeletedUser({
				originalAuthorId: botUserId,
				channelRepository: this.deps.channelRepository,
				userRepository: this.apiContext.services.users,
				snowflakeService: this.apiContext.services.snowflake,
			});
			const guildIds = await this.apiContext.services.users.getUserGuildIds(botUserId);
			await this.apiContext.services.users.deleteUserSecondaryIndices(botUserId);
			await this.apiContext.services.users.removeFromAllGuilds(botUserId);
			for (const guildId of guildIds) {
				try {
					await this.apiContext.services.gateway.dispatchGuild({
						guildId,
						event: 'GUILD_MEMBER_REMOVE',
						data: {user: {id: botUserId.toString()}},
					});
					await this.apiContext.services.gateway.leaveGuild({userId: botUserId, guildId});
				} catch (error) {
					Logger.error(
						{
							error,
							applicationId: applicationId.toString(),
							botUserId: botUserId.toString(),
							guildId: guildId.toString(),
						},
						'Failed to dispatch guild removal for deleted bot',
					);
				}
			}
			const botUser = await this.apiContext.services.users.findUniqueAssert(botUserId);
			await this.apiContext.services.users.patchUpsert(
				botUserId,
				{
					username: DELETED_USER_USERNAME,
					global_name: DELETED_USER_GLOBAL_NAME,
					discriminator: 0,
					email: null,
					email_verified: false,
					password_hash: null,
					password_last_changed_at: null,
					totp_secret: null,
					authenticator_types: new Set(),
					avatar_hash: null,
					banner_hash: null,
					bio: null,
					pronouns: null,
					accent_color: null,
					timezone: null,
					timezone_privacy_flags: ProfileFieldPrivacyFlags.EVERYONE,
					date_of_birth: null,
					flags: UserFlags.DELETED,
					premium_type: null,
					premium_since: null,
					premium_until: null,
					premium_gift_extension_ends_at: null,
					stripe_customer_id: null,
					stripe_subscription_id: null,
					pending_deletion_at: null,
					deletion_reason_code: null,
					deletion_public_reason: null,
					deletion_audit_log_reason: null,
				},
				botUser.toRow(),
			);
			await this.deps.userCacheService.invalidateUserCache(botUserId);
			if (replacementAuthorId) {
				await this.deps.userCacheService.invalidateUserCache(replacementAuthorId);
			}
			Logger.info(
				{
					applicationId: applicationId.toString(),
					botUserId: botUserId.toString(),
					replacementAuthorId: replacementAuthorId?.toString() ?? null,
				},
				'Anonymized bot user associated with application',
			);
		}
		await this.deps.applicationRepository.deleteApplication(applicationId);
		Logger.info({applicationId: applicationId.toString()}, 'Successfully deleted application');
	}

	async rotateBotToken(
		userId: UserID,
		applicationId: ApplicationID,
	): Promise<{
		token: string;
		preview: string;
	}> {
		const application = await this.verifyOwnership(userId, applicationId);
		if (!application.hasBotUser()) {
			throw new BotUserNotFoundError();
		}
		const {token, hash, preview} = await this.deps.botAuthService.generateBotToken(applicationId);
		const botTokenCreatedAt = new Date();
		const updatedRow: ApplicationRow = {
			...application.toRow(),
			bot_token_hash: hash,
			bot_token_preview: preview,
			bot_token_created_at: botTokenCreatedAt,
		};
		await this.deps.applicationRepository.upsertApplication(updatedRow);
		Logger.info({applicationId: applicationId.toString()}, 'Successfully rotated bot token');
		const botUserId = application.getBotUserId();
		if (botUserId !== null) {
			await this.apiContext.services.gateway.terminateAllSessionsForUser({
				userId: botUserId,
			});
		}
		return {token, preview};
	}

	async rotateClientSecret(
		userId: UserID,
		applicationId: ApplicationID,
	): Promise<{
		clientSecret: string;
	}> {
		const application = await this.verifyOwnership(userId, applicationId);
		const clientSecret = generateOAuthTokenSecret();
		const clientSecretHash = await hashPassword(clientSecret);
		const clientSecretCreatedAt = new Date();
		const updatedRow: ApplicationRow = {
			...application.toRow(),
			client_secret_hash: clientSecretHash,
			client_secret_created_at: clientSecretCreatedAt,
		};
		await this.deps.applicationRepository.upsertApplication(updatedRow);
		Logger.info({applicationId: applicationId.toString()}, 'Successfully rotated client secret');
		return {clientSecret};
	}

	async updateBotProfile(
		userId: UserID,
		applicationId: ApplicationID,
		args: BotProfileUpdateRequest,
	): Promise<{
		user: User;
		application: Application;
	}> {
		const application = await this.verifyOwnership(userId, applicationId);
		const botUserId = application.getBotUserId();
		if (botUserId === null) {
			throw new BotUserNotFoundError();
		}
		const botUser = await this.apiContext.services.users.findUnique(botUserId);
		if (!botUser) {
			throw new BotUserNotFoundError();
		}
		contentModerationService.scanText(args.bio ?? null, {
			userId: botUserId,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: 'profile_field',
		});
		if (args.discriminator !== undefined && args.discriminator !== botUser.discriminator) {
			throw InputValidationError.fromCode('discriminator', ValidationErrorCodes.BOT_DISCRIMINATOR_CANNOT_BE_CHANGED);
		}
		const updates: Partial<UserRow> = {};
		const newUsername = args.username ?? botUser.username;
		const usernameChanged = args.username !== undefined && args.username !== botUser.username;
		if (usernameChanged) {
			const result = await this.deps.discriminatorService.resolveUsernameChange({
				currentUsername: botUser.username,
				currentDiscriminator: botUser.discriminator,
				newUsername,
			});
			if (result.username !== botUser.username) {
				updates.username = result.username;
			}
			if (result.discriminator !== botUser.discriminator) {
				updates.discriminator = result.discriminator;
			}
			if (profileSubstringBlocklistCache.containsBannedSubstring('username', result.username)) {
				throw new ContentBlockedError();
			}
			if (result.username !== botUser.username || result.discriminator !== botUser.discriminator) {
				await enforceFluxerTagChangeRateLimit({
					rateLimitService: this.apiContext.services.rateLimit,
					userId: botUserId,
					errorPath: 'username',
				});
			}
		}
		updates.global_name = null;
		if (args.bio !== undefined) {
			if (args.bio && profileSubstringBlocklistCache.containsBannedSubstring('bio', args.bio)) {
				throw new ContentBlockedError();
			}
			updates.bio = args.bio;
		}
		if (args.bot_flags !== undefined) {
			const mask = UserFlags.FRIENDLY_BOT | UserFlags.FRIENDLY_BOT_MANUAL_APPROVAL;
			const updatedFlags = (botUser.flags & ~mask) | (BigInt(args.bot_flags) & mask);
			if (updatedFlags !== botUser.flags) {
				updates.flags = updatedFlags;
			}
		}
		const preparedAssets = await this.prepareBotAssets(botUser, args, updates);
		let updatedUser: User;
		try {
			updatedUser = await this.apiContext.services.users.patchUpsert(botUserId, updates, botUser.toRow());
		} catch (err) {
			Logger.error(
				{error: err, applicationId: applicationId.toString(), botUserId: botUserId.toString()},
				'Bot profile update failed with unknown commit status; retaining uploaded assets',
			);
			throw err;
		}
		try {
			await runAllInOrder(
				[
					() =>
						this.apiContext.services.contactChangeLog.recordDiff({
							oldUser: botUser,
							newUser: updatedUser,
							reason: 'user_requested',
							actorUserId: userId,
						}),
					() => this.deps.entityAssetService.commitAssetChanges(preparedAssets),
					() =>
						this.apiContext.services.gateway.dispatchPresence({
							userId: updatedUser.id,
							event: 'USER_UPDATE',
							data: mapUserToPrivateResponse(updatedUser),
						}),
					async () => {
						if (hasPartialUserFieldsChanged(botUser, updatedUser)) {
							await this.deps.userCacheService.setUserPartialResponseFromUser(updatedUser);
						}
					},
				],
				'Failed to finalize bot profile',
			);
		} catch (err) {
			Logger.error(
				{error: err, applicationId: applicationId.toString(), botUserId: botUserId.toString()},
				'Failed to finalize bot profile after successful DB update',
			);
			throw err;
		}
		Logger.info(
			{applicationId: applicationId.toString(), botUserId: botUserId.toString()},
			'Successfully updated bot profile',
		);
		return {
			user: updatedUser,
			application,
		};
	}

	private async prepareBotAssets(
		user: User,
		data: BotProfileUpdateRequest,
		updates: Partial<UserRow>,
	): Promise<Array<PreparedAssetUpload>> {
		const preparedAssets: Array<PreparedAssetUpload> = [];
		for (const {field, hash, previousHash} of BOT_IMAGE_FIELDS) {
			const image = data[field];
			if (image === undefined) continue;
			const prepared = await this.deps.entityAssetService.prepareAssetUpload({
				assetType: field,
				entityType: 'user',
				entityId: user.id,
				previousHash: user[previousHash],
				base64Image: image,
				errorPath: field,
			});
			updates[hash] = prepared.newHash;
			preparedAssets.push(prepared);
		}
		return preparedAssets;
	}
}
