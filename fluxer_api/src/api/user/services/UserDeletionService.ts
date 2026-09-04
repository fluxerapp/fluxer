// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	DELETED_USER_DISCRIMINATOR,
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
	ProfileFieldPrivacyFlags,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms} from 'itty-time';
import type Stripe from 'stripe';
import {createMessageID, createUserID, type MessageID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {mapChannelToResponse} from '../../channel/ChannelMappers';
import type {ChannelRepository} from '../../channel/ChannelRepository';
import type {FavoriteMemeRepository} from '../../favorite_meme/FavoriteMemeRepository';
import type {GuildRepository} from '../../guild/repositories/GuildRepository';
import type {IPurgeQueue} from '../../infrastructure/BunnyPurgeQueue';
import type {DiscriminatorService} from '../../infrastructure/DiscriminatorService';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {IStorageService} from '../../infrastructure/IStorageService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import type {ApplicationRepository} from '../../oauth/repositories/ApplicationRepository';
import type {OAuth2TokenRepository} from '../../oauth/repositories/OAuth2TokenRepository';
import type {WorkerTaskName} from '../../worker/WorkerLaneConfig';
import type {UserRepository} from '../repositories/UserRepository';

const CHUNK_SIZE = 100;

interface UserDeletionDependencies {
	userRepository: UserRepository;
	guildRepository: GuildRepository;
	channelRepository: ChannelRepository;
	favoriteMemeRepository: FavoriteMemeRepository;
	oauth2TokenRepository: OAuth2TokenRepository;
	storageService: IStorageService;
	purgeQueue: IPurgeQueue;
	userCacheService: UserCacheService;
	gatewayService: IGatewayService;
	snowflakeService: ISnowflakeService;
	discriminatorService: DiscriminatorService;
	stripe: Stripe | null;
	applicationRepository: ApplicationRepository;
	workerService: IWorkerService<WorkerTaskName>;
}

export async function processUserDeletion(
	userId: UserID,
	deletionReasonCode: number,
	deps: UserDeletionDependencies,
): Promise<void> {
	const {
		userRepository,
		guildRepository,
		channelRepository,
		favoriteMemeRepository,
		oauth2TokenRepository,
		storageService,
		purgeQueue,
		userCacheService,
		gatewayService,
		snowflakeService,
		stripe,
		applicationRepository,
		workerService,
	} = deps;
	Logger.debug({userId, deletionReasonCode}, 'Starting user account deletion');
	const user = await userRepository.findUnique(userId);
	if (!user) {
		Logger.warn({userId}, 'User not found, skipping deletion');
		return;
	}
	if (user.stripeSubscriptionId && stripe) {
		const MAX_RETRIES = 3;
		let lastError: unknown = null;
		for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
			try {
				Logger.debug(
					{userId, subscriptionId: user.stripeSubscriptionId, attempt},
					'Canceling active Stripe subscription',
				);
				const canceledSubscription = await stripe.subscriptions.cancel(user.stripeSubscriptionId, {
					invoice_now: false,
					prorate: false,
				});
				try {
					await getBillingRepository().subscriptions.upsertFromStripe(canceledSubscription, {
						knownUserId: user.id,
						snapshotCapturedAt: new Date(),
					});
				} catch (mirrorErr) {
					Logger.error(
						{mirrorErr, subId: canceledSubscription.id},
						'Mirror upsert failed after Stripe write; reconciler will heal',
					);
				}
				Logger.debug({userId, subscriptionId: user.stripeSubscriptionId}, 'Stripe subscription cancelled successfully');
				lastError = null;
				break;
			} catch (error) {
				lastError = error;
				const isLastAttempt = attempt === MAX_RETRIES - 1;
				Logger.error(
					{
						error,
						userId,
						subscriptionId: user.stripeSubscriptionId,
						attempt: attempt + 1,
						maxRetries: MAX_RETRIES,
						willRetry: !isLastAttempt,
					},
					isLastAttempt
						? 'Failed to cancel Stripe subscription after all retries'
						: 'Failed to cancel Stripe subscription, retrying with exponential backoff',
				);
				if (!isLastAttempt) {
					const backoffDelay = ms('1 second') * 2 ** attempt + randomInt(500);
					await new Promise((resolve) => setTimeout(resolve, backoffDelay));
				}
			}
		}
		if (lastError) {
			const error = new Error(
				`Failed to cancel Stripe subscription ${user.stripeSubscriptionId} for user ${userId} after ${MAX_RETRIES} attempts. User deletion halted to prevent billing issues.`,
				{cause: lastError},
			);
			throw error;
		}
	}
	const deletedUserId = createUserID(await snowflakeService.generate());
	Logger.debug({userId, deletedUserId}, 'Creating dedicated deleted user record');
	await userRepository.create({
		user_id: deletedUserId,
		username: DELETED_USER_USERNAME,
		discriminator: DELETED_USER_DISCRIMINATOR,
		global_name: DELETED_USER_GLOBAL_NAME,
		bot: false,
		system: false,
		email: null,
		email_verified: null,
		email_bounced: null,
		password_hash: null,
		password_last_changed_at: null,
		totp_secret: null,
		authenticator_types: null,
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
		flags: UserFlags.DELETED,
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
	});
	await userRepository.deleteUserSecondaryIndices(deletedUserId);
	Logger.debug({userId}, 'Leaving all guilds');
	const guildIds = await userRepository.getUserGuildIds(userId);
	for (const guildId of guildIds) {
		try {
			const member = await guildRepository.getMember(guildId, userId);
			if (!member) {
				Logger.debug({userId, guildId}, 'Member not found in guild, skipping member cleanup');
			} else {
				if (member.avatarHash) {
					try {
						const key = `guilds/${guildId}/users/${userId}/avatars/${member.avatarHash}`;
						await storageService.deleteObject(Config.s3.buckets.cdn, key);
						await purgeQueue.addUrls([`${Config.endpoints.media}/${key}`]);
					} catch (error) {
						Logger.error(
							{error, userId, guildId, avatarHash: member.avatarHash},
							'Failed to delete guild member avatar',
						);
					}
				}
				if (member.bannerHash) {
					try {
						const key = `guilds/${guildId}/users/${userId}/banners/${member.bannerHash}`;
						await storageService.deleteObject(Config.s3.buckets.cdn, key);
						await purgeQueue.addUrls([`${Config.endpoints.media}/${key}`]);
					} catch (error) {
						Logger.error(
							{error, userId, guildId, bannerHash: member.bannerHash},
							'Failed to delete guild member banner',
						);
					}
				}
				await guildRepository.deleteMember(guildId, userId);
				const guild = await guildRepository.findUnique(guildId);
				if (guild) {
					await guildRepository.upsertPartial(
						guildId,
						{member_count: Math.max(0, guild.memberCount - 1)},
						guild.toRow(),
					);
				}
				await gatewayService.dispatchGuild({
					guildId,
					event: 'GUILD_MEMBER_REMOVE',
					data: {user: {id: userId.toString()}},
				});
			}
		} catch (error) {
			Logger.error({error, userId, guildId}, 'Failed to remove user from guild membership');
		}
		try {
			await gatewayService.leaveGuild({userId, guildId});
			Logger.debug({userId, guildId}, 'Left guild successfully');
		} catch (error) {
			Logger.error({error, userId, guildId}, 'Failed to leave guild in gateway');
		}
	}
	Logger.debug({userId}, 'Leaving all group DMs');
	const allPrivateChannels = await userRepository.listPrivateChannels(userId);
	const groupDmChannels = allPrivateChannels.filter((channel) => channel.type === ChannelTypes.GROUP_DM);
	for (const channel of groupDmChannels) {
		try {
			const updatedRecipientIds = new Set<UserID>(channel.recipientIds);
			updatedRecipientIds.delete(userId);
			let newOwnerId = channel.ownerId;
			if (userId === channel.ownerId && updatedRecipientIds.size > 0) {
				newOwnerId = Array.from(updatedRecipientIds)[0];
			}
			if (updatedRecipientIds.size === 0) {
				await channelRepository.delete(channel.id);
				await userRepository.closeDmForUser(userId, channel.id);
				const channelResponse = await mapChannelToResponse({
					channel,
					currentUserId: null,
					userCacheService,
					requestCache: createRequestCache(),
				});
				await gatewayService.dispatchPresence({
					userId,
					event: 'CHANNEL_DELETE',
					data: channelResponse,
				});
				Logger.debug({userId, channelId: channel.id}, 'Deleted empty group DM');
				continue;
			}
			const updatedNicknames = new Map(channel.nicknames);
			updatedNicknames.delete(userId.toString());
			await channelRepository.upsert({
				...channel.toRow(),
				owner_id: newOwnerId,
				recipient_ids: updatedRecipientIds,
				nicks: updatedNicknames.size > 0 ? updatedNicknames : null,
			});
			await userRepository.closeDmForUser(userId, channel.id);
			const messageId = createMessageID(await snowflakeService.generateForChannel(channel.id));
			await channelRepository.upsertMessage({
				channel_id: channel.id,
				bucket: BucketUtils.makeBucket(messageId),
				message_id: messageId,
				author_id: userId,
				type: MessageTypes.RECIPIENT_REMOVE,
				webhook_id: null,
				webhook_name: null,
				webhook_avatar_hash: null,
				content: null,
				edited_timestamp: null,
				pinned_timestamp: null,
				flags: 0,
				mention_everyone: false,
				mention_users: new Set([userId]),
				mention_roles: null,
				mention_channels: null,
				attachments: null,
				embeds: null,
				sticker_items: null,
				message_reference: null,
				message_snapshots: null,
				call: null,
				has_reaction: false,
				version: 1,
			});
			const recipientUserResponse = await userCacheService.getUserPartialResponse(userId, createRequestCache());
			for (const recId of updatedRecipientIds) {
				await gatewayService.dispatchPresence({
					userId: recId,
					event: 'CHANNEL_RECIPIENT_REMOVE',
					data: {
						channel_id: channel.id.toString(),
						user: recipientUserResponse,
					},
				});
			}
			const channelResponse = await mapChannelToResponse({
				channel,
				currentUserId: null,
				userCacheService,
				requestCache: createRequestCache(),
			});
			await gatewayService.dispatchPresence({
				userId,
				event: 'CHANNEL_DELETE',
				data: channelResponse,
			});
			Logger.debug({userId, channelId: channel.id}, 'Left group DM successfully');
		} catch (error) {
			Logger.error({error, userId, channelId: channel.id}, 'Failed to leave group DM');
		}
	}
	Logger.debug({userId}, 'Anonymizing user messages');
	let lastMessageId: MessageID | undefined;
	let processedCount = 0;
	while (true) {
		const messagesToAnonymize = await channelRepository.listMessagesByAuthor(userId, CHUNK_SIZE, lastMessageId);
		if (messagesToAnonymize.length === 0) {
			break;
		}
		for (const {channelId, messageId} of messagesToAnonymize) {
			await channelRepository.anonymizeMessage(channelId, messageId, deletedUserId);
		}
		processedCount += messagesToAnonymize.length;
		lastMessageId = messagesToAnonymize[messagesToAnonymize.length - 1].messageId;
		Logger.debug({userId, processedCount, chunkSize: messagesToAnonymize.length}, 'Anonymized message chunk');
		if (messagesToAnonymize.length < CHUNK_SIZE) {
			break;
		}
	}
	Logger.debug({userId, totalProcessed: processedCount}, 'Completed message anonymization');
	Logger.debug({userId}, 'Deleting S3 objects');
	if (user.avatarHash) {
		try {
			await storageService.deleteAvatar({prefix: 'avatars', key: `${userId}/${user.avatarHash}`});
			await purgeQueue.addUrls([`${Config.endpoints.media}/avatars/${userId}/${user.avatarHash}`]);
			Logger.debug({userId, avatarHash: user.avatarHash}, 'Deleted avatar');
		} catch (error) {
			Logger.error({error, userId}, 'Failed to delete avatar');
		}
	}
	if (user.bannerHash) {
		try {
			await storageService.deleteAvatar({prefix: 'banners', key: `${userId}/${user.bannerHash}`});
			await purgeQueue.addUrls([`${Config.endpoints.media}/banners/${userId}/${user.bannerHash}`]);
			Logger.debug({userId, bannerHash: user.bannerHash}, 'Deleted banner');
		} catch (error) {
			Logger.error({error, userId}, 'Failed to delete banner');
		}
	}
	const favoriteMemes = await favoriteMemeRepository.findByUserId(userId);
	for (const meme of favoriteMemes) {
		try {
			await storageService.deleteObject(Config.s3.buckets.cdn, meme.storageKey);
			Logger.debug({userId, memeId: meme.id}, 'Deleted favorite meme');
		} catch (error) {
			Logger.error({error, userId, memeId: meme.id}, 'Failed to delete favorite meme');
		}
	}
	await favoriteMemeRepository.deleteAllByUserId(userId);
	Logger.debug({userId}, 'Deleting OAuth tokens');
	await Promise.all([
		oauth2TokenRepository.deleteAllAccessTokensForUser(userId),
		oauth2TokenRepository.deleteAllRefreshTokensForUser(userId),
	]);
	Logger.debug({userId}, 'Deleting owned developer applications and bots');
	try {
		const applications = await applicationRepository.listApplicationsByOwner(userId);
		for (const application of applications) {
			await workerService.addJob('applicationProcessDeletion', {
				applicationId: application.applicationId.toString(),
			});
		}
		Logger.debug({userId, applicationCount: applications.length}, 'Scheduled application deletions');
	} catch (error) {
		Logger.error({error, userId}, 'Failed to schedule application deletions');
	}
	Logger.debug({userId}, 'Deleting user data');
	await Promise.all([
		guildRepository.deleteAllBansForUser(userId),
		userRepository.deleteUserSettings(userId),
		userRepository.deleteAllUserGuildSettings(userId),
		userRepository.deleteAllRelationships(userId),
		userRepository.deleteAllNotes(userId),
		userRepository.deleteAllReadStates(userId),
		userRepository.deleteAllSavedMessages(userId),
		userRepository.deleteAllAuthSessions(userId),
		userRepository.deleteAllMfaBackupCodes(userId),
		userRepository.deleteAllWebAuthnCredentials(userId),
		userRepository.deleteAllPushSubscriptions(userId),
		userRepository.deleteAllRecentMentions(userId),
		userRepository.deleteAllAuthorizedIps(userId),
		userRepository.deletePinnedDmsByUserId(userId),
	]);
	await userRepository.deleteUserSecondaryIndices(userId);
	const userForAnonymization = await userRepository.findUniqueAssert(userId);
	Logger.debug({userId}, 'Anonymizing user record');
	const anonymisedUser = await userRepository.patchUpsert(
		userId,
		{
			username: DELETED_USER_USERNAME,
			discriminator: DELETED_USER_DISCRIMINATOR,
			global_name: DELETED_USER_GLOBAL_NAME,
			email: null,
			email_verified: false,
			password_hash: null,
			totp_secret: null,
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
			authenticator_types: new Set(),
		},
		userForAnonymization.toRow(),
	);
	await userCacheService.setUserPartialResponseFromUser(anonymisedUser);
	Logger.debug({userId, deletionReasonCode}, 'User account anonymization completed successfully');
}
