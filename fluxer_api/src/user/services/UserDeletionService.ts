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

import type Stripe from 'stripe';
import {type ChannelID, createMessageID, createUserID, type MessageID, type UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {ChannelTypes, MessageTypes, UserFlags} from '~/Constants';
import {mapChannelToResponse} from '~/channel/ChannelModel';
import type {ChannelRepository} from '~/channel/ChannelRepository';
import type {FavoriteMemeRepository} from '~/favorite_meme/FavoriteMemeRepository';
import type {GuildRepository} from '~/guild/repositories/GuildRepository';
import type {CloudflarePurgeQueue, NoopCloudflarePurgeQueue} from '~/infrastructure/CloudflarePurgeQueue';
import type {DiscriminatorService} from '~/infrastructure/DiscriminatorService';
import type {GatewayService} from '~/infrastructure/GatewayService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {StorageService} from '~/infrastructure/StorageService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {Logger} from '~/Logger';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {ApplicationRepository} from '~/oauth/repositories/ApplicationRepository';
import type {OAuth2TokenRepository} from '~/oauth/repositories/OAuth2TokenRepository';
import type {UserRepository} from '~/user/UserRepository';
import * as BucketUtils from '~/utils/BucketUtils';
import {randomString} from '~/utils/RandomUtils';
import type {WorkerService} from '~/worker/WorkerService';

function createRequestCache(): RequestCache {
	return {
		userPartials: new Map(),
		clear: () => {},
	};
}

const CHUNK_SIZE = 100;
const DELETED_USERNAME = '__deleted__';

export interface UserDeletionDependencies {
	userRepository: UserRepository;
	guildRepository: GuildRepository;
	channelRepository: ChannelRepository;
	favoriteMemeRepository: FavoriteMemeRepository;
	oauth2TokenRepository: OAuth2TokenRepository;
	storageService: StorageService;
	cloudflarePurgeQueue: CloudflarePurgeQueue | NoopCloudflarePurgeQueue;
	userCacheService: UserCacheService;
	gatewayService: GatewayService;
	snowflakeService: SnowflakeService;
	discriminatorService: DiscriminatorService;
	stripe: Stripe | null;
	applicationRepository: ApplicationRepository;
	workerService: WorkerService;
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
		cloudflarePurgeQueue,
		userCacheService,
		gatewayService,
		snowflakeService,
		discriminatorService,
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
		try {
			Logger.debug({userId, subscriptionId: user.stripeSubscriptionId}, 'Canceling active Stripe subscription');
			await stripe.subscriptions.cancel(user.stripeSubscriptionId, {
				invoice_now: false,
				prorate: false,
			});
			Logger.debug({userId, subscriptionId: user.stripeSubscriptionId}, 'Stripe subscription cancelled successfully');
		} catch (error) {
			Logger.error(
				{error, userId, subscriptionId: user.stripeSubscriptionId},
				'Failed to cancel Stripe subscription, continuing with deletion',
			);
		}
	}

	const deletedUserId = createUserID(snowflakeService.generate());
	Logger.debug({userId, deletedUserId}, 'Creating dedicated deleted user record');

	let foundUsername: string;
	let foundDiscriminator: number;
	while (true) {
		foundUsername = `DeletedUser${randomString(8)}`;
		const discriminatorResult = await discriminatorService.generateDiscriminator({
			username: foundUsername,
			isPremium: false,
		});
		if (!discriminatorResult.available || discriminatorResult.discriminator === -1) {
			continue;
		}
		foundDiscriminator = discriminatorResult.discriminator;
		break;
	}

	await userRepository.create({
		user_id: deletedUserId,
		username: foundUsername,
		discriminator: foundDiscriminator,
		global_name: 'Deleted User',
		bot: false,
		system: true,
		email: null,
		email_verified: null,
		email_bounced: null,
		phone: null,
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
		date_of_birth: null,
		locale: null,
		flags: UserFlags.DELETED,
		premium_type: null,
		premium_since: null,
		premium_until: null,
		premium_will_cancel: null,
		premium_billing_cycle: null,
		premium_lifetime_sequence: null,
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
		first_refund_at: null,
		beta_code_allowance: null,
		beta_code_last_reset_at: null,
		gift_inventory_server_seq: null,
		gift_inventory_client_seq: null,
		premium_onboarding_dismissed_at: null,
		version: 1,
	});

	await userRepository.deleteUserSecondaryIndices(deletedUserId);

	Logger.debug({userId}, 'Leaving all guilds');
	const guildIds = await userRepository.getUserGuildIds(userId);

	for (const guildId of guildIds) {
		try {
			const member = await guildRepository.getMember(guildId, userId);
			if (!member) {
				Logger.debug({userId, guildId}, 'Member not found in guild, skipping');
				continue;
			}

			if (member.avatarHash) {
				try {
					const key = `guilds/${guildId}/users/${userId}/avatars/${member.avatarHash}`;
					await storageService.deleteObject(Config.s3.buckets.cdn, key);
					await cloudflarePurgeQueue.addUrls([`${Config.endpoints.media}/${key}`]);
				} catch (error) {
					Logger.error({error, userId, guildId, avatarHash: member.avatarHash}, 'Failed to delete guild member avatar');
				}
			}

			if (member.bannerHash) {
				try {
					const key = `guilds/${guildId}/users/${userId}/banners/${member.bannerHash}`;
					await storageService.deleteObject(Config.s3.buckets.cdn, key);
					await cloudflarePurgeQueue.addUrls([`${Config.endpoints.media}/${key}`]);
				} catch (error) {
					Logger.error({error, userId, guildId, bannerHash: member.bannerHash}, 'Failed to delete guild member banner');
				}
			}

			await guildRepository.deleteMember(guildId, userId);

			const guild = await guildRepository.findUnique(guildId);
			if (guild) {
				const guildRow = guild.toRow();
				await guildRepository.upsert({
					...guildRow,
					member_count: Math.max(0, guild.memberCount - 1),
				});
			}

			await gatewayService.dispatchGuild({
				guildId,
				event: 'GUILD_MEMBER_REMOVE',
				data: {user: {id: userId.toString()}},
			});

			await gatewayService.leaveGuild({userId, guildId});

			Logger.debug({userId, guildId}, 'Left guild successfully');
		} catch (error) {
			Logger.error({error, userId, guildId}, 'Failed to leave guild');
		}
	}

	Logger.debug({userId}, 'Leaving all group DMs');

	const allPrivateChannels = await userRepository.listPrivateChannels(userId);
	const groupDmChannels = allPrivateChannels.filter((channel) => channel.type === ChannelTypes.GROUP_DM);

	for (const channel of groupDmChannels) {
		try {
			const updatedRecipientIds = new Set(channel.recipientIds);
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

			const messageId = createMessageID(snowflakeService.generate());

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

	let lastChannelId: ChannelID | undefined;
	let lastMessageId: MessageID | undefined;
	let processedCount = 0;

	while (true) {
		const messagesToAnonymize = await channelRepository.listMessagesByAuthor(
			userId,
			CHUNK_SIZE,
			lastChannelId,
			lastMessageId,
		);

		if (messagesToAnonymize.length === 0) {
			break;
		}

		for (const {channelId, messageId} of messagesToAnonymize) {
			await channelRepository.anonymizeMessage(channelId, messageId, deletedUserId);
		}

		processedCount += messagesToAnonymize.length;
		lastChannelId = messagesToAnonymize[messagesToAnonymize.length - 1].channelId;
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
			await cloudflarePurgeQueue.addUrls([`${Config.endpoints.media}/avatars/${userId}/${user.avatarHash}`]);
			Logger.debug({userId, avatarHash: user.avatarHash}, 'Deleted avatar');
		} catch (error) {
			Logger.error({error, userId}, 'Failed to delete avatar');
		}
	}

	if (user.bannerHash) {
		try {
			await storageService.deleteAvatar({prefix: 'banners', key: `${userId}/${user.bannerHash}`});
			await cloudflarePurgeQueue.addUrls([`${Config.endpoints.media}/banners/${userId}/${user.bannerHash}`]);
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
		userRepository.deleteAllBetaCodes(userId),
		userRepository.deleteAllRecentMentions(userId),
		userRepository.deleteAllAuthorizedIps(userId),
		userRepository.deletePendingVerification(userId),
		userRepository.deletePinnedDmsByUserId(userId),
	]);

	await userRepository.deleteUserSecondaryIndices(userId);

	Logger.debug({userId}, 'Anonymizing user record');

	await userRepository.patchUpsert(userId, {
		username: DELETED_USERNAME,
		discriminator: 0,
		email: null,
		email_verified: false,
		phone: null,
		password_hash: null,
		totp_secret: null,
		avatar_hash: null,
		banner_hash: null,
		bio: null,
		pronouns: null,
		accent_color: null,
		date_of_birth: null,
		flags: UserFlags.DELETED,
		premium_type: null,
		premium_since: null,
		premium_until: null,
		stripe_customer_id: null,
		stripe_subscription_id: null,
		pending_deletion_at: null,
		authenticator_types: new Set(),
	});

	Logger.debug({userId, deletionReasonCode}, 'User account anonymization completed successfully');
	getMetricsService().counter({
		name: 'user.deletion',
		dimensions: {
			reason_code: deletionReasonCode.toString(),
			source: 'worker',
		},
	});
}
