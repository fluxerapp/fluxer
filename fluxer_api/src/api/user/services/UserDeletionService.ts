// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomInt} from 'node:crypto';
import type {AdminRepository} from '@app/api/admin/AdminRepository';
import {deleteRecoveryKit} from '@app/api/auth/AuthRecoveryKit';
import {revokeAllAuthSessions} from '@app/api/auth/AuthSessionRevocation';
import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import {createMessageID, createUserID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {UserMessageDeletionService} from '@app/api/channel/services/message/UserMessageDeletionService';
import type {ConnectionRepository} from '@app/api/connection/ConnectionRepository';
import {IP_BAN_REFRESH_CHANNEL} from '@app/api/constants/IpBan';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import type {FavoriteMemeRepository} from '@app/api/favorite_meme/FavoriteMemeRepository';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {guildVisitRepository} from '@app/api/guild/visits/GuildVisitorAccess';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import type {IPurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {REGISTRATION_PENDING_APPROVAL_TRAIT} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import type {OperatorPassServices} from '@app/api/middleware/OperatorPassServiceFactory';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {getEnforcementDecisionRepository, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {ApplicationRepository} from '@app/api/oauth/repositories/ApplicationRepository';
import type {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {endOperatorPassesForDeletion} from '@app/api/operator_pass/OperatorPassHoldingService';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {isPendingDeletionBlocked} from '@app/api/user/services/PendingDeletionCoordinator';
import {
	deletedUserIdentity,
	isEnforcementDeletion,
	isSsoLinkedUser,
	personalFieldsToClear,
} from '@app/api/user/UserHelpers';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms} from 'itty-time';
import type Stripe from 'stripe';

const CHUNK_SIZE = 100;
const ssoIdentityRepository = new SsoIdentityRepository();

interface UserDeletionDependencies {
	operatorPassServices: Pick<OperatorPassServices, 'repository' | 'wallService' | 'holdingService'>;
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

	stripe: Stripe | null;
	applicationRepository: ApplicationRepository;
	workerService: IWorkerService<WorkerTaskName>;
	connectionRepository: ConnectionRepository;
	storeEntitlementService: StoreEntitlementService;
	adminRepository: AdminRepository;
	cacheService: ICacheService;
}

async function releaseSsoIdentitiesOf(user: User, deletionReasonCode: number): Promise<void> {
	if (!isSsoLinkedUser(user)) return;
	if (isEnforcementDeletion({enforcementDecisionId: user.enforcementDecisionId, deletionReasonCode})) return;
	const released = await ssoIdentityRepository.releaseIdentitiesForUser(user.id);
	if (released > 0) {
		Logger.debug({userId: user.id, released}, 'Released SSO identities');
	}
}

async function withdrawPendingRegistrationOf(user: User): Promise<void> {
	if (!user.traits.has(REGISTRATION_PENDING_APPROVAL_TRAIT)) return;
	await getInstanceConfigRepository().removePendingRegistration(user.id.toString());
}

async function removeNetworkBlocksOf(userId: UserID, deps: UserDeletionDependencies): Promise<void> {
	const decisions = await getEnforcementDecisionRepository().listBySubject('user', userId);
	if (decisions.length === 0) return;
	const removed = await deps.adminRepository.removeDecisionIpBans(decisions.map((decision) => decision.decision_id));
	if (removed.length > 0) {
		await deps.cacheService.publish(IP_BAN_REFRESH_CHANNEL, 'refresh');
	}
}

async function createDeletedUserPlaceholder(
	userRepository: UserRepository,
	snowflakeService: ISnowflakeService,
	userId: UserID,
): Promise<UserID> {
	const deletedUserId = createUserID(await snowflakeService.generate());
	Logger.debug({userId, deletedUserId}, 'Creating dedicated deleted user record');
	await userRepository.create({
		...EMPTY_USER_ROW,
		...deletedUserIdentity(),
		user_id: deletedUserId,
		bot: false,
		system: false,
	});
	await userRepository.deleteUserSecondaryIndices(deletedUserId);
	return deletedUserId;
}

export async function processUserDeletion(
	userId: UserID,
	pendingDeletionAt: Date,
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
		connectionRepository,
		storeEntitlementService,
	} = deps;
	Logger.debug({userId, deletionReasonCode}, 'Starting user account deletion');
	const scheduledUser = await userRepository.findUnique(userId);
	if (
		!scheduledUser ||
		scheduledUser.pendingDeletionAt?.getTime() !== pendingDeletionAt.getTime() ||
		(!scheduledUser.deletionStartedAt &&
			(pendingDeletionAt.getTime() > Date.now() || isPendingDeletionBlocked(scheduledUser)))
	) {
		Logger.info({userId, pendingDeletionAt}, 'Account deletion schedule is no longer eligible');
		return;
	}
	if (scheduledUser.stripeSubscriptionId && stripe) {
		const MAX_RETRIES = 3;
		let lastError: unknown = null;
		for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
			try {
				Logger.debug(
					{userId, subscriptionId: scheduledUser.stripeSubscriptionId, attempt},
					'Canceling active Stripe subscription',
				);
				const canceledSubscription = await stripe.subscriptions.cancel(scheduledUser.stripeSubscriptionId, {
					invoice_now: false,
					prorate: false,
				});
				try {
					await getBillingRepository().subscriptions.upsertFromStripe(canceledSubscription, {
						knownUserId: scheduledUser.id,
						snapshotCapturedAt: new Date(),
					});
				} catch (mirrorErr) {
					Logger.error(
						{mirrorErr, subId: canceledSubscription.id},
						'Mirror upsert failed after Stripe write; reconciler will heal',
					);
				}
				Logger.debug(
					{userId, subscriptionId: scheduledUser.stripeSubscriptionId},
					'Stripe subscription cancelled successfully',
				);
				lastError = null;
				break;
			} catch (error) {
				lastError = error;
				const isLastAttempt = attempt === MAX_RETRIES - 1;
				Logger.error(
					{
						error,
						userId,
						subscriptionId: scheduledUser.stripeSubscriptionId,
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
				`Failed to cancel Stripe subscription ${scheduledUser.stripeSubscriptionId} for user ${userId} after ${MAX_RETRIES} attempts. User deletion halted to prevent billing issues.`,
				{cause: lastError},
			);
			throw error;
		}
	}
	const user = await userRepository.startDeletion(userId, pendingDeletionAt);
	if (!user) {
		Logger.info({userId, pendingDeletionAt}, 'Account deletion schedule is no longer eligible');
		return;
	}
	await removeNetworkBlocksOf(userId, deps);
	await releaseSsoIdentitiesOf(user, deletionReasonCode);
	await withdrawPendingRegistrationOf(user);
	await storeEntitlementService.stopBillingForDeletedUser(userId);
	await connectionRepository.sealAndDeleteForUser(userId);
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
				await channelRepository.channelData.delete(channel.id);
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
			await channelRepository.channelData.upsert({
				...channel.toRow(),
				owner_id: newOwnerId,
				recipient_ids: updatedRecipientIds,
				nicks: updatedNicknames.size > 0 ? updatedNicknames : null,
			});
			await userRepository.closeDmForUser(userId, channel.id);
			const messageId = createMessageID(await snowflakeService.generateForChannel(channel.id));
			await channelRepository.messages.upsertMessage({
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
	if (user.pendingBulkMessageDeletionAt) {
		const deleted = await new UserMessageDeletionService({
			channelRepository,
			gatewayService,
			storageService,
			purgeQueue,
			workerService,
		}).deleteUserMessagesBulk(userId);
		await userRepository.patchUpsert(
			userId,
			{
				pending_bulk_message_deletion_at: null,
				pending_bulk_message_deletion_channel_count: null,
				pending_bulk_message_deletion_message_count: null,
			},
			user.toRow(),
		);
		Logger.debug({userId, deleted}, 'Deleted messages scheduled for deletion before anonymizing the rest');
	}
	Logger.debug({userId}, 'Anonymizing user messages');
	let lastMessageId: MessageID | undefined;
	let processedCount = 0;
	let deletedUserId: UserID | null = null;
	while (true) {
		const messagesToAnonymize = await channelRepository.messages.listMessagesByAuthor(
			userId,
			CHUNK_SIZE,
			lastMessageId,
		);
		if (messagesToAnonymize.length === 0) {
			break;
		}
		deletedUserId ??= await createDeletedUserPlaceholder(userRepository, snowflakeService, userId);
		for (const {channelId, messageId} of messagesToAnonymize) {
			await channelRepository.messages.anonymizeMessage(channelId, messageId, deletedUserId);
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
	const operatorPassRepository = deps.operatorPassServices.repository;
	await Promise.all([
		guildRepository.deleteAllBansForUser(userId),
		guildVisitRepository().deleteAllForUser(userId),
		userRepository.deleteUserSettings(userId),
		userRepository.deleteAllUserGuildSettings(userId),
		userRepository.deleteAllRelationships(userId),
		userRepository.deleteAllNotes(userId),
		userRepository.deleteAllReadStates(userId),
		userRepository.deleteAllSavedMessages(userId),
		revokeAllAuthSessions({users: userRepository, gateway: gatewayService}, userId),
		userRepository.deleteAllMfaBackupCodes(userId),
		userRepository.deleteAllWebAuthnCredentials(userId),
		deleteRecoveryKit(userId),
		userRepository.deleteAllPushSubscriptions(userId),
		userRepository.deleteAllRecentMentions(userId),
		userRepository.deleteAllAuthorizedIps(userId),
		userRepository.deletePinnedDmsByUserId(userId),
		endOperatorPassesForDeletion(operatorPassRepository, userId).then(async (buyers) => {
			await operatorPassRepository.deleteOperatorPass(userId);
			for (const buyer of buyers) {
				await deps.operatorPassServices.holdingService.syncHolding(buyer);
			}
		}),
		deps.operatorPassServices.wallService.forgetForDeletion(userId),
	]);
	await userRepository.deleteUserSecondaryIndices(userId);
	const userForAnonymization = await userRepository.findUniqueAssert(userId);
	Logger.debug({userId}, 'Anonymizing user record');
	const anonymisedUser = await userRepository.anonymizeForDeletion(userForAnonymization, {
		...deletedUserIdentity(),
		...personalFieldsToClear(userForAnonymization.toRow()),
	});

	await userCacheService.setUserPartialResponseFromUser(anonymisedUser);
	await userRepository.completeDeletion(anonymisedUser);
	await emitActivity('account_deleted', userId.toString(), {user_id: userId.toString()}, null, userId.toString());
	Logger.debug({userId, deletionReasonCode}, 'User account anonymization completed successfully');
}
