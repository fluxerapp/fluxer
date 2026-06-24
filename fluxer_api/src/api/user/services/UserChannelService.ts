// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_GROUP_DM_RECIPIENTS, MAX_GROUP_DMS_PER_USER} from '@fluxer/constants/src/LimitConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {CannotSendMessagesToUserError} from '@fluxer/errors/src/domains/channel/CannotSendMessagesToUserError';
import type {GroupDmUnaddableRecipient} from '@fluxer/errors/src/domains/channel/GroupDmRecipientsNotAddableError';
import {GroupDmRecipientsNotAddableError} from '@fluxer/errors/src/domains/channel/GroupDmRecipientsNotAddableError';
import {MaxGroupDmRecipientsError} from '@fluxer/errors/src/domains/channel/MaxGroupDmRecipientsError';
import {MaxGroupDmsError} from '@fluxer/errors/src/domains/channel/MaxGroupDmsError';
import {UnclaimedAccountCannotJoinGroupDmsError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotJoinGroupDmsError';
import {UnclaimedAccountCannotSendDirectMessagesError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotSendDirectMessagesError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {CreatePrivateChannelRequest} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';
import type {ApiContext} from '../../ApiContext';
import {requireEmailVerified} from '../../auth/EmailVerificationUtils';
import type {ChannelID, UserID} from '../../BrandedTypes';
import {createChannelID, createMessageID, createUserID} from '../../BrandedTypes';
import {mapChannelToResponse} from '../../channel/ChannelMappers';
import {SYSTEM_USER_ID} from '../../constants/Core';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {ChannelService} from '../../channel/services/ChannelService';
import {dispatchMessageCreateBroadcast} from '../../channel/services/message/MessageGatewayDispatch';
import {
	createMessageResponseDataService,
	messageResponseAccessForGuild,
} from '../../channel/services/message/MessageResponseDataService';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../models/Channel';
import type {Message} from '../../models/Message';
import type {User} from '../../models/User';
import type {UserPermissionUtils} from '../../utils/UserPermissionUtils';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserChannelRepository} from '../repositories/IUserChannelRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import {isBugHunterBotUser} from '../UserHelpers';
import type {DirectMessageSpamMitigationService} from './DirectMessageSpamMitigationService';
import {createDirectMessageSpamMitigationService} from './DirectMessageSpamMitigationService';

interface UserChannelRepository extends IUserAccountRepository, IUserChannelRepository, IUserRelationshipRepository {}

export class UserChannelService {
	private readonly userRepository: UserChannelRepository;
	private readonly channelService: ChannelService;
	private readonly channelRepository: IChannelRepository;
	private readonly gatewayService: IGatewayService;
	private readonly snowflakeService: ISnowflakeService;
	private readonly userPermissionUtils: UserPermissionUtils;
	private readonly limitConfigService: LimitConfigService;
	private readonly dmSpamMitigationService: DirectMessageSpamMitigationService | null;

	constructor(
		apiContext: ApiContext,
		channelService: ChannelService,
		channelRepository: IChannelRepository,
		userPermissionUtils: UserPermissionUtils,
		limitConfigService: LimitConfigService,
	);

	constructor(
		userRepository: UserChannelRepository,
		channelService: ChannelService,
		channelRepository: IChannelRepository,
		gatewayService: IGatewayService,
		snowflakeService: ISnowflakeService,
		userPermissionUtils: UserPermissionUtils,
		limitConfigService: LimitConfigService,
	);

	constructor(
		...args:
			| [
					apiContext: ApiContext,
					channelService: ChannelService,
					channelRepository: IChannelRepository,
					userPermissionUtils: UserPermissionUtils,
					limitConfigService: LimitConfigService,
			  ]
			| [
					userRepository: UserChannelRepository,
					channelService: ChannelService,
					channelRepository: IChannelRepository,
					gatewayService: IGatewayService,
					snowflakeService: ISnowflakeService,
					userPermissionUtils: UserPermissionUtils,
					limitConfigService: LimitConfigService,
			  ]
	) {
		if (args.length === 5) {
			const [apiContext, channelService, channelRepository, userPermissionUtils, limitConfigService] = args;
			const {users, gateway, snowflake} = apiContext.services;
			this.userRepository = users;
			this.gatewayService = gateway;
			this.snowflakeService = snowflake;
			this.channelService = channelService;
			this.channelRepository = channelRepository;
			this.userPermissionUtils = userPermissionUtils;
			this.limitConfigService = limitConfigService;
			this.dmSpamMitigationService = createDirectMessageSpamMitigationService(apiContext, this.userRepository);
			return;
		}
		const [
			userRepository,
			channelService,
			channelRepository,
			gatewayService,
			snowflakeService,
			userPermissionUtils,
			limitConfigService,
		] = args;
		this.userRepository = userRepository;
		this.gatewayService = gatewayService;
		this.snowflakeService = snowflakeService;
		this.channelService = channelService;
		this.channelRepository = channelRepository;
		this.userPermissionUtils = userPermissionUtils;
		this.limitConfigService = limitConfigService;
		this.dmSpamMitigationService = null;
	}

	async getPrivateChannels(userId: UserID): Promise<Array<Channel>> {
		return await this.userRepository.listPrivateChannels(userId);
	}

	async createOrOpenDMChannel({
		userId,
		data,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		data: CreatePrivateChannelRequest;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const callingUser = await this.userRepository.findUnique(userId);
		if (!callingUser) throw new UnknownUserError();
		if (callingUser.isUnclaimedAccount()) {
			if (data.recipients !== undefined) {
				throw new UnclaimedAccountCannotJoinGroupDmsError();
			}
			throw new UnclaimedAccountCannotSendDirectMessagesError();
		}
		requireEmailVerified(callingUser, 'direct_message');
		if (data.recipients !== undefined) {
			return await this.createGroupDMChannel({
				userId,
				recipients: data.recipients,
				userCacheService,
				requestCache,
			});
		}
		if (data.recipient_id == null) {
			throw InputValidationError.fromCode('recipient_id', ValidationErrorCodes.RECIPIENT_IDS_CANNOT_BE_EMPTY);
		}
		const recipientId = createUserID(data.recipient_id);
		if (userId === recipientId) {
			throw InputValidationError.fromCode('recipient_id', ValidationErrorCodes.CANNOT_DM_YOURSELF);
		}
		if (this.dmSpamMitigationService?.shouldSuppressDirectMessageDelivery(callingUser)) {
			const targetUser = await this.userRepository.findUnique(recipientId);
			if (!targetUser) throw new UnknownUserError();
			return await this.createOrOpenLocalOnlyDMChannel({userId, recipientId, userCacheService, requestCache});
		}
		const existingChannel = await this.userRepository.findExistingDmState(userId, recipientId);
		if (existingChannel) {
			return await this.reopenExistingDMChannel({userId, existingChannel, userCacheService, requestCache});
		}
		const targetUser = await this.userRepository.findUnique(recipientId);
		if (!targetUser) throw new UnknownUserError();
		if (recipientId === SYSTEM_USER_ID) {
			return await this.createNewDMChannel({userId, recipientId, userCacheService, requestCache});
		}
		await this.validateNewDmAllowed({sender: callingUser, recipient: targetUser});
		const channel = await this.createNewDMChannel({userId, recipientId, userCacheService, requestCache});
		return channel;
	}

	async pinDmChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		const channel = await this.channelService.channelData.operations.getChannel({userId, channelId});
		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.CHANNEL_MUST_BE_DM_OR_GROUP_DM);
		}
		if (!channel.recipientIds.has(userId)) {
			throw new MissingAccessError();
		}
		const newPinnedDMs = await this.userRepository.addPinnedDm(userId, channelId);
		await this.gatewayService.dispatchPresence({
			userId: userId,
			event: 'USER_PINNED_DMS_UPDATE',
			data: newPinnedDMs.map(String),
		});
	}

	async unpinDmChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		const channel = await this.channelService.channelData.operations.getChannel({userId, channelId});
		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw InputValidationError.fromCode('channel_id', ValidationErrorCodes.CHANNEL_MUST_BE_DM_OR_GROUP_DM);
		}
		if (!channel.recipientIds.has(userId)) {
			throw new MissingAccessError();
		}
		const newPinnedDMs = await this.userRepository.removePinnedDm(userId, channelId);
		await this.gatewayService.dispatchPresence({
			userId: userId,
			event: 'USER_PINNED_DMS_UPDATE',
			data: newPinnedDMs.map(String),
		});
	}

	async preloadDMMessages(params: {
		userId: UserID;
		channelIds: Array<ChannelID>;
	}): Promise<Record<string, MessageResponse | null>> {
		const {userId, channelIds} = params;
		if (channelIds.length > 100) {
			throw InputValidationError.fromCode('channels', ValidationErrorCodes.CANNOT_PRELOAD_MORE_THAN_100_CHANNELS);
		}
		const responseDataService = createMessageResponseDataService();
		const results: Record<string, MessageResponse | null> = {};
		const fetchPromises = channelIds.map(async (channelId) => {
			try {
				const channel = await this.channelService.channelData.operations.getChannel({userId, channelId});
				if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
					return;
				}
				if (!channel.recipientIds.has(userId)) {
					return;
				}
				const messages = await responseDataService.listMessages({
					userId,
					channelId,
					limit: 1,
					access: messageResponseAccessForGuild(null),
				});
				results[channelId.toString()] = messages[0] ?? null;
			} catch {
				results[channelId.toString()] = null;
			}
		});
		await Promise.all(fetchPromises);
		return results;
	}

	async getExistingDmForUsers(userId: UserID, recipientId: UserID): Promise<Channel | null> {
		return await this.userRepository.findExistingDmState(userId, recipientId);
	}

	private async validateNewDmAllowed({sender, recipient}: {sender: User; recipient: User}): Promise<void> {
		if (isBugHunterBotUser(sender)) {
			return;
		}
		const [senderBlockedRecipient, recipientBlockedSender, friendship] = await Promise.all([
			this.userRepository.getRelationship(sender.id, recipient.id, RelationshipTypes.BLOCKED),
			this.userRepository.getRelationship(recipient.id, sender.id, RelationshipTypes.BLOCKED),
			this.userRepository.getRelationship(sender.id, recipient.id, RelationshipTypes.FRIEND),
		]);
		if (senderBlockedRecipient || recipientBlockedSender) {
			throw new CannotSendMessagesToUserError();
		}
		if (friendship) {
			return;
		}
		const hasMutualGuilds = await this.userPermissionUtils.checkMutualGuildsForDmAccessAsync({
			userId: sender.id,
			targetId: recipient.id,
		});
		if (!hasMutualGuilds) {
			throw new CannotSendMessagesToUserError();
		}
	}

	async ensureDmOpenForBothUsers({
		userId,
		recipientId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const existingChannel = await this.userRepository.findExistingDmState(userId, recipientId);
		if (existingChannel) {
			const [isUserOpen, isRecipientOpen] = await Promise.all([
				this.userRepository.isDmChannelOpen(userId, existingChannel.id),
				this.userRepository.isDmChannelOpen(recipientId, existingChannel.id),
			]);
			if (!isUserOpen) {
				await this.userRepository.openPrivateChannelForUser(userId, existingChannel);
				await this.dispatchChannelCreate({userId, channel: existingChannel, userCacheService, requestCache});
			}
			if (!isRecipientOpen) {
				await this.userRepository.openPrivateChannelForUser(recipientId, existingChannel);
				await this.dispatchChannelCreate({
					userId: recipientId,
					channel: existingChannel,
					userCacheService,
					requestCache,
				});
			}
			return existingChannel;
		}
		return await this.createNewDmForBothUsers({userId, recipientId, userCacheService, requestCache});
	}

	async reopenDmForBothUsers({
		userId,
		recipientId,
		existingChannel,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		existingChannel: Channel;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		await this.reopenExistingDMChannel({userId, existingChannel, userCacheService, requestCache});
		await this.reopenExistingDMChannel({
			userId: recipientId,
			existingChannel,
			userCacheService,
			requestCache,
		});
	}

	async createNewDmForBothUsers({
		userId,
		recipientId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const newChannel = await this.createNewDMChannel({
			userId,
			recipientId,
			userCacheService,
			requestCache,
		});
		await this.userRepository.openPrivateChannelForUser(recipientId, newChannel);
		await this.dispatchChannelCreate({userId: recipientId, channel: newChannel, userCacheService, requestCache});
		return newChannel;
	}

	private async reopenExistingDMChannel({
		userId,
		existingChannel,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		existingChannel: Channel;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		await this.userRepository.openPrivateChannelForUser(userId, existingChannel);
		await this.dispatchChannelCreate({userId, channel: existingChannel, userCacheService, requestCache});
		return existingChannel;
	}

	private async createNewDMChannel({
		userId,
		recipientId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const channelId = createChannelID(await this.snowflakeService.generate());
		const newChannel = await this.userRepository.createDmChannelAndState(userId, recipientId, channelId);
		await this.userRepository.openPrivateChannelForUser(userId, newChannel);
		await this.dispatchChannelCreate({userId, channel: newChannel, userCacheService, requestCache});
		return newChannel;
	}

	private async createOrOpenLocalOnlyDMChannel({
		userId,
		recipientId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipientId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const privateChannels = await this.userRepository.listPrivateChannels(userId);
		const existingChannel = privateChannels.find(
			(channel) =>
				channel.type === ChannelTypes.DM && channel.ownerId === userId && channel.recipientIds.has(recipientId),
		);
		if (existingChannel) {
			return await this.reopenExistingDMChannel({userId, existingChannel, userCacheService, requestCache});
		}
		const channelId = createChannelID(await this.snowflakeService.generate());
		const newChannel = await this.userRepository.createLocalOnlyDmChannel(userId, recipientId, channelId);
		await this.userRepository.openPrivateChannelForUser(userId, newChannel);
		await this.dispatchChannelCreate({userId, channel: newChannel, userCacheService, requestCache});
		return newChannel;
	}

	private async createGroupDMChannel({
		userId,
		recipients,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		recipients: Array<bigint>;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		const fallbackRecipientLimit = MAX_GROUP_DM_RECIPIENTS;
		const recipientLimit = this.resolveLimitForUser(
			await this.userRepository.findUnique(userId),
			'max_group_dm_recipients',
			fallbackRecipientLimit,
		);
		if (recipients.length + 1 > recipientLimit) {
			throw new MaxGroupDmRecipientsError(recipientLimit);
		}
		const recipientIds = recipients.map(createUserID);
		const uniqueRecipientIds = new Set(recipientIds);
		if (uniqueRecipientIds.size !== recipientIds.length) {
			throw InputValidationError.fromCode('recipients', ValidationErrorCodes.DUPLICATE_RECIPIENTS_NOT_ALLOWED);
		}
		if (uniqueRecipientIds.has(userId)) {
			throw InputValidationError.fromCode('recipients', ValidationErrorCodes.CANNOT_ADD_YOURSELF_TO_GROUP_DM);
		}
		const usersToCheck = new Set<UserID>([userId, ...recipientIds]);
		await this.ensureUsersWithinGroupDmLimit(usersToCheck);
		const senderUser = await this.userRepository.findUnique(userId);
		const senderIsBot = senderUser?.isBot ?? false;
		const unaddableRecipients: Array<GroupDmUnaddableRecipient> = [];
		const addableRecipients: Array<UserID> = [];
		for (const recipientId of recipientIds) {
			const targetUser = await this.userRepository.findUnique(recipientId);
			if (!targetUser) {
				unaddableRecipients.push({user_id: recipientId.toString(), reason: 'unknown_user'});
				continue;
			}
			if (senderIsBot) {
				const hasMutualGuilds = await this.userPermissionUtils.checkMutualGuildsAsync({
					userId,
					targetId: recipientId,
				});
				if (!hasMutualGuilds) {
					unaddableRecipients.push({user_id: recipientId.toString(), reason: 'blocked'});
					continue;
				}
			} else {
				const friendship = await this.userRepository.getRelationship(userId, recipientId, RelationshipTypes.FRIEND);
				if (!friendship) {
					unaddableRecipients.push({user_id: recipientId.toString(), reason: 'not_friends'});
					continue;
				}
			}
			const allowed = await this.userPermissionUtils.checkGroupDmAddPermission({userId, targetId: recipientId});
			if (!allowed) {
				unaddableRecipients.push({user_id: recipientId.toString(), reason: 'group_dm_add_disabled'});
				continue;
			}
			addableRecipients.push(recipientId);
		}
		if (unaddableRecipients.length > 0) {
			throw new GroupDmRecipientsNotAddableError({
				unaddableRecipients,
				addableRecipients: addableRecipients.map((id) => id.toString()),
			});
		}
		const channelId = createChannelID(await this.snowflakeService.generate());
		const allRecipients = new Set([userId, ...recipientIds]);
		const channelData = {
			channel_id: channelId,
			guild_id: null,
			type: ChannelTypes.GROUP_DM,
			name: null,
			topic: null,
			icon_hash: null,
			url: null,
			parent_id: null,
			position: 0,
			owner_id: userId,
			recipient_ids: allRecipients,
			nsfw: false,
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
		};
		const newChannel = await this.channelRepository.upsert(channelData);
		for (const recipientId of allRecipients) {
			await this.userRepository.openPrivateChannelForUser(recipientId, newChannel);
		}
		const systemMessages: Array<Message> = [];
		for (const recipientId of recipientIds) {
			const messageId = createMessageID(await this.snowflakeService.generateForChannel(channelId));
			const message = await this.channelRepository.upsertMessage({
				channel_id: channelId,
				bucket: BucketUtils.makeBucket(messageId),
				message_id: messageId,
				author_id: userId,
				type: MessageTypes.RECIPIENT_ADD,
				webhook_id: null,
				webhook_name: null,
				webhook_avatar_hash: null,
				content: null,
				edited_timestamp: null,
				pinned_timestamp: null,
				flags: 0,
				mention_everyone: false,
				mention_users: new Set([recipientId]),
				mention_roles: null,
				mention_channels: null,
				attachments: null,
				embeds: null,
				sticker_items: null,
				message_reference: null,
				message_snapshots: null,
				call: null,
				nsfw_emojis: null,
				has_reaction: false,
				version: 1,
			});
			systemMessages.push(message);
		}
		for (const recipientId of allRecipients) {
			await this.dispatchChannelCreate({userId: recipientId, channel: newChannel, userCacheService, requestCache});
		}
		for (const message of systemMessages) {
			await this.dispatchSystemMessage({
				channel: newChannel,
				message,
			});
		}
		return newChannel;
	}

	private async dispatchSystemMessage({channel, message}: {channel: Channel; message: Message}): Promise<void> {
		await dispatchMessageCreateBroadcast({
			channel,
			message,
			gatewayService: this.gatewayService,
		});
	}

	private async dispatchChannelCreate({
		userId,
		channel,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		channel: Channel;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		const channelResponse = await mapChannelToResponse({
			channel,
			currentUserId: userId,
			userCacheService,
			requestCache,
		});
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'CHANNEL_CREATE',
			data: channelResponse,
		});
	}

	private async ensureUsersWithinGroupDmLimit(userIds: Iterable<UserID>): Promise<void> {
		for (const userId of userIds) {
			await this.ensureUserWithinGroupDmLimit(userId);
		}
	}

	private async ensureUserWithinGroupDmLimit(userId: UserID): Promise<void> {
		const summaries = await this.userRepository.listPrivateChannelSummaries(userId);
		const openGroupDms = summaries.filter((summary) => summary.open && summary.isGroupDm).length;
		const user = await this.userRepository.findUnique(userId);
		const fallbackLimit = MAX_GROUP_DMS_PER_USER;
		const limit = this.resolveLimitForUser(user ?? null, 'max_group_dms_per_user', fallbackLimit);
		if (openGroupDms >= limit) {
			throw new MaxGroupDmsError(limit);
		}
	}

	private resolveLimitForUser(user: User | null, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({user});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}
}
