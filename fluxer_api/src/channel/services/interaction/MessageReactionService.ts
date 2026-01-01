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

import emojiRegex from 'emoji-regex';
import {type ChannelID, createEmojiID, type MessageID, type UserID} from '~/BrandedTypes';
import {GuildOperations, MAX_REACTIONS_PER_MESSAGE, MAX_USERS_PER_MESSAGE_REACTION, Permissions} from '~/Constants';
import {
	CommunicationDisabledError,
	FeatureTemporarilyDisabledError,
	InputValidationError,
	MaxReactionsPerMessageError,
	MaxUsersPerMessageReactionError,
	MissingPermissionsError,
	UnknownMessageError,
} from '~/Errors';
import {isGuildMemberTimedOut} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import {getMetricsService} from '~/infrastructure/MetricsService';
import type {Channel, Message, MessageReaction} from '~/Models';
import type {IUserRepository} from '~/user/IUserRepository';
import {mapUserToPartialResponse, type UserPartialResponse} from '~/user/UserModel';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import {MessageInteractionBase, type ParsedEmoji} from './MessageInteractionBase';

const REACTION_CUSTOM_EMOJI_REGEX = /^(.+):(\d+)$/;

export class MessageReactionService extends MessageInteractionBase {
	constructor(
		gatewayService: IGatewayService,
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepository,
	) {
		super(gatewayService);
	}

	async getUsersForReaction({
		authChannel,
		messageId,
		emoji,
		limit,
		after,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		emoji: string;
		limit?: number;
		after?: UserID;
	}): Promise<Array<UserPartialResponse>> {
		const {channel} = authChannel;
		this.ensureTextChannel(channel);

		const validatedLimit = limit ? Math.min(Math.max(limit, 1), MAX_USERS_PER_MESSAGE_REACTION) : 25;

		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) throw new UnknownMessageError();

		const parsedEmoji = this.parseEmojiWithoutValidation(emoji);
		const afterUserId = after;

		const reactions = await this.channelRepository.messageInteractions.listReactionUsers(
			channel.id,
			messageId,
			parsedEmoji.name,
			validatedLimit,
			afterUserId,
			parsedEmoji.id ? createEmojiID(BigInt(parsedEmoji.id)) : undefined,
		);

		if (!reactions.length) return [];

		const userIds = reactions.map((reaction: MessageReaction) => reaction.userId);
		const users = await this.userRepository.listUsers(userIds);

		return users.map(mapUserToPartialResponse);
	}

	async addReaction({
		authChannel,
		messageId,
		emoji,
		userId,
		sessionId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		emoji: string;
		userId: UserID;
		sessionId?: string;
	}): Promise<void> {
		try {
			const {channel, guild, hasPermission, checkPermission} = authChannel;
			this.ensureTextChannel(channel);
			const isMemberTimedOut = isGuildMemberTimedOut(authChannel.member);

			if (guild && !(await hasPermission(Permissions.READ_MESSAGE_HISTORY))) {
				throw new MissingPermissionsError();
			}

			if (this.isOperationDisabled(guild, GuildOperations.REACTIONS)) {
				throw new FeatureTemporarilyDisabledError();
			}

			const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
			if (!message) throw new UnknownMessageError();

			const parsedEmojiBasic = this.parseEmojiWithoutValidation(emoji);
			const emojiId = parsedEmojiBasic.id ? createEmojiID(BigInt(parsedEmojiBasic.id)) : undefined;

			const userReactionExists = await this.channelRepository.messageInteractions.checkUserReactionExists(
				channel.id,
				messageId,
				userId,
				parsedEmojiBasic.name,
				emojiId,
			);

			if (userReactionExists) return;

			const reactionCount = await this.channelRepository.messageInteractions.countReactionUsers(
				channel.id,
				messageId,
				parsedEmojiBasic.name,
				emojiId,
			);

			if (reactionCount === 0 && guild) {
				await checkPermission(Permissions.ADD_REACTIONS);
			}

			let parsedEmoji: ParsedEmoji;

			if (reactionCount > 0) {
				parsedEmoji = parsedEmojiBasic;
			} else {
				parsedEmoji = await this.parseAndValidateEmoji({
					emoji,
					guildId: channel.guildId?.toString() || undefined,
					userId,
					hasPermission: channel.guildId ? hasPermission : undefined,
				});
			}

			if (reactionCount >= MAX_USERS_PER_MESSAGE_REACTION) {
				throw new MaxUsersPerMessageReactionError();
			}

			const uniqueReactionCount = await this.channelRepository.messageInteractions.countUniqueReactions(
				channel.id,
				messageId,
			);

			if (isMemberTimedOut && reactionCount === 0) {
				throw new CommunicationDisabledError();
			}

			if (uniqueReactionCount >= MAX_REACTIONS_PER_MESSAGE) {
				throw new MaxReactionsPerMessageError();
			}

			await this.channelRepository.messageInteractions.addReaction(
				channel.id,
				messageId,
				userId,
				parsedEmoji.name,
				emojiId,
				parsedEmoji.animated ?? false,
			);

			await this.dispatchMessageReactionAdd({
				channel,
				messageId,
				emoji: parsedEmoji,
				userId,
				sessionId,
			});

			getMetricsService().counter({name: 'reaction.add'});
		} catch (error) {
			getMetricsService().counter({name: 'reaction.add.error'});
			throw error;
		}
	}

	async removeReaction({
		authChannel,
		messageId,
		emoji,
		targetId,
		sessionId,
		actorId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		emoji: string;
		targetId: UserID;
		sessionId?: string;
		actorId: UserID;
	}): Promise<void> {
		try {
			const {channel, guild, hasPermission} = authChannel;
			this.ensureTextChannel(channel);

			if (this.isOperationDisabled(guild, GuildOperations.REACTIONS)) {
				throw new FeatureTemporarilyDisabledError();
			}

			const parsedEmoji = this.parseEmojiWithoutValidation(emoji);

			const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
			if (!message) return;

			const isRemovingOwnReaction = targetId === actorId;
			if (!isRemovingOwnReaction) {
				await this.assertCanModerateMessageReactions({channel, message, actorId, hasPermission});
			}

			const emojiId = parsedEmoji.id ? createEmojiID(BigInt(parsedEmoji.id)) : undefined;
			await this.channelRepository.messageInteractions.removeReaction(
				channel.id,
				messageId,
				targetId,
				parsedEmoji.name,
				emojiId,
			);

			await this.dispatchMessageReactionRemove({
				channel,
				messageId,
				emoji: parsedEmoji,
				userId: targetId,
				sessionId,
			});

			getMetricsService().counter({name: 'reaction.remove'});
		} catch (error) {
			getMetricsService().counter({name: 'reaction.remove.error'});
			throw error;
		}
	}

	async removeAllReactionsForEmoji({
		authChannel,
		messageId,
		emoji,
		actorId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		emoji: string;
		actorId: UserID;
	}): Promise<void> {
		const {channel, guild, hasPermission} = authChannel;
		this.ensureTextChannel(channel);

		if (this.isOperationDisabled(guild, GuildOperations.REACTIONS)) {
			throw new FeatureTemporarilyDisabledError();
		}

		const parsedEmoji = this.parseEmojiWithoutValidation(emoji);

		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) return;

		await this.assertCanModerateMessageReactions({channel, message, actorId, hasPermission});

		const emojiId = parsedEmoji.id ? createEmojiID(BigInt(parsedEmoji.id)) : undefined;
		await this.channelRepository.messageInteractions.removeAllReactionsForEmoji(
			channel.id,
			messageId,
			parsedEmoji.name,
			emojiId,
		);

		await this.dispatchMessageReactionRemoveAllForEmoji({channel, messageId, emoji: parsedEmoji});
	}

	async removeAllReactions({
		authChannel,
		messageId,
		actorId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		actorId: UserID;
	}): Promise<void> {
		const {channel, guild, hasPermission} = authChannel;
		this.ensureTextChannel(channel);

		if (this.isOperationDisabled(guild, GuildOperations.REACTIONS)) {
			throw new FeatureTemporarilyDisabledError();
		}

		const message = await this.channelRepository.messages.getMessage(channel.id, messageId);
		if (!message) return;

		await this.assertCanModerateMessageReactions({channel, message, actorId, hasPermission});

		await this.channelRepository.messageInteractions.removeAllReactions(channel.id, messageId);

		await this.dispatchMessageReactionRemoveAll({channel, messageId});
	}

	async getMessageReactions({
		authChannel,
		messageId,
	}: {
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
	}): Promise<Array<MessageReaction>> {
		return this.channelRepository.messageInteractions.listMessageReactions(authChannel.channel.id, messageId);
	}

	async setHasReaction(channelId: ChannelID, messageId: MessageID, hasReaction: boolean): Promise<void> {
		return this.channelRepository.messageInteractions.setHasReaction(channelId, messageId, hasReaction);
	}

	private async assertCanModerateMessageReactions({
		channel,
		message,
		actorId,
		hasPermission,
	}: {
		channel: Channel;
		message: Message;
		actorId: UserID;
		hasPermission: (permission: bigint) => Promise<boolean>;
	}): Promise<void> {
		if (message.authorId === actorId) {
			return;
		}

		if (!channel.guildId) {
			throw new MissingPermissionsError();
		}

		const canManageMessages = await hasPermission(Permissions.MANAGE_MESSAGES);
		if (!canManageMessages) {
			throw new MissingPermissionsError();
		}
	}

	private parseEmojiWithoutValidation(emoji: string): {name: string; id?: string; animated?: boolean} {
		const decodedEmoji = decodeURIComponent(emoji);
		const customEmojiMatch = decodedEmoji.match(REACTION_CUSTOM_EMOJI_REGEX);

		if (customEmojiMatch) {
			const [, name, id] = customEmojiMatch;
			return {
				id,
				name: name || 'unknown',
			};
		}

		return {name: decodedEmoji};
	}

	private async parseAndValidateEmoji({
		emoji,
		guildId,
		userId,
		hasPermission,
	}: {
		emoji: string;
		guildId?: string | undefined;
		userId?: UserID;
		hasPermission?: (permission: bigint) => Promise<boolean>;
	}): Promise<ParsedEmoji> {
		const decodedEmoji = decodeURIComponent(emoji);
		const customEmojiMatch = decodedEmoji.match(REACTION_CUSTOM_EMOJI_REGEX);

		if (customEmojiMatch) {
			const [, , id] = customEmojiMatch;
			const emojiIdBigInt = createEmojiID(BigInt(id));

			let isPremium = false;
			if (userId) {
				const user = await this.userRepository.findUnique(userId);
				isPremium = user?.canUseGlobalExpressions() ?? false;
			}

			const emoji = await this.guildRepository.getEmojiById(emojiIdBigInt);
			if (!emoji) {
				throw InputValidationError.create('emoji', 'Custom emoji not found');
			}

			if (!isPremium && emoji.guildId.toString() !== guildId) {
				throw InputValidationError.create('emoji', 'Cannot use custom emojis outside of source guilds without premium');
			}

			if (hasPermission) {
				const canUseExternalEmojis = await hasPermission(Permissions.USE_EXTERNAL_EMOJIS);
				if (!canUseExternalEmojis) {
					throw new MissingPermissionsError();
				}
			}

			return {
				id,
				name: emoji.name,
				animated: emoji.isAnimated,
			};
		}

		const isValidUnicodeEmoji = emojiRegex().test(decodedEmoji);
		if (!isValidUnicodeEmoji) {
			throw InputValidationError.create('emoji', 'Not a valid Unicode emoji');
		}

		return {name: decodedEmoji};
	}

	private async dispatchMessageReactionAdd(params: {
		channel: Channel;
		messageId: MessageID;
		emoji: ParsedEmoji;
		userId: UserID;
		sessionId?: string;
	}): Promise<void> {
		await this.dispatchEvent({
			channel: params.channel,
			event: 'MESSAGE_REACTION_ADD',
			data: {
				channel_id: params.channel.id.toString(),
				message_id: params.messageId.toString(),
				emoji: params.emoji,
				user_id: params.userId.toString(),
				session_id: params.sessionId,
			},
		});
	}

	private async dispatchMessageReactionRemove(params: {
		channel: Channel;
		messageId: MessageID;
		emoji: ParsedEmoji;
		userId: UserID;
		sessionId?: string;
	}): Promise<void> {
		await this.dispatchEvent({
			channel: params.channel,
			event: 'MESSAGE_REACTION_REMOVE',
			data: {
				channel_id: params.channel.id.toString(),
				message_id: params.messageId.toString(),
				emoji: params.emoji,
				user_id: params.userId.toString(),
				session_id: params.sessionId,
			},
		});
	}

	private async dispatchMessageReactionRemoveAllForEmoji(params: {
		channel: Channel;
		messageId: MessageID;
		emoji: ParsedEmoji;
	}): Promise<void> {
		const event = params.channel.guildId ? 'MESSAGE_REACTION_REMOVE_EMOJI' : 'MESSAGE_REACTION_REMOVE_ALL';
		await this.dispatchEvent({
			channel: params.channel,
			event,
			data: {
				channel_id: params.channel.id.toString(),
				message_id: params.messageId.toString(),
				emoji: params.emoji,
			},
		});
	}

	private async dispatchMessageReactionRemoveAll(params: {channel: Channel; messageId: MessageID}): Promise<void> {
		await this.dispatchEvent({
			channel: params.channel,
			event: 'MESSAGE_REACTION_REMOVE_ALL',
			data: {
				channel_id: params.channel.id.toString(),
				message_id: params.messageId.toString(),
			},
		});
	}
}
