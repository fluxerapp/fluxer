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

import {type ChannelID, createGuildID, type MessageID, type UserID} from '~/BrandedTypes';
import {ChannelTypes} from '~/Constants';
import {mapChannelToResponse} from '~/channel/ChannelModel';
import type {AllowedMentionsRequest, MessageRequest, MessageUpdateRequest} from '~/channel/MessageTypes';
import {CannotEditOtherUserMessageError} from '~/Errors';
import type {GuildResponse} from '~/guild/GuildModel';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {Channel, Message, User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {ReadStateService} from '~/read_state/ReadStateService';
import type {IUserRepository} from '~/user/IUserRepository';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import {isPersonalNotesChannel} from './MessageHelpers';
import type {MessageMentionService} from './MessageMentionService';
import type {MessagePersistenceService} from './MessagePersistenceService';
import {incrementDmMentionCounts} from './ReadStateHelpers';

interface RecipientOpenState {
	recipientId: UserID;
	isOpen: boolean;
}

export class MessageProcessingService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userRepository: IUserRepository,
		private userCacheService: UserCacheService,
		private gatewayService: IGatewayService,
		private readStateService: ReadStateService,
		private mentionService: MessageMentionService,
	) {}

	async processMessageAfterCreation(params: {
		message: Message;
		channel: Channel;
		guild: GuildResponse | null;
		user: User;
		data: MessageRequest;
		referencedMessage: Message | null;
		mentionHere?: boolean;
	}): Promise<void> {
		const {message, guild, user, mentionHere = false} = params;

		await this.mentionService.handleMentionTasks({
			guildId: guild ? createGuildID(BigInt(guild.id)) : null,
			message,
			authorId: user.id,
			mentionHere,
		});
	}

	async updateDMRecipients({
		channel,
		channelId,
		requestCache,
	}: {
		channel: Channel;
		channelId: ChannelID;
		requestCache: RequestCache;
	}): Promise<void> {
		if (channel.guildId || channel.type !== ChannelTypes.DM) return;
		if (!channel.recipientIds || channel.recipientIds.size !== 2) return;

		const recipientIds = Array.from(channel.recipientIds);
		const isGroupDm = false;

		const openStates = await this.batchCheckDmChannelOpen(recipientIds, channelId);

		const closedRecipients = openStates.filter((state) => !state.isOpen);
		if (closedRecipients.length === 0) return;

		await Promise.all(
			closedRecipients.map((state) =>
				this.openDmAndDispatch({
					recipientId: state.recipientId,
					channelId,
					channel,
					isGroupDm,
					requestCache,
				}),
			),
		);
	}

	private async batchCheckDmChannelOpen(
		recipientIds: Array<UserID>,
		channelId: ChannelID,
	): Promise<Array<RecipientOpenState>> {
		return Promise.all(
			recipientIds.map(async (recipientId) => ({
				recipientId,
				isOpen: await this.userRepository.isDmChannelOpen(recipientId, channelId),
			})),
		);
	}

	private async openDmAndDispatch(params: {
		recipientId: UserID;
		channelId: ChannelID;
		channel: Channel;
		isGroupDm: boolean;
		requestCache: RequestCache;
	}): Promise<void> {
		const {recipientId, channelId, channel, isGroupDm, requestCache} = params;

		await this.userRepository.openDmForUser(recipientId, channelId, isGroupDm);

		const channelResponse = await mapChannelToResponse({
			channel,
			currentUserId: recipientId,
			userCacheService: this.userCacheService,
			requestCache,
		});

		await this.gatewayService.dispatchPresence({
			userId: recipientId,
			event: 'CHANNEL_CREATE',
			data: channelResponse,
		});
	}

	async updateReadStates({
		user,
		guild,
		channel,
		channelId,
	}: {
		user: User;
		guild: GuildResponse | null;
		channel: Channel;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<void> {
		if (user.isBot) return;

		if (!guild) {
			const recipients = await this.userRepository.listUsers(Array.from(channel.recipientIds));
			await incrementDmMentionCounts({
				readStateService: this.readStateService,
				user,
				recipients,
				channelId,
			});
		}
	}

	async fetchMessagesAround({
		channelId,
		limit,
		around,
	}: {
		channelId: ChannelID;
		limit: number;
		around: MessageID;
	}): Promise<Array<Message>> {
		const initialOlder = Math.floor(limit / 2);
		const initialNewer = Math.max(0, limit - 1 - initialOlder);

		const targetPromise = this.channelRepository.messages.getMessage(channelId, around);

		let newerMessages = await this.channelRepository.messages.listMessages(channelId, undefined, initialNewer, around, {
			immediateAfter: true,
		});

		let olderMessages = await this.channelRepository.messages.listMessages(channelId, around, initialOlder, undefined, {
			restrictToBeforeBucket: false,
		});

		const targetMessage = await targetPromise;

		const haveCenter = targetMessage ? 1 : 0;
		const total = newerMessages.length + haveCenter + olderMessages.length;

		if (total < limit) {
			const missing = limit - total;

			if (newerMessages.length < initialNewer) {
				const need = missing;
				const cursor = olderMessages.length > 0 ? olderMessages[olderMessages.length - 1].id : around;
				const more = await this.channelRepository.messages.listMessages(channelId, cursor, need, undefined, {
					restrictToBeforeBucket: false,
				});
				olderMessages = [...olderMessages, ...more];
			} else {
				const need = missing;
				const cursor = newerMessages.length > 0 ? newerMessages[0].id : around;
				const more = await this.channelRepository.messages.listMessages(channelId, undefined, need, cursor, {
					immediateAfter: true,
				});
				newerMessages = [...more, ...newerMessages];
			}
		}

		const out: Array<Message> = [];
		const seen = new Set<string>();
		const push = (m: Message) => {
			const id = m.id.toString();
			if (seen.has(id)) return;
			seen.add(id);
			out.push(m);
		};

		for (const m of newerMessages) push(m);
		if (targetMessage) push(targetMessage);
		for (const m of olderMessages) push(m);

		return out.slice(0, limit);
	}

	async handleMentions(params: {
		channel: Channel;
		message: Message;
		referencedMessageOnSend: Message | null;
		allowedMentions: AllowedMentionsRequest | null;
		guild?: GuildResponse | null;
		canMentionEveryone: boolean;
		canMentionRoles: boolean;
	}): Promise<void> {
		const {channel, message, referencedMessageOnSend, allowedMentions, guild, canMentionEveryone, canMentionRoles} =
			params;

		if (message.authorId && isPersonalNotesChannel({userId: message.authorId, channelId: channel.id})) {
			return;
		}

		const content = message.content;
		if (!content) return;

		const mentions = this.mentionService.extractMentions({
			content,
			referencedMessage: referencedMessageOnSend,
			message,
			channelType: channel.type,
			allowedMentions,
			guild,
			canMentionEveryone,
		});

		const {validUserIds, validRoleIds} = await this.mentionService.validateMentions({
			userMentions: mentions.userMentions,
			roleMentions: mentions.roleMentions,
			channel,
			canMentionRoles,
		});

		const updatedMessageData = {
			...message.toRow(),
			flags: mentions.flags,
			mention_users: validUserIds.length > 0 ? new Set(validUserIds) : null,
			mention_roles: validRoleIds.length > 0 ? new Set(validRoleIds) : null,
			mention_everyone: mentions.mentionsEveryone,
		};

		await this.channelRepository.messages.upsertMessage(updatedMessageData, message.toRow());
	}

	async handleNonAuthorEdit(params: {
		message: Message;
		messageId: MessageID;
		data: MessageUpdateRequest;
		guild: GuildResponse | null;
		hasPermission: (permission: bigint) => Promise<boolean>;
		channel: Channel;
		requestCache: RequestCache;
		persistenceService: MessagePersistenceService;
		dispatchMessageUpdate: (params: {channel: Channel; message: Message; requestCache: RequestCache}) => Promise<void>;
	}): Promise<Message> {
		const {message, data, guild, hasPermission, channel, requestCache, persistenceService, dispatchMessageUpdate} =
			params;

		const editResult = await persistenceService.handleNonAuthorEdit({
			message,
			data,
			guild,
			hasPermission,
		});

		if (editResult.canEdit && editResult.updatedFlags !== undefined) {
			const updatedRowData = {
				...message.toRow(),
				flags: editResult.updatedFlags,
			};

			const updatedMessage = await this.channelRepository.messages.upsertMessage(updatedRowData, message.toRow());
			await dispatchMessageUpdate({channel, message: updatedMessage, requestCache});
			return updatedMessage;
		}

		throw new CannotEditOtherUserMessageError();
	}
}
