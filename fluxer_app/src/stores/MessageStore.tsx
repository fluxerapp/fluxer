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

import {action, makeAutoObservable, reaction} from 'mobx';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import {FAVORITES_GUILD_ID, MAX_MESSAGES_PER_CHANNEL, ME, MessageStates} from '~/Constants';
import type {JumpOptions} from '~/lib/ChannelMessages';
import {ChannelMessages} from '~/lib/ChannelMessages';
import type {GuildMember} from '~/records/GuildMemberRecord';
import type {Message, MessageRecord} from '~/records/MessageRecord';
import type {Presence} from '~/stores/PresenceStore';
import type {ReactionEmoji} from '~/utils/ReactionUtils';
import ChannelStore from './ChannelStore';
import ConnectionStore from './ConnectionStore';
import DimensionStore from './DimensionStore';
import GuildStore from './GuildStore';
import RelationshipStore from './RelationshipStore';
import SelectedChannelStore from './SelectedChannelStore';
import SelectedGuildStore from './SelectedGuildStore';
import UserStore from './UserStore';

type ChannelId = string;
type MessageId = string;
type GuildId = string;

interface GuildMemberUpdateAction {
	type: 'GUILD_MEMBER_UPDATE';
	guildId: string;
	member: GuildMember;
}

interface PresenceUpdateAction {
	type: 'PRESENCE_UPDATE';
	presence: Presence;
}

interface PendingMessageJump {
	channelId: ChannelId;
	messageId: MessageId;
}

class MessageStore {
	pendingMessageJump: PendingMessageJump | null = null;
	updateCounter = 0;
	private pendingFullHydration = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get version(): number {
		return this.updateCounter;
	}

	@action
	private notifyChange(): void {
		this.updateCounter += 1;
	}

	getMessages(channelId: ChannelId): ChannelMessages {
		return ChannelMessages.getOrCreate(channelId);
	}

	getMessage(channelId: ChannelId, messageId: MessageId): MessageRecord | undefined {
		return ChannelMessages.getOrCreate(channelId).get(messageId);
	}

	getLastEditableMessage(channelId: ChannelId): MessageRecord | undefined {
		const messages = this.getMessages(channelId).toArray();
		for (let i = messages.length - 1; i >= 0; i--) {
			const message = messages[i];
			if (message.isCurrentUserAuthor() && message.state === MessageStates.SENT && message.isUserMessage()) {
				return message;
			}
		}
		return;
	}

	jumpedMessageId(channelId: ChannelId): MessageId | null | undefined {
		const channel = ChannelMessages.get(channelId);
		return channel?.jumpTargetId;
	}

	hasPresent(channelId: ChannelId): boolean {
		const channel = ChannelMessages.get(channelId);
		return channel?.hasPresent() ?? false;
	}

	@action
	handleConnectionClosed(): boolean {
		let didUpdate = false;
		ChannelMessages.forEach((messages) => {
			if (messages.loadingMore) {
				ChannelMessages.commit(messages.mutate({loadingMore: false}));
				didUpdate = true;
			}
		});
		if (didUpdate) {
			this.notifyChange();
		}
		return false;
	}

	@action
	handleSessionInvalidated(): boolean {
		const channelIds: Array<string> = [];
		ChannelMessages.forEach((messages) => channelIds.push(messages.channelId));
		for (const channelId of channelIds) {
			ChannelMessages.clear(channelId);
			DimensionStore.clearChannelDimensions(channelId);
		}
		this.pendingMessageJump = null;
		this.pendingFullHydration = true;
		this.notifyChange();
		return true;
	}

	@action
	handleResumed(): boolean {
		ChannelMessages.forEach((messages) => {
			ChannelMessages.commit(messages.mutate({ready: true}));
		});
		this.notifyChange();
		return true;
	}

	@action
	handleConnectionOpen(): boolean {
		const selectedChannelId = SelectedChannelStore.currentChannelId;
		let didHydrateSelectedChannel = false;

		if (this.pendingMessageJump && this.pendingMessageJump.channelId === selectedChannelId) {
			MessageActionCreators.jumpToMessage(this.pendingMessageJump.channelId, this.pendingMessageJump.messageId, true);
			this.pendingMessageJump = null;
			this.pendingFullHydration = false;
			return true;
		}

		ChannelMessages.forEach((messages) => {
			if (messages.channelId === selectedChannelId && ChannelStore.getChannel(messages.channelId) != null) {
				this.startChannelHydration(messages.channelId, {forceScrollToBottom: this.pendingFullHydration});
				didHydrateSelectedChannel = true;
			} else {
				ChannelMessages.clear(messages.channelId);
				DimensionStore.clearChannelDimensions(messages.channelId);
			}
		});

		if (this.pendingFullHydration && !didHydrateSelectedChannel && selectedChannelId) {
			this.startChannelHydration(selectedChannelId, {forceScrollToBottom: true});
			didHydrateSelectedChannel = true;
		}

		this.pendingFullHydration = false;

		if (!didHydrateSelectedChannel && selectedChannelId && ChannelStore.getChannel(selectedChannelId)) {
			const messages = ChannelMessages.getOrCreate(selectedChannelId);
			if (!messages.ready && !messages.loadingMore && messages.length === 0) {
				ChannelMessages.commit(messages.mutate({loadingMore: true}));
				MessageActionCreators.fetchMessages(selectedChannelId, null, null, MAX_MESSAGES_PER_CHANNEL);
				didHydrateSelectedChannel = true;
			}
		}

		this.notifyChange();
		return didHydrateSelectedChannel;
	}

	private startChannelHydration(channelId: ChannelId, options: {forceScrollToBottom?: boolean} = {}): void {
		if (!ChannelStore.getChannel(channelId)) return;

		const {forceScrollToBottom = false} = options;
		const messages = ChannelMessages.getOrCreate(channelId);

		ChannelMessages.commit(messages.mutate({loadingMore: true, ready: false, error: false}));

		if (forceScrollToBottom) {
			DimensionStore.updateChannelDimensions(channelId, 1, 1, 0);
		}

		MessageActionCreators.fetchMessages(channelId, null, null, MAX_MESSAGES_PER_CHANNEL);
	}

	@action
	handleChannelSelect(action: {guildId?: GuildId; channelId?: ChannelId | null; messageId?: MessageId}): boolean {
		const channelId = action.channelId ?? action.guildId;
		if (channelId == null || channelId === ME) {
			return false;
		}

		const currentChannel = ChannelStore.getChannel(channelId);

		let messages = ChannelMessages.getOrCreate(channelId);
		if (messages.jumpTargetId) {
			messages = messages.mutate({jumpTargetId: null, jumped: false});
			ChannelMessages.commit(messages);
			this.notifyChange();
		}

		if (action.messageId && !ConnectionStore.isConnected) {
			this.pendingMessageJump = {channelId, messageId: action.messageId};
			return true;
		}

		if (ConnectionStore.isConnected && action.messageId) {
			MessageActionCreators.jumpToMessage(channelId, action.messageId, true);
			return false;
		}

		if (!ConnectionStore.isConnected || messages.loadingMore || messages.ready) {
			if (messages.ready && DimensionStore.isAtBottom(channelId)) {
				ChannelMessages.commit(messages.truncateTop(MAX_MESSAGES_PER_CHANNEL));
				this.notifyChange();
			}
			return false;
		}

		const isPrivateChannel = currentChannel?.isPrivate() ?? false;
		const isNonGuildChannel = action.guildId == null || action.guildId === ME || isPrivateChannel;
		const isFavoritesGuild = action.guildId === FAVORITES_GUILD_ID;

		let guildExists = false;
		if (isFavoritesGuild && !isPrivateChannel) {
			const channelGuildId = currentChannel?.guildId;
			guildExists = channelGuildId ? !!GuildStore.getGuild(channelGuildId) : false;
		} else if (action.guildId && !isPrivateChannel) {
			guildExists = !!GuildStore.getGuild(action.guildId);
		}

		if (!isNonGuildChannel && !guildExists) {
			return false;
		}

		ChannelMessages.commit(messages.mutate({loadingMore: true}));
		this.notifyChange();

		MessageActionCreators.fetchMessages(channelId, null, null, MAX_MESSAGES_PER_CHANNEL);
		return false;
	}

	@action
	handleGuildUnavailable(guildId: GuildId, unavailable: boolean): boolean {
		if (!unavailable) {
			return false;
		}

		let didUpdate = false;
		const selectedChannelId = SelectedChannelStore.currentChannelId;
		let selectedChannelAffected = false;

		ChannelMessages.forEach(({channelId}) => {
			const channel = ChannelStore.getChannel(channelId);
			if (channel && channel.guildId === guildId) {
				ChannelMessages.clear(channelId);
				DimensionStore.clearChannelDimensions(channelId);
				didUpdate = true;

				if (channelId === selectedChannelId) {
					selectedChannelAffected = true;
				}
			}
		});

		if (selectedChannelAffected) {
			this.pendingFullHydration = true;
		}

		if (didUpdate) {
			this.notifyChange();
		}

		return didUpdate;
	}

	@action
	handleGuildCreate(action: {guild: {id: GuildId}}): boolean {
		if (SelectedGuildStore.selectedGuildId !== action.guild.id) {
			return false;
		}

		const selectedChannelId = SelectedChannelStore.selectedChannelIds.get(action.guild.id);
		if (!selectedChannelId) {
			return false;
		}

		const currentMessages = ChannelMessages.get(selectedChannelId);

		const didChannelSelect = this.handleChannelSelect({
			guildId: action.guild.id,
			channelId: selectedChannelId,
			messageId: undefined,
		});

		if (!didChannelSelect && currentMessages && currentMessages.length === 0 && !currentMessages.ready) {
			ChannelMessages.commit(currentMessages.mutate({loadingMore: true}));
			MessageActionCreators.fetchMessages(selectedChannelId, null, null, MAX_MESSAGES_PER_CHANNEL);
			this.notifyChange();
			return true;
		}

		return didChannelSelect;
	}

	@action
	handleLoadMessages(action: {channelId: ChannelId; jump?: JumpOptions}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		ChannelMessages.commit(messages.loadStart(action.jump));
		this.notifyChange();
		return false;
	}

	@action
	handleTruncateMessages(action: {channelId: ChannelId; truncateBottom?: boolean; truncateTop?: boolean}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId).truncate(
			action.truncateBottom ?? false,
			action.truncateTop ?? false,
		);
		ChannelMessages.commit(messages);
		this.notifyChange();
		return false;
	}

	@action
	handleLoadMessagesSuccessCached(action: {
		channelId: ChannelId;
		jump?: JumpOptions;
		before?: string;
		after?: string;
		limit: number;
	}): boolean {
		let messages = ChannelMessages.getOrCreate(action.channelId);

		if (action.jump?.present) {
			messages = messages.jumpToPresent(action.limit);
		} else if (action.jump?.messageId) {
			messages = messages.jumpToMessage(
				action.jump.messageId,
				action.jump.flash,
				action.jump.offset,
				action.jump.returnMessageId,
				action.jump.jumpType,
			);
		} else if (action.before || action.after) {
			messages = messages.loadFromCache(action.before != null, action.limit);
		}

		ChannelMessages.commit(messages);
		this.notifyChange();
		return false;
	}

	@action
	handleLoadMessagesSuccess(action: {
		channelId: ChannelId;
		isBefore?: boolean;
		isAfter?: boolean;
		jump?: JumpOptions;
		hasMoreBefore?: boolean;
		hasMoreAfter?: boolean;
		cached?: boolean;
		messages: Array<Message>;
	}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId).loadComplete({
			newMessages: action.messages,
			isBefore: action.isBefore,
			isAfter: action.isAfter,
			jump: action.jump,
			hasMoreBefore: action.hasMoreBefore,
			hasMoreAfter: action.hasMoreAfter,
			cached: action.cached,
		});
		ChannelMessages.commit(messages);
		this.notifyChange();
		return false;
	}

	@action
	handleLoadMessagesFailure(action: {channelId: ChannelId}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		ChannelMessages.commit(messages.mutate({loadingMore: false, error: true}));
		this.notifyChange();
		return false;
	}

	@action
	handleIncomingMessage(action: {channelId: ChannelId; message: Message}): boolean {
		ChannelStore.handleMessageCreate({message: action.message});

		const existing = ChannelMessages.get(action.channelId);
		if (!existing?.ready) {
			return false;
		}

		const updated = existing.receiveMessage(action.message, DimensionStore.isAtBottom(action.channelId));
		ChannelMessages.commit(updated);
		this.notifyChange();
		return false;
	}

	@action
	handleSendFailed(action: {channelId: ChannelId; nonce: string}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing || !existing.has(action.nonce)) return false;

		const updated = existing.update(action.nonce, (message) => message.withUpdates({state: MessageStates.FAILED}));
		ChannelMessages.commit(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleMessageDelete(action: {id: MessageId; channelId: ChannelId}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing || !existing.has(action.id)) {
			return false;
		}

		let messages = existing;

		if (messages.revealedMessageId === action.id) {
			const messageAfter = messages.getAfter(action.id);
			messages = messages.mutate({revealedMessageId: messageAfter?.id ?? null});
		}

		messages = messages.remove(action.id);
		ChannelMessages.commit(messages);

		this.notifyChange();
		return true;
	}

	@action
	handleMessageDeleteBulk(action: {ids: Array<MessageId>; channelId: ChannelId}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;

		let messages = existing.removeMany(action.ids);
		if (messages === existing) return false;

		if (messages.revealedMessageId != null && action.ids.includes(messages.revealedMessageId)) {
			const after = messages.getAfter(messages.revealedMessageId);
			messages = messages.mutate({revealedMessageId: after?.id ?? null});
		}

		ChannelMessages.commit(messages);
		this.notifyChange();
		return true;
	}

	@action
	handleMessageUpdate(action: {message: Message}): boolean {
		const messageId = action.message.id;
		const channelId = action.message.channel_id;

		const existing = ChannelMessages.get(channelId);
		if (!existing || !existing.has(messageId)) return false;

		const updated = existing.update(messageId, (message) => {
			if (message.isEditing && action.message.state === undefined) {
				return message.withUpdates({...action.message, state: MessageStates.SENT});
			}
			return message.withUpdates(action.message);
		});

		ChannelMessages.commit(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleUserUpdate(action: {user: {id: string}}): boolean {
		let hasChanges = false;

		ChannelMessages.forEach((messages) => {
			let changedInChannel = false;

			const updatedMessages = messages.map((message) => {
				if (message.author.id !== action.user.id) return message;
				const updatedAuthor = UserStore.getUser(action.user.id);
				const updated = message.withUpdates({author: updatedAuthor?.toJSON()});
				if (updated !== message) {
					changedInChannel = true;
					hasChanges = true;
				}
				return updated;
			});

			if (changedInChannel) {
				ChannelMessages.commit(messages.reset(updatedMessages));
			}
		});

		if (hasChanges) {
			this.notifyChange();
		}
		return hasChanges;
	}

	@action
	handleGuildMemberUpdate(action: GuildMemberUpdateAction): boolean {
		let hasChanges = false;

		ChannelMessages.forEach((messages) => {
			const channel = ChannelStore.getChannel(messages.channelId);
			if (channel == null || channel.guildId !== action.guildId) return;

			let changedInChannel = false;

			const updatedMessages = messages.map((message) => {
				if (message.author.id !== action.member.user.id) return message;
				const updatedAuthor = UserStore.getUser(action.member.user.id);
				const updated = message.withUpdates({author: updatedAuthor?.toJSON()});
				if (updated !== message) {
					changedInChannel = true;
					hasChanges = true;
				}
				return updated;
			});

			if (changedInChannel) {
				ChannelMessages.commit(messages.reset(updatedMessages));
			}
		});

		if (hasChanges) {
			this.notifyChange();
		}
		return hasChanges;
	}

	@action
	handlePresenceUpdate(action: PresenceUpdateAction): boolean {
		if (!action.presence.user.username && !action.presence.user.avatar && !action.presence.user.discriminator) {
			return false;
		}

		let hasChanges = false;

		ChannelMessages.forEach((messages) => {
			const channel = ChannelStore.getChannel(messages.channelId);
			if (action.presence.guild_id && channel && channel.guildId !== action.presence.guild_id) return;

			let changedInChannel = false;

			const updatedMessages = messages.map((message) => {
				if (message.author.id !== action.presence.user.id) return message;
				const updatedAuthor = UserStore.getUser(action.presence.user.id);
				const updated = message.withUpdates({author: updatedAuthor?.toJSON()});
				if (updated !== message) {
					changedInChannel = true;
					hasChanges = true;
				}
				return updated;
			});

			if (changedInChannel) {
				ChannelMessages.commit(messages.reset(updatedMessages));
			}
		});

		if (hasChanges) {
			this.notifyChange();
		}
		return hasChanges;
	}

	@action
	handleCleanup(): boolean {
		ChannelMessages.forEach(({channelId}) => {
			if (ChannelStore.getChannel(channelId) == null) {
				ChannelMessages.clear(channelId);
				DimensionStore.clearChannelDimensions(channelId);
			}
		});
		this.notifyChange();
		return false;
	}

	@action
	handleRelationshipUpdate(): boolean {
		ChannelMessages.forEach((messages) => {
			const updatedMessages = messages.map((message) =>
				message.withUpdates({
					blocked: RelationshipStore.isBlocked(message.author.id),
				}),
			);
			ChannelMessages.commit(messages.reset(updatedMessages));
		});
		this.notifyChange();
		return false;
	}

	@action
	handleMessageReveal(action: {channelId: ChannelId; messageId: MessageId | null}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		ChannelMessages.commit(messages.mutate({revealedMessageId: action.messageId}));
		this.notifyChange();
		return true;
	}

	@action
	handleClearJumpTarget(action: {channelId: ChannelId}): boolean {
		const messages = ChannelMessages.get(action.channelId);
		if (messages?.jumpTargetId != null) {
			ChannelMessages.commit(messages.mutate({jumpTargetId: null, jumped: false}));
			this.notifyChange();
			return true;
		}
		return false;
	}

	@action
	handleReaction(action: {
		type: 'MESSAGE_REACTION_ADD' | 'MESSAGE_REACTION_REMOVE';
		channelId: ChannelId;
		messageId: MessageId;
		userId: string;
		emoji: ReactionEmoji;
		optimistic?: boolean;
	}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;

		const currentUser = UserStore.getCurrentUser();
		const isCurrentUser = currentUser?.id === action.userId;

		if (action.optimistic && !isCurrentUser) return false;

		const updated = existing.update(action.messageId, (message) => {
			return action.type === 'MESSAGE_REACTION_ADD'
				? message.withReaction(action.emoji, true, isCurrentUser)
				: message.withReaction(action.emoji, false, isCurrentUser);
		});

		ChannelMessages.commit(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleRemoveAllReactions(action: {channelId: ChannelId; messageId: MessageId}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;

		const updated = existing.update(action.messageId, (message) => message.withUpdates({reactions: []}));
		ChannelMessages.commit(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleMessagePreload(action: {messages: Record<ChannelId, Message>}): boolean {
		let hasChanges = false;

		for (const [channelId, messageData] of Object.entries(action.messages)) {
			if (!messageData?.id || !messageData.author) continue;

			ChannelStore.handleMessageCreate({message: messageData});

			const channelMessages = ChannelMessages.getOrCreate(channelId);
			if (!channelMessages.has(messageData.id)) {
				ChannelMessages.commit(channelMessages.receiveMessage(messageData, false));
				hasChanges = true;
			}
		}

		if (hasChanges) {
			this.notifyChange();
		}

		return hasChanges;
	}

	@action
	handleOptimisticEdit(action: {
		channelId: ChannelId;
		messageId: MessageId;
		content: string;
	}): {originalContent: string; originalEditedTimestamp: string | null} | null {
		const {channelId, messageId, content} = action;
		const existing = ChannelMessages.get(channelId);
		if (!existing) return null;

		const originalMessage = existing.get(messageId);
		if (!originalMessage) return null;

		const rollbackData = {
			originalContent: originalMessage.content,
			originalEditedTimestamp: originalMessage.editedTimestamp?.toISOString() ?? null,
		};

		const updated = existing.update(messageId, (msg) =>
			msg.withUpdates({
				content,
				state: MessageStates.EDITING,
			}),
		);

		ChannelMessages.commit(updated);
		this.notifyChange();

		return rollbackData;
	}

	@action
	handleEditRollback(action: {
		channelId: ChannelId;
		messageId: MessageId;
		originalContent: string;
		originalEditedTimestamp: string | null;
	}): void {
		const {channelId, messageId, originalContent, originalEditedTimestamp} = action;

		const existing = ChannelMessages.get(channelId);
		if (!existing || !existing.has(messageId)) return;

		const updated = existing.update(messageId, (msg) =>
			msg.withUpdates({
				content: originalContent,
				edited_timestamp: originalEditedTimestamp ?? undefined,
				state: MessageStates.SENT,
			}),
		);

		ChannelMessages.commit(updated);
		this.notifyChange();
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.version,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new MessageStore();
