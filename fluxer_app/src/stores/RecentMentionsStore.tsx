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

import {makeAutoObservable} from 'mobx';
import {ChannelTypes} from '~/Constants';
import {makePersistent} from '~/lib/MobXPersistence';
import type {Channel} from '~/records/ChannelRecord';
import {type Message, MessageRecord, messageMentionsCurrentUser} from '~/records/MessageRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildNSFWAgreeStore from '~/stores/GuildNSFWAgreeStore';
import GuildStore from '~/stores/GuildStore';
import MobileMentionToastStore from '~/stores/MobileMentionToastStore';
import type {ReactionEmoji} from '~/utils/ReactionUtils';

export interface MentionFilters {
	includeEveryone: boolean;
	includeRoles: boolean;
	includeGuilds: boolean;
}

class RecentMentionsStore {
	recentMentions: Array<MessageRecord> = [];
	fetched = false;
	hasMore = true;
	isLoadingMore = false;
	filters: MentionFilters = {
		includeEveryone: true,
		includeRoles: true,
		includeGuilds: true,
	};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'RecentMentionsStore', ['filters']);
	}

	getFilters(): MentionFilters {
		return this.filters;
	}

	getHasMore(): boolean {
		return this.hasMore;
	}

	getIsLoadingMore(): boolean {
		return this.isLoadingMore;
	}

	getAccessibleMentions(): ReadonlyArray<MessageRecord> {
		return this.recentMentions.filter((message) => this.isMessageAccessible(message));
	}

	private isMessageAccessible(message: MessageRecord): boolean {
		const channel = ChannelStore.getChannel(message.channelId);
		if (!channel) {
			return false;
		}

		switch (channel.type) {
			case ChannelTypes.DM:
			case ChannelTypes.DM_PERSONAL_NOTES:
				return true;

			case ChannelTypes.GROUP_DM:
				return channel.recipientIds.length > 0;

			case ChannelTypes.GUILD_TEXT:
			case ChannelTypes.GUILD_VOICE: {
				if (!channel.guildId) return false;
				const guild = GuildStore.getGuild(channel.guildId);
				return guild != null;
			}

			default:
				return false;
		}
	}

	handleConnectionOpen(): void {
		this.recentMentions = this.recentMentions.filter((message) => this.isMessageAccessible(message));
	}

	handleFetchPending(): void {
		this.isLoadingMore = true;
	}

	handleRecentMentionsFetchSuccess(messages: ReadonlyArray<Message>): void {
		const filteredMessages = this.filterMessages(messages);
		const isLoadMore = this.isLoadingMore && this.fetched;

		if (isLoadMore) {
			this.recentMentions.push(...filteredMessages.map((m) => new MessageRecord(m)));
		} else {
			this.recentMentions = filteredMessages.map((message) => new MessageRecord(message));
		}

		this.fetched = true;
		this.hasMore = messages.length === 25;
		this.isLoadingMore = false;
	}

	handleRecentMentionsFetchError(): void {
		this.isLoadingMore = false;
	}

	updateFilters(filters: Partial<MentionFilters>): void {
		Object.assign(this.filters, filters);
		this.fetched = false;
	}

	private filterMessages(messages: ReadonlyArray<Message>): ReadonlyArray<Message> {
		return messages.filter((message) => {
			const channel = ChannelStore.getChannel(message.channel_id);
			if (!channel) return false;

			if (channel.isNSFW()) {
				return !GuildNSFWAgreeStore.shouldShowGate(channel.id);
			}
			return true;
		});
	}

	handleChannelDelete(channel: Channel): void {
		this.recentMentions = this.recentMentions.filter((message) => message.channelId !== channel.id);
	}

	handleGuildDelete(guildId: string): void {
		this.recentMentions = this.recentMentions.filter((message) => {
			const channel = ChannelStore.getChannel(message.channelId);
			return !channel || channel.guildId !== guildId;
		});
	}

	handleMessageUpdate(message: Message): void {
		const index = this.recentMentions.findIndex((m) => m.id === message.id);
		if (index === -1) return;

		this.recentMentions[index] = this.recentMentions[index].withUpdates(message);
	}

	handleMessageDelete(messageId: string): void {
		this.recentMentions = this.recentMentions.filter((message) => message.id !== messageId);
		MobileMentionToastStore.dequeue(messageId);
	}

	handleMessageCreate(message: Message): void {
		if (!messageMentionsCurrentUser(message)) {
			return;
		}

		const channel = ChannelStore.getChannel(message.channel_id);
		if (!channel) return;

		if (channel.isNSFW()) {
			if (GuildNSFWAgreeStore.shouldShowGate(channel.id)) {
				return;
			}
		}

		const messageRecord = new MessageRecord(message);
		this.recentMentions.unshift(messageRecord);
		MobileMentionToastStore.enqueue(messageRecord);
	}

	private updateMessageWithReaction(messageId: string, updater: (message: MessageRecord) => MessageRecord): void {
		const index = this.recentMentions.findIndex((m) => m.id === messageId);
		if (index === -1) return;

		this.recentMentions[index] = updater(this.recentMentions[index]);
	}

	handleMessageReactionAdd(messageId: string, userId: string, emoji: ReactionEmoji): void {
		this.updateMessageWithReaction(messageId, (message) =>
			message.withReaction(emoji, true, userId === AuthenticationStore.currentUserId),
		);
	}

	handleMessageReactionRemove(messageId: string, userId: string, emoji: ReactionEmoji): void {
		this.updateMessageWithReaction(messageId, (message) =>
			message.withReaction(emoji, false, userId === AuthenticationStore.currentUserId),
		);
	}

	handleMessageReactionRemoveAll(messageId: string): void {
		this.updateMessageWithReaction(messageId, (message) => message.withUpdates({reactions: []}));
	}

	handleMessageReactionRemoveEmoji(messageId: string, emoji: ReactionEmoji): void {
		this.updateMessageWithReaction(messageId, (message) => message.withoutReactionEmoji(emoji));
	}
}

export default new RecentMentionsStore();
