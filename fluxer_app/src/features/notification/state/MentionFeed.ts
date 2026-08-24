// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import Guilds from '@app/features/guild/state/Guilds';
import {Message, messageMentionsCurrentUser} from '@app/features/messaging/models/MessagingMessage';
import {
	DEFAULT_MENTION_FILTERS,
	type MentionFilters,
	messageMatchesMentionTypeFilters,
} from '@app/features/notification/utils/MentionFeedFilters';
import Relationships from '@app/features/relationship/state/Relationships';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGES_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import type {Channel as WireChannel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {RecentMentionsSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

export type {MentionFilters};

const DEFAULT_FILTERS = DEFAULT_MENTION_FILTERS;

class MentionFeed {
	recentMentions: Array<Message> = [];
	fetched = false;
	hasMore = true;
	isLoadingMore = false;
	filters: MentionFilters = {...DEFAULT_FILTERS};
	private fetchGeneration = 0;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'recentMentions',
			schema: RecentMentionsSettingsSchema,
			persist: ['filters'],
			toMessage: (s) => ({
				includeEveryone:
					s.filters.includeEveryone === DEFAULT_FILTERS.includeEveryone ? undefined : s.filters.includeEveryone,
				includeRoles: s.filters.includeRoles === DEFAULT_FILTERS.includeRoles ? undefined : s.filters.includeRoles,
				includeGuilds: s.filters.includeGuilds === DEFAULT_FILTERS.includeGuilds ? undefined : s.filters.includeGuilds,
			}),
			applyMessage: (s, m) => {
				s.filters = {
					includeEveryone: m.includeEveryone ?? DEFAULT_FILTERS.includeEveryone,
					includeRoles: m.includeRoles ?? DEFAULT_FILTERS.includeRoles,
					includeGuilds: m.includeGuilds ?? DEFAULT_FILTERS.includeGuilds,
				};
			},
		});
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

	getAccessibleMentions(): ReadonlyArray<Message> {
		return this.recentMentions.filter(
			(message) => this.isMessageAccessible(message) && !Relationships.isBlocked(message.author.id),
		);
	}

	private isMessageAccessible(message: Message): boolean {
		const channel = Channels.getChannel(message.channelId);
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
				const guild = Guilds.getGuild(channel.guildId);
				return guild != null;
			}
			default:
				return false;
		}
	}

	handleGatewayReady(): void {
		this.recentMentions = [];
		this.fetched = false;
		this.hasMore = true;
		this.isLoadingMore = false;
		this.fetchGeneration++;
	}

	handleFetchPending(): number {
		this.isLoadingMore = true;
		this.fetchGeneration++;
		return this.fetchGeneration;
	}

	handleRecentMentionsFetchSuccess(requestId: number, messages: ReadonlyArray<WireMessage>): void {
		if (requestId !== this.fetchGeneration) return;
		const filteredMessages = this.filterFetchedMessages(messages);
		const isLoadMore = this.isLoadingMore && this.fetched;
		if (isLoadMore) {
			this.recentMentions.push(...filteredMessages.map((m) => new Message(m, {missingReactions: 'preserve'})));
		} else {
			this.recentMentions = filteredMessages.map((message) => new Message(message, {missingReactions: 'preserve'}));
		}
		this.fetched = true;
		this.hasMore = messages.length === MAX_MESSAGES_PER_CHANNEL;
		this.isLoadingMore = false;
	}

	handleRecentMentionsFetchError(requestId: number): void {
		if (requestId !== this.fetchGeneration) return;
		this.isLoadingMore = false;
	}

	updateFilters(filters: Partial<MentionFilters>): void {
		this.filters = {...this.filters, ...filters};
		this.fetched = false;
		this.hasMore = true;
		this.isLoadingMore = false;
		this.fetchGeneration++;
	}

	private filterFetchedMessages(messages: ReadonlyArray<WireMessage>): ReadonlyArray<WireMessage> {
		return messages.filter((message) => {
			if (Relationships.isBlocked(message.author.id)) return false;
			const channel = Channels.getChannel(message.channel_id);
			if (!channel) return false;
			return !GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null});
		});
	}

	private isMessageIncludedByFilters(message: WireMessage | Message, channel: {guildId?: string | null}): boolean {
		if (!this.filters.includeGuilds && channel.guildId != null) return false;
		return messageMatchesMentionTypeFilters(message, this.filters);
	}

	handleChannelDelete(channel: Pick<WireChannel, 'id'>): void {
		this.recentMentions = this.recentMentions.filter((message) => message.channelId !== channel.id);
	}

	handleGuildDelete(guildId: string): void {
		this.recentMentions = this.recentMentions.filter((message) => {
			const channel = Channels.getChannel(message.channelId);
			return !channel || channel.guildId !== guildId;
		});
	}

	handleMessageUpdate(message: WireMessage): void {
		const index = this.recentMentions.findIndex((m) => m.id === message.id);
		if (index === -1) return;
		this.recentMentions[index] = this.recentMentions[index].withUpdates(message);
	}

	handleMessageDelete(messageId: string): void {
		this.recentMentions = this.recentMentions.filter((message) => message.id !== messageId);
	}

	handleMessagesDelete(messageIds: ReadonlyArray<string>): void {
		const deletedIds = new Set(messageIds);
		this.recentMentions = this.recentMentions.filter((message) => !deletedIds.has(message.id));
	}

	handleMessageCreate(message: WireMessage): void {
		if (!messageMentionsCurrentUser(message)) {
			return;
		}
		if (Relationships.isBlocked(message.author.id)) {
			return;
		}
		const channel = Channels.getChannel(message.channel_id);
		if (!channel) return;
		if (!this.isMessageIncludedByFilters(message, channel)) {
			return;
		}
		if (GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null})) {
			return;
		}
		const messageRecord = new Message(message, {missingReactions: 'preserve'});
		this.recentMentions = this.recentMentions.filter((recentMention) => recentMention.id !== messageRecord.id);
		this.recentMentions.unshift(messageRecord);
	}

	private touchMessage(messageId: string): void {
		const index = this.recentMentions.findIndex((m) => m.id === messageId);
		if (index === -1) return;
		this.recentMentions[index] = this.recentMentions[index].withUpdates({});
	}

	handleMessageReactionAdd(messageId: string): void {
		this.touchMessage(messageId);
	}

	handleMessageReactionRemove(messageId: string): void {
		this.touchMessage(messageId);
	}

	handleMessageReactionRemoveAll(messageId: string): void {
		this.touchMessage(messageId);
	}

	handleMessageReactionRemoveEmoji(messageId: string): void {
		this.touchMessage(messageId);
	}
}

export default new MentionFeed();
