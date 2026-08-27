// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {resolveReadStateEntryStatus} from '@app/features/read_state/state/read_states/ReadStateEntryStatusMachine';
import {resolveReadStateMention} from '@app/features/read_state/state/read_states/ReadStateMentionMachine';
import {compareMessageIds, normalizeCount, snowflakeTimestamp} from '@app/features/read_state/state/read_states/shared';
import Relationships from '@app/features/relationship/state/Relationships';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export class ReadStateEntry {
	readonly channelId: string;
	storedGuildId: string | null = null;
	messagesLoaded = false;
	readStateKnown = false;
	private storedLastMessageId: string | null = null;
	private storedLastMessageTimestamp = 0;
	private storedAckMessageId: string | null = null;
	private storedAckMessageTimestamp = 0;
	acknowledgedPinTimestamp = 0;
	lastPinTimestamp = 0;
	ackedManually = false;
	private storedOldestUnreadMessageId: string | null = null;
	oldestUnreadNeedsRecompute = false;
	private storedStickyUnreadMessageId: string | null = null;
	estimated = false;
	private storedUnreadCount = 0;
	private storedMentionCount = 0;
	inFlightAckMessageId: string | null = null;
	serverVersion: string | null = null;
	snapshot?: {
		unread: boolean;
		mentionCount: number;
		guildUnread: boolean | null;
		guildMentionCount: number | null;
		takenAt: number;
	};

	constructor(channelId: string) {
		this.channelId = channelId;
	}

	get guildId(): string | null {
		const channel = Channels.getChannel(this.channelId);
		return channel?.guildId ?? this.storedGuildId ?? null;
	}

	get lastMessageId(): string | null {
		return this.storedLastMessageId;
	}

	set lastMessageId(messageId: string | null) {
		this.storedLastMessageId = messageId;
		this.storedLastMessageTimestamp = snowflakeTimestamp(messageId);
	}

	get lastMessageTimestamp(): number {
		return this.storedLastMessageTimestamp;
	}

	get ackMessageId(): string | null {
		return this.storedAckMessageId;
	}

	set ackMessageId(messageId: string | null) {
		this.storedAckMessageId = messageId;
		this.storedAckMessageTimestamp = snowflakeTimestamp(messageId);
	}

	get oldestUnreadMessageId(): string | null {
		return this.storedOldestUnreadMessageId;
	}

	set oldestUnreadMessageId(messageId: string | null) {
		this.storedOldestUnreadMessageId = messageId;
		this.oldestUnreadNeedsRecompute = false;
	}

	get stickyUnreadMessageId(): string | null {
		return this.storedStickyUnreadMessageId;
	}

	set stickyUnreadMessageId(messageId: string | null) {
		this.storedStickyUnreadMessageId = messageId;
	}

	get visualUnreadMessageId(): string | null {
		return this.storedStickyUnreadMessageId ?? this.storedOldestUnreadMessageId;
	}

	clearStickyUnread(): void {
		this.storedStickyUnreadMessageId = null;
	}

	get unreadCount(): number {
		return this.storedUnreadCount;
	}

	set unreadCount(count: number) {
		this.storedUnreadCount = normalizeCount(count);
	}

	get mentionCount(): number {
		return this.storedMentionCount;
	}

	set mentionCount(count: number) {
		this.storedMentionCount = normalizeCount(count);
	}

	get oldestUnreadMessageTimestamp(): number {
		return snowflakeTimestamp(this.oldestUnreadMessageId);
	}

	get ackTimestamp(): number {
		if (Number.isNaN(this.storedAckMessageTimestamp)) {
			return 0;
		}
		return this.storedAckMessageTimestamp;
	}

	get isPrivate(): boolean {
		const channel = Channels.getChannel(this.channelId);
		return channel?.isPrivate() ?? false;
	}

	supportsUnreadTracking(): boolean {
		return Channels.getChannel(this.channelId) != null || this.storedGuildId != null;
	}

	private get statusModel() {
		return resolveReadStateEntryStatus({
			supportsUnreadTracking: this.supportsUnreadTracking(),
			hasBlockedDirectMessageRecipient: this.hasBlockedDirectMessageRecipient(),
			readStateKnown: this.readStateKnown,
			lastMessageId: this.storedLastMessageId,
			ackMessageId: this.storedAckMessageId,
			mentionCount: this.mentionCount,
		});
	}

	canBeUnread(): boolean {
		return this.statusModel.canBeUnread;
	}

	supportsMentions(): boolean {
		return this.statusModel.supportsMentions;
	}

	hasUnread(): boolean {
		return this.statusModel.hasUnread;
	}

	private hasBlockedDirectMessageRecipient(): boolean {
		const channel = Channels.getChannel(this.channelId);
		if (channel?.type !== ChannelTypes.DM) {
			return false;
		}
		const recipientId = channel.getRecipientId();
		return recipientId != null && Relationships.isBlocked(recipientId);
	}

	hasMentions(): boolean {
		return this.mentionCount > 0;
	}

	isUnreadOrMentioned(): boolean {
		return this.statusModel.isUnreadOrMentioned;
	}

	computeGuildChannelBadge(
		channel: {
			isPrivate(): boolean;
			guildId?: string;
		},
		_isOptInEnabled: boolean,
		isChannelMuted: boolean,
		isGuildMuted: boolean,
	): {
		mentionCount: number;
		unread: boolean;
	} {
		if (!channel.isPrivate() && !this.supportsUnreadTracking()) {
			return {mentionCount: 0, unread: false};
		}
		const mentionCount = this.supportsMentions() ? this.mentionCount : 0;
		if (isChannelMuted || isGuildMuted) {
			return {mentionCount, unread: false};
		}
		return {
			mentionCount,
			unread: this.hasUnread(),
		};
	}

	rebuild(
		ackMessageId?: string | null,
		{
			recomputeMentions = false,
		}: {
			recomputeMentions?: boolean;
		} = {},
	): void {
		const previousUnreadCount = this.storedUnreadCount;
		if (ackMessageId !== undefined) {
			this.ackMessageId = ackMessageId;
			this.readStateKnown = true;
		} else {
			this.ackMessageId = this.storedAckMessageId;
		}
		this.oldestUnreadMessageId = null;
		this.estimated = false;
		this.unreadCount = 0;
		if (recomputeMentions) {
			this.mentionCount = 0;
		}
		if (!this.hasUnread()) {
			return;
		}
		const currentUser = Users.getCurrentUser();
		if (currentUser == null) {
			return;
		}
		const messages = Messages.getMessages(this.channelId);
		const isPrivate = this.isPrivate;
		const userId = currentUser.id;
		const guildId = this.guildId;
		const channelId = this.channelId;
		const suppressEveryone = recomputeMentions ? UserGuildSettings.isEveryoneMentionSuppressed(guildId) : false;
		const suppressRoles = recomputeMentions ? UserGuildSettings.isRoleMentionSuppressed(guildId) : false;
		const isMuted = recomputeMentions ? UserGuildSettings.isGuildOrChannelMuted(guildId, channelId) : false;
		const member = recomputeMentions && guildId ? GuildMembers.getMember(guildId, userId) : null;
		const memberRoles = member?.roles ?? null;
		let foundAckMessage = false;
		let loadedOlderMessages = false;
		let oldestUnread: string | null = null;
		let loadedUnreadCount = 0;
		messages.forEachBuffered((message) => {
			if (!foundAckMessage) {
				foundAckMessage = message.id === this.storedAckMessageId;
			} else if (this.storedOldestUnreadMessageId == null) {
				this.storedOldestUnreadMessageId = message.id;
			}
			if (compareMessageIds(message.id, this.storedAckMessageId) > 0) {
				loadedUnreadCount++;
				if (recomputeMentions && !Relationships.isBlocked(message.author.id)) {
					const mentions = message.mentions;
					const mentionEveryone = message.mentionEveryone;
					const mentionRoles = message.mentionRoles;
					const hasUserMention = mentions?.some((m) => m.id === userId) ?? false;
					const hasEveryoneMention = !suppressEveryone && !!mentionEveryone;
					const hasRoleMention = !suppressRoles && hasMatchingRoleMention(mentionRoles, memberRoles);
					const mention = resolveReadStateMention({
						authorBlocked: false,
						hasUserMention,
						hasEveryoneMention,
						hasRoleMention,
						isPrivate,
						isMuted,
					});
					if (mention.shouldMention) {
						this.mentionCount++;
					}
				}
				oldestUnread ??= message.id;
			} else {
				loadedOlderMessages = true;
			}
		});
		const hasUnreadBoundary = foundAckMessage || loadedOlderMessages || !messages.hasMoreBefore;
		const hasNewestMessages = messages.hasNewestMessages();
		this.estimated = !hasNewestMessages || (!hasUnreadBoundary && messages.length === loadedUnreadCount);
		if (this.estimated) {
			this.unreadCount = Math.max(previousUnreadCount, loadedUnreadCount);
		} else {
			this.unreadCount = loadedUnreadCount;
		}
		this.oldestUnreadMessageId = this.storedOldestUnreadMessageId ?? oldestUnread;
	}

	shouldMentionFor(message: MessageModel | WireMessage, userId: string, isPrivate: boolean): boolean {
		const authorBlocked = Relationships.isBlocked(message.author.id);
		const suppressEveryone = UserGuildSettings.isEveryoneMentionSuppressed(this.guildId);
		const suppressRoles = UserGuildSettings.isRoleMentionSuppressed(this.guildId);
		const mentions = message.mentions;
		const mentionEveryone = 'mentionEveryone' in message ? message.mentionEveryone : message.mention_everyone;
		const mentionRoles = 'mentionRoles' in message ? message.mentionRoles : message.mention_roles;
		const hasUserMention = mentions?.some((m) => m.id === userId) ?? false;
		const hasEveryoneMention = !suppressEveryone && !!mentionEveryone;
		const hasRoleMention = !suppressRoles && this.hasMatchingMemberRoleMention(userId, mentionRoles);
		const isMuted = UserGuildSettings.isGuildOrChannelMuted(this.guildId, this.channelId);
		return resolveReadStateMention({
			authorBlocked,
			hasUserMention,
			hasEveryoneMention,
			hasRoleMention,
			isPrivate,
			isMuted,
		}).shouldMention;
	}

	private hasMatchingMemberRoleMention(userId: string, mentionRoles?: ReadonlyArray<string> | null): boolean {
		const guildId = this.guildId;
		if (!guildId) return false;
		const member = GuildMembers.getMember(guildId, userId);
		return hasMatchingRoleMention(mentionRoles, member?.roles ?? null);
	}

	computeMentionCountAfterAck(messageId: string): number {
		const currentUser = Users.getCurrentUser();
		if (currentUser == null) {
			return 0;
		}
		const ackTimestamp = snowflakeTimestamp(messageId);
		if (ackTimestamp === 0 || Number.isNaN(ackTimestamp)) {
			return 0;
		}
		const messages = Messages.getMessages(this.channelId);
		const isPrivate = this.isPrivate;
		let mentionCount = 0;
		messages.forEachBuffered((message) => {
			if (snowflakeTimestamp(message.id) <= ackTimestamp) {
				return;
			}
			if (this.shouldMentionFor(message, currentUser.id, isPrivate)) {
				mentionCount++;
			}
		});
		return mentionCount;
	}
}

function hasMatchingRoleMention(
	mentionRoles: ReadonlyArray<string> | null | undefined,
	memberRoles: ReadonlySet<string> | null,
): boolean {
	if (memberRoles == null || mentionRoles == null || mentionRoles.length === 0) return false;
	for (const roleId of mentionRoles) {
		if (memberRoles.has(roleId)) return true;
	}
	return false;
}
