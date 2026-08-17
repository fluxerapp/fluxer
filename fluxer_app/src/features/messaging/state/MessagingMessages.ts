// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import type {Presence} from '@app/features/gateway/types/GatewayPresenceTypes';
import Guilds from '@app/features/guild/state/Guilds';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {JumpOptions} from '@app/features/messaging/state/ChannelMessages';
import {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import type {UnreadJumpAnchor} from '@app/features/read_state/state/ReadStates';
import Relationships from '@app/features/relationship/state/Relationships';
import Dimension from '@app/features/ui/state/Dimension';
import Users from '@app/features/user/state/Users';
import {FAVORITES_GUILD_ID, ME} from '@fluxer/constants/src/AppConstants';
import {MessageStates} from '@fluxer/constants/src/ChannelConstants';
import {type JumpType, JumpTypes} from '@fluxer/constants/src/JumpConstants';
import {MAX_MESSAGES_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import type {ChannelId} from '@fluxer/schema/src/branded/WireIds';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {action, makeAutoObservable, reaction} from 'mobx';

interface GuildMemberUpdateAction {
	type: 'GUILD_MEMBER_UPDATE';
	guildId: string;
	member: GuildMemberData;
}

interface PresenceUpdateAction {
	type: 'PRESENCE_UPDATE';
	presence: Presence;
}

export interface PendingJumpDispatch {
	messageId: string;
	flash?: boolean;
	offset?: number;
	returnTargetId?: string;
	returnChannelId?: string | null;
	returnGuildId?: string | null;
	jumpType?: JumpType;
}

class Messages {
	private pendingJumpDispatches = new Map<string, PendingJumpDispatch>();
	private messageRefsByAuthor = new Map<string, Map<string, Set<string>>>();
	private indexedAuthorsByChannel = new Map<string, Map<string, string>>();
	updateCounter = 0;
	private pendingFullHydration = false;

	constructor() {
		makeAutoObservable<this, 'messageRefsByAuthor' | 'indexedAuthorsByChannel'>(
			this,
			{
				messageRefsByAuthor: false,
				indexedAuthorsByChannel: false,
			},
			{autoBind: true},
		);
	}

	get version(): number {
		return this.updateCounter;
	}

	@action
	private notifyChange(): void {
		this.updateCounter += 1;
	}

	private commitMessages(messages: ChannelMessages): ChannelMessages {
		const committed = ChannelMessages.commit(messages) ?? messages;
		this.pruneStaleIndexedChannels();
		this.indexChannelMessages(committed);
		return committed;
	}

	private clearMessages(channelId: string): void {
		ChannelMessages.clear(channelId);
		this.unindexChannelMessages(channelId);
	}

	private indexChannelMessages(messages: ChannelMessages): void {
		this.unindexChannelMessages(messages.channelId);
		if (messages.length === 0) {
			return;
		}
		const channelIndex = new Map<string, string>();
		messages.forEach((message) => {
			const authorId = message.author.id;
			channelIndex.set(message.id, authorId);
			let refsByChannel = this.messageRefsByAuthor.get(authorId);
			if (!refsByChannel) {
				refsByChannel = new Map();
				this.messageRefsByAuthor.set(authorId, refsByChannel);
			}
			let messageIds = refsByChannel.get(messages.channelId);
			if (!messageIds) {
				messageIds = new Set();
				refsByChannel.set(messages.channelId, messageIds);
			}
			messageIds.add(message.id);
		});
		this.indexedAuthorsByChannel.set(messages.channelId, channelIndex);
	}

	private unindexChannelMessages(channelId: string): void {
		const channelIndex = this.indexedAuthorsByChannel.get(channelId);
		if (!channelIndex) {
			return;
		}
		for (const [messageId, authorId] of channelIndex) {
			this.unindexMessageRef(authorId, channelId, messageId);
		}
		this.indexedAuthorsByChannel.delete(channelId);
	}

	private unindexMessageRef(authorId: string, channelId: string, messageId: string): void {
		const refsByChannel = this.messageRefsByAuthor.get(authorId);
		if (!refsByChannel) {
			return;
		}
		const messageIds = refsByChannel.get(channelId);
		if (!messageIds) {
			return;
		}
		messageIds.delete(messageId);
		if (messageIds.size === 0) {
			refsByChannel.delete(channelId);
		}
		if (refsByChannel.size === 0) {
			this.messageRefsByAuthor.delete(authorId);
		}
	}

	private pruneStaleIndexedChannels(): void {
		for (const channelId of Array.from(this.indexedAuthorsByChannel.keys())) {
			if (!ChannelMessages.get(channelId)) {
				this.unindexChannelMessages(channelId);
			}
		}
	}

	private patchAuthorMessages(
		userId: string,
		updater: (message: Message) => Message,
		options: {guildId?: string | null; patchMissingChannels?: boolean} = {},
	): boolean {
		const refsByChannel = this.messageRefsByAuthor.get(userId);
		if (!refsByChannel) {
			return false;
		}
		let hasChanges = false;
		const channelRefs = Array.from(refsByChannel, ([channelId, messageIds]) => ({
			channelId,
			messageIds: Array.from(messageIds),
		}));
		for (const {channelId, messageIds} of channelRefs) {
			let messages = ChannelMessages.get(channelId);
			if (!messages) {
				this.unindexChannelMessages(channelId);
				continue;
			}
			if (options.guildId != null) {
				const channel = Channels.getChannel(channelId);
				if (channel == null) {
					if (!options.patchMissingChannels) {
						continue;
					}
				} else if (channel.guildId !== options.guildId) {
					continue;
				}
			}
			const previous = messages;
			for (const messageId of messageIds) {
				if (!messages.has(messageId, false)) {
					this.unindexMessageRef(userId, channelId, messageId);
					continue;
				}
				messages = messages.update(messageId, updater);
			}
			if (messages !== previous) {
				this.commitMessages(messages);
				hasChanges = true;
			}
		}
		return hasChanges;
	}

	getMessages(channelId: string): ChannelMessages {
		const messages = ChannelMessages.getOrCreate(channelId);
		this.pruneStaleIndexedChannels();
		return messages;
	}

	getCachedMessages(channelId: string): ChannelMessages | undefined {
		return ChannelMessages.get(channelId);
	}

	shouldPreloadLatestPage(channelId: string): boolean {
		if (!GatewayConnection.isConnected || !Channels.getChannel(channelId)) {
			return false;
		}
		const messages = ChannelMessages.get(channelId);
		return !messages || (messages.length === 0 && !messages.loadingMore && !messages.ready);
	}

	@action
	preloadLatestPage(channelId: string, guildId?: string | null, unreadAnchor?: UnreadJumpAnchor | null): boolean {
		if (!this.shouldPreloadLatestPage(channelId)) {
			return false;
		}
		const channel = Channels.getChannel(channelId);
		const resolvedGuildId = guildId ?? channel?.guildId ?? (channel?.isPrivate() ? ME : undefined);
		this.handleChannelSelect({guildId: resolvedGuildId ?? undefined, channelId, unreadAnchor});
		return true;
	}

	getMessage(channelId: string, messageId: string): Message | undefined {
		return ChannelMessages.getOrCreate(channelId).get(messageId);
	}

	getLastEditableMessage(channelId: string): Message | undefined {
		return this.getMessages(channelId).findNewest((message) => {
			return message.isCurrentUserAuthor() && message.state === MessageStates.SENT && message.isUserMessage();
		});
	}

	jumpedMessageId(channelId: string): string | null | undefined {
		const channel = ChannelMessages.get(channelId);
		return channel?.jumpTargetId;
	}

	hasPresent(channelId: string): boolean {
		const channel = ChannelMessages.get(channelId);
		return channel?.hasPresent() ?? false;
	}

	@action
	setPendingJumpDispatch(channelId: string, dispatch: PendingJumpDispatch): void {
		this.pendingJumpDispatches.set(channelId, dispatch);
	}

	private takePendingJumpDispatch(channelId: string, messageId: string): PendingJumpDispatch | null {
		const dispatch = this.pendingJumpDispatches.get(channelId);
		if (!dispatch) return null;
		this.pendingJumpDispatches.delete(channelId);
		return dispatch.messageId === messageId ? dispatch : null;
	}

	@action
	handleConnectionClosed(): boolean {
		let didUpdate = false;
		ChannelMessages.forEach((messages) => {
			if (messages.loadingMore) {
				this.commitMessages(messages.mutate({loadingMore: false}));
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
			this.clearMessages(channelId);
			Dimension.clearChannelDimensions(channelId);
		}
		this.messageRefsByAuthor.clear();
		this.indexedAuthorsByChannel.clear();
		this.pendingJumpDispatches.clear();
		this.pendingFullHydration = true;
		this.notifyChange();
		return true;
	}

	@action
	handleResumed(): boolean {
		ChannelMessages.forEach((messages) => {
			this.commitMessages(messages.mutate({ready: true}));
		});
		this.notifyChange();
		return true;
	}

	@action
	handleConnectionOpen(resolveUnreadAnchor: (channelId: string) => UnreadJumpAnchor | null): boolean {
		const selectedChannelId = SelectedChannel.currentChannelId;
		let didHydrateSelectedChannel = false;
		if (selectedChannelId) {
			const pending = this.pendingJumpDispatches.get(selectedChannelId);
			if (pending) {
				this.pendingJumpDispatches.delete(selectedChannelId);
				MessageCommands.jumpToMessage({channelId: selectedChannelId, ...pending});
				this.pendingFullHydration = false;
				return true;
			}
		}
		ChannelMessages.forEach((messages) => {
			if (messages.channelId === selectedChannelId && Channels.getChannel(messages.channelId) != null) {
				this.startChannelHydration(messages.channelId, {
					forceScrollToBottom: this.pendingFullHydration,
					unreadAnchor: resolveUnreadAnchor(messages.channelId),
				});
				didHydrateSelectedChannel = true;
			} else {
				this.clearMessages(messages.channelId);
				Dimension.clearChannelDimensions(messages.channelId);
			}
		});
		if (this.pendingFullHydration && !didHydrateSelectedChannel && selectedChannelId) {
			this.startChannelHydration(selectedChannelId, {forceScrollToBottom: true});
			didHydrateSelectedChannel = true;
		}
		this.pendingFullHydration = false;
		if (!didHydrateSelectedChannel && selectedChannelId && Channels.getChannel(selectedChannelId)) {
			const messages = ChannelMessages.getOrCreate(selectedChannelId);
			if (!messages.ready && !messages.loadingMore && messages.length === 0) {
				this.commitMessages(messages.mutate({loadingMore: true}));
				const unreadAnchor = resolveUnreadAnchor(selectedChannelId);
				if (unreadAnchor != null) {
					MessageCommands.jumpToMessage({
						channelId: selectedChannelId,
						messageId: unreadAnchor.messageId,
						offset: unreadAnchor.offset,
						flash: false,
						jumpType: JumpTypes.INSTANT,
					});
				} else {
					MessageCommands.fetchMessages(selectedChannelId, null, null, MAX_MESSAGES_PER_CHANNEL);
				}
				didHydrateSelectedChannel = true;
			}
		}
		this.notifyChange();
		return didHydrateSelectedChannel;
	}

	private startChannelHydration(
		channelId: string,
		options: {
			forceScrollToBottom?: boolean;
			unreadAnchor?: UnreadJumpAnchor | null;
		} = {},
	): void {
		if (!Channels.getChannel(channelId)) return;
		const {forceScrollToBottom = false, unreadAnchor = null} = options;
		const messages = ChannelMessages.getOrCreate(channelId);
		this.commitMessages(messages.mutate({loadingMore: true, ready: false, error: false}));
		if (forceScrollToBottom) {
			Dimension.updateChannelDimensions(channelId, 1, 1, 0);
			MessageCommands.fetchMessages(channelId, null, null, MAX_MESSAGES_PER_CHANNEL);
			return;
		}
		if (unreadAnchor != null) {
			MessageCommands.jumpToMessage({
				channelId,
				messageId: unreadAnchor.messageId,
				offset: unreadAnchor.offset,
				flash: false,
				jumpType: JumpTypes.INSTANT,
			});
			return;
		}
		MessageCommands.fetchMessages(channelId, null, null, MAX_MESSAGES_PER_CHANNEL);
	}

	@action
	handleChannelSelect(action: {
		guildId?: string;
		channelId?: string | null;
		messageId?: string;
		unreadAnchor?: UnreadJumpAnchor | null;
	}): boolean {
		const channelId = action.channelId ?? action.guildId;
		if (channelId == null || channelId === ME) {
			return false;
		}
		const currentChannel = Channels.getChannel(channelId);
		const messages = ChannelMessages.getOrCreate(channelId);
		if (action.messageId) {
			const stashed = this.takePendingJumpDispatch(channelId, action.messageId);
			const dispatch: PendingJumpDispatch = stashed ?? {
				messageId: action.messageId,
				flash: true,
				jumpType: JumpTypes.INSTANT,
			};
			if (!GatewayConnection.isConnected) {
				this.pendingJumpDispatches.set(channelId, dispatch);
				return true;
			}
			MessageCommands.jumpToMessage({channelId, ...dispatch});
			return false;
		}
		if (!GatewayConnection.isConnected || messages.loadingMore || messages.ready) {
			if (messages.ready && Dimension.isAtBottom(channelId)) {
				this.commitMessages(messages.truncateTop(MAX_MESSAGES_PER_CHANNEL));
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
			guildExists = channelGuildId ? !!Guilds.getGuild(channelGuildId) : false;
		} else if (action.guildId && !isPrivateChannel) {
			guildExists = !!Guilds.getGuild(action.guildId);
		}
		if (!isNonGuildChannel && !guildExists) {
			return false;
		}
		this.commitMessages(messages.mutate({loadingMore: true}));
		this.notifyChange();
		const unreadAnchor = action.unreadAnchor ?? null;
		if (unreadAnchor != null) {
			MessageCommands.jumpToMessage({
				channelId,
				messageId: unreadAnchor.messageId,
				offset: unreadAnchor.offset,
				flash: false,
				jumpType: JumpTypes.INSTANT,
			});
			return false;
		}
		MessageCommands.fetchMessages(channelId, null, null, MAX_MESSAGES_PER_CHANNEL);
		return false;
	}

	@action
	handleGuildUnavailable(guildId: string, unavailable: boolean): boolean {
		if (!unavailable) {
			return false;
		}
		let didUpdate = false;
		const selectedChannelId = SelectedChannel.currentChannelId;
		let selectedChannelAffected = false;
		ChannelMessages.forEach(({channelId}) => {
			const channel = Channels.getChannel(channelId);
			if (channel && channel.guildId === guildId) {
				this.clearMessages(channelId);
				Dimension.clearChannelDimensions(channelId);
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
	handleGuildCreate(action: {
		guild: {
			id: string;
		};
	}): boolean {
		if (SelectedGuild.selectedGuildId !== action.guild.id) {
			return false;
		}
		const selectedChannelId = SelectedChannel.selectedChannelIds.get(action.guild.id);
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
			this.commitMessages(currentMessages.mutate({loadingMore: true}));
			MessageCommands.fetchMessages(selectedChannelId, null, null, MAX_MESSAGES_PER_CHANNEL);
			this.notifyChange();
			return true;
		}
		return didChannelSelect;
	}

	@action
	handleLoadMessages(action: {channelId: string; jump?: JumpOptions}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		this.commitMessages(messages.loadStart(action.jump));
		this.notifyChange();
		return false;
	}

	@action
	handleTruncateMessages(action: {channelId: string; truncateBottom?: boolean; truncateTop?: boolean}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId).truncate(
			action.truncateBottom ?? false,
			action.truncateTop ?? false,
		);
		this.commitMessages(messages);
		this.notifyChange();
		return false;
	}

	@action
	handleLoadMessagesSuccessCached(action: {
		channelId: string;
		jump?: JumpOptions;
		before?: string;
		after?: string;
		limit: number;
	}): boolean {
		let messages = ChannelMessages.getOrCreate(action.channelId);
		if (action.jump?.present) {
			messages = messages.jumpToPresent(action.limit);
		} else if (action.jump?.messageId) {
			messages = messages.jumpToMessage({
				messageId: action.jump.messageId,
				flash: action.jump.flash,
				offset: action.jump.offset,
				returnTargetId: action.jump.returnMessageId,
				returnChannelId: action.jump.returnChannelId,
				returnGuildId: action.jump.returnGuildId,
				jumpType: action.jump.jumpType,
			});
		} else if (action.before || action.after) {
			messages = messages.loadFromCache(action.before != null, action.limit);
		}
		this.commitMessages(messages);
		this.notifyChange();
		return false;
	}

	@action
	handleLoadMessagesSuccess(action: {
		channelId: string;
		isBefore?: boolean;
		isAfter?: boolean;
		jump?: JumpOptions;
		hasMoreBefore?: boolean;
		hasMoreAfter?: boolean;
		cached?: boolean;
		messages: Array<WireMessage>;
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
		this.commitMessages(messages);
		this.notifyChange();
		return false;
	}

	@action
	handleLoadMessagesFailure(action: {channelId: string}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		this.commitMessages(messages.mutate({loadingMore: false, error: true}));
		this.notifyChange();
		return false;
	}

	@action
	handleLoadMessagesBlocked(action: {channelId: string}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		if (!messages.loadingMore && !messages.error) {
			return false;
		}
		this.commitMessages(messages.mutate({loadingMore: false, error: false}));
		this.notifyChange();
		return true;
	}

	@action
	handleIncomingMessage(action: {channelId: string; message: WireMessage}): boolean {
		Channels.handleMessageCreate({message: action.message});
		const existing = ChannelMessages.get(action.channelId);
		if (!existing?.ready) {
			return false;
		}
		const updated = existing.receiveMessage(action.message, Dimension.isAtBottom(action.channelId));
		this.commitMessages(updated);
		this.notifyChange();
		return false;
	}

	@action
	handleSendFailed(action: {channelId: string; nonce: string}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing || !existing.has(action.nonce)) return false;
		const updated = existing.update(action.nonce, (message) => message.withUpdates({state: MessageStates.FAILED}));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleSendRetry(action: {channelId: string; messageId: string}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing || !existing.has(action.messageId)) return false;
		const updated = existing.update(action.messageId, (message) => message.withUpdates({state: MessageStates.SENDING}));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleMessageDelete(action: {id: string; channelId: string}): boolean {
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
		this.commitMessages(messages);
		this.notifyChange();
		return true;
	}

	@action
	handleMessageDeleteBulk(action: {ids: Array<string>; channelId: string}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;
		let messages = existing.removeMany(action.ids);
		if (messages === existing) return false;
		if (messages.revealedMessageId != null && action.ids.includes(messages.revealedMessageId)) {
			const after = messages.getAfter(messages.revealedMessageId);
			messages = messages.mutate({revealedMessageId: after?.id ?? null});
		}
		this.commitMessages(messages);
		this.notifyChange();
		return true;
	}

	@action
	handleMessageUpdate(action: {message: WireMessage}): boolean {
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
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleUserUpdate(action: {
		user: {
			id: string;
		};
	}): boolean {
		const userId = action.user.id;
		const updatedAuthor = Users.getUser(userId);
		if (!updatedAuthor) return false;
		const authorJson = updatedAuthor.toJSON();
		const hasChanges = this.patchAuthorMessages(userId, (message) => message.withUpdates({author: authorJson}));
		if (hasChanges) {
			this.notifyChange();
		}
		return hasChanges;
	}

	@action
	handleGuildMemberUpdate(action: GuildMemberUpdateAction): boolean {
		const userId = action.member.user.id;
		const updatedAuthor = Users.getUser(userId);
		if (!updatedAuthor) return false;
		const authorJson = updatedAuthor.toJSON();
		const hasChanges = this.patchAuthorMessages(userId, (message) => message.withUpdates({author: authorJson}), {
			guildId: action.guildId,
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
		const userId = action.presence.user.id;
		const updatedAuthor = Users.getUser(userId);
		if (!updatedAuthor) return false;
		const authorJson = updatedAuthor.toJSON();
		const guildId = action.presence.guild_id ?? null;
		const hasChanges = this.patchAuthorMessages(userId, (message) => message.withUpdates({author: authorJson}), {
			guildId,
			patchMissingChannels: true,
		});
		if (hasChanges) {
			this.notifyChange();
		}
		return hasChanges;
	}

	@action
	handleCleanup(): boolean {
		ChannelMessages.forEach(({channelId}) => {
			if (Channels.getChannel(channelId) == null) {
				this.clearMessages(channelId);
				Dimension.clearChannelDimensions(channelId);
			}
		});
		this.notifyChange();
		return false;
	}

	@action
	handleRelationshipUpdate(): boolean {
		let hasChanges = false;
		ChannelMessages.forEach((messages) => {
			const next = messages.patchMatching(
				(message) => message.blocked !== Relationships.isBlocked(message.author.id),
				(message) => message.withUpdates({blocked: Relationships.isBlocked(message.author.id)}),
			);
			if (next !== null) {
				this.commitMessages(next);
				hasChanges = true;
			}
		});
		if (hasChanges) this.notifyChange();
		return false;
	}

	@action
	handleMessageReveal(action: {channelId: string; messageId: string | null}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		this.commitMessages(messages.mutate({revealedMessageId: action.messageId}));
		this.notifyChange();
		return true;
	}

	@action
	handleClearJumpTarget(action: {channelId: string; clearReturnTarget?: boolean}): boolean {
		const messages = ChannelMessages.get(action.channelId);
		if (
			messages &&
			(messages.jumpTargetId != null ||
				messages.jumped ||
				(action.clearReturnTarget === true && messages.jumpReturnTargetId != null))
		) {
			this.commitMessages(messages.clearJumpTarget({clearReturnTarget: action.clearReturnTarget}));
			this.notifyChange();
			return true;
		}
		return false;
	}

	@action
	handleReaction(action: {
		type: 'MESSAGE_REACTION_ADD' | 'MESSAGE_REACTION_REMOVE';
		channelId: string;
		messageId: string;
		userId: string;
		emoji: ReactionEmoji;
		optimistic?: boolean;
		skipReactionStore?: boolean;
	}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;
		const currentUser = Users.getCurrentUser();
		const isCurrentUser = currentUser?.id === action.userId;
		if (action.optimistic && !isCurrentUser) return false;
		const updated = existing.update(action.messageId, (message) => {
			if (action.skipReactionStore) {
				return message.withUpdates({});
			}
			return action.type === 'MESSAGE_REACTION_ADD'
				? message.withReaction(action.emoji, true, isCurrentUser)
				: message.withReaction(action.emoji, false, isCurrentUser);
		});
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleRemoveAllReactions(action: {channelId: string; messageId: string}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;
		const updated = existing.update(action.messageId, (message) => message.withUpdates({reactions: []}));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleRemoveReactionEmoji(action: {channelId: string; messageId: string; emoji: ReactionEmoji}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;
		const updated = existing.update(action.messageId, (message) => message.withoutReactionEmoji(action.emoji));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	@action
	handleMessagePreload(action: {messages: Record<ChannelId, WireMessage>}): boolean {
		let hasChanges = false;
		for (const [channelId, messageData] of Object.entries(action.messages)) {
			if (!messageData?.id || !messageData.author) continue;
			Channels.handleMessageCreate({message: messageData});
			const channelMessages = ChannelMessages.getOrCreate(channelId);
			if (!channelMessages.has(messageData.id)) {
				this.commitMessages(channelMessages.receiveMessage(messageData, false));
				hasChanges = true;
			}
		}
		if (hasChanges) {
			this.notifyChange();
		}
		return hasChanges;
	}

	@action
	handleOptimisticEdit(action: {channelId: string; messageId: string; content: string}): {
		originalContent: string;
		originalEditedTimestamp: string | null;
	} | null {
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
		this.commitMessages(updated);
		this.notifyChange();
		return rollbackData;
	}

	@action
	handleEditRollback(action: {
		channelId: string;
		messageId: string;
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
		this.commitMessages(updated);
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

export default new Messages();
