// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import Channels from '@app/features/channel/state/Channels';
import type {SnapshotReadStateRow} from '@app/features/gateway/snapshot/SnapshotEntities';
import Guilds from '@app/features/guild/state/Guilds';
import Messages from '@app/features/messaging/state/MessagingMessages';
import AutoAck from '@app/features/notification/state/NotificationAutoAck';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {resolveReadStateAckDecision} from '@app/features/read_state/state/read_states/ReadStateAckMachine';
import {ReadStateAckQueue} from '@app/features/read_state/state/read_states/ReadStateAckQueue';
import {ReadStateEntry} from '@app/features/read_state/state/read_states/ReadStateEntry';
import {resolveReadStateIncomingMessageDecision} from '@app/features/read_state/state/read_states/ReadStateIncomingMessageMachine';
import {resolveReadStateServerAckDecision} from '@app/features/read_state/state/read_states/ReadStateServerAckMachine';
import {
	type AckOptions,
	type AppliedAck,
	type ArchivedReadState,
	type ChannelPayload,
	compareMessageIds,
	type GatewayReadState,
	isNewerMessageId,
	parseTimestamp,
	type ReadStateAckRequestEntry,
} from '@app/features/read_state/state/read_states/shared';
import Relationships from '@app/features/relationship/state/Relationships';
import Dimension from '@app/features/ui/state/Dimension';
import Users from '@app/features/user/state/Users';
import {
	ChannelTypes,
	GUILD_TEXT_BASED_CHANNEL_TYPES,
	TEXT_BASED_CHANNEL_TYPES,
} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_FEATURE_CHANNEL_TYPES, THREAD_ONLY_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import type {ChannelId, GuildId} from '@fluxer/schema/src/branded/WireIds';
import type {ReadStateAckResponse} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {observable, runInAction} from 'mobx';

export type {GatewayReadState};

const logger = new Logger('ReadStates');

class ReadStates {
	private readonly states = new Map<ChannelId, ReadStateEntry>();
	private readonly archivedStates = new Map<ChannelId, ArchivedReadState>();
	private readonly mentionChannels = new Set<ChannelId>();
	private readonly listeners = new Set<() => void>();
	private readonly versionBox = observable.box(0);
	private readonly privateChannelVersionBox = observable.box(0);
	private hasPendingPrivateChannelChange = false;
	private readonly ackQueue = new ReadStateAckQueue({
		isPreviewChannel: (channelId) => this.isPreviewChannel(channelId),
		currentUserId: () => Users.getCurrentUser()?.id ?? null,
		getState: (channelId) => this.getIfExists(channelId),
		postAckEntries: (entries) => this.postAckEntries(entries),
		applyResponse: (response) => this.applyAckResponse(response, false),
		reportError: (error) => logger.error('Failed to flush read-state acknowledgements:', error),
	});
	updateCounter = 0;
	private pendingChanges = new Map<ChannelId, GuildId | null>();
	private pendingGlobalRecompute = false;
	private suppressVersionBumpDepth = 0;
	private hasSuppressedVersionBump = false;

	get version(): number {
		return this.versionBox.get();
	}

	get privateChannelVersion(): number {
		return this.privateChannelVersionBox.get();
	}

	private setVersion(version: number): void {
		this.updateCounter = version;
		const bumpPrivateChannelVersion = this.hasPendingPrivateChannelChange;
		this.hasPendingPrivateChannelChange = false;
		runInAction(() => {
			this.versionBox.set(version);
			if (bumpPrivateChannelVersion) {
				this.privateChannelVersionBox.set(this.privateChannelVersionBox.get() + 1);
			}
		});
	}

	private notePrivateChannelChange(guildId: string | null | undefined): void {
		if (guildId != null) {
			return;
		}
		this.hasPendingPrivateChannelChange = true;
	}

	private bumpVersion(): void {
		if (this.suppressVersionBumpDepth > 0) {
			this.hasSuppressedVersionBump = true;
			return;
		}
		this.flushVersionBump();
	}

	private flushVersionBump(): void {
		this.setVersion(this.updateCounter + 1);
		for (const listener of Array.from(this.listeners)) {
			listener();
		}
	}

	private suppressVersionBumps(callback: () => void): void {
		this.suppressVersionBumpDepth += 1;
		try {
			callback();
		} finally {
			this.suppressVersionBumpDepth -= 1;
			if (this.suppressVersionBumpDepth === 0 && this.hasSuppressedVersionBump) {
				this.hasSuppressedVersionBump = false;
				this.flushVersionBump();
			}
		}
	}

	private refreshMentionChannel(channelId: string): void {
		const state = this.states.get(channelId as ChannelId);
		if (state != null && state.mentionCount > 0 && state.supportsMentions()) {
			this.mentionChannels.add(channelId as ChannelId);
		} else {
			this.mentionChannels.delete(channelId as ChannelId);
		}
	}

	private rebuildMentionChannels(): void {
		this.mentionChannels.clear();
		for (const channelId of this.states.keys()) {
			this.refreshMentionChannel(channelId);
		}
	}

	private setMentionCount(state: ReadStateEntry, mentionCount: number): void {
		if (mentionCount > state.mentionCount) this.mentionChannels.delete(state.channelId as ChannelId);
		state.mentionCount = mentionCount;
		this.refreshMentionChannel(state.channelId);
	}

	private clearUnreadStateIfRead(state: ReadStateEntry): void {
		if (!state.hasUnread()) {
			state.estimated = false;
			state.unreadCount = 0;
			state.oldestUnreadMessageId = null;
		}
	}

	private notifyChange(
		channelId?: string,
		{
			global = false,
			guildIdOverride,
		}: {
			global?: boolean;
			guildIdOverride?: string | null;
		} = {},
	): void {
		if (global) {
			this.rebuildMentionChannels();
			this.pendingGlobalRecompute = true;
			this.pendingChanges.clear();
			this.notePrivateChannelChange(null);
		} else if (channelId != null && !this.pendingGlobalRecompute) {
			this.refreshMentionChannel(channelId);
			const entry = this.states.get(channelId as ChannelId);
			const guildId = guildIdOverride !== undefined ? guildIdOverride : (entry?.guildId ?? null);
			this.notePrivateChannelChange(guildId);
			this.pendingChanges.set(channelId as ChannelId, guildId as GuildId | null);
		}
		this.bumpVersion();
	}

	consumePendingChanges(): {
		all: boolean;
		channelIds: Array<ChannelId>;
		changes: Array<{
			channelId: string;
			guildId: string | null;
		}>;
	} {
		const all = this.pendingGlobalRecompute;
		const changes: Array<{
			channelId: string;
			guildId: string | null;
		}> = [];
		const channelIds: Array<ChannelId> = [];
		if (!all) {
			for (const [channelId, guildId] of this.pendingChanges.entries()) {
				channelIds.push(channelId);
				changes.push({channelId, guildId});
			}
		}
		this.pendingGlobalRecompute = false;
		this.pendingChanges.clear();
		return {all, channelIds, changes};
	}

	get(channelId: string): ReadStateEntry {
		let entry = this.states.get(channelId as ChannelId);
		if (entry == null) {
			entry = new ReadStateEntry(channelId);
			this.states.set(channelId as ChannelId, entry);
		}
		return entry;
	}

	getIfExists(channelId: string): ReadStateEntry | undefined {
		return this.states.get(channelId as ChannelId);
	}

	clear(channelId: string): boolean {
		const entry = this.states.get(channelId as ChannelId);
		if (entry == null) {
			return false;
		}
		const guildId = entry.guildId;
		this.ackQueue.cancel(channelId);
		this.states.delete(channelId as ChannelId);
		this.mentionChannels.delete(channelId as ChannelId);
		this.notifyChange(channelId, {guildIdOverride: guildId});
		return true;
	}

	private archiveState(channelId: string): void {
		const entry = this.states.get(channelId as ChannelId);
		if (entry == null) {
			return;
		}
		this.archivedStates.set(channelId as ChannelId, {
			ackMessageId: entry.ackMessageId,
			acknowledgedPinTimestamp: entry.acknowledgedPinTimestamp,
		});
	}

	clearAll(): void {
		this.reset();
		this.notifyChange(undefined, {global: true});
	}

	private reset(): void {
		this.states.clear();
		this.archivedStates.clear();
		this.mentionChannels.clear();
		this.pendingChanges.clear();
		this.pendingGlobalRecompute = false;
		this.ackQueue.clear();
	}

	get mentionChannelIds(): Array<ChannelId> {
		const ids: Array<ChannelId> = [];
		for (const channelId of Array.from(this.mentionChannels)) {
			const state = this.getIfExists(channelId);
			if (state?.supportsMentions()) {
				ids.push(channelId);
			} else {
				this.mentionChannels.delete(channelId);
			}
		}
		return ids;
	}

	isAutomaticAckEnabled(channelId: string): boolean {
		return AutoAck.isAutomaticAckEnabled(channelId);
	}

	getUnreadCount(channelId: string): number {
		this.versionBox.get();
		const state = this.getIfExists(channelId);
		if (state == null || !state.canBeUnread() || !state.hasUnread()) return 0;
		return state.unreadCount;
	}

	getMentionCount(channelId: string): number {
		this.versionBox.get();
		const state = this.getIfExists(channelId);
		if (state == null || !state.supportsMentions()) return 0;
		return state.mentionCount;
	}

	getManualAckMentionCount(channelId: string, messageId: string): number {
		const state = this.getIfExists(channelId);
		return state?.computeMentionCountAfterAck(messageId) ?? 0;
	}

	hasUnread(channelId: string): boolean {
		this.versionBox.get();
		const state = this.getIfExists(channelId);
		return !!(state?.canBeUnread() && state.hasUnread());
	}

	hasUnreadPrivateChannel(channelId: string): boolean {
		this.privateChannelVersionBox.get();
		const state = this.getIfExists(channelId);
		return !!(state?.canBeUnread() && state.hasUnread());
	}

	getPrivateChannelUnreadCount(channelId: string): number {
		this.privateChannelVersionBox.get();
		const state = this.getIfExists(channelId);
		if (state == null || !state.canBeUnread() || !state.hasUnread()) return 0;
		return state.unreadCount;
	}

	getPrivateChannelMentionCount(channelId: string): number {
		this.privateChannelVersionBox.get();
		const state = this.getIfExists(channelId);
		if (state == null || !state.supportsMentions()) return 0;
		return state.mentionCount;
	}

	isUnreadOrMentioned(channelId: string): boolean {
		this.versionBox.get();
		const state = this.getIfExists(channelId);
		return !!(state?.canBeUnread() && state.isUnreadOrMentioned());
	}

	ackMessageId(channelId: string): string | null {
		const state = this.getIfExists(channelId);
		return state?.canBeUnread() ? state.ackMessageId : null;
	}

	lastMessageId(channelId: string): string | null {
		const state = this.getIfExists(channelId);
		return state?.lastMessageId ?? null;
	}

	getOldestUnreadMessageId(channelId: string): string | null {
		const state = this.getIfExists(channelId);
		return state?.supportsUnreadTracking() ? state.oldestUnreadMessageId : null;
	}

	getVisualUnreadMessageId(channelId: string): string | null {
		const state = this.getIfExists(channelId);
		return state?.supportsUnreadTracking() ? state.visualUnreadMessageId : null;
	}

	getChannelIds(): Array<ChannelId> {
		return Array.from(this.states.keys());
	}

	captureMentionCounts(): Map<string, number> {
		const counts = new Map<string, number>();
		for (const [channelId, state] of this.states) {
			if (state.mentionCount > 0) {
				counts.set(channelId, state.mentionCount);
			}
		}
		return counts;
	}

	clearStickyUnread(channelId: string): void {
		const state = this.getIfExists(channelId);
		if (state != null) {
			state.clearStickyUnread();
			this.notifyChange(channelId);
		}
	}

	private isPreviewChannel(channelId: string): boolean {
		const guildId = Channels.getChannel(channelId)?.guildId;
		return guildId != null && Guilds.getGuild(guildId)?.visiting === true;
	}

	async sendManualAck(channelId: string, messageId: string, mentionCount: number): Promise<void> {
		if (this.isPreviewChannel(channelId)) return;
		const response = await this.postAckEntries([
			{
				channel_id: channelId,
				message_id: messageId,
				mention_count: mentionCount,
				manual: true,
			},
		]);
		if (response == null) {
			this.handleMessageAck({channelId, messageId, mentionCount, manual: true});
			return;
		}
		this.applyAckResponse(response, true);
	}

	hasUnreadPins(channelId: string): boolean {
		const state = this.getIfExists(channelId);
		return !!(state?.canBeUnread() && state.lastPinTimestamp > state.acknowledgedPinTimestamp);
	}

	ackPins(channelId: string): void {
		const state = this.get(channelId);
		if (!this.applyPinAck(state)) {
			return;
		}
		http.post(Endpoints.CHANNEL_PINS_ACK(channelId)).catch((error) => {
			logger.error(`Failed to ack pins for ${channelId}:`, error);
		});
		this.notifyChange(channelId);
	}

	handleGatewayReady(action: {readState: Array<GatewayReadState>; channels: Array<ChannelPayload>}): void {
		this.suppressVersionBumps(() => {
			const preservedAcks = this.ackQueue.capture();
			this.reset();
			const readStates = action.readState;
			const channelsWithReadState = new Set<ChannelId>();
			for (const readState of readStates) {
				channelsWithReadState.add(readState.id as ChannelId);
				const state = this.get(readState.id);
				this.setMentionCount(state, readState.mention_count ?? 0);
				state.ackMessageId = readState.last_message_id ?? null;
				state.acknowledgedPinTimestamp = parseTimestamp(readState.last_pin_timestamp);
				state.serverVersion = readState.version ?? null;
			}
			for (const channel of action.channels) {
				if (!TEXT_BASED_CHANNEL_TYPES.has(channel.type) && !THREAD_FEATURE_CHANNEL_TYPES.has(channel.type)) continue;
				const state = this.get(channel.id);
				state.lastMessageId = channel.last_message_id ?? null;
				state.lastPinTimestamp = parseTimestamp(channel.last_pin_timestamp);
				state.storedGuildId = channel.guild_id ?? null;
				if (!channelsWithReadState.has(channel.id as ChannelId)) {
					this.setMentionCount(state, 0);
				}
				this.clearUnreadStateIfRead(state);
			}
			this.ackQueue.restore(preservedAcks);
			this.notifyChange(undefined, {global: true});
		});
	}

	hydrateFromSnapshot(readStates: ReadonlyMap<string, SnapshotReadStateRow>): void {
		this.suppressVersionBumps(() => {
			const preservedAcks = this.ackQueue.capture();
			this.reset();
			for (const [channelId, row] of readStates) {
				const state = this.get(channelId);
				this.setMentionCount(state, row.mentionCount);
				state.ackMessageId = row.ackMessageId;
				state.acknowledgedPinTimestamp = row.ackPinTimestamp;
				state.serverVersion = row.serverVersion;
				state.lastMessageId = row.lastMessageId;
				state.storedGuildId = row.guildId;
				state.lastPinTimestamp = Channels.getChannel(channelId)?.lastPinTimestamp?.getTime() ?? 0;
				this.clearUnreadStateIfRead(state);
			}
			this.ackQueue.restore(preservedAcks);
			this.notifyChange(undefined, {global: true});
		});
	}

	handleGuildCreate(action: {
		guild: {
			id: string;
			channels?: ReadonlyArray<ChannelPayload>;
			threads?: ReadonlyArray<ChannelPayload>;
		};
	}): void {
		this.suppressVersionBumps(() => {
			if (action.guild.channels) {
				for (const channel of action.guild.channels) {
					if (!GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type) && !THREAD_ONLY_CHANNEL_TYPES.has(channel.type)) {
						continue;
					}
					this.seedGuildChannel(action.guild.id, channel);
				}
			}
			if (action.guild.threads) {
				for (const thread of action.guild.threads) {
					this.seedGuildChannel(action.guild.id, thread);
				}
			}
			this.notifyChange(undefined, {global: true});
		});
	}

	private seedGuildChannel(guildId: string, channel: ChannelPayload): void {
		const state = this.get(channel.id);
		state.lastMessageId = channel.last_message_id ?? null;
		state.lastPinTimestamp = parseTimestamp(channel.last_pin_timestamp);
		state.storedGuildId = guildId;
		this.clearUnreadStateIfRead(state);
		this.refreshMentionChannel(channel.id);
	}

	handleLoadMessages(action: {channelId: string; isAfter?: boolean; messages: Array<WireMessage>}): void {
		const state = this.get(action.channelId);
		state.messagesLoaded = true;
		const messages = Messages.getMessages(action.channelId);
		const newestMessage = messages.last();
		if (newestMessage != null && isNewerMessageId(newestMessage.id, state.lastMessageId)) {
			state.lastMessageId = newestMessage.id;
		}
		const landedOnNewestWindow = messages.hasNewestMessages();
		const landedOnAck = state.ackMessageId != null && messages.jumpDestinationId === state.ackMessageId;
		if (state.hasUnread() || landedOnNewestWindow || landedOnAck) {
			state.rebuild();
			this.clearUnreadStateIfRead(state);
		} else if (action.isAfter && state.ackMessageId != null && messages.has(state.ackMessageId, true)) {
			state.unreadCount += action.messages.length;
			if (state.oldestUnreadMessageId == null) {
				state.rebuild();
			}
		}
		this.notifyChange(action.channelId);
	}

	handleIncomingMessage(action: {channelId: string; message: WireMessage}): void {
		const state = this.get(action.channelId);
		if (action.message.guild_id != null) {
			state.storedGuildId = action.message.guild_id;
		}
		const reopenedPrivateChannel =
			state.isPrivate && !state.messagesLoaded && state.lastMessageId === action.message.id;
		const coveredByLastMessage =
			!reopenedPrivateChannel &&
			state.lastMessageId != null &&
			compareMessageIds(state.lastMessageId, action.message.id) >= 0;
		const currentUser = Users.getCurrentUser();
		const authorBlocked = Relationships.isBlocked(action.message.author.id);
		const hadUnreadOrMentions = state.isUnreadOrMentioned();
		if (isNewerMessageId(action.message.id, state.lastMessageId)) {
			state.lastMessageId = action.message.id;
		}
		const decision = resolveReadStateIncomingMessageDecision({
			isCurrentUserAuthor: currentUser != null && action.message.author.id === currentUser.id,
			automaticAckEnabled: this.isAutomaticAckEnabled(action.channelId),
			isAtBottom: Dimension.channelPinnedToEnd(action.channelId),
			authorBlocked,
			hadUnreadOrMentions,
			messageId: action.message.id,
			ackMessageId: state.ackMessageId,
			coveredByLastMessage,
		});
		switch (decision.type) {
			case 'ackCurrentUserMessage':
				this.ackQueue.cancel(action.channelId);
				state.clearStickyUnread();
				this.applyAck(state, {messageId: action.message.id, local: true});
				this.notifyChange(action.channelId);
				return;
			case 'ackAutomaticMessage': {
				const result = this.applyAck(state, {messageId: action.message.id, preserveStickyUnread: true});
				if (result.acked && result.messageId != null) {
					this.queueAppliedAck(action.channelId, result, {
						immediate: false,
					});
				}
				this.notifyChange(action.channelId);
				return;
			}
			case 'ackBlockedMessage': {
				const result = this.applyAck(state, {messageId: action.message.id, local: true});
				if (result.acked && result.messageId != null) {
					this.queueAppliedAck(action.channelId, result, {
						immediate: false,
					});
				}
				this.notifyChange(action.channelId);
				return;
			}
			case 'ignoreBlockedMessage':
			case 'coveredByAck':
				this.notifyChange(action.channelId);
				return;
			case 'recordUnread':
				if (state.oldestUnreadMessageId == null || state.oldestUnreadNeedsRecompute) {
					state.oldestUnreadMessageId = action.message.id;
				}
				if (!decision.coveredByLastMessage) {
					state.unreadCount++;
				}
				if (currentUser != null && state.shouldMentionFor(action.message, currentUser.id, state.isPrivate)) {
					this.setMentionCount(state, state.mentionCount + 1);
				}
				this.notifyChange(action.channelId);
				return;
		}
	}

	handleMessageDelete(action: {channelId: string}): void {
		this.getIfExists(action.channelId)?.rebuild();
		this.notifyChange(action.channelId);
	}

	handleForumPostDiscarded(channelId: string, lastMessageId: string | null): void {
		const state = this.getIfExists(channelId);
		if (state == null) return;
		state.lastMessageId = lastMessageId;
		this.clearUnreadStateIfRead(state);
		this.notifyChange(channelId);
	}

	handleChannelCreate(action: {channel: ChannelPayload}): void {
		const isThread = THREAD_FEATURE_CHANNEL_TYPES.has(action.channel.type);
		if (!TEXT_BASED_CHANNEL_TYPES.has(action.channel.type) && !isThread) {
			return;
		}
		const state = this.get(action.channel.id);
		if (isThread && !isNewerMessageId(action.channel.last_message_id, state.lastMessageId)) {
			state.storedGuildId = action.channel.guild_id ?? state.storedGuildId;
			this.notifyChange(action.channel.id);
			return;
		}
		state.lastMessageId = action.channel.last_message_id ?? null;
		state.lastPinTimestamp = parseTimestamp(action.channel.last_pin_timestamp);
		state.storedGuildId = action.channel.guild_id ?? null;
		const archivedState = this.archivedStates.get(action.channel.id as ChannelId);
		if (archivedState != null) {
			state.ackMessageId = archivedState.ackMessageId;
			state.acknowledgedPinTimestamp = archivedState.acknowledgedPinTimestamp;
			this.archivedStates.delete(action.channel.id as ChannelId);
		}
		this.notifyChange(action.channel.id);
	}

	handlePassiveLastMessageUpdates(channels: Record<string, string>, guildId?: string): void {
		const changedChannels: Array<ChannelId> = [];
		for (const [channelId, lastMessageId] of Object.entries(channels)) {
			const channel = Channels.getChannel(channelId);
			const state = this.getIfExists(channelId);
			if (
				channel == null ||
				state == null ||
				(guildId != null &&
					(channel.guildId !== guildId ||
						!(TEXT_BASED_CHANNEL_TYPES.has(channel.type) || THREAD_FEATURE_CHANNEL_TYPES.has(channel.type))))
			) {
				continue;
			}
			let changed = false;
			if (guildId != null) {
				changed = state.guildId !== guildId;
				state.storedGuildId = guildId;
			}
			if (isNewerMessageId(lastMessageId, state.lastMessageId)) {
				state.lastMessageId = lastMessageId;
				changed = true;
				this.clearUnreadStateIfRead(state);
			}
			if (changed) {
				changedChannels.push(channelId as ChannelId);
			}
		}
		if (changedChannels.length === 0) return;
		for (const channelId of changedChannels) {
			if (!this.pendingGlobalRecompute) {
				const entry = this.states.get(channelId);
				this.notePrivateChannelChange(entry?.guildId ?? null);
				this.pendingChanges.set(channelId, (entry?.guildId ?? null) as GuildId | null);
			}
		}
		this.bumpVersion();
	}

	handleChannelDelete(action: {
		channel: {
			id: string;
			type?: number;
			guild_id?: string;
		};
	}): void {
		const state = this.getIfExists(action.channel.id);
		if (
			state != null &&
			(action.channel.type === ChannelTypes.DM ||
				action.channel.type === ChannelTypes.GROUP_DM ||
				action.channel.type === ChannelTypes.DM_PERSONAL_NOTES)
		) {
			if (action.channel.type === ChannelTypes.GROUP_DM) {
				this.ackQueue.cancel(action.channel.id);
			}
			state.messagesLoaded = false;
			state.ackedManually = false;
			state.clearStickyUnread();
			state.estimated = false;
			state.unreadCount = 0;
			state.oldestUnreadMessageId = null;
			this.notifyChange(action.channel.id);
			return;
		}
		if (
			action.channel.guild_id != null &&
			(GUILD_TEXT_BASED_CHANNEL_TYPES.has(action.channel.type ?? -1) ||
				THREAD_FEATURE_CHANNEL_TYPES.has(action.channel.type ?? -1))
		) {
			this.archiveState(action.channel.id);
		}
		this.clear(action.channel.id);
	}

	handleChannelAck(action: {channelId: string; messageId?: string; immediate?: boolean; force?: boolean}): void {
		const state = this.get(action.channelId);
		const result = this.applyAck(state, {
			messageId: action.messageId,
			immediate: action.immediate,
			force: action.force,
			isExplicitUserAction: true,
		});
		if (result.acked && result.messageId != null) {
			this.queueAppliedAck(action.channelId, result, {
				immediate: action.immediate || action.force,
			});
			this.notifyChange(action.channelId);
		}
	}

	handleBulkChannelAck(
		entries: Array<{
			channelId: string;
			messageId: string;
		}>,
	): void {
		if (entries.length === 0) return;
		const changedChannels: Array<string> = [];
		for (const entry of entries) {
			const state = this.get(entry.channelId);
			const result = this.applyAck(state, {
				messageId: entry.messageId,
				immediate: true,
				force: true,
				isExplicitUserAction: true,
			});
			if (!result.acked || result.messageId == null) continue;
			this.queueAppliedAck(entry.channelId, result, {immediate: true});
			changedChannels.push(entry.channelId);
		}
		if (changedChannels.length === 0) return;
		for (const channelId of changedChannels) {
			this.refreshMentionChannel(channelId);
			if (!this.pendingGlobalRecompute) {
				const entry = this.states.get(channelId as ChannelId);
				this.notePrivateChannelChange(entry?.guildId ?? null);
				this.pendingChanges.set(channelId as ChannelId, (entry?.guildId ?? null) as GuildId | null);
			}
		}
		this.bumpVersion();
	}

	handleChannelAckWithStickyUnread(action: {channelId: string}): void {
		const state = this.get(action.channelId);
		const lastMessageId = state.lastMessageId;
		if (lastMessageId == null) {
			return;
		}
		const ackedMessageId = state.ackMessageId;
		const hasUnreads = state.unreadCount > 0;
		const ackBehind = ackedMessageId == null || compareMessageIds(ackedMessageId, lastMessageId) < 0;
		if (!hasUnreads && !ackBehind) {
			return;
		}
		const result = this.applyAck(state, {
			messageId: lastMessageId,
			preserveStickyUnread: true,
		});
		if (result.acked && result.messageId != null) {
			this.queueAppliedAck(action.channelId, result, {immediate: false});
			this.notifyChange(action.channelId);
		}
	}

	handleChannelPinsAck(action: {channelId: string; timestamp?: string}): void {
		const state = this.get(action.channelId);
		if (this.applyPinAck(state, action.timestamp)) {
			this.notifyChange(action.channelId);
		}
	}

	handleChannelPinsUpdate(action: {channelId: string; lastPinTimestamp: string}): void {
		const state = this.get(action.channelId);
		const newTimestamp = parseTimestamp(action.lastPinTimestamp);
		if (state.lastPinTimestamp !== newTimestamp) {
			state.lastPinTimestamp = newTimestamp;
			this.notifyChange(action.channelId);
		}
	}

	handleMessageAck(action: {
		channelId: string;
		messageId: string;
		mentionCount?: number;
		manual: boolean;
		version?: string;
	}): void {
		const state = this.get(action.channelId);
		const readStateWasKnown = state.ackMessageId != null;
		const mentionCount = action.mentionCount;
		const decision = resolveReadStateServerAckDecision({
			messageId: action.messageId,
			ackMessageId: state.ackMessageId,
			version: action.version,
			serverVersion: state.serverVersion,
			manual: action.manual,
			readStateWasKnown,
			hasMentionCount: mentionCount != null,
		});
		switch (decision.type) {
			case 'ignoreStaleVersion':
				return;
			case 'applyManualAck':
				state.clearStickyUnread();
				state.ackedManually = true;
				state.rebuild(action.messageId, {recomputeMentions: true});
				state.serverVersion = action.version ?? state.serverVersion;
				this.ackQueue.cancel(action.channelId);
				AutoAck.disableForChannel(action.channelId);
				if (mentionCount != null) {
					this.setMentionCount(state, mentionCount);
				}
				this.notifyChange(action.channelId);
				return;
			case 'ignoreOlderMessage':
				state.serverVersion = action.version ?? state.serverVersion;
				return;
			case 'refreshCurrentAck':
				state.serverVersion = action.version ?? state.serverVersion;
				if (decision.shouldUpdateMentionCount && mentionCount != null) {
					this.setMentionCount(state, mentionCount);
				}
				if (decision.shouldRefreshUnreadEstimate) {
					this.clearUnreadStateIfRead(state);
				}
				if (decision.shouldNotify) {
					this.notifyChange(action.channelId);
				}
				this.ackQueue.cancelIfCovered(action.channelId, action.messageId);
				return;
			case 'advanceAck': {
				const result = this.applyAck(state, {messageId: action.messageId, local: true});
				if (!result.acked) {
					return;
				}
				if (decision.shouldUpdateMentionCount && mentionCount != null) {
					this.setMentionCount(state, mentionCount);
				}
				state.serverVersion = action.version ?? state.serverVersion;
				this.ackQueue.cancelIfCovered(action.channelId, action.messageId);
				this.notifyChange(action.channelId);
				return;
			}
		}
	}

	handleClearManualAck(action: {channelId: string}): void {
		const state = this.get(action.channelId);
		if (state.ackedManually) {
			state.ackedManually = false;
			this.notifyChange(action.channelId);
		}
	}

	handleRelationshipUpdate(): void {
		for (const state of this.states.values()) {
			if (state.messagesLoaded && state.isUnreadOrMentioned()) {
				state.rebuild(undefined, {recomputeMentions: true});
			}
		}
		this.notifyChange(undefined, {global: true});
	}

	subscribe(callback: () => void): () => void {
		this.listeners.add(callback);
		callback();
		return () => {
			this.listeners.delete(callback);
		};
	}

	async flushPendingAcks(): Promise<void> {
		await this.ackQueue.flush('all');
	}

	private applyAck(state: ReadStateEntry, options: AckOptions): AppliedAck {
		const {
			messageId,
			local = false,
			force = false,
			isExplicitUserAction = false,
			preserveStickyUnread = false,
		} = options;
		const decision = resolveReadStateAckDecision({
			requestedMessageId: messageId,
			lastMessageId: state.lastMessageId,
			ackMessageId: state.ackMessageId,
			ackedManually: state.ackedManually,
			messagesLoaded: state.messagesLoaded,
			supportsUnreadTracking: state.supportsUnreadTracking(),
			hasMentions: state.hasMentions(),
			hasOldestUnreadMessage: state.oldestUnreadMessageId != null,
			hasStickyUnreadMessage: state.stickyUnreadMessageId != null,
			local,
			force,
			isExplicitUserAction,
			preserveStickyUnread,
		});
		if (decision.type === 'ignored') {
			return {acked: false, messageId: null, hadMentions: false};
		}
		if (decision.shouldPreserveStickyUnread) {
			state.stickyUnreadMessageId = state.oldestUnreadMessageId;
		}
		state.estimated = false;
		state.unreadCount = 0;
		this.setMentionCount(state, 0);
		state.ackMessageId = decision.messageId;
		state.oldestUnreadMessageId = null;
		if (decision.shouldClearManualAck) {
			state.ackedManually = false;
			state.clearStickyUnread();
		}
		return {acked: true, messageId: decision.messageId, hadMentions: decision.hadMentions};
	}

	private applyPinAck(state: ReadStateEntry, timestamp?: string | null): boolean {
		const newTimestamp = timestamp == null ? state.lastPinTimestamp : parseTimestamp(timestamp);
		const ackTimestamp = newTimestamp !== 0 ? newTimestamp : state.lastPinTimestamp;
		if (state.acknowledgedPinTimestamp === ackTimestamp) {
			return false;
		}
		state.acknowledgedPinTimestamp = ackTimestamp;
		return true;
	}

	private queueAppliedAck(
		channelId: string,
		result: AppliedAck,
		options: {
			immediate?: boolean;
		},
	): void {
		if (!result.acked || result.messageId == null) return;
		this.ackQueue.enqueue(channelId, result.messageId, {immediate: options.immediate, hadMentions: result.hadMentions});
	}

	private async postAckEntries(entries: Array<ReadStateAckRequestEntry>): Promise<ReadStateAckResponse | null> {
		try {
			const response = await http.post<ReadStateAckResponse>(Endpoints.READ_STATES_ACK, {
				body: {read_states: entries},
			});
			return response.body;
		} catch (error) {
			if (!isMissingReadStateAckEndpoint(error)) {
				throw error;
			}
			await this.postLegacyAckEntries(entries);
			return null;
		}
	}

	private async postLegacyAckEntries(entries: Array<ReadStateAckRequestEntry>): Promise<void> {
		const automaticEntries = entries.filter((entry) => !entry.manual);
		const manualEntries = entries.filter((entry) => entry.manual);
		if (automaticEntries.length > 0) {
			await http.post(Endpoints.READ_STATES_ACK_BULK, {
				body: {
					read_states: automaticEntries.map((entry) => ({
						channel_id: entry.channel_id,
						message_id: entry.message_id,
					})),
				},
			});
		}
		await Promise.all(
			manualEntries.map((entry) =>
				http.post(Endpoints.CHANNEL_MESSAGE_ACK(entry.channel_id, entry.message_id), {
					body: {
						manual: true,
						mention_count: entry.mention_count ?? 0,
					},
				}),
			),
		);
	}

	private applyAckResponse(response: ReadStateAckResponse | null, manual: boolean): void {
		if (response == null) {
			return;
		}
		for (const readState of response.read_states) {
			if (readState.last_message_id == null) {
				continue;
			}
			this.handleMessageAck({
				channelId: readState.id,
				messageId: readState.last_message_id,
				mentionCount: readState.mention_count ?? 0,
				manual,
				version: readState.version,
			});
		}
	}
}

export default new ReadStates();

function isMissingReadStateAckEndpoint(error: unknown): boolean {
	return error instanceof HttpError && (error.status === 404 || error.status === 405);
}
