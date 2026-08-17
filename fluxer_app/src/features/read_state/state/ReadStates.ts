// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import Channels from '@app/features/channel/state/Channels';
import Messages from '@app/features/messaging/state/MessagingMessages';
import AutoAck from '@app/features/notification/state/NotificationAutoAck';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {resolveReadStateAckDecision} from '@app/features/read_state/state/read_states/ReadStateAckMachine';
import {ReadStateEntry} from '@app/features/read_state/state/read_states/ReadStateEntry';
import {resolveReadStateIncomingMessageDecision} from '@app/features/read_state/state/read_states/ReadStateIncomingMessageMachine';
import {resolveReadStateServerAckDecision} from '@app/features/read_state/state/read_states/ReadStateServerAckMachine';
import {
	resolveUnreadJumpAnchor,
	type UnreadJumpAnchor,
} from '@app/features/read_state/state/read_states/ReadStateUnreadAnchor';
import {
	ACK_BATCH_DELAY_MS,
	ACK_BATCH_SIZE,
	ACK_RETRY_BASE_DELAY_MS,
	ACK_RETRY_MAX_DELAY_MS,
	type AckOptions,
	type AppliedAck,
	type ArchivedReadState,
	type ChannelPayload,
	chunkEntries,
	compareMessageIds,
	compareReadStateVersions,
	type GatewayReadState,
	isNewerMessageId,
	type PendingAck,
	parseTimestamp,
	type ReadStateAckRequestEntry,
	type ReadStateAckResponse,
	type Timer,
} from '@app/features/read_state/state/read_states/shared';
import Relationships from '@app/features/relationship/state/Relationships';
import Dimension from '@app/features/ui/state/Dimension';
import Users from '@app/features/user/state/Users';
import {
	ChannelTypes,
	GUILD_TEXT_BASED_CHANNEL_TYPES,
	TEXT_BASED_CHANNEL_TYPES,
} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelId, GuildId} from '@fluxer/schema/src/branded/WireIds';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {decodeReadStateProto} from '@fluxer/schema/src/domains/read_state/ReadStateProtoCodec';
import {observable, runInAction} from 'mobx';

export type {GatewayReadState};

const logger = new Logger('ReadStates');

export type {UnreadJumpAnchor};

class ReadStates {
	private readonly states = new Map<ChannelId, ReadStateEntry>();
	private readonly archivedStates = new Map<ChannelId, ArchivedReadState>();
	private readonly mentionChannels = new Set<ChannelId>();
	private readonly listeners = new Set<() => void>();
	private readonly versionBox = observable.box(0);
	private readonly privateChannelVersionBox = observable.box(0);
	private hasPendingPrivateChannelChange = false;
	private readonly pendingAcks = new Map<ChannelId, PendingAck>();
	updateCounter = 0;
	private pendingChanges = new Map<ChannelId, GuildId | null>();
	private pendingGlobalRecompute = false;
	private ackFlushTimer: Timer | null = null;
	private isFlushingAcks = false;
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
		if (state != null && state.mentionCount > 0 && state.canHaveMentions()) {
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
		state.mentionCount = mentionCount;
		this.refreshMentionChannel(state.channelId);
	}

	private refreshUnreadEstimate(state: ReadStateEntry): void {
		if (!state.hasUnread()) {
			state.estimated = false;
			state.unreadCount = 0;
			state.oldestUnreadMessageId = null;
			return;
		}
		if (state.unreadCount === 0) {
			state.estimated = true;
			state.unreadCount = Math.max(1, state.mentionCount);
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
		this.cancelPendingAck(channelId);
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
			ackPinTimestamp: entry.ackPinTimestamp,
			readStateKnown: entry.readStateKnown,
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
		this.pendingAcks.clear();
		this.clearAckFlushTimer();
	}

	get mentionChannelIds(): Array<ChannelId> {
		const ids: Array<ChannelId> = [];
		for (const channelId of Array.from(this.mentionChannels)) {
			const state = this.getIfExists(channelId);
			if (state?.canHaveMentions()) {
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
		if (state == null || !state.canHaveMentions()) return 0;
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
		if (state == null || !state.canHaveMentions()) return 0;
		return state.mentionCount;
	}

	hasUnreadOrMentions(channelId: string): boolean {
		this.versionBox.get();
		const state = this.getIfExists(channelId);
		return !!(state?.canBeUnread() && state.hasUnreadOrMentions());
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
		return state?.canTrackUnreads() ? state.oldestUnreadMessageId : null;
	}

	getVisualUnreadMessageId(channelId: string): string | null {
		const state = this.getIfExists(channelId);
		return state?.canTrackUnreads() ? state.visualUnreadMessageId : null;
	}

	getUnreadJumpAnchor(channelId: string): UnreadJumpAnchor | null {
		const state = this.getIfExists(channelId);
		if (state == null) {
			return null;
		}
		return resolveUnreadJumpAnchor({
			canBeUnread: state.canBeUnread(),
			canTrackUnreads: state.canTrackUnreads(),
			hasUnread: state.hasUnread(),
			oldestUnreadMessageId: state.oldestUnreadMessageId,
			ackMessageId: state.ackMessageId,
		});
	}

	getChannelIds(): Array<ChannelId> {
		return Array.from(this.states.keys());
	}

	clearStickyUnread(channelId: string): void {
		const state = this.getIfExists(channelId);
		if (state != null) {
			state.clearStickyUnread();
			this.notifyChange(channelId);
		}
	}

	async sendManualAck(channelId: string, messageId: string, mentionCount: number): Promise<void> {
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
		return !!(state?.canBeUnread() && state.lastPinTimestamp > state.ackPinTimestamp);
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

	handleConnectionOpen(action: {
		readState: Array<GatewayReadState>;
		readStateProto?: string;
		channels: Array<ChannelPayload>;
	}): void {
		this.suppressVersionBumps(() => {
			this.reset();
			const readStates = this.decodeReadStateBundle(action.readStateProto, action.readState);
			const channelsWithReadState = new Set<ChannelId>();
			for (const readState of readStates) {
				channelsWithReadState.add(readState.id as ChannelId);
				const state = this.get(readState.id);
				state.readStateKnown = true;
				this.setMentionCount(state, readState.mention_count ?? 0);
				state.ackMessageId = readState.last_message_id ?? null;
				state.ackPinTimestamp = parseTimestamp(readState.last_pin_timestamp);
				state.serverVersion = readState.version ?? null;
			}
			for (const channel of action.channels) {
				if (!TEXT_BASED_CHANNEL_TYPES.has(channel.type)) continue;
				const state = this.get(channel.id);
				state.lastMessageId = channel.last_message_id ?? null;
				state.lastPinTimestamp = parseTimestamp(channel.last_pin_timestamp);
				state._guildId = channel.guild_id ?? null;
				if (!channelsWithReadState.has(channel.id as ChannelId)) {
					this.setMentionCount(state, 0);
				}
				this.refreshUnreadEstimate(state);
			}
			this.notifyChange(undefined, {global: true});
		});
	}

	handleGuildCreate(action: {
		guild: {
			id: string;
			channels?: ReadonlyArray<ChannelPayload>;
		};
	}): void {
		this.suppressVersionBumps(() => {
			if (action.guild.channels) {
				for (const channel of action.guild.channels) {
					if (!GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)) continue;
					const state = this.get(channel.id);
					state.lastMessageId = channel.last_message_id ?? null;
					state.lastPinTimestamp = parseTimestamp(channel.last_pin_timestamp);
					state._guildId = action.guild.id;
					this.refreshUnreadEstimate(state);
					this.refreshMentionChannel(channel.id);
				}
			}
			this.notifyChange(undefined, {global: true});
		});
	}

	handleLoadMessages(action: {channelId: string; isAfter?: boolean; messages: Array<WireMessage>}): void {
		const state = this.get(action.channelId);
		state.loadedMessages = true;
		const messages = Messages.getMessages(action.channelId);
		const newestMessage = messages.last();
		if (newestMessage != null && isNewerMessageId(newestMessage.id, state.lastMessageId)) {
			state.lastMessageId = newestMessage.id;
		}
		if (state.hasUnread()) {
			state.rebuild();
			this.refreshUnreadEstimate(state);
		} else if (action.isAfter && state.ackMessageId != null && messages.has(state.ackMessageId, true)) {
			state.unreadCount += action.messages.length;
		}
		this.notifyChange(action.channelId);
	}

	handleIncomingMessage(action: {channelId: string; message: WireMessage}): void {
		const state = this.get(action.channelId);
		if (action.message.guild_id != null) {
			state._guildId = action.message.guild_id;
		}
		const previousLastMessageId = state.lastMessageId;
		const currentUser = Users.getCurrentUser();
		const authorBlocked = Relationships.isBlocked(action.message.author.id);
		const hadUnreadOrMentions = state.hasUnreadOrMentions();
		if (isNewerMessageId(action.message.id, state.lastMessageId)) {
			state.lastMessageId = action.message.id;
		}
		const decision = resolveReadStateIncomingMessageDecision({
			isCurrentUserAuthor: currentUser != null && action.message.author.id === currentUser.id,
			automaticAckEnabled: this.isAutomaticAckEnabled(action.channelId),
			isAtBottom: Dimension.isAtBottom(action.channelId),
			authorBlocked,
			hadUnreadOrMentions,
			readStateKnown: state.readStateKnown,
			messageId: action.message.id,
			ackMessageId: state.ackMessageId,
			previousLastMessageId,
		});
		switch (decision.type) {
			case 'ackCurrentUserMessage':
				this.cancelPendingAck(action.channelId);
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
				if (decision.initializeUnknownReadState) {
					state.ackMessageId = previousLastMessageId;
					state.readStateKnown = true;
				}
				if (state.oldestUnreadMessageId == null || state.oldestUnreadMessageIdStale) {
					state.oldestUnreadMessageId = action.message.id;
				}
				state.unreadCount++;
				if (currentUser != null && state.shouldMentionFor(action.message, currentUser.id, state.isPrivate)) {
					this.setMentionCount(state, state.mentionCount + 1);
				}
				this.notifyChange(action.channelId);
				return;
		}
	}

	handleMessageDelete(action: {channelId: string}): void {
		this.notifyChange(action.channelId);
	}

	handleChannelCreate(action: {channel: ChannelPayload}): void {
		if (!TEXT_BASED_CHANNEL_TYPES.has(action.channel.type)) {
			return;
		}
		const state = this.get(action.channel.id);
		state.lastMessageId = action.channel.last_message_id ?? null;
		state.lastPinTimestamp = parseTimestamp(action.channel.last_pin_timestamp);
		state._guildId = action.channel.guild_id ?? null;
		const archivedState = this.archivedStates.get(action.channel.id as ChannelId);
		if (archivedState != null) {
			state.ackMessageId = archivedState.ackMessageId;
			state.ackPinTimestamp = archivedState.ackPinTimestamp;
			state.readStateKnown = archivedState.readStateKnown;
			this.archivedStates.delete(action.channel.id as ChannelId);
		}
		if (
			(action.channel.type === ChannelTypes.DM ||
				action.channel.type === ChannelTypes.GROUP_DM ||
				action.channel.type === ChannelTypes.DM_PERSONAL_NOTES) &&
			action.channel.last_message_id != null
		) {
			state.readStateKnown = true;
			state.ackMessageId = action.channel.last_message_id;
		} else if (GUILD_TEXT_BASED_CHANNEL_TYPES.has(action.channel.type) && state.hasUnread()) {
			this.refreshUnreadEstimate(state);
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
				(guildId != null && (channel.guildId !== guildId || !TEXT_BASED_CHANNEL_TYPES.has(channel.type)))
			) {
				continue;
			}
			let changed = false;
			if (guildId != null) {
				changed = state.guildId !== guildId;
				state._guildId = guildId;
			}
			if (isNewerMessageId(lastMessageId, state.lastMessageId)) {
				state.lastMessageId = lastMessageId;
				changed = true;
				this.refreshUnreadEstimate(state);
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
		if (action.channel.guild_id != null && GUILD_TEXT_BASED_CHANNEL_TYPES.has(action.channel.type ?? -1)) {
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
		const readStateWasKnown = state.readStateKnown;
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
				state.readStateKnown = true;
				state.clearStickyUnread();
				state.isManualAck = true;
				state.rebuild(action.messageId, {recomputeMentions: true});
				state.serverVersion = action.version ?? state.serverVersion;
				this.cancelPendingAck(action.channelId);
				AutoAck.disableForChannel(action.channelId);
				if (mentionCount != null) {
					this.setMentionCount(state, mentionCount);
				}
				this.notifyChange(action.channelId);
				return;
			case 'ignoreOlderMessage':
				state.readStateKnown = true;
				state.serverVersion = action.version ?? state.serverVersion;
				return;
			case 'refreshCurrentAck':
				state.readStateKnown = true;
				state.serverVersion = action.version ?? state.serverVersion;
				if (decision.shouldUpdateMentionCount && mentionCount != null) {
					this.setMentionCount(state, mentionCount);
				}
				if (decision.shouldRefreshUnreadEstimate) {
					this.refreshUnreadEstimate(state);
				}
				if (decision.shouldNotify) {
					this.notifyChange(action.channelId);
				}
				this.cancelPendingAckIfCovered(action.channelId, action.messageId);
				return;
			case 'advanceAck': {
				state.readStateKnown = true;
				const result = this.applyAck(state, {messageId: action.messageId, local: true});
				if (!result.acked) {
					return;
				}
				if (decision.shouldUpdateMentionCount && mentionCount != null) {
					this.setMentionCount(state, mentionCount);
				}
				state.serverVersion = action.version ?? state.serverVersion;
				this.cancelPendingAckIfCovered(action.channelId, action.messageId);
				this.notifyChange(action.channelId);
				return;
			}
		}
	}

	handleClearManualAck(action: {channelId: string}): void {
		const state = this.get(action.channelId);
		if (state.isManualAck) {
			state.isManualAck = false;
			this.notifyChange(action.channelId);
		}
	}

	handleRelationshipUpdate(): void {
		for (const state of this.states.values()) {
			if (state.hasUnreadOrMentions()) {
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
		await this.flushDueAcks(true);
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
			isManualAck: state.isManualAck,
			loadedMessages: state.loadedMessages,
			canTrackUnreads: state.canTrackUnreads(),
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
		state.readStateKnown = true;
		state.ackMessageId = decision.messageId;
		state.oldestUnreadMessageId = null;
		if (decision.shouldClearManualAck) {
			state.isManualAck = false;
			state.clearStickyUnread();
		}
		this.cancelPendingAckIfCovered(state.channelId, decision.messageId);
		return {acked: true, messageId: decision.messageId, hadMentions: decision.hadMentions};
	}

	private applyPinAck(state: ReadStateEntry, timestamp?: string | null): boolean {
		const newTimestamp = timestamp == null ? state.lastPinTimestamp : parseTimestamp(timestamp);
		const ackTimestamp = newTimestamp !== 0 ? newTimestamp : state.lastPinTimestamp;
		if (state.ackPinTimestamp === ackTimestamp) {
			return false;
		}
		state.ackPinTimestamp = ackTimestamp;
		return true;
	}

	private queueAck(
		channelId: string,
		messageId: string,
		options: {
			immediate?: boolean;
			hadMentions?: boolean;
		},
	): void {
		const now = Date.now();
		const delay = options.immediate || options.hadMentions ? 0 : ACK_BATCH_DELAY_MS;
		const deadline = now + delay;
		const existing = this.pendingAcks.get(channelId as ChannelId);
		const existingIsNewer = existing != null && compareMessageIds(existing.messageId, messageId) > 0;
		const pending: PendingAck = {
			channelId,
			messageId: existingIsNewer ? existing.messageId : messageId,
			deadline: existing == null ? deadline : Math.min(existing.deadline, deadline),
			attempt: existing?.attempt ?? 0,
		};
		this.pendingAcks.set(channelId as ChannelId, pending);
		const state = this.getIfExists(channelId);
		if (state != null) {
			state.outgoingAck = pending.messageId;
		}
		this.scheduleAckFlush();
	}

	private queueAppliedAck(
		channelId: string,
		result: AppliedAck,
		options: {
			immediate?: boolean;
		},
	): void {
		if (!result.acked || result.messageId == null) return;
		this.queueAck(channelId, result.messageId, {immediate: options.immediate, hadMentions: result.hadMentions});
	}

	private clearAckFlushTimer(): void {
		if (this.ackFlushTimer != null) {
			clearTimeout(this.ackFlushTimer);
			this.ackFlushTimer = null;
		}
	}

	private scheduleAckFlush(): void {
		this.clearAckFlushTimer();
		if (this.pendingAcks.size === 0 || this.isFlushingAcks) {
			return;
		}
		let nextDeadline = Number.POSITIVE_INFINITY;
		for (const pending of this.pendingAcks.values()) {
			nextDeadline = Math.min(nextDeadline, pending.deadline);
		}
		const delay = Math.max(0, nextDeadline - Date.now());
		this.ackFlushTimer = setTimeout(() => {
			this.ackFlushTimer = null;
			void this.flushDueAcks(false);
		}, delay);
	}

	private cancelPendingAck(channelId: string): void {
		const deleted = this.pendingAcks.delete(channelId as ChannelId);
		const state = this.getIfExists(channelId);
		if (state != null) {
			state.outgoingAck = null;
		}
		if (deleted) {
			this.scheduleAckFlush();
		}
	}

	private cancelPendingAckIfCovered(channelId: string, ackedMessageId: string): void {
		const pending = this.pendingAcks.get(channelId as ChannelId);
		if (pending == null) return;
		if (compareMessageIds(ackedMessageId, pending.messageId) < 0) return;
		this.cancelPendingAck(channelId);
	}

	private async flushDueAcks(force: boolean): Promise<void> {
		if (this.isFlushingAcks) {
			return;
		}
		this.clearAckFlushTimer();
		const now = Date.now();
		const due = Array.from(this.pendingAcks.values()).filter((pending) => force || pending.deadline <= now);
		if (due.length === 0) {
			this.scheduleAckFlush();
			return;
		}
		this.isFlushingAcks = true;
		try {
			for (const chunk of chunkEntries(due, ACK_BATCH_SIZE)) {
				await this.sendAckChunk(chunk);
			}
		} finally {
			this.isFlushingAcks = false;
			this.scheduleAckFlush();
		}
	}

	private async sendAckChunk(entries: Array<PendingAck>): Promise<void> {
		try {
			const response = await this.postAckEntries(
				entries.map((entry) => ({
					channel_id: entry.channelId,
					message_id: entry.messageId,
				})),
			);
			this.applyAckResponse(response, false);
			for (const entry of entries) {
				const current = this.pendingAcks.get(entry.channelId as ChannelId);
				if (current?.messageId === entry.messageId) {
					this.pendingAcks.delete(entry.channelId as ChannelId);
					const state = this.getIfExists(entry.channelId);
					if (state?.outgoingAck === entry.messageId) {
						state.outgoingAck = null;
					}
				}
			}
		} catch (error) {
			logger.error('Failed to flush read-state acknowledgements:', error);
			const now = Date.now();
			for (const entry of entries) {
				const current = this.pendingAcks.get(entry.channelId as ChannelId);
				if (current == null || current.messageId !== entry.messageId) continue;
				const attempt = current.attempt + 1;
				const retryDelay = Math.min(ACK_RETRY_MAX_DELAY_MS, ACK_RETRY_BASE_DELAY_MS * 2 ** (attempt - 1));
				current.attempt = attempt;
				current.deadline = now + retryDelay;
			}
		}
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
		for (const readState of this.decodeReadStateBundle(response.read_state_proto, response.read_states)) {
			if (readState.last_message_id == null) {
				const state = this.get(readState.id);
				if (readState.version != null && compareReadStateVersions(readState.version, state.serverVersion) < 0) {
					continue;
				}
				state.readStateKnown = true;
				state.ackMessageId = null;
				state.serverVersion = readState.version ?? state.serverVersion;
				this.setMentionCount(state, readState.mention_count ?? 0);
				state.rebuild(null, {recomputeMentions: manual});
				this.refreshUnreadEstimate(state);
				this.notifyChange(readState.id);
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

	private decodeReadStateBundle(
		readStateProto: string | null | undefined,
		fallbackReadStates: Array<GatewayReadState>,
	): Array<GatewayReadState> {
		if (readStateProto == null) {
			return fallbackReadStates;
		}
		try {
			return decodeReadStateProto(readStateProto);
		} catch (error) {
			logger.warn('Failed to decode read_state_proto, falling back to read_states:', error);
			return fallbackReadStates;
		}
	}
}

export default new ReadStates();

function isMissingReadStateAckEndpoint(error: unknown): boolean {
	return error instanceof HttpError && (error.status === 404 || error.status === 405);
}
