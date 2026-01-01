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
import {ChannelTypes, Permissions} from '~/Constants';
import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {Message, MessageRecord} from '~/records/MessageRecord';
import {compare as compareSnowflakes, extractTimestamp, fromTimestamp} from '~/utils/SnowflakeUtils';
import AutoAckStore from './AutoAckStore';
import ChannelStore from './ChannelStore';
import DimensionStore from './DimensionStore';
import GuildAvailabilityStore from './GuildAvailabilityStore';
import GuildStore from './GuildStore';
import MessageStore from './MessageStore';
import PermissionStore from './PermissionStore';
import RelationshipStore from './RelationshipStore';
import UserGuildSettingsStore from './UserGuildSettingsStore';
import UserStore from './UserStore';

const logger = new Logger('ReadStateStore');

type ChannelId = string;
type MessageId = string;
type GuildId = string;

const CAN_READ_PERMISSIONS = Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY;
const OLD_MESSAGE_AGE_THRESHOLD = 7 * 24 * 60 * 60 * 1000;
const RECENT_MESSAGE_THRESHOLD = 3 * 24 * 60 * 60 * 1000;

export interface GatewayReadState {
	id: ChannelId;
	mention_count?: number;
	last_message_id?: string | null;
	last_pin_timestamp?: string | null;
}

interface ChannelPayload {
	id: ChannelId;
	type: number;
	guild_id?: GuildId;
	last_message_id?: string | null;
	last_pin_timestamp?: string | null;
}

function parseTimestamp(timestamp?: string | null): number {
	if (timestamp == null) return 0;
	const parsed = Date.parse(timestamp);
	return Number.isNaN(parsed) ? 0 : parsed;
}

class ReadStateEntry {
	readonly channelId: ChannelId;

	_guildId: GuildId | null = null;
	loadedMessages = false;

	private _lastMessageId: MessageId | null = null;
	private _lastMessageTimestamp = 0;
	private _ackMessageId: MessageId | null = null;
	private _ackMessageTimestamp = 0;

	ackPinTimestamp = 0;
	lastPinTimestamp = 0;

	isManualAck = false;

	private _oldestUnreadMessageId: MessageId | null = null;
	oldestUnreadMessageIdStale = false;

	private _stickyUnreadMessageId: MessageId | null = null;
	estimated = false;
	private _unreadCount = 0;
	private _mentionCount = 0;

	outgoingAck: MessageId | null = null;
	private outgoingAckTimer: NodeJS.Timeout | null = null;

	snapshot?: {
		unread: boolean;
		mentionCount: number;
		guildUnread: boolean | null;
		guildMentionCount: number | null;
		takenAt: number;
	};

	constructor(channelId: ChannelId) {
		this.channelId = channelId;
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private get now(): number {
		return Date.now();
	}

	get guildId(): GuildId | null {
		const channel = ChannelStore.getChannel(this.channelId);
		return channel?.guildId ?? this._guildId ?? null;
	}

	get lastMessageId(): MessageId | null {
		return this._lastMessageId;
	}

	set lastMessageId(messageId: MessageId | null) {
		this._lastMessageId = messageId;
		this._lastMessageTimestamp = messageId != null ? extractTimestamp(messageId) : 0;
	}

	get lastMessageTimestamp(): number {
		return this._lastMessageTimestamp;
	}

	get ackMessageId(): MessageId | null {
		return this._ackMessageId;
	}

	set ackMessageId(messageId: MessageId | null) {
		this._ackMessageId = messageId;
		this._ackMessageTimestamp = messageId != null ? extractTimestamp(messageId) : 0;
	}

	get oldestUnreadMessageId(): MessageId | null {
		return this._oldestUnreadMessageId;
	}

	set oldestUnreadMessageId(messageId: MessageId | null) {
		this._oldestUnreadMessageId = messageId;
		this.oldestUnreadMessageIdStale = false;
	}

	get stickyUnreadMessageId(): MessageId | null {
		return this._stickyUnreadMessageId;
	}

	set stickyUnreadMessageId(messageId: MessageId | null) {
		this._stickyUnreadMessageId = messageId;
	}

	get visualUnreadMessageId(): MessageId | null {
		return this._stickyUnreadMessageId ?? this._oldestUnreadMessageId;
	}

	clearStickyUnread(): void {
		this._stickyUnreadMessageId = null;
	}

	get unreadCount(): number {
		return this._unreadCount;
	}

	set unreadCount(count: number) {
		this._unreadCount = count;
	}

	get mentionCount(): number {
		return this._mentionCount;
	}

	set mentionCount(count: number) {
		this._mentionCount = count;
	}

	get oldestUnreadTimestamp(): number {
		return this.oldestUnreadMessageId != null ? extractTimestamp(this.oldestUnreadMessageId) : 0;
	}

	get ackTimestamp(): number {
		if (this._ackMessageTimestamp !== 0 && !Number.isNaN(this._ackMessageTimestamp)) {
			return this._ackMessageTimestamp;
		}

		this.computeAckTimestamp();
		return this._ackMessageTimestamp;
	}

	@action
	private computeAckTimestamp(): void {
		const guildId = this.guildId;

		if (guildId != null) {
			const guild = GuildStore.getGuild(guildId);
			if (guild != null && guild.joinedAt != null) {
				const joinTimestamp = Date.parse(guild.joinedAt);
				if (!Number.isNaN(joinTimestamp)) {
					this._ackMessageTimestamp = joinTimestamp;
					this._ackMessageId = fromTimestamp(joinTimestamp);
					return;
				}
			}
		}

		const channelTimestamp = extractTimestamp(this.channelId);
		if (channelTimestamp == null || Number.isNaN(channelTimestamp)) {
			this._ackMessageTimestamp = this.now;
			return;
		}

		this._ackMessageTimestamp = channelTimestamp;
		this._ackMessageId = fromTimestamp(channelTimestamp);
	}

	get isPrivate(): boolean {
		const channel = ChannelStore.getChannel(this.channelId);
		return channel?.isPrivate() ?? false;
	}

	canTrackUnreads(): boolean {
		const channel = ChannelStore.getChannel(this.channelId);
		if (channel == null) {
			return false;
		}

		const canTrack = channel.isPrivate() || PermissionStore.can(CAN_READ_PERMISSIONS, channel);
		return canTrack;
	}

	canBeUnread(): boolean {
		if (!this.canTrackUnreads()) {
			return false;
		}

		const guildId = this.guildId;
		if (guildId == null) return true;

		const now = this.now;
		if (this._lastMessageTimestamp < now - OLD_MESSAGE_AGE_THRESHOLD) {
			if (!this.hasRecentlyVisitedAndRead(now) && !this.hasMentions()) {
				return false;
			}
		}

		return true;
	}

	canHaveMentions(): boolean {
		if (this.mentionCount === 0) {
			return false;
		}
		if (!this.canTrackUnreads()) {
			return false;
		}

		const guildId = this.guildId;
		if (guildId == null) {
			return true;
		}

		const now = this.now;
		if (this._lastMessageTimestamp < now - OLD_MESSAGE_AGE_THRESHOLD) {
			return false;
		}

		return true;
	}

	hasUnread(): boolean {
		const ackTimestamp = this.ackTimestamp;
		const lastMessageTimestamp = this._lastMessageTimestamp;

		if (
			Number.isNaN(ackTimestamp) ||
			Number.isNaN(lastMessageTimestamp) ||
			ackTimestamp === 0 ||
			lastMessageTimestamp === 0
		) {
			return false;
		}

		return ackTimestamp < lastMessageTimestamp;
	}

	hasMentions(): boolean {
		return this.mentionCount > 0;
	}

	hasUnreadOrMentions(): boolean {
		return this.hasUnread() || this.hasMentions();
	}

	hasRecentlyVisitedAndRead(now = this.now): boolean {
		if (this._lastMessageTimestamp === 0) return false;
		if (this._ackMessageId == null) return false;

		const ackTime = this.ackTimestamp;
		return ackTime > now - RECENT_MESSAGE_THRESHOLD;
	}

	getGuildChannelUnreadState(
		channel: {isPrivate(): boolean; guildId?: string},
		isOptInEnabled: boolean,
		isChannelMuted: boolean,
		isGuildMuted: boolean,
	): {mentionCount: number; unread: boolean} {
		const now = this.now;

		if (isOptInEnabled && this._lastMessageTimestamp < now - OLD_MESSAGE_AGE_THRESHOLD) {
			if (!this.hasRecentlyVisitedAndRead(now) && this.mentionCount <= 0) {
				return {mentionCount: 0, unread: false};
			}
		}

		if (!channel.isPrivate() && !this.canTrackUnreads()) {
			return {mentionCount: 0, unread: false};
		}

		if (isChannelMuted || isGuildMuted) {
			return {mentionCount: this.mentionCount, unread: false};
		}

		const ackTimestamp = this.ackTimestamp;
		const lastMessageTimestamp = this._lastMessageTimestamp;

		const unread =
			ackTimestamp !== 0 &&
			!Number.isNaN(ackTimestamp) &&
			lastMessageTimestamp !== 0 &&
			!Number.isNaN(lastMessageTimestamp) &&
			ackTimestamp < lastMessageTimestamp;

		return {
			mentionCount: this.mentionCount,
			unread,
		};
	}

	rebuild(ackMessageId?: MessageId | null, {recomputeMentions = false}: {recomputeMentions?: boolean} = {}): void {
		this.ackMessageId = ackMessageId ?? this._ackMessageId;
		this.oldestUnreadMessageId = null;
		this.estimated = false;
		this.unreadCount = 0;

		if (recomputeMentions) {
			this.mentionCount = 0;
		}

		if (!this.hasUnread()) {
			return;
		}

		const currentUser = UserStore.getCurrentUser();
		if (currentUser == null) {
			return;
		}

		const messages = MessageStore.getMessages(this.channelId);
		const ackTimestamp = this.ackTimestamp;
		const isPrivate = this.isPrivate;

		let foundAckMessage = false;
		let loadedOlderMessages = false;
		let oldestUnread: MessageId | null = null;

		messages.forAll((message) => {
			if (!foundAckMessage) {
				foundAckMessage = message.id === this._ackMessageId;
			} else if (this._oldestUnreadMessageId == null) {
				this._oldestUnreadMessageId = message.id;
			}

			if (extractTimestamp(message.id) > ackTimestamp) {
				this.unreadCount++;

				if (recomputeMentions && this.shouldMentionFor(message, currentUser.id, isPrivate)) {
					this.mentionCount++;
				}

				oldestUnread ??= message.id;
			} else {
				loadedOlderMessages = true;
			}
		});

		this.estimated =
			!messages.hasPresent() || (!(foundAckMessage || loadedOlderMessages) && messages.length === this.unreadCount);

		this.oldestUnreadMessageId = this._oldestUnreadMessageId ?? oldestUnread;
	}

	shouldMentionFor(message: MessageRecord | Message, userId: string, isPrivate: boolean): boolean {
		if (RelationshipStore.isBlocked(message.author.id)) {
			return false;
		}

		const suppressEveryone = UserGuildSettingsStore.isSuppressEveryoneEnabled(this.guildId);
		const suppressRoles = UserGuildSettingsStore.isSuppressRolesEnabled(this.guildId);

		const mentions = message.mentions;
		const mentionEveryone = 'mentionEveryone' in message ? message.mentionEveryone : message.mention_everyone;
		const mentionRoles = 'mentionRoles' in message ? message.mentionRoles : message.mention_roles;

		const hasUserMention = mentions?.some((m) => m.id === userId) ?? false;
		const hasEveryoneMention = !suppressEveryone && !!mentionEveryone;
		const hasRoleMention = !suppressRoles && (mentionRoles?.length ?? 0) > 0;

		let shouldMention = hasUserMention || hasEveryoneMention || hasRoleMention;
		const isMuted = UserGuildSettingsStore.isGuildOrChannelMuted(this.guildId, this.channelId);
		if (!shouldMention && isPrivate && !isMuted) {
			shouldMention = true;
		}

		return shouldMention;
	}

	computeMentionCountAfterAck(messageId: MessageId): number {
		const currentUser = UserStore.getCurrentUser();
		if (currentUser == null) {
			return 0;
		}

		const ackTimestamp = extractTimestamp(messageId);
		if (Number.isNaN(ackTimestamp)) {
			return 0;
		}

		const messages = MessageStore.getMessages(this.channelId);
		const isPrivate = this.isPrivate;
		let mentionCount = 0;

		messages.forAll((message) => {
			if (extractTimestamp(message.id) <= ackTimestamp) {
				return;
			}

			if (this.shouldMentionFor(message, currentUser.id, isPrivate)) {
				mentionCount++;
			}
		});

		return mentionCount;
	}

	ackPins(timestamp?: string | null): boolean {
		if (!this.canTrackUnreads()) {
			return false;
		}

		if (timestamp == null) {
			if (this.lastPinTimestamp === this.ackPinTimestamp) {
				return false;
			}

			http.post({url: Endpoints.CHANNEL_PINS_ACK(this.channelId)}).catch((error) => {
				logger.error(`Failed to ack pins for ${this.channelId}:`, error);
			});
		}

		const newTimestamp = parseTimestamp(timestamp);
		this.ackPinTimestamp = newTimestamp !== 0 ? newTimestamp : this.lastPinTimestamp;
		return true;
	}

	clearOutgoingAck(): void {
		this.outgoingAck = null;
		if (this.outgoingAckTimer != null) {
			clearTimeout(this.outgoingAckTimer);
			this.outgoingAckTimer = null;
		}
	}

	private shouldAck(force: boolean, local: boolean, isExplicitUserAction: boolean): boolean {
		if (force || local || isExplicitUserAction) {
			return true;
		}

		if (this.isManualAck) {
			return false;
		}

		if (!this.loadedMessages) {
			return false;
		}

		return true;
	}

	ack(options: {
		messageId?: MessageId | null;
		local?: boolean;
		immediate?: boolean;
		force?: boolean;
		isExplicitUserAction?: boolean;
		preserveStickyUnread?: boolean;
	}): boolean {
		const {
			messageId,
			local = false,
			immediate = false,
			force = false,
			isExplicitUserAction = false,
			preserveStickyUnread = false,
		} = options;

		if (!this.shouldAck(force, local, isExplicitUserAction)) {
			return false;
		}

		if (!force && !this.canTrackUnreads()) {
			return false;
		}

		const hadMentions = this.hasMentions();

		if (preserveStickyUnread && this._oldestUnreadMessageId != null && this._stickyUnreadMessageId == null) {
			this._stickyUnreadMessageId = this._oldestUnreadMessageId;
		}

		this.estimated = false;
		this.unreadCount = 0;
		this.mentionCount = 0;

		const finalMessageId = messageId ?? this.lastMessageId;
		if (finalMessageId == null) {
			return false;
		}

		this.ackMessageId = finalMessageId;
		this.oldestUnreadMessageId = null;

		if (local || force || isExplicitUserAction) {
			this.isManualAck = false;
			this._stickyUnreadMessageId = null;
		}

		if (local) {
			return true;
		}

		if (this.outgoingAckTimer != null) {
			clearTimeout(this.outgoingAckTimer);
			this.outgoingAckTimer = null;
		}

		const delay = hadMentions || immediate ? 0 : 3000;
		this.outgoingAck = finalMessageId;

		this.outgoingAckTimer = setTimeout(
			action(() => {
				void this.sendAck();
				this.outgoingAck = null;
				this.outgoingAckTimer = null;
			}),
			delay,
		);

		return true;
	}

	private async sendAck(): Promise<void> {
		const {outgoingAck} = this;
		if (outgoingAck == null) return;

		try {
			await http.post({
				url: Endpoints.CHANNEL_MESSAGE_ACK(this.channelId, outgoingAck),
				body: {},
			});
		} catch (error) {
			logger.error(`Failed to ack ${outgoingAck} in ${this.channelId}:`, error);
		}
	}

	dispose(): void {
		this.clearOutgoingAck();
	}
}

class ReadStateStore {
	private readonly states = new Map<ChannelId, ReadStateEntry>();
	private mentionChannels = new Set<ChannelId>();

	private setMentionCount(state: ReadStateEntry, mentionCount: number): void {
		const normalized = Math.max(0, mentionCount);
		state.mentionCount = normalized;
		if (normalized > 0 && state.canHaveMentions()) {
			this.mentionChannels.add(state.channelId);
		} else {
			this.mentionChannels.delete(state.channelId);
		}
	}

	updateCounter = 0;

	private pendingChanges = new Map<ChannelId, GuildId | null>();
	private pendingGlobalRecompute = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get version(): number {
		return this.updateCounter;
	}

	@action
	private notifyChange(channelId?: ChannelId, {global = false}: {global?: boolean} = {}): void {
		if (global) {
			this.pendingGlobalRecompute = true;
			this.pendingChanges.clear();
		} else if (channelId != null && !this.pendingGlobalRecompute) {
			const entry = this.states.get(channelId);
			const guildId = entry?.guildId ?? null;
			this.pendingChanges.set(channelId, guildId);
		}
		this.updateCounter++;
	}

	@action
	consumePendingChanges(): {
		all: boolean;
		channelIds: Array<ChannelId>;
		changes: Array<{channelId: ChannelId; guildId: GuildId | null}>;
	} {
		const all = this.pendingGlobalRecompute;

		const changes: Array<{channelId: ChannelId; guildId: GuildId | null}> = [];
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

	get(channelId: ChannelId): ReadStateEntry {
		let entry = this.states.get(channelId);
		if (entry == null) {
			entry = new ReadStateEntry(channelId);
			this.states.set(channelId, entry);
		}
		return entry;
	}

	getIfExists(channelId: ChannelId): ReadStateEntry | undefined {
		return this.states.get(channelId);
	}

	clear(channelId: ChannelId): boolean {
		const entry = this.states.get(channelId);
		if (entry == null) {
			return false;
		}

		this.notifyChange(channelId);

		entry.dispose();
		this.states.delete(channelId);
		this.mentionChannels.delete(channelId);
		return true;
	}

	clearAll(): void {
		for (const entry of this.states.values()) {
			entry.dispose();
		}
		this.states.clear();
		this.mentionChannels.clear();
		this.notifyChange(undefined, {global: true});
	}

	get mentionChannelIds(): Array<ChannelId> {
		const ids: Array<ChannelId> = [];
		for (const channelId of this.mentionChannels) {
			const state = this.getIfExists(channelId);
			if (state?.canHaveMentions()) {
				ids.push(channelId);
			} else {
				this.mentionChannels.delete(channelId);
			}
		}
		return ids;
	}

	isAutomaticAckEnabled(channelId: ChannelId): boolean {
		return AutoAckStore.isAutomaticAckEnabled(channelId);
	}

	getUnreadCount(channelId: ChannelId): number {
		const state = this.getIfExists(channelId);
		return state?.canBeUnread() ? state.unreadCount : 0;
	}

	getMentionCount(channelId: ChannelId): number {
		const state = this.getIfExists(channelId);
		return state?.canHaveMentions() ? state.mentionCount : 0;
	}

	getManualAckMentionCount(channelId: ChannelId, messageId: MessageId): number {
		const state = this.getIfExists(channelId);
		return state?.computeMentionCountAfterAck(messageId) ?? 0;
	}

	hasUnread(channelId: ChannelId): boolean {
		const state = this.getIfExists(channelId);
		return !!(state?.canBeUnread() && state.hasUnread());
	}

	hasUnreadOrMentions(channelId: ChannelId): boolean {
		const state = this.getIfExists(channelId);
		return !!(state?.canBeUnread() && state.hasUnreadOrMentions());
	}

	ackMessageId(channelId: ChannelId): MessageId | null {
		const state = this.getIfExists(channelId);
		return state?.canBeUnread() ? state.ackMessageId : null;
	}

	lastMessageId(channelId: ChannelId): MessageId | null {
		const state = this.getIfExists(channelId);
		return state?.lastMessageId ?? null;
	}

	getOldestUnreadMessageId(channelId: ChannelId): MessageId | null {
		const state = this.getIfExists(channelId);
		return state?.canTrackUnreads() ? state.oldestUnreadMessageId : null;
	}

	getVisualUnreadMessageId(channelId: ChannelId): MessageId | null {
		const state = this.getIfExists(channelId);
		return state?.canTrackUnreads() ? state.visualUnreadMessageId : null;
	}

	getChannelIds(): Array<ChannelId> {
		return Array.from(this.states.keys());
	}

	clearStickyUnread(channelId: ChannelId): void {
		const state = this.getIfExists(channelId);
		if (state != null) {
			state.clearStickyUnread();
			this.notifyChange(channelId);
		}
	}

	hasUnreadPins(channelId: ChannelId): boolean {
		const state = this.getIfExists(channelId);
		return !!(state?.canBeUnread() && state.lastPinTimestamp > state.ackPinTimestamp);
	}

	ackPins(channelId: ChannelId): void {
		const state = this.get(channelId);
		if (state.ackPins()) {
			this.notifyChange(channelId);
		}
	}

	handleConnectionOpen(action: {readState: Array<GatewayReadState>; channels: Array<ChannelPayload>}): void {
		this.clearAll();

		const allGuildsAvailable = GuildAvailabilityStore.totalUnavailableGuilds === 0;
		const channelsWithReadState = new Set<ChannelId>();

		for (const readState of action.readState) {
			channelsWithReadState.add(readState.id);

			const state = this.get(readState.id);
			this.setMentionCount(state, readState.mention_count ?? 0);
			state.ackMessageId = readState.last_message_id ?? null;
			state.ackPinTimestamp = parseTimestamp(readState.last_pin_timestamp);

			if (allGuildsAvailable && !state.canTrackUnreads()) {
				state.ackMessageId = null;
				this.setMentionCount(state, 0);
				state.unreadCount = 0;
			}
		}

		for (const channel of action.channels) {
			if (channel.type === ChannelTypes.GUILD_VOICE) continue;

			const state = this.get(channel.id);
			state.lastMessageId = channel.last_message_id ?? null;
			state.lastPinTimestamp = parseTimestamp(channel.last_pin_timestamp);
			state._guildId = channel.guild_id ?? null;

			if (!channelsWithReadState.has(channel.id)) {
				state.ackMessageId = null;
				this.setMentionCount(state, 0);
			}

			if (state.hasUnread()) {
				state.estimated = true;
				state.unreadCount = Math.max(1, state.mentionCount);
			}
		}

		this.notifyChange(undefined, {global: true});
	}

	handleGuildCreate(action: {guild: {id: GuildId; channels?: ReadonlyArray<ChannelPayload>}}): void {
		if (action.guild.channels) {
			for (const channel of action.guild.channels) {
				if (channel.type === ChannelTypes.GUILD_VOICE) continue;

				const state = this.get(channel.id);
				state.lastMessageId = channel.last_message_id ?? null;
				state.lastPinTimestamp = parseTimestamp(channel.last_pin_timestamp);
				state._guildId = action.guild.id;
			}
		}
		this.notifyChange(undefined, {global: true});
	}

	handleLoadMessages(action: {channelId: ChannelId; isAfter?: boolean; messages: Array<Message>}): void {
		const state = this.get(action.channelId);
		state.loadedMessages = true;

		const messages = MessageStore.getMessages(action.channelId);

		if (messages.hasPresent() || (messages.jumpTargetId != null && messages.jumpTargetId === state.ackMessageId)) {
			state.rebuild();
		} else if (action.isAfter && state.ackMessageId != null && messages.has(state.ackMessageId, true)) {
			state.unreadCount += action.messages.length;
		}

		this.notifyChange(action.channelId);
	}

	handleIncomingMessage(action: {channelId: ChannelId; message: Message}): void {
		const state = this.get(action.channelId);
		const currentUser = UserStore.getCurrentUser();

		if (compareSnowflakes(action.message.id, state.lastMessageId ?? '') > 0) {
			state.lastMessageId = action.message.id;
		}

		if (currentUser != null && action.message.author.id === currentUser.id) {
			if (state.outgoingAck != null) {
				state.clearOutgoingAck();
			}
			state.clearStickyUnread();
			state.ack({messageId: action.message.id, local: true});
			this.notifyChange(action.channelId);
			return;
		}

		if (this.isAutomaticAckEnabled(action.channelId) && DimensionStore.isAtBottom(action.channelId)) {
			state.ack({messageId: action.message.id, preserveStickyUnread: true});
			this.notifyChange(action.channelId);
			return;
		}

		if (state.oldestUnreadMessageId == null || state.oldestUnreadMessageIdStale) {
			state.oldestUnreadMessageId = action.message.id;
		}

		state.unreadCount++;

		if (currentUser != null && !RelationshipStore.isBlocked(action.message.author.id)) {
			const shouldMention = state.shouldMentionFor(action.message, currentUser.id, state.isPrivate);
			if (shouldMention) {
				state.mentionCount++;
				this.mentionChannels.add(state.channelId);
			}
		}

		this.notifyChange(action.channelId);
	}

	handleMessageDelete(action: {channelId: ChannelId}): void {
		const state = this.get(action.channelId);
		state.rebuild();
		this.notifyChange(action.channelId);
	}

	handleChannelCreate(action: {channel: ChannelPayload}): void {
		if (
			action.channel.type !== ChannelTypes.DM &&
			action.channel.type !== ChannelTypes.GROUP_DM &&
			action.channel.type !== ChannelTypes.GUILD_TEXT
		) {
			return;
		}

		const state = this.get(action.channel.id);
		state.lastMessageId = action.channel.last_message_id ?? null;
		state.lastPinTimestamp = parseTimestamp(action.channel.last_pin_timestamp);
		state._guildId = action.channel.guild_id ?? null;

		if (
			(action.channel.type === ChannelTypes.DM || action.channel.type === ChannelTypes.GROUP_DM) &&
			action.channel.last_message_id != null
		) {
			state.ackMessageId = action.channel.last_message_id;
		}

		this.notifyChange(action.channel.id);
	}

	handleChannelDelete(action: {channel: {id: ChannelId}}): void {
		this.clear(action.channel.id);
	}

	handleChannelAck(action: {channelId: ChannelId; messageId?: MessageId; immediate?: boolean; force?: boolean}): void {
		const state = this.get(action.channelId);
		state.ack({
			messageId: action.messageId,
			immediate: action.immediate,
			force: action.force,
			isExplicitUserAction: true,
		});
		this.notifyChange(action.channelId);
	}

	handleChannelAckWithStickyUnread(action: {channelId: ChannelId}): void {
		const state = this.get(action.channelId);

		const lastMessageId = state.lastMessageId;
		if (lastMessageId == null) {
			return;
		}

		const ackedMessageId = state.ackMessageId;
		const hasUnreads = state.unreadCount > 0;
		const ackBehind = ackedMessageId == null || compareSnowflakes(ackedMessageId, lastMessageId) < 0;

		if (!hasUnreads && !ackBehind) {
			return;
		}

		const didAck = state.ack({
			messageId: lastMessageId,
			preserveStickyUnread: true,
		});

		if (didAck) {
			this.notifyChange(action.channelId);
		}
	}

	handleChannelPinsAck(action: {channelId: ChannelId; timestamp?: string}): void {
		const state = this.get(action.channelId);
		state.ackPins(action.timestamp);
		this.notifyChange(action.channelId);
	}

	handleChannelPinsUpdate(action: {channelId: ChannelId; lastPinTimestamp: string}): void {
		const state = this.get(action.channelId);
		const newTimestamp = parseTimestamp(action.lastPinTimestamp);

		if (state.lastPinTimestamp !== newTimestamp) {
			state.lastPinTimestamp = newTimestamp;
			this.notifyChange(action.channelId);
		}
	}

	handleMessageAck(action: {channelId: ChannelId; messageId: MessageId; mentionCount?: number; manual: boolean}): void {
		const state = this.get(action.channelId);
		const mentionCount = action.mentionCount;
		if (action.manual) {
			state.clearStickyUnread();
			state.isManualAck = true;
			state.rebuild(action.messageId, {recomputeMentions: true});
			state.clearOutgoingAck();
			AutoAckStore.disableForChannel(action.channelId);
			if (mentionCount != null) {
				this.setMentionCount(state, mentionCount);
			}
			this.notifyChange(action.channelId);
			return;
		}

		if (action.messageId === state.ackMessageId) {
			if (mentionCount != null) {
				this.setMentionCount(state, mentionCount);
				this.notifyChange(action.channelId);
			}
			return;
		}

		state.ack({messageId: action.messageId, local: true});

		if (mentionCount != null) {
			this.setMentionCount(state, mentionCount);
		}
		this.notifyChange(action.channelId);
	}

	handleClearManualAck(action: {channelId: ChannelId}): void {
		const state = this.get(action.channelId);
		if (state.isManualAck) {
			state.isManualAck = false;
			this.notifyChange(action.channelId);
		}
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.version,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new ReadStateStore();
