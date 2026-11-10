// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import {resetBootHydration} from '@app/features/boot_state/BootHydration';
import Channels from '@app/features/channel/state/Channels';
import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import Guilds from '@app/features/guild/state/Guilds';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import type {JumpOptions} from '@app/features/messaging/state/ChannelMessages';
import {ChannelMessages} from '@app/features/messaging/state/ChannelMessages';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
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
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

const STALE_WINDOW_REFETCH_INTERVAL_MS = 10_000;
const SEEDED_WINDOW_REFETCH_LIMIT = 100;
const BOOT_CATCHUP_MAX_PAGES = 4;
const STALE_SEEDED_WINDOW_PREFETCH_LIMIT = 4;
const staleWindowRefetchedAt = new Map<string, number>();

export interface SeededMessageWindow {
	readonly channelId: string;
	readonly messages: ReadonlyArray<WireMessage>;
	readonly hasMoreBefore: boolean;
	readonly hasMoreAfter: boolean;
	readonly anchorMessageId: string | null;
}

interface DeferredSeededOlderMessages {
	readonly hasMoreBefore: boolean;
	readonly oldestSeededId: string;
	readonly older: ReadonlyArray<WireMessage>;
	readonly tops: ReadonlyMap<string, number> | null;
}

interface StaleWindowRefetch {
	readonly limit: number;
	readonly jump?: JumpOptions;
	readonly seededAnchorMessageId?: string | null;
}

interface GuildMemberUpdateAction {
	type: 'GUILD_MEMBER_UPDATE';
	guildId: string;
	member: GuildMemberData;
}

interface PendingJumpDispatch {
	messageId: string;
	flash?: boolean;
	offset?: number;
	returnToMessageId?: string;
	returnChannelId?: string | null;
	returnGuildId?: string | null;
	jumpType?: JumpType;
}

class Messages {
	private pendingJumpDispatches = new Map<string, PendingJumpDispatch>();
	private messageRefsByAuthor = new Map<string, Map<string, Set<string>>>();
	private indexedAuthorsByChannel = new Map<string, Map<string, string>>();
	private seededAnchors = new Map<string, string | null>();
	private heldIncoming = new Map<string, Array<WireMessage>>();
	private deferredOlder = new Map<string, DeferredSeededOlderMessages>();
	private restoredTails = new Map<string, number>();
	reconcilingChannelIds = new Set<string>();
	private prefetchingSeededChannelIds = new Set<string>();
	private seededReconcileGenerations = new Map<string, number>();
	private lastSeededReconcileGeneration = 0;
	updateCounter = 0;
	cacheGeneration = 0;
	private pendingFullHydration = false;
	private bootRequests = new Map<string, Promise<unknown>>();

	constructor() {
		makeAutoObservable<
			this,
			| 'messageRefsByAuthor'
			| 'indexedAuthorsByChannel'
			| 'seededAnchors'
			| 'heldIncoming'
			| 'deferredOlder'
			| 'restoredTails'
			| 'bootRequests'
			| 'prefetchingSeededChannelIds'
			| 'seededReconcileGenerations'
			| 'lastSeededReconcileGeneration'
		>(
			this,
			{
				messageRefsByAuthor: false,
				indexedAuthorsByChannel: false,
				seededAnchors: false,
				heldIncoming: false,
				deferredOlder: false,
				restoredTails: false,
				bootRequests: false,
				prefetchingSeededChannelIds: false,
				seededReconcileGenerations: false,
				lastSeededReconcileGeneration: false,
			},
			{autoBind: true},
		);
	}

	get version(): number {
		return this.updateCounter;
	}

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
		this.seededAnchors.delete(channelId);
		this.bootRequests.delete(channelId);
		this.heldIncoming.delete(channelId);
		this.deferredOlder.delete(channelId);
		this.restoredTails.delete(channelId);
		this.reconcilingChannelIds.delete(channelId);
		this.prefetchingSeededChannelIds.delete(channelId);
		this.seededReconcileGenerations.delete(channelId);
		this.unindexChannelMessages(channelId);
	}

	private awaitPrefetchedWindow(channelId: string): void {
		this.prefetchingSeededChannelIds.delete(channelId);
		this.deferredOlder.delete(channelId);
		this.unindexChannelMessages(channelId);
		ChannelMessages.clear(channelId);
		this.commitMessages(ChannelMessages.getOrCreate(channelId).withPatch({loadingMore: true}));
		this.notifyChange();
	}

	seedFromBootState(windows: ReadonlyArray<SeededMessageWindow>): Map<string, ChannelMessages> {
		const seeded = new Map<string, ChannelMessages>();
		for (const window of windows) {
			const messages = new ChannelMessages(window.channelId).applyLoadedWindow({
				windowMessages: [...window.messages].reverse(),
				hasMoreBefore: window.hasMoreBefore,
				hasMoreAfter: window.hasMoreAfter,
				cached: true,
			});
			seeded.set(window.channelId, this.commitMessages(messages));
			this.seededAnchors.set(window.channelId, window.anchorMessageId);
		}
		this.notifyChange();
		return seeded;
	}

	deferSeededOlderMessages(
		seeded: ChannelMessages,
		older: ReadonlyArray<WireMessage>,
		hasMoreBefore = true,
		tops: ReadonlyMap<string, number> | null = null,
	): void {
		const oldest = seeded.first();
		if (oldest == null) {
			return;
		}
		for (const message of older) MessageReactions.hydrateMessageReactions(message.id, message.reactions);
		this.deferredOlder.set(seeded.channelId, {oldestSeededId: oldest.id, older, hasMoreBefore, tops});
	}

	deferredOlderReserve(channelId: string): {readonly heightPx: number; readonly hasMoreBefore: boolean} | null {
		const deferred = this.currentDeferredOlder(channelId);
		if (deferred === null) return null;
		const first = deferred.older[0];
		const top = first === undefined ? undefined : deferred.tops?.get(first.id);
		const mountedTop = deferred.tops?.get(deferred.messages.first()?.id ?? '');
		const heightPx = top === undefined || mountedTop === undefined ? 0 : Math.max(0, mountedTop - top);
		return {heightPx, hasMoreBefore: deferred.hasMoreBefore};
	}

	reserveRestoredTail(channelId: string, heightPx: number): void {
		this.restoredTails.set(channelId, heightPx);
	}

	restoredTailHeight(channelId: string): number | null {
		const heightPx = this.restoredTails.get(channelId);
		if (heightPx === undefined) return null;
		const messages = ChannelMessages.get(channelId);
		if (messages?.cached !== true || !messages.hasMoreAfter) {
			this.restoredTails.delete(channelId);
			return null;
		}
		return heightPx;
	}

	deferredOlderOffsets(channelId: string): {readonly mountedId: string; readonly offsets: Map<string, number>} | null {
		const deferred = this.currentDeferredOlder(channelId);
		const mountedId = deferred?.messages.first()?.id;
		const mountedTop = mountedId === undefined ? undefined : deferred?.tops?.get(mountedId);
		if (deferred == null || mountedId === undefined || mountedTop === undefined) return null;
		const offsets = new Map<string, number>();
		for (const message of deferred.older) {
			const top = deferred.tops?.get(message.id);
			if (top !== undefined) offsets.set(message.id, top - mountedTop);
		}
		return {mountedId, offsets};
	}

	hasDeferredOlderMessages(channelId: string): boolean {
		return this.currentDeferredOlder(channelId) !== null;
	}

	getMessagesForBootCapture(channelId: string): ChannelMessages | undefined {
		return this.getMessagesIncludingDeferred(channelId);
	}

	getMessagesIncludingDeferred(channelId: string): ChannelMessages | undefined {
		const deferred = this.currentDeferredOlder(channelId);
		if (deferred === null) return ChannelMessages.get(channelId);
		return deferred.messages.applyLoadedWindow({
			windowMessages: deferred.older.map((message) => this.deferredMessageForHydration(message)).reverse(),
			isBefore: true,
			hasMoreBefore: deferred.hasMoreBefore,
			cached: true,
		});
	}

	prependAllDeferredOlderMessages(): void {
		for (const channelId of Array.from(this.deferredOlder.keys())) {
			this.prependDeferredOlderMessages(channelId);
		}
	}

	prependDeferredOlderMessages(channelId: string, limit: number = Number.POSITIVE_INFINITY): boolean {
		const deferred = this.currentDeferredOlder(channelId);
		if (deferred === null) {
			return false;
		}
		const sliceStart = Math.max(0, deferred.older.length - limit);
		const prepended = deferred.older.slice(sliceStart);
		const remaining = deferred.older.slice(0, sliceStart);
		this.deferredOlder.delete(channelId);
		const grown = deferred.messages.applyLoadedWindow({
			windowMessages: prepended.map((message) => this.deferredMessageForHydration(message)).reverse(),
			isBefore: true,
			hasMoreBefore: remaining.length > 0 || deferred.hasMoreBefore,
			cached: true,
		});
		const committed = this.commitMessages(grown);
		if (remaining.length > 0) {
			this.deferSeededOlderMessages(
				committed,
				remaining.map((message) => this.deferredMessageForHydration(message)),
				deferred.hasMoreBefore,
				deferred.tops,
			);
		}
		this.notifyChange();
		return true;
	}

	private deferredMessageForHydration(message: WireMessage): WireMessage {
		return {
			...message,
			author: (!message.webhook_id && Users.getUser(message.author.id)?.toJSON()) || message.author,
			reactions: [...MessageReactions.getMessageReactions(message.id)],
		};
	}

	private patchDeferredOlderMessages(channelId: string, patch: (message: WireMessage) => WireMessage | null): boolean {
		const deferred = this.currentDeferredOlder(channelId);
		if (deferred === null) return false;
		const older: Array<WireMessage> = [];
		let changed = false;
		for (const message of deferred.older) {
			const next = patch(message);
			if (next !== message) changed = true;
			if (next !== null) older.push(next);
		}
		if (!changed) return false;
		if (older.length === 0) {
			this.deferredOlder.delete(channelId);
			if (!deferred.hasMoreBefore) this.commitMessages(deferred.messages.withPatch({hasMoreBefore: false}));
		} else
			this.deferredOlder.set(channelId, {
				oldestSeededId: deferred.messages.first()!.id,
				older,
				hasMoreBefore: deferred.hasMoreBefore,
				tops: deferred.tops,
			});
		this.notifyChange();
		return true;
	}

	private currentDeferredOlder(channelId: string): {
		readonly messages: ChannelMessages;
		readonly older: ReadonlyArray<WireMessage>;
		readonly hasMoreBefore: boolean;
		readonly tops: ReadonlyMap<string, number> | null;
	} | null {
		const deferred = this.deferredOlder.get(channelId);
		if (deferred === undefined) {
			return null;
		}
		const messages = ChannelMessages.get(channelId);
		if (messages == null || messages.first()?.id !== deferred.oldestSeededId) {
			this.deferredOlder.delete(channelId);
			return null;
		}
		return {messages, older: deferred.older, hasMoreBefore: deferred.hasMoreBefore, tops: deferred.tops};
	}

	hasBootReconciliation(): boolean {
		return this.bootRequests.size > 0;
	}

	async whenBootChannelsReconciled(): Promise<void> {
		await Promise.all(this.bootRequests.values());
	}

	refreshBootChannel(channelId: string): Promise<unknown> {
		const pending = this.bootRequests.get(channelId);
		if (pending !== undefined) return pending;
		const messages = ChannelMessages.get(channelId);
		if (!messages?.ready || messages.last() === undefined || !this.seededAnchors.has(channelId))
			return Promise.resolve();
		const generation = this.cacheGeneration;
		this.holdIncomingMessages(channelId);
		this.reconcilingChannelIds.add(channelId);
		const request = this.fetchBootChannelMessages(channelId, true).finally(() => {
			if (generation !== this.cacheGeneration || this.bootRequests.get(channelId) !== request) return;
			runInAction(() => {
				this.reconcilingChannelIds.delete(channelId);
				this.releaseHeldMessages(channelId);
			});
		});
		this.bootRequests.set(channelId, request);
		return request;
	}

	private async fetchBootChannelMessages(channelId: string, includeCachedWindow: boolean): Promise<void> {
		const messages = this.getMessagesForBootCapture(channelId);
		const sent = messages?.toArray().filter((message) => message.state === MessageStates.SENT) ?? [];
		const newest = sent.at(-1);
		if (newest === undefined || Channels.getChannel(channelId) === undefined || AccountScopedWork.isSuspended) return;
		const generation = this.cacheGeneration;
		const commits: Array<() => void> = [];
		let failed = false;
		const failure = () => {
			failed = true;
		};
		const deferCommit = (commit: () => void) => {
			commits.push(commit);
		};
		const newer = async () => {
			let cursor = newest.id;
			for (let page = 0; page < BOOT_CATCHUP_MAX_PAGES; page += 1) {
				if (generation !== this.cacheGeneration || AccountScopedWork.isSuspended) return;
				const fetched = await MessageCommands.fetchMessages(
					channelId,
					null,
					cursor,
					SEEDED_WINDOW_REFETCH_LIMIT,
					undefined,
					{
						staleRefetch: true,
						throwOnError: true,
						reconcileSeeded: {around: null, newer: true},
						deferCommit,
					},
				);
				if (fetched.length < SEEDED_WINDOW_REFETCH_LIMIT) return;
				const next = fetched[0]?.id;
				if (next === undefined || SnowflakeUtils.compare(next, cursor) <= 0) return;
				cursor = next;
			}
		};
		const requests: Array<Promise<unknown>> = [newer().catch(failure)];
		if (includeCachedWindow)
			requests.push(
				MessageCommands.fetchMessages(
					channelId,
					(BigInt(newest.id) + 1n).toString(),
					null,
					Math.min(SEEDED_WINDOW_REFETCH_LIMIT, sent.length + 1),
					undefined,
					{
						staleRefetch: true,
						throwOnError: true,
						reconcileSeeded: {around: null, through: newest.id},
						deferCommit,
					},
				).catch(failure),
			);
		await Promise.all(requests);
		if (generation !== this.cacheGeneration || failed) return;
		runInAction(() => {
			for (const commit of commits) commit();
		});
	}

	private catchUpBootChannel(channelId: string, initial: Promise<unknown>): void {
		const generation = this.cacheGeneration;
		const catchup = initial
			.then(() => {
				if (
					generation !== this.cacheGeneration ||
					this.bootRequests.get(channelId) !== catchup ||
					!GatewaySessions.foregroundConnection.isReady
				)
					return;
				this.holdIncomingMessages(channelId);
				this.reconcilingChannelIds.add(channelId);
				return this.fetchBootChannelMessages(channelId, this.seededAnchors.has(channelId));
			})
			.finally(() => {
				if (generation !== this.cacheGeneration || this.bootRequests.get(channelId) !== catchup) return;
				runInAction(() => {
					this.bootRequests.delete(channelId);
					this.reconcilingChannelIds.delete(channelId);
					this.releaseHeldMessages(channelId);
				});
			});
		this.bootRequests.set(channelId, catchup);
	}

	private refetchSeededWindow(channelId: string): boolean {
		const messages = ChannelMessages.get(channelId);
		if (this.bootRequests.has(channelId)) return true;
		if (!messages || !this.seededAnchors.has(channelId)) {
			return false;
		}
		const anchorMessageId = this.seededAnchors.get(channelId) ?? null;
		return this.refetchStaleWindow(channelId, {
			limit: SEEDED_WINDOW_REFETCH_LIMIT,
			seededAnchorMessageId: messages.hasMoreAfter ? anchorMessageId : null,
		});
	}

	private isCachedWindowBehindChannel(channelId: string): boolean {
		const messages = ChannelMessages.get(channelId);
		const newestCachedId = messages?.last()?.id;
		const lastMessageId = Channels.getChannel(channelId)?.lastMessageId;
		const reconcilingInPlace =
			this.reconcilingChannelIds.has(channelId) && !this.prefetchingSeededChannelIds.has(channelId);
		return (
			messages?.cached === true &&
			!messages.hasMoreAfter &&
			!reconcilingInPlace &&
			newestCachedId != null &&
			lastMessageId != null &&
			SnowflakeUtils.compare(lastMessageId, newestCachedId) > 0
		);
	}

	private prefetchSeededWindowsBehindChannels(selectedChannelId: string | null): void {
		const behind = Array.from(this.seededAnchors.keys()).filter(
			(channelId) => channelId !== selectedChannelId && this.isCachedWindowBehindChannel(channelId),
		);
		for (const channelId of behind.slice(0, STALE_SEEDED_WINDOW_PREFETCH_LIMIT)) {
			void this.reconcileSeededWindow(channelId, SEEDED_WINDOW_REFETCH_LIMIT, undefined, null, true);
		}
	}

	private dropSeededWindowsMissingFromReady(): void {
		for (const channelId of new Set([...this.seededAnchors.keys(), ...this.bootRequests.keys()])) {
			if (Channels.getChannel(channelId) == null) {
				this.clearMessages(channelId);
				Dimension.forgetChannelDimensions(channelId);
			}
		}
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

	private hasLoadedPage(messages: ChannelMessages): boolean {
		return messages.ready && messages.length > 0 && !messages.cached;
	}

	handlePassiveLastMessageUpdates(channels: Record<string, string>): void {
		let changed = false;
		for (const [channelId, lastMessageId] of Object.entries(channels)) {
			const messages = ChannelMessages.get(channelId);
			const newestLoadedId = messages?.last()?.id;
			if (!messages || messages.cached || !newestLoadedId || !messages.hasNewestMessages()) continue;
			if (SnowflakeUtils.compare(lastMessageId, newestLoadedId) <= 0) continue;
			this.commitMessages(messages.withPatch({cached: true}));
			changed = true;
		}
		if (changed) this.notifyChange();
	}

	shouldPreloadLatestPage(channelId: string): boolean {
		if (!GatewaySessions.foregroundConnection.isConnected || !Channels.getChannel(channelId)) {
			return false;
		}
		const messages = ChannelMessages.get(channelId);
		if (!messages) return true;
		if (messages.loadingMore || ChannelMessages.isRetained(channelId)) return false;
		return messages.length === 0 ? !messages.ready : messages.cached;
	}

	preloadLatestPage(channelId: string, guildId?: string | null): boolean {
		if (!this.shouldPreloadLatestPage(channelId)) {
			return false;
		}
		const channel = Channels.getChannel(channelId);
		const resolvedGuildId = guildId ?? channel?.guildId ?? (channel?.isPrivate() ? ME : undefined);
		this.handleChannelSelect({guildId: resolvedGuildId ?? undefined, channelId});
		return true;
	}

	getMessage(channelId: string, messageId: string): Message | undefined {
		return ChannelMessages.getOrCreate(channelId).get(messageId);
	}

	getLastEditableMessage(channelId: string): Message | undefined {
		return this.getMessages(channelId).searchFromNewest((message) => {
			return (
				message.isCurrentUserAuthor() &&
				message.state === MessageStates.SENT &&
				message.isUserMessage() &&
				!message.messageSnapshots
			);
		});
	}

	hasNewestMessages(channelId: string): boolean {
		const channel = ChannelMessages.get(channelId);
		return channel?.hasNewestMessages() ?? false;
	}

	setPendingJumpDispatch(channelId: string, dispatch: PendingJumpDispatch): void {
		this.pendingJumpDispatches.set(channelId, dispatch);
	}

	private takePendingJumpDispatch(channelId: string, messageId: string): PendingJumpDispatch | null {
		const dispatch = this.pendingJumpDispatches.get(channelId);
		if (!dispatch) return null;
		this.pendingJumpDispatches.delete(channelId);
		return dispatch.messageId === messageId ? dispatch : null;
	}

	handleSessionInvalidated(): boolean {
		resetBootHydration();
		const channelIds: Array<string> = [];
		ChannelMessages.forEach((messages) => {
			channelIds.push(messages.channelId);
		});
		for (const channelId of channelIds) {
			this.clearMessages(channelId);
			Dimension.forgetChannelDimensions(channelId);
		}
		this.messageRefsByAuthor.clear();
		this.indexedAuthorsByChannel.clear();
		this.pendingJumpDispatches.clear();
		this.bootRequests.clear();
		staleWindowRefetchedAt.clear();
		this.cacheGeneration += 1;
		this.seededAnchors.clear();
		this.heldIncoming.clear();
		this.deferredOlder.clear();
		this.reconcilingChannelIds.clear();
		this.prefetchingSeededChannelIds.clear();
		this.seededReconcileGenerations.clear();
		this.pendingFullHydration = true;
		this.notifyChange();
		return true;
	}

	handleGatewayReady(): boolean {
		if (AccountScopedWork.isSuspended) {
			return false;
		}
		const selectedChannelId = SelectedChannel.currentChannelId;
		let didHydrateSelectedChannel = false;
		this.dropSeededWindowsMissingFromReady();
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
			const bootRequest = this.bootRequests.get(messages.channelId);
			if (bootRequest !== undefined) {
				this.catchUpBootChannel(messages.channelId, bootRequest);
				if (messages.channelId === selectedChannelId) didHydrateSelectedChannel = true;
				return;
			}
			if (this.seededAnchors.has(messages.channelId)) {
				if (messages.channelId === selectedChannelId) {
					didHydrateSelectedChannel = this.refetchSeededWindow(messages.channelId);
				}
				return;
			}
			if (messages.channelId === selectedChannelId && Channels.getChannel(messages.channelId) != null) {
				this.startChannelHydration(messages.channelId, {forceScrollToBottom: this.pendingFullHydration});
				didHydrateSelectedChannel = true;
			} else {
				this.commitMessages(messages.withPatch({ready: false, loadingMore: false}));
			}
		});
		if (this.pendingFullHydration && !didHydrateSelectedChannel && selectedChannelId) {
			this.startChannelHydration(selectedChannelId, {forceScrollToBottom: true});
			didHydrateSelectedChannel = true;
		}
		this.pendingFullHydration = false;
		this.prefetchSeededWindowsBehindChannels(selectedChannelId);
		if (!didHydrateSelectedChannel && selectedChannelId && Channels.getChannel(selectedChannelId)) {
			const messages = ChannelMessages.getOrCreate(selectedChannelId);
			if (!messages.ready && !messages.loadingMore && messages.length === 0) {
				this.commitMessages(messages.withPatch({loadingMore: true}));
				MessageCommands.fetchMessages(selectedChannelId, null, null, MAX_MESSAGES_PER_CHANNEL);
				didHydrateSelectedChannel = true;
			}
		}
		this.notifyChange();
		return didHydrateSelectedChannel;
	}

	private startChannelHydration(channelId: string, options: {forceScrollToBottom?: boolean} = {}): void {
		if (!Channels.getChannel(channelId)) return;
		const {forceScrollToBottom = false} = options;
		const messages = ChannelMessages.getOrCreate(channelId);
		this.commitMessages(messages.withPatch({loadingMore: true, ready: false, error: false}));
		if (forceScrollToBottom) {
			Dimension.recordChannelDimensions(channelId, 1, 1, 0);
		}
		MessageCommands.fetchMessages(channelId, null, null, MAX_MESSAGES_PER_CHANNEL);
	}

	handleChannelSelect(action: {guildId?: string; channelId?: string | null; messageId?: string}): boolean {
		const channelId = action.channelId ?? action.guildId;
		if (channelId == null || channelId === ME) {
			return false;
		}
		const currentChannel = Channels.getChannel(channelId);
		if (GatewaySessions.foregroundConnection.isConnected && this.isCachedWindowBehindChannel(channelId)) {
			if (!action.messageId && this.prefetchingSeededChannelIds.has(channelId)) {
				this.awaitPrefetchedWindow(channelId);
				return false;
			}
			this.clearMessages(channelId);
		}
		const messages = ChannelMessages.getOrCreate(channelId);
		if (!action.messageId && GatewaySessions.foregroundConnection.isConnected && this.refetchSeededWindow(channelId)) {
			return false;
		}
		if (action.messageId) {
			const stashed = this.takePendingJumpDispatch(channelId, action.messageId);
			const dispatch: PendingJumpDispatch = stashed ?? {
				messageId: action.messageId,
				flash: true,
				jumpType: JumpTypes.INSTANT,
			};
			if (!GatewaySessions.foregroundConnection.isConnected) {
				this.pendingJumpDispatches.set(channelId, dispatch);
				return true;
			}
			MessageCommands.jumpToMessage({channelId, ...dispatch});
			return false;
		}
		if (!GatewaySessions.foregroundConnection.isConnected || messages.loadingMore || this.hasLoadedPage(messages)) {
			if (this.hasLoadedPage(messages) && Dimension.channelPinnedToEnd(channelId)) {
				this.commitMessages(messages.trimOldest(MAX_MESSAGES_PER_CHANNEL));
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
		this.commitMessages(messages.withPatch({loadingMore: true}));
		this.notifyChange();
		MessageCommands.fetchMessages(channelId, null, null, MAX_MESSAGES_PER_CHANNEL);
		return false;
	}

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
				Dimension.forgetChannelDimensions(channelId);
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
		if (
			!didChannelSelect &&
			currentMessages &&
			currentMessages.length === 0 &&
			!currentMessages.ready &&
			!currentMessages.loadingMore
		) {
			this.commitMessages(currentMessages.withPatch({loadingMore: true}));
			MessageCommands.fetchMessages(selectedChannelId, null, null, MAX_MESSAGES_PER_CHANNEL);
			this.notifyChange();
			return true;
		}
		return didChannelSelect;
	}

	handleLoadMessages(action: {channelId: string; jump?: JumpOptions}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		this.commitMessages(messages.beginLoad(action.jump));
		this.notifyChange();
		return false;
	}

	handleLoadMessagesSuccessCached(action: {
		channelId: string;
		jump?: JumpOptions;
		before?: string;
		after?: string;
		limit: number;
	}): boolean {
		let messages = ChannelMessages.getOrCreate(action.channelId);
		if (action.jump?.present) {
			messages = messages.jumpToLiveEdge(action.limit);
		} else if (action.jump?.messageId) {
			messages = messages.jumpToMessage({
				messageId: action.jump.messageId,
				flash: action.jump.flash,
				offset: action.jump.offset,
				returnToMessageId: action.jump.returnToMessageId,
				returnChannelId: action.jump.returnChannelId,
				returnGuildId: action.jump.returnGuildId,
				jumpType: action.jump.jumpType,
			});
		} else if (action.before || action.after) {
			messages = messages.drainBufferedSide(action.before != null, action.limit);
		}
		this.commitMessages(messages);
		this.notifyChange();
		return false;
	}

	handleLoadMessagesSuccess(action: {
		channelId: string;
		isBefore?: boolean;
		isAfter?: boolean;
		jump?: JumpOptions;
		hasMoreBefore?: boolean;
		hasMoreAfter?: boolean;
		cached?: boolean;
		reconcileSeeded?: {readonly around: boolean; readonly newer: boolean; readonly through?: string};
		messages: Array<WireMessage>;
	}): boolean {
		const existing = ChannelMessages.getOrCreate(action.channelId);
		const retained = this.currentDeferredOlder(action.channelId);
		const held = this.bootRequests.has(action.channelId) ? [] : this.takeHeldMessages(action.channelId);
		let loaded: ChannelMessages;
		if (action.reconcileSeeded != null && existing.ready && action.jump == null) {
			loaded = action.reconcileSeeded.newer
				? existing.mergeSeededNewer(action.messages, action.hasMoreAfter === true)
				: existing.reconcileSeededWindow({
						windowMessages: action.messages,
						hasMoreBefore: action.hasMoreBefore ?? false,
						hasMoreAfter: action.hasMoreAfter ?? false,
						around: action.reconcileSeeded.around,
						through: action.reconcileSeeded.through,
					});
		} else {
			loaded = existing.applyLoadedWindow({
				windowMessages: action.messages,
				isBefore: action.isBefore,
				isAfter: action.isAfter,
				jump: action.jump,
				hasMoreBefore: action.hasMoreBefore,
				hasMoreAfter: action.hasMoreAfter,
				cached: action.cached,
			});
		}
		let messages = this.applyIncomingMessages(loaded, held);
		let older: ReadonlyArray<WireMessage> | null = null;
		let olderHasMoreBefore = retained?.hasMoreBefore ?? true;
		const firstMounted = existing.first();
		if (retained !== null && firstMounted !== undefined && action.reconcileSeeded?.through !== undefined) {
			const deferred = messages.toArray().filter((message) => SnowflakeUtils.compare(message.id, firstMounted.id) < 0);
			if (deferred.length > 0) {
				older = deferred.map((message) => message.toBootStateJSON());
				olderHasMoreBefore = messages.hasMoreBefore;
				messages = messages.removeIds(deferred.map((message) => message.id)).withPatch({hasMoreBefore: true});
			}
		} else if (retained !== null && action.reconcileSeeded?.newer && messages.first()?.id === firstMounted?.id) {
			older = retained.older;
		}
		if (action.reconcileSeeded != null || (!action.isBefore && !action.isAfter)) {
			this.seededAnchors.delete(action.channelId);
			this.deferredOlder.delete(action.channelId);
		}
		const committed = this.commitMessages(messages);
		if (older !== null) this.deferSeededOlderMessages(committed, older, olderHasMoreBefore, retained?.tops ?? null);
		this.notifyChange();
		if (action.cached === true) {
			this.refetchStaleWindow(action.channelId, {limit: MAX_MESSAGES_PER_CHANNEL, jump: action.jump});
		}
		return false;
	}

	private refetchStaleWindow(channelId: string, request: StaleWindowRefetch): boolean {
		if (!GatewaySessions.foregroundConnection.isConnected || SelectedChannel.currentChannelId !== channelId) {
			return false;
		}
		const lastAttemptAt = staleWindowRefetchedAt.get(channelId) ?? 0;
		const now = Date.now();
		if (now - lastAttemptAt < STALE_WINDOW_REFETCH_INTERVAL_MS) {
			return false;
		}
		staleWindowRefetchedAt.set(channelId, now);
		const seeded = request.seededAnchorMessageId !== undefined;
		if (!seeded) {
			MessageCommands.fetchMessages(channelId, null, null, request.limit, request.jump, {staleRefetch: true});
			return true;
		}
		void this.reconcileSeededWindow(channelId, request.limit, request.jump, request.seededAnchorMessageId ?? null);
		return true;
	}

	private reconcileSeededWindow(
		channelId: string,
		limit: number,
		jump: JumpOptions | undefined,
		aroundMessageId: string | null,
		prefetch = false,
	): Promise<unknown> {
		const generation = this.lastSeededReconcileGeneration + 1;
		this.lastSeededReconcileGeneration = generation;
		this.seededReconcileGenerations.set(channelId, generation);
		this.seededAnchors.delete(channelId);
		this.holdIncomingMessages(channelId);
		this.reconcilingChannelIds.add(channelId);
		if (prefetch) {
			this.prefetchingSeededChannelIds.add(channelId);
		}
		return MessageCommands.fetchMessages(channelId, null, null, limit, jump, {
			staleRefetch: true,
			reconcileSeeded: {around: aroundMessageId, generation},
		}).finally(() => {
			if (!this.isSeededReconcileCurrent(channelId, generation)) {
				return;
			}
			this.seededReconcileGenerations.delete(channelId);
			this.reconcilingChannelIds.delete(channelId);
			this.prefetchingSeededChannelIds.delete(channelId);
			this.releaseHeldMessages(channelId);
		});
	}

	isSeededReconcileCurrent(channelId: string, generation: number): boolean {
		return this.seededReconcileGenerations.get(channelId) === generation;
	}

	isReconcilingSeededWindow(channelId: string): boolean {
		return this.reconcilingChannelIds.has(channelId);
	}

	isRestoredWindowPending(channelId: string): boolean {
		if (this.reconcilingChannelIds.has(channelId)) return true;
		return this.seededAnchors.has(channelId) && ChannelMessages.get(channelId)?.cached === true;
	}

	private holdIncomingMessages(channelId: string): void {
		if (!this.heldIncoming.has(channelId)) {
			this.heldIncoming.set(channelId, []);
		}
	}

	private takeHeldMessages(channelId: string): Array<WireMessage> {
		const held = this.heldIncoming.get(channelId) ?? [];
		this.heldIncoming.delete(channelId);
		return held;
	}

	private applyIncomingMessages(messages: ChannelMessages, incoming: ReadonlyArray<WireMessage>): ChannelMessages {
		const pinnedToEnd = Dimension.channelPinnedToEnd(messages.channelId);
		return incoming.reduce((next, message) => next.applyIncomingMessage(message, pinnedToEnd), messages);
	}

	private forgetHeldMessages(channelId: string, ids: ReadonlyArray<string>): void {
		this.patchDeferredOlderMessages(channelId, (message) => (ids.includes(message.id) ? null : message));
		const held = this.heldIncoming.get(channelId);
		if (held === undefined) {
			return;
		}
		this.heldIncoming.set(
			channelId,
			held.filter((message) => !ids.includes(message.id)),
		);
	}

	private releaseHeldMessages(channelId: string): void {
		const held = this.takeHeldMessages(channelId);
		const existing = ChannelMessages.get(channelId);
		if (held.length === 0 || !existing?.ready) {
			return;
		}
		this.commitMessages(this.applyIncomingMessages(existing, held));
		this.notifyChange();
	}

	handleLoadMessagesFailure(action: {channelId: string}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		this.commitMessages(messages.withPatch({loadingMore: false, error: true}));
		this.notifyChange();
		return false;
	}

	handleLoadMessagesBlocked(action: {channelId: string}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		if (!messages.loadingMore && !messages.error) {
			return false;
		}
		this.commitMessages(messages.withPatch({loadingMore: false, error: false}));
		this.notifyChange();
		return true;
	}

	handleIncomingMessage(action: {channelId: string; message: WireMessage}): boolean {
		Channels.handleMessageCreate({message: action.message});
		const existing = ChannelMessages.get(action.channelId);
		const held = this.heldIncoming.get(action.channelId);
		const confirmsPending = action.message.nonce != null && existing?.has(action.message.nonce) === true;
		if (held !== undefined && !confirmsPending) {
			held.push(action.message);
			return false;
		}
		if (!existing?.ready || (this.seededAnchors.has(action.channelId) && !confirmsPending)) {
			return false;
		}
		const updated = existing.applyIncomingMessage(action.message, Dimension.channelPinnedToEnd(action.channelId));
		this.commitMessages(updated);
		this.notifyChange();
		return false;
	}

	handleSendFailed(action: {channelId: string; nonce: string}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing?.has(action.nonce)) return false;
		const updated = existing.update(action.nonce, (message) => message.withUpdates({state: MessageStates.FAILED}));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	handleSendRetry(action: {channelId: string; messageId: string}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing?.has(action.messageId)) return false;
		const updated = existing.update(action.messageId, (message) => message.withUpdates({state: MessageStates.SENDING}));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	handleMessageDelete(action: {id: string; channelId: string}): boolean {
		this.forgetHeldMessages(action.channelId, [action.id]);
		const existing = ChannelMessages.get(action.channelId);
		if (!existing?.has(action.id)) {
			return false;
		}
		let messages = existing;
		if (messages.unblurredMessageId === action.id) {
			const messageAfter = messages.nextAfter(action.id);
			messages = messages.withPatch({unblurredMessageId: messageAfter?.id ?? null});
		}
		messages = messages.remove(action.id);
		this.commitMessages(messages);
		this.notifyChange();
		return true;
	}

	clearReferencedMessage(channelId: string, messageId: string): void {
		this.patchDeferredOlderMessages(channelId, (message) =>
			message.id === messageId ? {...message, referenced_message: undefined} : message,
		);
		const existing = ChannelMessages.get(channelId);
		if (!existing?.get(messageId)?.referencedMessage) return;
		this.commitMessages(existing.update(messageId, (message) => message.withoutReferencedMessage()));
		this.notifyChange();
	}

	handleMessageDeleteBulk(action: {ids: Array<string>; channelId: string}): boolean {
		this.forgetHeldMessages(action.channelId, action.ids);
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;
		let messages = existing.removeIds(action.ids);
		if (messages === existing) return false;
		if (messages.unblurredMessageId != null && action.ids.includes(messages.unblurredMessageId)) {
			const after = messages.nextAfter(messages.unblurredMessageId);
			messages = messages.withPatch({unblurredMessageId: after?.id ?? null});
		}
		this.commitMessages(messages);
		this.notifyChange();
		return true;
	}

	handleMessageUpdate(action: {message: WireMessage}): boolean {
		const messageId = action.message.id;
		const channelId = action.message.channel_id;
		const deferredChanged = this.patchDeferredOlderMessages(channelId, (message) => {
			if (message.id !== messageId) return message;
			if ('reactions' in action.message) MessageReactions.hydrateMessageReactions(messageId, action.message.reactions);
			if (action.message.author && !message.webhook_id) Users.cacheUsers([action.message.author]);
			return {...message, ...action.message};
		});
		const existing = ChannelMessages.get(channelId);
		if (!existing?.has(messageId)) return deferredChanged;
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

	forgetUnfocusedVisitChannel(channelId: string): void {
		if (SelectedChannel.currentChannelId === channelId) return;
		this.clearMessages(channelId);
		this.notifyChange();
	}

	handleGuildThreadsPurged(guildId: string): void {
		ChannelMessages.forEach(({channelId}) => {
			if (Channels.getChannel(channelId)?.guildId === guildId) {
				this.clearMessages(channelId);
				Dimension.forgetChannelDimensions(channelId);
			}
		});
	}

	handleCleanup(): boolean {
		ChannelMessages.forEach(({channelId}) => {
			if (Channels.getChannel(channelId) == null) {
				this.clearMessages(channelId);
				Dimension.forgetChannelDimensions(channelId);
			}
		});
		this.notifyChange();
		return false;
	}

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

	handleMessageReveal(action: {channelId: string; messageId: string | null}): boolean {
		const messages = ChannelMessages.getOrCreate(action.channelId);
		this.commitMessages(messages.withPatch({unblurredMessageId: action.messageId}));
		this.notifyChange();
		return true;
	}

	handleClearJumpTarget(action: {channelId: string; clearReturnTarget?: boolean}): boolean {
		const messages = ChannelMessages.get(action.channelId);
		if (
			messages &&
			(messages.jumpDestinationId != null ||
				messages.hasJumped ||
				(action.clearReturnTarget === true && messages.jumpReturnMessageId != null))
		) {
			this.commitMessages(messages.clearJumpTarget({clearReturnTarget: action.clearReturnTarget}));
			this.notifyChange();
			return true;
		}
		return false;
	}

	handleReaction(action: {
		type: 'MESSAGE_REACTION_ADD' | 'MESSAGE_REACTION_REMOVE';
		channelId: string;
		messageId: string;
		userId: string;
		emoji: ReactionEmoji;
		optimistic?: boolean;
	}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;
		const currentUser = Users.getCurrentUser();
		const isCurrentUser = currentUser?.id === action.userId;
		if (action.optimistic && !isCurrentUser) return false;
		const updated = existing.update(action.messageId, (message) => message.withUpdates({}));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	handleRemoveAllReactions(action: {channelId: string; messageId: string}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;
		const updated = existing.update(action.messageId, (message) => message.withUpdates({reactions: []}));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	handleRemoveReactionEmoji(action: {channelId: string; messageId: string; emoji: ReactionEmoji}): boolean {
		const existing = ChannelMessages.get(action.channelId);
		if (!existing) return false;
		const updated = existing.update(action.messageId, (message) => message.withoutReactionEmoji(action.emoji));
		this.commitMessages(updated);
		this.notifyChange();
		return true;
	}

	handleMessagePreload(action: {messages: Record<ChannelId, WireMessage>}): boolean {
		let hasChanges = false;
		for (const [channelId, messageData] of Object.entries(action.messages)) {
			if (!messageData?.id || !messageData.author) continue;
			Channels.handleMessageCreate({message: messageData});
			const channelMessages = ChannelMessages.getOrCreate(channelId);
			if (!channelMessages.has(messageData.id)) {
				this.commitMessages(channelMessages.applyIncomingMessage(messageData, false));
				hasChanges = true;
			}
		}
		if (hasChanges) {
			this.notifyChange();
		}
		return hasChanges;
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.version,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

const messages = new Messages();

ResettableStates.register(messages, () => messages.handleSessionInvalidated());

AccountScopedWork.registerTransition({
	suspend: () => {},
	resume: () => {},
	released: () => {
		if (GatewaySessions.foregroundConnection.isReady) {
			messages.handleGatewayReady();
		}
	},
});

export default messages;
