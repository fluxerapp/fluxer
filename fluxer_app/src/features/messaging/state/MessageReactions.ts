// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {
	emptyMap,
	mapToReactions,
	type ReactionMachineEvent,
	type ReactionMap,
	transitionReactionMap,
} from '@app/features/messaging/state/ReactionStateMachine';
import {
	createReactionUsersSnapshot,
	type FetchStatus,
	getReactionUsersFetchStatus,
	type ReactionUsersMachineEvent,
	type ReactionUsersMachineSnapshot,
	transitionReactionUsersSnapshot,
} from '@app/features/messaging/state/ReactionUsersStateMachine';
import {getReactionKey, type ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import type {MessageReaction} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export type {FetchStatus};

interface ReactorEntry {
	snapshot: ReactionUsersMachineSnapshot;
}

interface MessageReactionState {
	map: ReactionMap;
	reactions: ReadonlyArray<MessageReaction>;
	currentUserId: string | null;
	version: number;
}

type Listener = () => void;

const EMPTY_REACTIONS: ReadonlyArray<MessageReaction> = Object.freeze([]);
const EMPTY_USERS: ReadonlyArray<User> = Object.freeze([]);
const MAX_TRACKED_REACTION_REQUEST_KEYS = 512;
const createEmptyReactorEntry = (requestSerial = 0): ReactorEntry => ({
	snapshot: createReactionUsersSnapshot(requestSerial),
});

function isEmptyReactionHydration(event: ReactionMachineEvent): boolean {
	return event.type === 'reaction.hydrate' && (event.reactions == null || event.reactions.length === 0);
}

function isMissingReactionStateNoop(event: ReactionMachineEvent): boolean {
	switch (event.type) {
		case 'reaction.hydrate':
			return isEmptyReactionHydration(event);
		case 'reaction.add':
			return false;
		case 'reaction.remove':
		case 'reaction.removeAll':
		case 'reaction.removeEmoji':
		case 'reaction.trackReactor':
		case 'reaction.trackReactors':
		case 'reaction.untrackReactor':
			return true;
	}
}

export class MessageReactionsManager {
	private messageStates: Map<string, MessageReactionState> = new Map();
	private reactors: Map<string, ReactorEntry> = new Map();
	private _keysByMessage: Map<string, Set<string>> = new Map();
	private retiredReactionRequests: Map<string, number> = new Map();
	private inFlightReactionRequests: Map<string, Set<number>> = new Map();
	private messageListeners: Map<string, Set<Listener>> = new Map();
	private reactionListeners: Map<string, Set<Listener>> = new Map();
	private transactionDepth = 0;
	private pendingMessages = new Set<string>();
	private pendingReactions = new Set<string>();

	getMessageReactions(messageId: string): ReadonlyArray<MessageReaction> {
		return this.messageStates.get(messageId)?.reactions ?? EMPTY_REACTIONS;
	}

	hydrateMessageReactions(messageId: string, reactions: ReadonlyArray<MessageReaction> | null | undefined): void {
		this.commitReactionEvent(messageId, {
			type: 'reaction.hydrate',
			reactions,
			currentUserId: Authentication.currentUserId,
		});
	}

	replaceMessageReactions(messageId: string, reactions: ReadonlyArray<MessageReaction> | null | undefined): void {
		this.commitReactionEvent(messageId, {
			type: 'reaction.hydrate',
			reactions,
			currentUserId: Authentication.currentUserId,
		});
	}

	private commitReactionEvent(messageId: string, event: ReactionMachineEvent): void {
		const current = this.messageStates.get(messageId);
		if (!current && isMissingReactionStateNoop(event)) {
			if (isEmptyReactionHydration(event)) this.pruneAllReactionEntries(messageId);
			return;
		}
		const currentUserId = current?.currentUserId ?? Authentication.currentUserId;
		const nextMap = transitionReactionMap(current?.map ?? emptyMap(), event, currentUserId);
		const nextCurrentUserId =
			event.type === 'reaction.hydrate' ? (event.currentUserId ?? currentUserId) : currentUserId;
		this.commitMap(messageId, nextMap, nextCurrentUserId);
	}

	private commitMap(messageId: string, nextMap: ReactionMap, currentUserId: string | null): void {
		const current = this.messageStates.get(messageId);
		if (current && current.map === nextMap) {
			if (current.currentUserId !== currentUserId) current.currentUserId = currentUserId;
			return;
		}
		if (nextMap.size === 0) {
			this.pruneAllReactionEntries(messageId);
			if (!current) return;
			this.messageStates.delete(messageId);
			this.queueMessageNotify(messageId);
			return;
		}
		const nextReactions = mapToReactions(nextMap);
		this.pruneReactionEntries(messageId, nextMap);
		this.messageStates.set(messageId, {
			map: nextMap,
			reactions: nextReactions,
			currentUserId,
			version: (current?.version ?? 0) + 1,
		});
		this.queueMessageNotify(messageId);
	}

	getReactionEntry(messageId: string, emoji: ReactionEmoji): ReactorEntry | undefined {
		return this.reactors.get(getReactionKey(messageId, emoji));
	}

	getReactions(messageId: string, emoji: ReactionEmoji): ReadonlyArray<User> {
		return this.getReactionEntry(messageId, emoji)?.snapshot.context.userSnapshot ?? EMPTY_USERS;
	}

	getFetchStatus(messageId: string, emoji: ReactionEmoji): FetchStatus {
		const entry = this.getReactionEntry(messageId, emoji);
		return entry ? getReactionUsersFetchStatus(entry.snapshot) : 'idle';
	}

	getHasMore(messageId: string, emoji: ReactionEmoji): boolean {
		return this.getReactionEntry(messageId, emoji)?.snapshot.context.hasMore ?? true;
	}

	getLastUserId(messageId: string, emoji: ReactionEmoji): string | null {
		return this.getReactionEntry(messageId, emoji)?.snapshot.context.lastUserId ?? null;
	}

	getInitialFetchLimit(messageId: string, emoji: ReactionEmoji): number {
		return this.getReactionEntry(messageId, emoji)?.snapshot.context.initialFetchLimit ?? 0;
	}

	getReactionVersion(messageId: string, emoji: ReactionEmoji): number {
		return this.getReactionEntry(messageId, emoji)?.snapshot.context.version ?? 0;
	}

	private getOrCreateReactorEntry(messageId: string, emoji: ReactionEmoji): ReactorEntry {
		const key = getReactionKey(messageId, emoji);
		let entry = this.reactors.get(key);
		if (!entry) {
			entry = createEmptyReactorEntry(this.retiredReactionRequests.get(key) ?? 0);
			this.reactors.set(key, entry);
			let keys = this._keysByMessage.get(messageId);
			if (!keys) {
				keys = new Set();
				this._keysByMessage.set(messageId, keys);
			}
			keys.add(key);
		}
		return entry;
	}

	handleConnectionOpen(): void {
		const messageIds = Array.from(this.messageStates.keys());
		const reactionKeys = Array.from(this.reactors.keys());
		for (const key of reactionKeys) this.retireReactionEntry(key);
		this.messageStates.clear();
		this.reactors.clear();
		this._keysByMessage.clear();
		this.batch(() => {
			for (const messageId of messageIds) this.queueMessageNotify(messageId);
			for (const key of reactionKeys) this.queueReactionNotify(key);
		});
	}

	batch(run: () => void): void {
		this.transactionDepth += 1;
		try {
			run();
		} finally {
			this.transactionDepth -= 1;
			if (this.transactionDepth === 0) this.flushNotifications();
		}
	}

	handleReactionAdd(
		messageId: string,
		userId: string,
		emoji: ReactionEmoji,
		isCurrentUser = Authentication.currentUserId === userId,
	): void {
		this.commitReactionEvent(messageId, {type: 'reaction.add', emoji, userId, isCurrentUser});
		const user = Users.getUser(userId);
		if (user) {
			this.commitReactorEvent(messageId, emoji, this.getOrCreateReactorEntry(messageId, emoji), {
				type: 'user.add',
				user,
			});
		}
	}

	handleReactionRemove(
		messageId: string,
		userId: string,
		emoji: ReactionEmoji,
		isCurrentUser = Authentication.currentUserId === userId,
	): void {
		this.commitReactionEvent(messageId, {type: 'reaction.remove', emoji, userId, isCurrentUser});
		const entry = this.getReactionEntry(messageId, emoji);
		if (entry) this.commitReactorEvent(messageId, emoji, entry, {type: 'user.remove', userId});
	}

	handleReactionRemoveAll(messageId: string): void {
		this.commitReactionEvent(messageId, {type: 'reaction.removeAll'});
		this.pruneAllReactionEntries(messageId);
	}

	handleReactionRemoveEmoji(messageId: string, emoji: ReactionEmoji): void {
		this.commitReactionEvent(messageId, {type: 'reaction.removeEmoji', emoji});
		const key = getReactionKey(messageId, emoji);
		if (!this.reactors.has(key) && !this._keysByMessage.get(messageId)?.has(key)) return;
		this.retireReactionEntry(key);
		const keys = this._keysByMessage.get(messageId);
		if (keys) {
			keys.delete(key);
			if (keys.size === 0) this._keysByMessage.delete(messageId);
		}
		this.queueReactionNotify(key);
	}

	handleFetchPending(messageId: string, emoji: ReactionEmoji): number {
		const entry = this.getOrCreateReactorEntry(messageId, emoji);
		this.commitReactorEvent(messageId, emoji, entry, {type: 'fetch.pending'});
		const requestId = entry.snapshot.context.activeRequestId ?? entry.snapshot.context.requestSerial;
		this.trackInFlightRequest(getReactionKey(messageId, emoji), requestId);
		return requestId;
	}

	handleFetchSuccess(
		messageId: string,
		users: ReadonlyArray<UserPartial>,
		emoji: ReactionEmoji,
		requestedLimit?: number,
		responseHasMore?: boolean,
		totalCount?: number,
		requestId?: number,
		nextAfter?: string | null,
	): void {
		const key = getReactionKey(messageId, emoji);
		const ignored = this.shouldIgnoreFetchResult(key, requestId);
		this.settleInFlightRequest(key, requestId);
		if (ignored) return;
		const entry = this.getOrCreateReactorEntry(messageId, emoji);
		Users.cacheUsers(users.slice());
		this.commitReactorEvent(messageId, emoji, entry, {
			type: 'fetch.success',
			mode: 'replace',
			users,
			requestedLimit,
			responseHasMore,
			totalCount,
			requestId,
			nextAfter,
		});
		this.trackReactors(messageId, emoji, users);
	}

	handleFetchAppend(
		messageId: string,
		users: ReadonlyArray<UserPartial>,
		emoji: ReactionEmoji,
		requestedLimit?: number,
		responseHasMore?: boolean,
		totalCount?: number,
		requestId?: number,
		nextAfter?: string | null,
	): void {
		const key = getReactionKey(messageId, emoji);
		const ignored = this.shouldIgnoreFetchResult(key, requestId);
		this.settleInFlightRequest(key, requestId);
		if (ignored) return;
		const entry = this.getOrCreateReactorEntry(messageId, emoji);
		Users.cacheUsers(users.slice());
		this.commitReactorEvent(messageId, emoji, entry, {
			type: 'fetch.success',
			mode: 'append',
			users,
			requestedLimit,
			responseHasMore,
			totalCount,
			requestId,
			nextAfter,
		});
		this.trackReactors(messageId, emoji, users);
	}

	handleFetchError(messageId: string, emoji: ReactionEmoji, requestId?: number): void {
		const key = getReactionKey(messageId, emoji);
		const ignored = this.shouldIgnoreFetchResult(key, requestId);
		this.settleInFlightRequest(key, requestId);
		if (ignored) return;
		this.commitReactorEvent(messageId, emoji, this.getOrCreateReactorEntry(messageId, emoji), {
			type: 'fetch.error',
			requestId,
		});
	}

	subscribeMessage(messageId: string, listener: Listener): () => void {
		return this.subscribeTo(this.messageListeners, messageId, listener);
	}

	subscribeReaction(messageId: string, emoji: ReactionEmoji, listener: Listener): () => void {
		return this.subscribeTo(this.reactionListeners, getReactionKey(messageId, emoji), listener);
	}

	private trackReactors(messageId: string, emoji: ReactionEmoji, users: ReadonlyArray<UserPartial>): void {
		if (users.length === 0 || !this.messageStates.has(messageId)) return;
		this.commitReactionEvent(messageId, {
			type: 'reaction.trackReactors',
			emoji,
			userIds: users.map((user) => user.id),
		});
	}

	private commitReactorEvent(
		messageId: string,
		emoji: ReactionEmoji,
		entry: ReactorEntry,
		event: ReactionUsersMachineEvent,
	): void {
		const previousVersion = entry.snapshot.context.version;
		const nextSnapshot = transitionReactionUsersSnapshot(entry.snapshot, event);
		if (nextSnapshot === entry.snapshot || nextSnapshot.context.version === previousVersion) return;
		entry.snapshot = nextSnapshot;
		this.queueReactionNotify(getReactionKey(messageId, emoji));
	}

	private pruneReactionEntries(messageId: string, nextMap: ReactionMap): void {
		const keys = this._keysByMessage.get(messageId);
		if (!keys) return;
		const retainedKeys = new Set<string>();
		for (const record of nextMap.values()) {
			retainedKeys.add(getReactionKey(messageId, record.emoji));
		}
		for (const key of Array.from(keys)) {
			if (retainedKeys.has(key)) continue;
			keys.delete(key);
			this.retireReactionEntry(key);
			this.queueReactionNotify(key);
		}
		if (keys.size === 0) this._keysByMessage.delete(messageId);
	}

	private pruneAllReactionEntries(messageId: string): void {
		const keys = this._keysByMessage.get(messageId);
		if (!keys) return;
		for (const key of keys) {
			this.retireReactionEntry(key);
			this.queueReactionNotify(key);
		}
		this._keysByMessage.delete(messageId);
	}

	private retireReactionEntry(key: string, entry = this.reactors.get(key)): void {
		this.reactors.delete(key);
		if (!this.inFlightReactionRequests.has(key)) {
			this.retiredReactionRequests.delete(key);
			return;
		}
		const requestSerial = entry?.snapshot.context.requestSerial ?? 0;
		const nextRequestId = Math.max(this.retiredReactionRequests.get(key) ?? 0, requestSerial);
		this.retiredReactionRequests.set(key, nextRequestId);
	}

	private trackInFlightRequest(key: string, requestId: number): void {
		const requestIds = this.inFlightReactionRequests.get(key) ?? new Set<number>();
		requestIds.add(requestId);
		this.inFlightReactionRequests.delete(key);
		this.inFlightReactionRequests.set(key, requestIds);
		while (this.inFlightReactionRequests.size > MAX_TRACKED_REACTION_REQUEST_KEYS) {
			const oldest = this.inFlightReactionRequests.keys().next();
			if (oldest.done) break;
			this.inFlightReactionRequests.delete(oldest.value);
			this.retiredReactionRequests.delete(oldest.value);
		}
	}

	private settleInFlightRequest(key: string, requestId?: number): void {
		if (requestId == null) return;
		const requestIds = this.inFlightReactionRequests.get(key);
		if (!requestIds) return;
		requestIds.delete(requestId);
		if (requestIds.size > 0) return;
		this.inFlightReactionRequests.delete(key);
		this.retiredReactionRequests.delete(key);
	}

	private shouldIgnoreFetchResult(key: string, requestId?: number): boolean {
		const retiredRequestId = this.retiredReactionRequests.get(key);
		return requestId != null && retiredRequestId != null && requestId <= retiredRequestId;
	}

	private subscribeTo(map: Map<string, Set<Listener>>, key: string, listener: Listener): () => void {
		let listeners = map.get(key);
		if (!listeners) {
			listeners = new Set();
			map.set(key, listeners);
		}
		listeners.add(listener);
		return () => {
			const current = map.get(key);
			if (!current) return;
			current.delete(listener);
			if (current.size === 0) map.delete(key);
		};
	}

	private queueMessageNotify(messageId: string): void {
		this.pendingMessages.add(messageId);
		if (this.transactionDepth === 0) this.flushNotifications();
	}

	private queueReactionNotify(key: string): void {
		this.pendingReactions.add(key);
		if (this.transactionDepth === 0) this.flushNotifications();
	}

	private flushNotifications(): void {
		const messageIds = Array.from(this.pendingMessages);
		const reactionKeys = Array.from(this.pendingReactions);
		this.pendingMessages.clear();
		this.pendingReactions.clear();
		for (const messageId of messageIds) {
			const listeners = this.messageListeners.get(messageId);
			if (listeners) for (const listener of Array.from(listeners)) listener();
		}
		for (const key of reactionKeys) {
			const listeners = this.reactionListeners.get(key);
			if (listeners) for (const listener of Array.from(listeners)) listener();
		}
	}
}

export default new MessageReactionsManager();
