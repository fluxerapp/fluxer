// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {
	emptyMap,
	mapToAnswerCounts,
	type PollVoteMachineEvent,
	type PollVoteMap,
	transitionPollVoteMap,
} from '@app/features/messaging/state/PollVoteStateMachine';
import {
	createPollVoteUsersSnapshot,
	type FetchStatus,
	getPollVoteUsersFetchStatus,
	type PollVoteUsersMachineEvent,
	type PollVoteUsersMachineSnapshot,
	transitionPollVoteUsersSnapshot,
} from '@app/features/messaging/state/PollVoteUsersStateMachine';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import type {MessagePollAnswerCount} from '@fluxer/schema/src/domains/message/PollSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export type {FetchStatus};

interface VoterEntry {
	snapshot: PollVoteUsersMachineSnapshot;
}

interface MessagePollVoteState {
	map: PollVoteMap;
	votes: ReadonlyArray<MessagePollAnswerCount>;
	currentUserId: string | null;
	version: number;
}

type Listener = () => void;

export function getPollVoteKey(messageId: string, answerId: number) {
	return `${messageId}:${answerId}`;
}

const EMPTY_VOTES: ReadonlyArray<MessagePollAnswerCount> = Object.freeze([]);
const EMPTY_USERS: ReadonlyArray<User> = Object.freeze([]);
const createEmptyVoterEntry = (requestSerial = 0): VoterEntry => ({
	snapshot: createPollVoteUsersSnapshot(requestSerial),
});

function isEmptyPollVoteHydration(event: PollVoteMachineEvent): boolean {
	return event.type === 'pollVote.hydrate' && (event.votes == null || event.votes.length === 0);
}

function isMissingPollVoteStateNoop(event: PollVoteMachineEvent): boolean {
	switch (event.type) {
		case 'pollVote.hydrate':
			return isEmptyPollVoteHydration(event);
		case 'pollVote.add':
			return false;
		case 'pollVote.remove':
		case 'pollVote.trackVoter':
		case 'pollVote.trackVoters':
		case 'pollVote.untrackVoter':
			return true;
	}
}

export class MessagePollVotesManager {
	private messageStates: Map<string, MessagePollVoteState> = new Map();
	private voters: Map<string, VoterEntry> = new Map();
	private _keysByMessage: Map<string, Set<string>> = new Map();
	private retiredPollVoteRequests: Map<string, number> = new Map();
	private messageListeners: Map<string, Set<Listener>> = new Map();
	private reactionListeners: Map<string, Set<Listener>> = new Map();
	private transactionDepth = 0;
	private pendingMessages = new Set<string>();
	private pendingPollVotes = new Set<string>();

	getMessagePollVotes(messageId: string): ReadonlyArray<MessagePollAnswerCount> {
		return this.messageStates.get(messageId)?.votes ?? EMPTY_VOTES;
	}

	hydrateMessagePollVotes(messageId: string, votes: ReadonlyArray<MessagePollAnswerCount> | null | undefined): void {
		this.commitPollVoteEvent(messageId, {
			type: 'pollVote.hydrate',
			votes,
			currentUserId: Authentication.currentUserId,
		});
	}

	replaceMessagePollVotes(messageId: string, votes: ReadonlyArray<MessagePollAnswerCount> | null | undefined): void {
		this.commitPollVoteEvent(messageId, {
			type: 'pollVote.hydrate',
			votes,
			currentUserId: Authentication.currentUserId,
		});
	}

	private commitPollVoteEvent(messageId: string, event: PollVoteMachineEvent): void {
		const current = this.messageStates.get(messageId);
		if (!current && isMissingPollVoteStateNoop(event)) {
			if (isEmptyPollVoteHydration(event)) this.pruneAllPollVoteEntries(messageId);
			return;
		}
		const currentUserId = current?.currentUserId ?? Authentication.currentUserId;
		const nextMap = transitionPollVoteMap(current?.map ?? emptyMap(), event, currentUserId);
		const nextCurrentUserId =
			event.type === 'pollVote.hydrate' ? (event.currentUserId ?? currentUserId) : currentUserId;
		this.commitMap(messageId, nextMap, nextCurrentUserId);
	}

	private commitMap(messageId: string, nextMap: PollVoteMap, currentUserId: string | null): void {
		const current = this.messageStates.get(messageId);
		if (current && current.map === nextMap) {
			if (current.currentUserId !== currentUserId) current.currentUserId = currentUserId;
			return;
		}
		if (nextMap.size === 0) {
			this.pruneAllPollVoteEntries(messageId);
			if (!current) return;
			this.messageStates.delete(messageId);
			this.queueMessageNotify(messageId);
			return;
		}
		const nextPollVotes = mapToAnswerCounts(nextMap);
		this.prunePollVoteEntries(messageId, nextMap);
		this.messageStates.set(messageId, {
			map: nextMap,
			votes: nextPollVotes,
			currentUserId,
			version: (current?.version ?? 0) + 1,
		});
		this.queueMessageNotify(messageId);
	}

	getPollVoteEntry(messageId: string, answerId: number): VoterEntry | undefined {
		return this.voters.get(getPollVoteKey(messageId, answerId));
	}

	getPollVotes(messageId: string, answerId: number): ReadonlyArray<User> {
		return this.getPollVoteEntry(messageId, answerId)?.snapshot.context.userSnapshot ?? EMPTY_USERS;
	}

	getFetchStatus(messageId: string, answerId: number): FetchStatus {
		const entry = this.getPollVoteEntry(messageId, answerId);
		return entry ? getPollVoteUsersFetchStatus(entry.snapshot) : 'idle';
	}

	getHasMore(messageId: string, answerId: number): boolean {
		return this.getPollVoteEntry(messageId, answerId)?.snapshot.context.hasMore ?? true;
	}

	getLastUserId(messageId: string, answerId: number): string | null {
		return this.getPollVoteEntry(messageId, answerId)?.snapshot.context.lastUserId ?? null;
	}

	getInitialFetchLimit(messageId: string, answerId: number): number {
		return this.getPollVoteEntry(messageId, answerId)?.snapshot.context.initialFetchLimit ?? 0;
	}

	getPollVoteVersion(messageId: string, answerId: number): number {
		return this.getPollVoteEntry(messageId, answerId)?.snapshot.context.version ?? 0;
	}

	private getOrCreateVoterEntry(messageId: string, answerId: number): VoterEntry {
		const key = getPollVoteKey(messageId, answerId);
		let entry = this.voters.get(key);
		if (!entry) {
			entry = createEmptyVoterEntry(this.retiredPollVoteRequests.get(key) ?? 0);
			this.voters.set(key, entry);
			let keys = this._keysByMessage.get(messageId);
			if (!keys) {
				keys = new Set();
				this._keysByMessage.set(messageId, keys);
			}
			keys.add(key);
		}
		return entry;
	}

	handleGatewayReady(): void {
		const messageIds = Array.from(this.messageStates.keys());
		const reactionKeys = Array.from(this.voters.keys());
		for (const key of reactionKeys) this.retirePollVoteEntry(key);
		this.messageStates.clear();
		this.voters.clear();
		this._keysByMessage.clear();
		this.batch(() => {
			for (const messageId of messageIds) this.queueMessageNotify(messageId);
			for (const key of reactionKeys) this.queuePollVoteNotify(key);
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

	handlePollVoteAdd(
		messageId: string,
		userId: string,
		answerId: number,
		isCurrentUser = Authentication.currentUserId === userId,
	): void {
		this.commitPollVoteEvent(messageId, {type: 'pollVote.add', answerId, userId, isCurrentUser});
		const user = Users.getUser(userId);
		if (user) {
			this.commitVoterEvent(messageId, answerId, this.getOrCreateVoterEntry(messageId, answerId), {
				type: 'user.add',
				user,
			});
		}
	}

	handlePollVoteRemove(
		messageId: string,
		userId: string,
		answerId: number,
		isCurrentUser = Authentication.currentUserId === userId,
	): void {
		this.commitPollVoteEvent(messageId, {type: 'pollVote.remove', answerId, userId, isCurrentUser});
		const entry = this.getPollVoteEntry(messageId, answerId);
		if (entry) this.commitVoterEvent(messageId, answerId, entry, {type: 'user.remove', userId});
	}

	handleFetchPending(messageId: string, answerId: number): number {
		const entry = this.getOrCreateVoterEntry(messageId, answerId);
		this.commitVoterEvent(messageId, answerId, entry, {type: 'fetch.pending'});
		return entry.snapshot.context.activeRequestId ?? entry.snapshot.context.requestSerial;
	}

	handleFetchSuccess(
		messageId: string,
		users: ReadonlyArray<UserPartial>,
		answerId: number,
		requestedLimit?: number,
		responseHasMore?: boolean,
		totalCount?: number,
		requestId?: number,
		nextAfter?: string | null,
	): void {
		const key = getPollVoteKey(messageId, answerId);
		if (this.shouldIgnoreFetchResult(key, requestId)) return;
		const entry = this.getOrCreateVoterEntry(messageId, answerId);
		Users.cacheUsers(users.slice());
		this.commitVoterEvent(messageId, answerId, entry, {
			type: 'fetch.success',
			mode: 'replace',
			users,
			requestedLimit,
			responseHasMore,
			totalCount,
			requestId,
			nextAfter,
		});
		this.trackVoters(messageId, answerId, users);
	}

	handleFetchAppend(
		messageId: string,
		users: ReadonlyArray<UserPartial>,
		answerId: number,
		requestedLimit?: number,
		responseHasMore?: boolean,
		totalCount?: number,
		requestId?: number,
		nextAfter?: string | null,
	): void {
		const key = getPollVoteKey(messageId, answerId);
		if (this.shouldIgnoreFetchResult(key, requestId)) return;
		const entry = this.getOrCreateVoterEntry(messageId, answerId);
		Users.cacheUsers(users.slice());
		this.commitVoterEvent(messageId, answerId, entry, {
			type: 'fetch.success',
			mode: 'append',
			users,
			requestedLimit,
			responseHasMore,
			totalCount,
			requestId,
			nextAfter,
		});
		this.trackVoters(messageId, answerId, users);
	}

	handleFetchError(messageId: string, answerId: number, requestId?: number): void {
		const key = getPollVoteKey(messageId, answerId);
		if (this.shouldIgnoreFetchResult(key, requestId)) return;
		this.commitVoterEvent(messageId, answerId, this.getOrCreateVoterEntry(messageId, answerId), {
			type: 'fetch.error',
			requestId,
		});
	}

	subscribeMessage(messageId: string, listener: Listener): () => void {
		return this.subscribeTo(this.messageListeners, messageId, listener);
	}

	subscribePollVote(messageId: string, answerId: number, listener: Listener): () => void {
		return this.subscribeTo(this.reactionListeners, getPollVoteKey(messageId, answerId), listener);
	}

	private trackVoters(messageId: string, answerId: number, users: ReadonlyArray<UserPartial>): void {
		if (users.length === 0 || !this.messageStates.has(messageId)) return;
		this.commitPollVoteEvent(messageId, {
			type: 'pollVote.trackVoters',
			answerId,
			userIds: users.map((user) => user.id),
		});
	}

	private commitVoterEvent(
		messageId: string,
		answerId: number,
		entry: VoterEntry,
		event: PollVoteUsersMachineEvent,
	): void {
		const previousVersion = entry.snapshot.context.version;
		const nextSnapshot = transitionPollVoteUsersSnapshot(entry.snapshot, event);
		if (nextSnapshot === entry.snapshot || nextSnapshot.context.version === previousVersion) return;
		entry.snapshot = nextSnapshot;
		this.queuePollVoteNotify(getPollVoteKey(messageId, answerId));
	}

	private prunePollVoteEntries(messageId: string, nextMap: PollVoteMap): void {
		const keys = this._keysByMessage.get(messageId);
		if (!keys) return;
		const retainedKeys = new Set<string>();
		for (const record of nextMap.values()) {
			retainedKeys.add(getPollVoteKey(messageId, record.answerId));
		}
		for (const key of Array.from(keys)) {
			if (retainedKeys.has(key)) continue;
			keys.delete(key);
			this.retirePollVoteEntry(key);
			this.queuePollVoteNotify(key);
		}
		if (keys.size === 0) this._keysByMessage.delete(messageId);
	}

	private pruneAllPollVoteEntries(messageId: string): void {
		const keys = this._keysByMessage.get(messageId);
		if (!keys) return;
		for (const key of keys) {
			this.retirePollVoteEntry(key);
			this.queuePollVoteNotify(key);
		}
		this._keysByMessage.delete(messageId);
	}

	private retirePollVoteEntry(key: string, entry = this.voters.get(key)): void {
		const requestSerial = entry?.snapshot.context.requestSerial ?? 0;
		const nextRequestId = Math.max(this.retiredPollVoteRequests.get(key) ?? 0, requestSerial);
		this.retiredPollVoteRequests.set(key, nextRequestId);
		this.voters.delete(key);
	}

	private shouldIgnoreFetchResult(key: string, requestId?: number): boolean {
		const retiredRequestId = this.retiredPollVoteRequests.get(key);
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

	private queuePollVoteNotify(key: string): void {
		this.pendingPollVotes.add(key);
		if (this.transactionDepth === 0) this.flushNotifications();
	}

	private flushNotifications(): void {
		const messageIds = Array.from(this.pendingMessages);
		const reactionKeys = Array.from(this.pendingPollVotes);
		this.pendingMessages.clear();
		this.pendingPollVotes.clear();
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

export default new MessagePollVotesManager();
