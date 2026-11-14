// SPDX-License-Identifier: AGPL-3.0-or-later

import {User} from '@app/features/user/models/User';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

interface PollVoteUsersContext {
	users: ReadonlyMap<string, User>;
	userSnapshot: ReadonlyArray<User>;
	hasMore: boolean;
	lastUserId: string | null;
	initialFetchLimit: number;
	requestSerial: number;
	newestSettledRequest: number;
	activeRequestId: number | null;
	version: number;
}

interface PollVoteUsersInput {
	requestSerial?: number;
}

export type PollVoteUsersMachineEvent =
	| {type: 'fetch.pending'}
	| {
			type: 'fetch.success';
			mode: 'replace' | 'append';
			users: ReadonlyArray<UserPartial>;
			requestedLimit?: number;
			responseHasMore?: boolean;
			totalCount?: number;
			requestId?: number;
			nextAfter?: string | null;
	  }
	| {type: 'fetch.error'; requestId?: number}
	| {type: 'user.add'; user: User}
	| {type: 'user.remove'; userId: string};

const EMPTY_USERS: ReadonlyArray<User> = Object.freeze([]);

function normalizeTotalCount(totalCount: number | undefined): number | undefined {
	if (totalCount === undefined || !Number.isFinite(totalCount)) return undefined;
	return Math.max(0, Math.floor(totalCount));
}

function inferHasMore({
	userCount,
	pageUserCount,
	requestedLimit,
	responseHasMore,
	totalCount,
}: {
	userCount: number;
	pageUserCount: number;
	requestedLimit?: number;
	responseHasMore?: boolean;
	totalCount?: number;
}): boolean {
	if (responseHasMore !== undefined) return responseHasMore;
	if (pageUserCount === 0) return false;
	const normalizedTotalCount = normalizeTotalCount(totalCount);
	if (normalizedTotalCount !== undefined) return userCount < normalizedTotalCount;
	return requestedLimit !== undefined ? pageUserCount >= requestedLimit : false;
}

function freezeUserSnapshot(users: Iterable<User>): ReadonlyArray<User> {
	const snapshot = Array.from(users);
	return snapshot.length > 0 ? Object.freeze(snapshot) : EMPTY_USERS;
}

function toUserMap(users: ReadonlyArray<UserPartial>): Map<string, User> {
	const userMap = new Map<string, User>();
	for (const userPartial of users) userMap.set(userPartial.id, new User(userPartial));
	return userMap;
}

function getLastUserId(snapshot: ReadonlyArray<User>): string | null {
	return snapshot.length > 0 ? snapshot[snapshot.length - 1].id : null;
}

function isCompleteInitialFetch({
	pageUserCount,
	responseHasMore,
	totalCount,
}: {
	pageUserCount: number;
	responseHasMore?: boolean;
	totalCount?: number;
}): boolean {
	if (responseHasMore === false) return true;
	const normalizedTotalCount = normalizeTotalCount(totalCount);
	return normalizedTotalCount !== undefined && pageUserCount >= normalizedTotalCount;
}

function mergeInitialUsers(
	context: PollVoteUsersContext,
	fetchedUsers: ReadonlyArray<UserPartial>,
	replaceExisting: boolean,
): Pick<PollVoteUsersContext, 'users' | 'userSnapshot' | 'lastUserId'> {
	if (replaceExisting) {
		const users = toUserMap(fetchedUsers);
		const userSnapshot = freezeUserSnapshot(users.values());
		return {users, userSnapshot, lastUserId: getLastUserId(userSnapshot)};
	}
	if (fetchedUsers.length === 0) {
		if (context.users.size === 0) {
			return {users: context.users, userSnapshot: EMPTY_USERS, lastUserId: null};
		}
		return {
			users: context.users,
			userSnapshot: context.userSnapshot,
			lastUserId: context.lastUserId,
		};
	}
	const fetchedById = toUserMap(fetchedUsers);
	const nextUsers = new Map(context.users);
	const prefixIds: Array<string> = [];
	for (const [userId, user] of fetchedById) {
		nextUsers.set(userId, user);
		prefixIds.push(userId);
	}
	const prefixIdSet = new Set(prefixIds);
	const orderedUsers: Array<User> = [];
	for (const userId of prefixIds) {
		const user = nextUsers.get(userId);
		if (user) orderedUsers.push(user);
	}
	for (const user of context.userSnapshot) {
		if (!prefixIdSet.has(user.id)) orderedUsers.push(nextUsers.get(user.id) ?? user);
	}
	const userSnapshot = freezeUserSnapshot(orderedUsers);
	return {users: nextUsers, userSnapshot, lastUserId: getLastUserId(userSnapshot)};
}

function appendUsers(
	context: PollVoteUsersContext,
	fetchedUsers: ReadonlyArray<UserPartial>,
): Pick<PollVoteUsersContext, 'users' | 'userSnapshot' | 'lastUserId'> {
	if (fetchedUsers.length === 0) {
		return {users: context.users, userSnapshot: context.userSnapshot, lastUserId: context.lastUserId};
	}
	const users = new Map(context.users);
	const orderedUsers = [...context.userSnapshot];
	for (const userPartial of fetchedUsers) {
		const user = new User(userPartial);
		if (!users.has(userPartial.id)) orderedUsers.push(user);
		users.set(userPartial.id, user);
	}
	return {
		users,
		userSnapshot: freezeUserSnapshot(orderedUsers),
		lastUserId: fetchedUsers[fetchedUsers.length - 1].id,
	};
}

function settleFetch(
	context: PollVoteUsersContext,
	requestId: number | undefined,
): Pick<PollVoteUsersContext, 'newestSettledRequest' | 'activeRequestId'> {
	if (requestId == null) {
		return {
			newestSettledRequest: context.newestSettledRequest,
			activeRequestId: context.activeRequestId,
		};
	}
	const activeRequestId = context.activeRequestId;
	return {
		newestSettledRequest: Math.max(context.newestSettledRequest, requestId),
		activeRequestId: activeRequestId === requestId ? null : activeRequestId,
	};
}

function applyFetchSuccess(
	context: PollVoteUsersContext,
	event: Extract<PollVoteUsersMachineEvent, {type: 'fetch.success'}>,
) {
	const merged =
		event.mode === 'append'
			? appendUsers(context, event.users)
			: mergeInitialUsers(
					context,
					event.users,
					isCompleteInitialFetch({
						pageUserCount: event.users.length,
						responseHasMore: event.responseHasMore,
						totalCount: event.totalCount,
					}),
				);
	const lastUserId = event.nextAfter !== undefined ? event.nextAfter : merged.lastUserId;
	return {
		...context,
		...merged,
		lastUserId,
		initialFetchLimit:
			event.mode === 'replace'
				? Math.max(context.initialFetchLimit, event.requestedLimit ?? event.users.length)
				: context.initialFetchLimit,
		hasMore: inferHasMore({
			userCount: merged.users.size,
			pageUserCount: event.users.length,
			requestedLimit: event.requestedLimit,
			responseHasMore: event.responseHasMore,
			totalCount: event.totalCount,
		}),
		...settleFetch(context, event.requestId),
		version: context.version + 1,
	};
}

function addUser(context: PollVoteUsersContext, user: User): PollVoteUsersContext {
	const users = new Map(context.users);
	users.set(user.id, user);
	const userSnapshot = freezeUserSnapshot(users.values());
	return {
		...context,
		users,
		userSnapshot,
		lastUserId: getLastUserId(userSnapshot),
		version: context.version + 1,
	};
}

function removeUser(context: PollVoteUsersContext, userId: string): PollVoteUsersContext {
	const users = new Map(context.users);
	users.delete(userId);
	const userSnapshot = freezeUserSnapshot(users.values());
	return {
		...context,
		users,
		userSnapshot,
		lastUserId: getLastUserId(userSnapshot),
		version: context.version + 1,
	};
}

export const pollVoteUsersStateMachine = setup({
	types: {} as {
		context: PollVoteUsersContext;
		events: PollVoteUsersMachineEvent;
		input: PollVoteUsersInput;
	},
	actions: {
		startFetch: assign(({context}) => {
			const requestId = context.requestSerial + 1;
			return {
				requestSerial: requestId,
				activeRequestId: requestId,
				version: context.version + 1,
			};
		}),
		applyFetchSuccess: assign(({context, event}) =>
			event.type === 'fetch.success' ? applyFetchSuccess(context, event) : context,
		),
		applyFetchError: assign(({context, event}) =>
			event.type === 'fetch.error'
				? {
						...settleFetch(context, event.requestId),
						version: context.version + 1,
					}
				: context,
		),
		addUser: assign(({context, event}) => (event.type === 'user.add' ? addUser(context, event.user) : context)),
		removeUser: assign(({context, event}) =>
			event.type === 'user.remove' ? removeUser(context, event.userId) : context,
		),
	},
	guards: {
		isFetchResultCurrent: ({context, event}) => {
			if (event.type !== 'fetch.success' && event.type !== 'fetch.error') return false;
			if (event.requestId == null) return true;
			if (event.requestId < context.newestSettledRequest) return false;
			return context.activeRequestId == null || event.requestId === context.activeRequestId;
		},
		isUserMissing: ({context, event}) => event.type === 'user.add' && !context.users.has(event.user.id),
		isUserKnown: ({context, event}) => event.type === 'user.remove' && context.users.has(event.userId),
	},
}).createMachine({
	id: 'messagePollVoteUsers',
	context: ({input}) => {
		const requestSerial = input.requestSerial ?? 0;
		return {
			users: new Map(),
			userSnapshot: EMPTY_USERS,
			hasMore: true,
			lastUserId: null,
			initialFetchLimit: 0,
			requestSerial,
			newestSettledRequest: requestSerial,
			activeRequestId: null,
			version: 0,
		};
	},
	initial: 'idle',
	states: {
		idle: {
			on: {
				'fetch.pending': {target: 'pending', actions: 'startFetch'},
				'fetch.success': {guard: 'isFetchResultCurrent', target: 'success', actions: 'applyFetchSuccess'},
				'fetch.error': {guard: 'isFetchResultCurrent', target: 'error', actions: 'applyFetchError'},
				'user.add': {guard: 'isUserMissing', actions: 'addUser'},
				'user.remove': {guard: 'isUserKnown', actions: 'removeUser'},
			},
		},
		pending: {
			on: {
				'fetch.pending': {target: 'pending', actions: 'startFetch'},
				'fetch.success': {guard: 'isFetchResultCurrent', target: 'success', actions: 'applyFetchSuccess'},
				'fetch.error': {guard: 'isFetchResultCurrent', target: 'error', actions: 'applyFetchError'},
				'user.add': {guard: 'isUserMissing', actions: 'addUser'},
				'user.remove': {guard: 'isUserKnown', actions: 'removeUser'},
			},
		},
		success: {
			on: {
				'fetch.pending': {target: 'pending', actions: 'startFetch'},
				'fetch.success': {guard: 'isFetchResultCurrent', target: 'success', actions: 'applyFetchSuccess'},
				'fetch.error': {guard: 'isFetchResultCurrent', target: 'error', actions: 'applyFetchError'},
				'user.add': {guard: 'isUserMissing', actions: 'addUser'},
				'user.remove': {guard: 'isUserKnown', actions: 'removeUser'},
			},
		},
		error: {
			on: {
				'fetch.pending': {target: 'pending', actions: 'startFetch'},
				'fetch.success': {guard: 'isFetchResultCurrent', target: 'success', actions: 'applyFetchSuccess'},
				'fetch.error': {guard: 'isFetchResultCurrent', target: 'error', actions: 'applyFetchError'},
				'user.add': {guard: 'isUserMissing', actions: 'addUser'},
				'user.remove': {guard: 'isUserKnown', actions: 'removeUser'},
			},
		},
	},
});

export type PollVoteUsersMachineSnapshot = SnapshotFrom<typeof pollVoteUsersStateMachine>;

export function createPollVoteUsersSnapshot(requestSerial = 0): PollVoteUsersMachineSnapshot {
	return getInitialSnapshot(pollVoteUsersStateMachine, {requestSerial});
}

export function transitionPollVoteUsersSnapshot(
	snapshot: PollVoteUsersMachineSnapshot,
	event: PollVoteUsersMachineEvent,
): PollVoteUsersMachineSnapshot {
	return transition(pollVoteUsersStateMachine, snapshot, event)[0] as PollVoteUsersMachineSnapshot;
}

export function getPollVoteUsersFetchStatus(snapshot: PollVoteUsersMachineSnapshot): FetchStatus {
	return typeof snapshot.value === 'string' ? (snapshot.value as FetchStatus) : 'idle';
}
