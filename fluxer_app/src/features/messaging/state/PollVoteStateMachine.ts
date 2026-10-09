// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessagePollAnswerCount} from '@fluxer/schema/src/domains/message/PollSchemas';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export interface PollVoteRecord {
	answerId: number;
	count: number;
	me: boolean;
	knownVoters: ReadonlySet<string>;
	removedVoters: ReadonlySet<string>;
}

export type PollVoteMap = ReadonlyMap<number, PollVoteRecord>;

interface PollVoteMachineContext {
	map: PollVoteMap;
	currentUserId: string | null;
}

interface PollVoteMachineInput {
	map?: PollVoteMap;
	currentUserId?: string | null;
}

export type PollVoteMachineEvent =
	| {
			type: 'pollVote.hydrate';
			votes: ReadonlyArray<MessagePollAnswerCount> | null | undefined;
			currentUserId?: string | null;
	  }
	| {type: 'pollVote.add'; answerId: number; userId: string; isCurrentUser: boolean}
	| {type: 'pollVote.remove'; answerId: number; userId: string; isCurrentUser: boolean}
	| {type: 'pollVote.trackVoters'; answerId: number; userIds: ReadonlyArray<string>}
	| {type: 'pollVote.trackVoter'; answerId: number; userId: string}
	| {type: 'pollVote.untrackVoter'; answerId: number; userId: string};

export function emptyMap(): PollVoteMap {
	return new Map();
}

export function recordToAnswerCount(record: PollVoteRecord): MessagePollAnswerCount {
	return Object.freeze({
		id: record.answerId,
		count: record.count,
		me_voted: record.me ? true : undefined,
	}) as MessagePollAnswerCount;
}

export function mapToAnswerCounts(map: PollVoteMap): ReadonlyArray<MessagePollAnswerCount> {
	const out: Array<MessagePollAnswerCount> = [];
	for (const record of map.values()) {
		if (record.count > 0) out.push(recordToAnswerCount(record));
	}
	return out.length > 0 ? Object.freeze(out) : EMPTY_ANSWER_COUNTS;
}

export function getRecord(map: PollVoteMap, answerId: number): PollVoteRecord | undefined {
	return map.get(answerId);
}

function withRecord(map: PollVoteMap, answerId: number, record: PollVoteRecord | null): PollVoteMap {
	const next = new Map(map);
	if (record === null) {
		next.delete(answerId);
	} else {
		next.set(answerId, record);
	}
	return next;
}

function clamp(n: number): number {
	return n < 0 ? 0 : n;
}

const EMPTY_ANSWER_COUNTS: ReadonlyArray<MessagePollAnswerCount> = Object.freeze([]);
const EMPTY_SET: ReadonlySet<string> = new Set();

function addToMap(map: PollVoteMap, answerId: number, userId: string, isCurrentUser: boolean): PollVoteMap {
	const existing = map.get(answerId);
	if (!existing) {
		const reactors = new Set<string>([userId]);
		return withRecord(map, answerId, {
			answerId,
			count: 1,
			me: isCurrentUser,
			knownVoters: reactors,
			removedVoters: EMPTY_SET,
		});
	}
	let removedReactors = existing.removedVoters;
	if (removedReactors.has(userId)) {
		const next = new Set(removedReactors);
		next.delete(userId);
		removedReactors = next;
	}
	if (existing.knownVoters.has(userId)) {
		if ((isCurrentUser && !existing.me) || removedReactors !== existing.removedVoters) {
			return withRecord(map, answerId, {...existing, me: existing.me || isCurrentUser, removedVoters: removedReactors});
		}
		return map;
	}
	const reactors = new Set(existing.knownVoters);
	reactors.add(userId);
	return withRecord(map, answerId, {
		answerId: existing.answerId,
		count: existing.count + 1,
		me: existing.me || isCurrentUser,
		knownVoters: reactors,
		removedVoters: removedReactors,
	});
}

function removeFromMap(map: PollVoteMap, answerId: number, userId: string, isCurrentUser: boolean): PollVoteMap {
	const existing = map.get(answerId);
	if (!existing) return map;
	if (existing.removedVoters.has(userId)) {
		if (!existing.knownVoters.has(userId) && !(isCurrentUser && existing.me)) return map;
		const reactors = new Set(existing.knownVoters);
		reactors.delete(userId);
		return withRecord(map, answerId, {
			...existing,
			me: isCurrentUser ? false : existing.me,
			knownVoters: reactors,
		});
	}
	const wasKnown = existing.knownVoters.has(userId);
	let nextCount = existing.count;
	let nextMe = existing.me;
	let nextReactors = existing.knownVoters;
	if (wasKnown) {
		const reactors = new Set(existing.knownVoters);
		reactors.delete(userId);
		nextReactors = reactors;
		nextCount = clamp(existing.count - 1);
		if (isCurrentUser) nextMe = false;
	} else if (isCurrentUser && existing.me) {
		nextCount = clamp(existing.count - 1);
		nextMe = false;
	} else if (!isCurrentUser) {
		nextCount = clamp(existing.count - 1);
	} else {
		return map;
	}
	if (nextCount <= 0) {
		return withRecord(map, answerId, null);
	}
	const removed = new Set(existing.removedVoters);
	removed.add(userId);
	return withRecord(map, answerId, {
		answerId: existing.answerId,
		count: nextCount,
		me: nextMe,
		knownVoters: nextReactors,
		removedVoters: removed,
	});
}

function countHydrationTombstones(
	removedVoters: ReadonlySet<string>,
	wireMe: boolean,
	currentUserId?: string | null,
): number {
	if (removedVoters.size === 0) return 0;
	let count = removedVoters.size;
	if (currentUserId != null && removedVoters.has(currentUserId) && !wireMe) {
		count -= 1;
	}
	return count;
}

function hydrateMap(
	map: PollVoteMap,
	wire: ReadonlyArray<MessagePollAnswerCount> | null | undefined,
	currentUserId?: string | null,
): PollVoteMap {
	if (!wire || wire.length === 0) {
		return map.size === 0 ? map : emptyMap();
	}
	const next = new Map<number, PollVoteRecord>();
	for (const reaction of wire) {
		const wireCount = Math.max(0, Math.floor(reaction.count ?? 0));
		if (wireCount === 0) continue;
		const key = reaction.id ?? 0;
		const wireMe = reaction.me_voted ?? false;
		const prev = map.get(key);
		if (!prev) {
			next.set(key, {
				answerId: reaction.id ?? 0,
				count: wireCount,
				me: wireMe,
				knownVoters: EMPTY_SET,
				removedVoters: EMPTY_SET,
			});
			continue;
		}
		const reactors = new Set<string>();
		for (const userId of prev.knownVoters) {
			if (!prev.removedVoters.has(userId)) reactors.add(userId);
		}
		const hydrationTombstones = countHydrationTombstones(prev.removedVoters, wireMe, currentUserId);
		const count = Math.max(clamp(wireCount - hydrationTombstones), reactors.size);
		const currentUserWasRemoved = currentUserId != null && prev.removedVoters.has(currentUserId);
		next.set(key, {
			answerId: reaction.id ?? 0,
			count,
			me: currentUserWasRemoved ? false : wireMe || prev.me,
			knownVoters: reactors,
			removedVoters: prev.removedVoters,
		});
	}
	if (mapsEqual(map, next)) return map;
	return next;
}

function trackReactorInMap(map: PollVoteMap, answerId: number, userId: string): PollVoteMap {
	const existing = map.get(answerId);
	if (!existing) return map;
	if (existing.knownVoters.has(userId)) return map;
	const reactors = new Set(existing.knownVoters);
	reactors.add(userId);
	const count = Math.max(existing.count, reactors.size);
	return withRecord(map, answerId, {...existing, count, knownVoters: reactors});
}

function trackReactorsInMap(map: PollVoteMap, answerId: number, userIds: ReadonlyArray<string>): PollVoteMap {
	if (userIds.length === 0 || !map.has(answerId)) return map;
	let next = map;
	for (const userId of userIds) next = trackReactorInMap(next, answerId, userId);
	return next;
}

function untrackReactorInMap(map: PollVoteMap, answerId: number, userId: string): PollVoteMap {
	const existing = map.get(answerId);
	if (!existing) return map;
	if (!existing.knownVoters.has(userId)) return map;
	const reactors = new Set(existing.knownVoters);
	reactors.delete(userId);
	return withRecord(map, answerId, {...existing, knownVoters: reactors});
}

export const pollVoteStateMachine = setup({
	types: {} as {
		context: PollVoteMachineContext;
		events: PollVoteMachineEvent;
		input: PollVoteMachineInput;
	},
	actions: {
		applyHydration: assign({
			map: ({context, event}) =>
				event.type === 'pollVote.hydrate'
					? hydrateMap(context.map, event.votes, event.currentUserId ?? context.currentUserId)
					: context.map,
			currentUserId: ({context, event}) =>
				event.type === 'pollVote.hydrate' ? (event.currentUserId ?? context.currentUserId) : context.currentUserId,
		}),
		applyAdd: assign({
			map: ({context, event}) =>
				event.type === 'pollVote.add'
					? addToMap(context.map, event.answerId, event.userId, event.isCurrentUser)
					: context.map,
		}),
		applyRemove: assign({
			map: ({context, event}) =>
				event.type === 'pollVote.remove'
					? removeFromMap(context.map, event.answerId, event.userId, event.isCurrentUser)
					: context.map,
		}),
		applyTrackVoter: assign({
			map: ({context, event}) =>
				event.type === 'pollVote.trackVoter'
					? trackReactorInMap(context.map, event.answerId, event.userId)
					: context.map,
		}),
		applyTrackVoters: assign({
			map: ({context, event}) =>
				event.type === 'pollVote.trackVoters'
					? trackReactorsInMap(context.map, event.answerId, event.userIds)
					: context.map,
		}),
		applyUntrackVoter: assign({
			map: ({context, event}) =>
				event.type === 'pollVote.untrackVoter'
					? untrackReactorInMap(context.map, event.answerId, event.userId)
					: context.map,
		}),
	},
	guards: {
		hasVotes: ({context}) => context.map.size > 0,
	},
}).createMachine({
	id: 'messagePollVoteAggregate',
	context: ({input}) => ({
		map: input.map ?? emptyMap(),
		currentUserId: input.currentUserId ?? null,
	}),
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'hasVotes', target: 'active'}, {target: 'empty'}],
		},
		empty: {
			on: {
				'pollVote.hydrate': {target: 'routing', actions: 'applyHydration'},
				'pollVote.add': {target: 'routing', actions: 'applyAdd'},
				'pollVote.remove': {target: 'routing', actions: 'applyRemove'},
				'pollVote.trackVoter': {target: 'routing', actions: 'applyTrackVoter'},
				'pollVote.trackVoters': {target: 'routing', actions: 'applyTrackVoters'},
				'pollVote.untrackVoter': {target: 'routing', actions: 'applyUntrackVoter'},
			},
		},
		active: {
			on: {
				'pollVote.hydrate': {target: 'routing', actions: 'applyHydration'},
				'pollVote.add': {target: 'routing', actions: 'applyAdd'},
				'pollVote.remove': {target: 'routing', actions: 'applyRemove'},
				'pollVote.trackVoter': {target: 'routing', actions: 'applyTrackVoter'},
				'pollVote.trackVoters': {target: 'routing', actions: 'applyTrackVoters'},
				'pollVote.untrackVoter': {target: 'routing', actions: 'applyUntrackVoter'},
			},
		},
	},
});

export type PollVoteMachineSnapshot = SnapshotFrom<typeof pollVoteStateMachine>;
export type PollVoteMachineStateValue = 'empty' | 'active';

export function createPollVoteMachineSnapshot(
	map: PollVoteMap = emptyMap(),
	currentUserId?: string | null,
): PollVoteMachineSnapshot {
	return getInitialSnapshot(pollVoteStateMachine, {map, currentUserId});
}

export function transitionPollVoteSnapshot(
	snapshot: PollVoteMachineSnapshot,
	event: PollVoteMachineEvent,
): PollVoteMachineSnapshot {
	if (isSnapshotNoop(snapshot, event)) return snapshot;
	return transition(pollVoteStateMachine, snapshot, event)[0] as PollVoteMachineSnapshot;
}

export function transitionPollVoteMap(
	map: PollVoteMap,
	event: PollVoteMachineEvent,
	currentUserId?: string | null,
): PollVoteMap {
	switch (event.type) {
		case 'pollVote.hydrate':
			return hydrateMap(map, event.votes, event.currentUserId ?? currentUserId);
		case 'pollVote.add':
			return addToMap(map, event.answerId, event.userId, event.isCurrentUser);
		case 'pollVote.remove':
			return removeFromMap(map, event.answerId, event.userId, event.isCurrentUser);
		case 'pollVote.trackVoter':
			return trackReactorInMap(map, event.answerId, event.userId);
		case 'pollVote.trackVoters':
			return trackReactorsInMap(map, event.answerId, event.userIds);
		case 'pollVote.untrackVoter':
			return untrackReactorInMap(map, event.answerId, event.userId);
	}
}

export function getPollVoteStateValue(snapshot: PollVoteMachineSnapshot): PollVoteMachineStateValue {
	return snapshot.value === 'active' ? 'active' : 'empty';
}

export function applyAdd(map: PollVoteMap, answerId: number, userId: string, isCurrentUser: boolean): PollVoteMap {
	return transitionPollVoteMap(map, {type: 'pollVote.add', answerId, userId, isCurrentUser});
}

export function applyRemove(map: PollVoteMap, answerId: number, userId: string, isCurrentUser: boolean): PollVoteMap {
	return transitionPollVoteMap(map, {type: 'pollVote.remove', answerId, userId, isCurrentUser});
}

export function hydrate(
	map: PollVoteMap,
	wire: ReadonlyArray<MessagePollAnswerCount> | null | undefined,
	currentUserId?: string | null,
): PollVoteMap {
	return transitionPollVoteMap(map, {type: 'pollVote.hydrate', votes: wire, currentUserId}, currentUserId);
}

export function trackVoter(map: PollVoteMap, answerId: number, userId: string): PollVoteMap {
	return transitionPollVoteMap(map, {type: 'pollVote.trackVoter', answerId, userId});
}

export function trackVoters(map: PollVoteMap, answerId: number, userIds: ReadonlyArray<string>): PollVoteMap {
	return transitionPollVoteMap(map, {type: 'pollVote.trackVoters', answerId, userIds});
}

export function untrackVoter(map: PollVoteMap, answerId: number, userId: string): PollVoteMap {
	return transitionPollVoteMap(map, {type: 'pollVote.untrackVoter', answerId, userId});
}

function isSnapshotNoop(snapshot: PollVoteMachineSnapshot, event: PollVoteMachineEvent): boolean {
	const map = snapshot.context.map;
	switch (event.type) {
		case 'pollVote.hydrate':
			return (
				(event.votes == null || event.votes.length === 0) &&
				map.size === 0 &&
				(event.currentUserId == null || event.currentUserId === snapshot.context.currentUserId)
			);
		case 'pollVote.remove':
		case 'pollVote.trackVoter':
		case 'pollVote.trackVoters':
		case 'pollVote.untrackVoter':
			return map.size === 0 || !map.has(event.answerId);
		case 'pollVote.add':
			return false;
	}
}

function mapsEqual(a: PollVoteMap, b: PollVoteMap): boolean {
	if (a === b) return true;
	if (a.size !== b.size) return false;
	for (const [key, left] of a) {
		const right = b.get(key);
		if (!right) return false;
		if (left.count !== right.count || left.me !== right.me) return false;
		if (left.answerId !== right.answerId) return false;
		if (left.knownVoters.size !== right.knownVoters.size) return false;
		for (const id of left.knownVoters) {
			if (!right.knownVoters.has(id)) return false;
		}
		if (left.removedVoters.size !== right.removedVoters.size) return false;
		for (const id of left.removedVoters) {
			if (!right.removedVoters.has(id)) return false;
		}
	}
	return true;
}

export function answerCountsEqual(
	a: ReadonlyArray<MessagePollAnswerCount>,
	b: ReadonlyArray<MessagePollAnswerCount>,
): boolean {
	if (a === b) return true;
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) {
		const left = a[i];
		const right = b[i];
		if (left.count !== right.count) return false;
		if (left.me_voted !== right.me_voted) return false;
		if (left.id !== right.id) return false;
	}
	return true;
}
