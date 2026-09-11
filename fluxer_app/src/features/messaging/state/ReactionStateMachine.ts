// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import type {MessageReaction} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface ReactionRecord {
	emoji: ReactionEmoji;
	count: number;
	me: boolean;
	knownReactors: ReadonlySet<string>;
	removedReactors: ReadonlySet<string>;
}

export type ReactionMap = ReadonlyMap<string, ReactionRecord>;

interface ReactionMachineContext {
	map: ReactionMap;
	currentUserId: string | null;
}

interface ReactionMachineInput {
	map?: ReactionMap;
	currentUserId?: string | null;
}

export type ReactionMachineEvent =
	| {
			type: 'reaction.hydrate';
			reactions: ReadonlyArray<MessageReaction> | null | undefined;
			currentUserId?: string | null;
	  }
	| {type: 'reaction.add'; emoji: ReactionEmoji; userId: string; isCurrentUser: boolean}
	| {type: 'reaction.remove'; emoji: ReactionEmoji; userId: string; isCurrentUser: boolean}
	| {type: 'reaction.removeAll'}
	| {type: 'reaction.removeEmoji'; emoji: ReactionEmoji}
	| {type: 'reaction.trackReactors'; emoji: ReactionEmoji; userIds: ReadonlyArray<string>}
	| {type: 'reaction.trackReactor'; emoji: ReactionEmoji; userId: string}
	| {type: 'reaction.untrackReactor'; emoji: ReactionEmoji; userId: string};

export function getEmojiKey(emoji: ReactionEmoji): string {
	return `${emoji.id ?? ''}:${emoji.name}`;
}

export function sameEmoji(left: ReactionEmoji, right: ReactionEmoji): boolean {
	return (left.id ?? null) === (right.id ?? null) && left.name === right.name;
}

export function emptyMap(): ReactionMap {
	return new Map();
}

export function recordToReaction(record: ReactionRecord): MessageReaction {
	return Object.freeze({
		emoji: record.emoji,
		count: record.count,
		me: record.me ? true : undefined,
	}) as MessageReaction;
}

export function mapToReactions(map: ReactionMap): ReadonlyArray<MessageReaction> {
	const out: Array<MessageReaction> = [];
	for (const record of map.values()) {
		if (record.count > 0) out.push(recordToReaction(record));
	}
	return out.length > 0 ? Object.freeze(out) : EMPTY_REACTIONS;
}

export function getRecord(map: ReactionMap, emoji: ReactionEmoji): ReactionRecord | undefined {
	return map.get(getEmojiKey(emoji));
}

function withRecord(map: ReactionMap, key: string, record: ReactionRecord | null): ReactionMap {
	const next = new Map(map);
	if (record === null) {
		next.delete(key);
	} else {
		next.set(key, record);
	}
	return next;
}

function clamp(n: number): number {
	return n < 0 ? 0 : n;
}

const EMPTY_REACTIONS: ReadonlyArray<MessageReaction> = Object.freeze([]);
const EMPTY_SET: ReadonlySet<string> = new Set();

function addToMap(map: ReactionMap, emoji: ReactionEmoji, userId: string, isCurrentUser: boolean): ReactionMap {
	const key = getEmojiKey(emoji);
	const existing = map.get(key);
	if (!existing) {
		const reactors = new Set<string>([userId]);
		return withRecord(map, key, {
			emoji,
			count: 1,
			me: isCurrentUser,
			knownReactors: reactors,
			removedReactors: EMPTY_SET,
		});
	}
	let removedReactors = existing.removedReactors;
	if (removedReactors.has(userId)) {
		const next = new Set(removedReactors);
		next.delete(userId);
		removedReactors = next;
	}
	if (existing.knownReactors.has(userId)) {
		if ((isCurrentUser && !existing.me) || removedReactors !== existing.removedReactors) {
			return withRecord(map, key, {...existing, me: existing.me || isCurrentUser, removedReactors});
		}
		return map;
	}
	const reactors = new Set(existing.knownReactors);
	reactors.add(userId);
	return withRecord(map, key, {
		emoji: existing.emoji,
		count: existing.count + 1,
		me: existing.me || isCurrentUser,
		knownReactors: reactors,
		removedReactors,
	});
}

function removeFromMap(map: ReactionMap, emoji: ReactionEmoji, userId: string, isCurrentUser: boolean): ReactionMap {
	const key = getEmojiKey(emoji);
	const existing = map.get(key);
	if (!existing) return map;
	if (existing.removedReactors.has(userId)) {
		if (!existing.knownReactors.has(userId) && !(isCurrentUser && existing.me)) return map;
		const reactors = new Set(existing.knownReactors);
		reactors.delete(userId);
		return withRecord(map, key, {
			...existing,
			me: isCurrentUser ? false : existing.me,
			knownReactors: reactors,
		});
	}
	const wasKnown = existing.knownReactors.has(userId);
	let nextCount = existing.count;
	let nextMe = existing.me;
	let nextReactors = existing.knownReactors;
	if (wasKnown) {
		const reactors = new Set(existing.knownReactors);
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
		return withRecord(map, key, null);
	}
	const removed = new Set(existing.removedReactors);
	removed.add(userId);
	return withRecord(map, key, {
		emoji: existing.emoji,
		count: nextCount,
		me: nextMe,
		knownReactors: nextReactors,
		removedReactors: removed,
	});
}

function removeEmojiFromMap(map: ReactionMap, emoji: ReactionEmoji): ReactionMap {
	const key = getEmojiKey(emoji);
	if (!map.has(key)) return map;
	return withRecord(map, key, null);
}

function removeAllFromMap(map: ReactionMap): ReactionMap {
	if (map.size === 0) return map;
	return emptyMap();
}

function countHydrationTombstones(
	removedReactors: ReadonlySet<string>,
	wireMe: boolean,
	currentUserId?: string | null,
): number {
	if (removedReactors.size === 0) return 0;
	let count = removedReactors.size;
	if (currentUserId != null && removedReactors.has(currentUserId) && !wireMe) {
		count -= 1;
	}
	return count;
}

function hydrateMap(
	map: ReactionMap,
	wire: ReadonlyArray<MessageReaction> | null | undefined,
	currentUserId?: string | null,
): ReactionMap {
	if (!wire || wire.length === 0) {
		return map.size === 0 ? map : emptyMap();
	}
	const next = new Map<string, ReactionRecord>();
	for (const reaction of wire) {
		const wireCount = Math.max(0, Math.floor(reaction.count ?? 0));
		if (wireCount === 0) continue;
		const key = getEmojiKey(reaction.emoji);
		const wireMe = Boolean(reaction.me);
		const prev = map.get(key);
		if (!prev) {
			next.set(key, {
				emoji: reaction.emoji,
				count: wireCount,
				me: wireMe,
				knownReactors: EMPTY_SET,
				removedReactors: EMPTY_SET,
			});
			continue;
		}
		const reactors = new Set<string>();
		for (const userId of prev.knownReactors) {
			if (!prev.removedReactors.has(userId)) reactors.add(userId);
		}
		const hydrationTombstones = countHydrationTombstones(prev.removedReactors, wireMe, currentUserId);
		const count = Math.max(clamp(wireCount - hydrationTombstones), reactors.size);
		const currentUserWasRemoved = currentUserId != null && prev.removedReactors.has(currentUserId);
		next.set(key, {
			emoji: reaction.emoji,
			count,
			me: currentUserWasRemoved ? false : wireMe || prev.me,
			knownReactors: reactors,
			removedReactors: prev.removedReactors,
		});
	}
	if (mapsEqual(map, next)) return map;
	return next;
}

function trackReactorInMap(map: ReactionMap, emoji: ReactionEmoji, userId: string): ReactionMap {
	const key = getEmojiKey(emoji);
	const existing = map.get(key);
	if (!existing) return map;
	if (existing.knownReactors.has(userId)) return map;
	const reactors = new Set(existing.knownReactors);
	reactors.add(userId);
	const count = Math.max(existing.count, reactors.size);
	return withRecord(map, key, {...existing, count, knownReactors: reactors});
}

function trackReactorsInMap(map: ReactionMap, emoji: ReactionEmoji, userIds: ReadonlyArray<string>): ReactionMap {
	if (userIds.length === 0 || !map.has(getEmojiKey(emoji))) return map;
	let next = map;
	for (const userId of userIds) next = trackReactorInMap(next, emoji, userId);
	return next;
}

function untrackReactorInMap(map: ReactionMap, emoji: ReactionEmoji, userId: string): ReactionMap {
	const key = getEmojiKey(emoji);
	const existing = map.get(key);
	if (!existing) return map;
	if (!existing.knownReactors.has(userId)) return map;
	const reactors = new Set(existing.knownReactors);
	reactors.delete(userId);
	return withRecord(map, key, {...existing, knownReactors: reactors});
}

export const reactionStateMachine = setup({
	types: {} as {
		context: ReactionMachineContext;
		events: ReactionMachineEvent;
		input: ReactionMachineInput;
	},
	actions: {
		applyHydration: assign({
			map: ({context, event}) =>
				event.type === 'reaction.hydrate'
					? hydrateMap(context.map, event.reactions, event.currentUserId ?? context.currentUserId)
					: context.map,
			currentUserId: ({context, event}) =>
				event.type === 'reaction.hydrate' ? (event.currentUserId ?? context.currentUserId) : context.currentUserId,
		}),
		applyAdd: assign({
			map: ({context, event}) =>
				event.type === 'reaction.add'
					? addToMap(context.map, event.emoji, event.userId, event.isCurrentUser)
					: context.map,
		}),
		applyRemove: assign({
			map: ({context, event}) =>
				event.type === 'reaction.remove'
					? removeFromMap(context.map, event.emoji, event.userId, event.isCurrentUser)
					: context.map,
		}),
		applyRemoveAll: assign({
			map: ({context}) => removeAllFromMap(context.map),
		}),
		applyRemoveEmoji: assign({
			map: ({context, event}) =>
				event.type === 'reaction.removeEmoji' ? removeEmojiFromMap(context.map, event.emoji) : context.map,
		}),
		applyTrackReactor: assign({
			map: ({context, event}) =>
				event.type === 'reaction.trackReactor'
					? trackReactorInMap(context.map, event.emoji, event.userId)
					: context.map,
		}),
		applyTrackReactors: assign({
			map: ({context, event}) =>
				event.type === 'reaction.trackReactors'
					? trackReactorsInMap(context.map, event.emoji, event.userIds)
					: context.map,
		}),
		applyUntrackReactor: assign({
			map: ({context, event}) =>
				event.type === 'reaction.untrackReactor'
					? untrackReactorInMap(context.map, event.emoji, event.userId)
					: context.map,
		}),
	},
	guards: {
		hasReactions: ({context}) => context.map.size > 0,
	},
}).createMachine({
	id: 'messageReactionAggregate',
	context: ({input}) => ({
		map: input.map ?? emptyMap(),
		currentUserId: input.currentUserId ?? null,
	}),
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'hasReactions', target: 'active'}, {target: 'empty'}],
		},
		empty: {
			on: {
				'reaction.hydrate': {target: 'routing', actions: 'applyHydration'},
				'reaction.add': {target: 'routing', actions: 'applyAdd'},
				'reaction.remove': {target: 'routing', actions: 'applyRemove'},
				'reaction.removeAll': {target: 'routing', actions: 'applyRemoveAll'},
				'reaction.removeEmoji': {target: 'routing', actions: 'applyRemoveEmoji'},
				'reaction.trackReactor': {target: 'routing', actions: 'applyTrackReactor'},
				'reaction.trackReactors': {target: 'routing', actions: 'applyTrackReactors'},
				'reaction.untrackReactor': {target: 'routing', actions: 'applyUntrackReactor'},
			},
		},
		active: {
			on: {
				'reaction.hydrate': {target: 'routing', actions: 'applyHydration'},
				'reaction.add': {target: 'routing', actions: 'applyAdd'},
				'reaction.remove': {target: 'routing', actions: 'applyRemove'},
				'reaction.removeAll': {target: 'routing', actions: 'applyRemoveAll'},
				'reaction.removeEmoji': {target: 'routing', actions: 'applyRemoveEmoji'},
				'reaction.trackReactor': {target: 'routing', actions: 'applyTrackReactor'},
				'reaction.trackReactors': {target: 'routing', actions: 'applyTrackReactors'},
				'reaction.untrackReactor': {target: 'routing', actions: 'applyUntrackReactor'},
			},
		},
	},
});

export type ReactionMachineSnapshot = SnapshotFrom<typeof reactionStateMachine>;
export type ReactionMachineStateValue = 'empty' | 'active';

export function createReactionMachineSnapshot(
	map: ReactionMap = emptyMap(),
	currentUserId?: string | null,
): ReactionMachineSnapshot {
	return initialTransition(reactionStateMachine, {map, currentUserId})[0];
}

export function transitionReactionSnapshot(
	snapshot: ReactionMachineSnapshot,
	event: ReactionMachineEvent,
): ReactionMachineSnapshot {
	if (isSnapshotNoop(snapshot, event)) return snapshot;
	return transition(reactionStateMachine, snapshot, event)[0] as ReactionMachineSnapshot;
}

export function transitionReactionMap(
	map: ReactionMap,
	event: ReactionMachineEvent,
	currentUserId?: string | null,
): ReactionMap {
	switch (event.type) {
		case 'reaction.hydrate':
			return hydrateMap(map, event.reactions, event.currentUserId ?? currentUserId);
		case 'reaction.add':
			return addToMap(map, event.emoji, event.userId, event.isCurrentUser);
		case 'reaction.remove':
			return removeFromMap(map, event.emoji, event.userId, event.isCurrentUser);
		case 'reaction.removeAll':
			return removeAllFromMap(map);
		case 'reaction.removeEmoji':
			return removeEmojiFromMap(map, event.emoji);
		case 'reaction.trackReactor':
			return trackReactorInMap(map, event.emoji, event.userId);
		case 'reaction.trackReactors':
			return trackReactorsInMap(map, event.emoji, event.userIds);
		case 'reaction.untrackReactor':
			return untrackReactorInMap(map, event.emoji, event.userId);
	}
}

export function getReactionStateValue(snapshot: ReactionMachineSnapshot): ReactionMachineStateValue {
	return snapshot.value === 'active' ? 'active' : 'empty';
}

export function applyAdd(map: ReactionMap, emoji: ReactionEmoji, userId: string, isCurrentUser: boolean): ReactionMap {
	return transitionReactionMap(map, {type: 'reaction.add', emoji, userId, isCurrentUser});
}

export function applyRemove(
	map: ReactionMap,
	emoji: ReactionEmoji,
	userId: string,
	isCurrentUser: boolean,
): ReactionMap {
	return transitionReactionMap(map, {type: 'reaction.remove', emoji, userId, isCurrentUser});
}

export function applyRemoveEmoji(map: ReactionMap, emoji: ReactionEmoji): ReactionMap {
	return transitionReactionMap(map, {type: 'reaction.removeEmoji', emoji});
}

export function applyRemoveAll(map: ReactionMap): ReactionMap {
	return transitionReactionMap(map, {type: 'reaction.removeAll'});
}

export function hydrate(
	map: ReactionMap,
	wire: ReadonlyArray<MessageReaction> | null | undefined,
	currentUserId?: string | null,
): ReactionMap {
	return transitionReactionMap(map, {type: 'reaction.hydrate', reactions: wire, currentUserId}, currentUserId);
}

export function trackReactor(map: ReactionMap, emoji: ReactionEmoji, userId: string): ReactionMap {
	return transitionReactionMap(map, {type: 'reaction.trackReactor', emoji, userId});
}

export function trackReactors(map: ReactionMap, emoji: ReactionEmoji, userIds: ReadonlyArray<string>): ReactionMap {
	return transitionReactionMap(map, {type: 'reaction.trackReactors', emoji, userIds});
}

export function untrackReactor(map: ReactionMap, emoji: ReactionEmoji, userId: string): ReactionMap {
	return transitionReactionMap(map, {type: 'reaction.untrackReactor', emoji, userId});
}

function isSnapshotNoop(snapshot: ReactionMachineSnapshot, event: ReactionMachineEvent): boolean {
	const map = snapshot.context.map;
	switch (event.type) {
		case 'reaction.hydrate':
			return (
				(event.reactions == null || event.reactions.length === 0) &&
				map.size === 0 &&
				(event.currentUserId == null || event.currentUserId === snapshot.context.currentUserId)
			);
		case 'reaction.removeAll':
			return map.size === 0;
		case 'reaction.remove':
		case 'reaction.removeEmoji':
		case 'reaction.trackReactor':
		case 'reaction.trackReactors':
		case 'reaction.untrackReactor':
			return map.size === 0 || !map.has(getEmojiKey(event.emoji));
		case 'reaction.add':
			return false;
	}
}

function mapsEqual(a: ReactionMap, b: ReactionMap): boolean {
	if (a === b) return true;
	if (a.size !== b.size) return false;
	for (const [key, left] of a) {
		const right = b.get(key);
		if (!right) return false;
		if (left.count !== right.count || left.me !== right.me) return false;
		if (!sameEmoji(left.emoji, right.emoji)) return false;
		if (left.knownReactors.size !== right.knownReactors.size) return false;
		for (const id of left.knownReactors) {
			if (!right.knownReactors.has(id)) return false;
		}
		if (left.removedReactors.size !== right.removedReactors.size) return false;
		for (const id of left.removedReactors) {
			if (!right.removedReactors.has(id)) return false;
		}
	}
	return true;
}

export function reactionsEqual(a: ReadonlyArray<MessageReaction>, b: ReadonlyArray<MessageReaction>): boolean {
	if (a === b) return true;
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) {
		const left = a[i];
		const right = b[i];
		if (left.count !== right.count) return false;
		if (Boolean(left.me) !== Boolean(right.me)) return false;
		if (!sameEmoji(left.emoji, right.emoji)) return false;
	}
	return true;
}
