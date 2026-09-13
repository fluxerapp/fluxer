// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	applyAdd,
	applyRemove,
	applyRemoveAll,
	applyRemoveEmoji,
	createReactionMachineSnapshot,
	emptyMap,
	getEmojiKey,
	getReactionStateValue,
	getRecord,
	hydrate,
	mapToReactions,
	reactionsEqual,
	sameEmoji,
	trackReactor,
	trackReactors,
	transitionReactionMap,
	transitionReactionSnapshot,
	untrackReactor,
} from '@app/features/messaging/state/ReactionStateMachine';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {describe, expect, it} from 'vitest';

const ME = 'me-user-id';
const ALICE = 'alice-id';
const BOB = 'bob-id';
const CAROL = 'carol-id';
const FIRE: ReactionEmoji = {name: '🔥'};
const HEART: ReactionEmoji = {name: '❤️'};
const CUSTOM: ReactionEmoji = {id: '1234567890', name: 'custom_emoji'};
const CUSTOM_OTHER: ReactionEmoji = {id: '9999999999', name: 'custom_emoji'};
const add = (map: any, emoji: ReactionEmoji, userId: string, isMe = userId === ME) =>
	applyAdd(map, emoji, userId, isMe);
const remove = (map: any, emoji: ReactionEmoji, userId: string, isMe = userId === ME) =>
	applyRemove(map, emoji, userId, isMe);

describe('ReactionStateMachine: emoji identity', () => {
	it('same unicode emoji is equal', () => {
		expect(sameEmoji({name: '🔥'}, {name: '🔥'})).toBe(true);
	});
	it('different unicode emoji not equal', () => {
		expect(sameEmoji({name: '🔥'}, {name: '❤️'})).toBe(false);
	});
	it('custom emoji equal by id+name', () => {
		expect(sameEmoji({id: '1', name: 'x'}, {id: '1', name: 'x'})).toBe(true);
	});
	it('custom emoji with same name but different id not equal', () => {
		expect(sameEmoji({id: '1', name: 'x'}, {id: '2', name: 'x'})).toBe(false);
	});
	it('null and undefined ids treated equivalently', () => {
		expect(sameEmoji({id: null, name: '🔥'}, {name: '🔥'})).toBe(true);
	});
	it('emoji key collision-free for different emojis', () => {
		const k1 = getEmojiKey({name: '🔥'});
		const k2 = getEmojiKey({name: '❤️'});
		expect(k1).not.toBe(k2);
	});
});

describe('ReactionStateMachine: applyAdd', () => {
	it('creates record on first add', () => {
		const m = add(emptyMap(), FIRE, ALICE);
		const rec = getRecord(m, FIRE);
		expect(rec).toBeDefined();
		expect(rec!.count).toBe(1);
		expect(rec!.me).toBe(false);
		expect(rec!.knownReactors.has(ALICE)).toBe(true);
	});
	it('increments count for second distinct reactor', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, BOB);
		expect(getRecord(m, FIRE)!.count).toBe(2);
	});
	it('is idempotent for same userId (no double count)', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, ALICE);
		m = add(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(1);
	});
	it('sets me=true when current user adds', () => {
		const m = add(emptyMap(), FIRE, ME);
		expect(getRecord(m, FIRE)!.me).toBe(true);
	});
	it('preserves me=true when other user adds after me', () => {
		let m = add(emptyMap(), FIRE, ME);
		m = add(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.me).toBe(true);
		expect(getRecord(m, FIRE)!.count).toBe(2);
	});
	it('upgrades me=false to me=true if current user re-adds (recovery)', () => {
		let m = add(emptyMap(), FIRE, ME, false);
		expect(getRecord(m, FIRE)!.me).toBe(false);
		m = add(m, FIRE, ME, true);
		expect(getRecord(m, FIRE)!.me).toBe(true);
		expect(getRecord(m, FIRE)!.count).toBe(1);
	});
	it('different emojis tracked independently', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, HEART, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(1);
		expect(getRecord(m, HEART)!.count).toBe(1);
	});
	it('custom emojis with different ids are tracked separately', () => {
		let m = add(emptyMap(), CUSTOM, ALICE);
		m = add(m, CUSTOM_OTHER, ALICE);
		expect(getRecord(m, CUSTOM)!.count).toBe(1);
		expect(getRecord(m, CUSTOM_OTHER)!.count).toBe(1);
	});
});

describe('ReactionStateMachine: applyRemove (PRIMARY BUG)', () => {
	it('REGRESSION: 2 reactors, one removes → reaction is preserved with count 1', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, BOB);
		expect(getRecord(m, FIRE)!.count).toBe(2);
		m = remove(m, FIRE, ALICE);
		const rec = getRecord(m, FIRE);
		expect(rec).toBeDefined();
		expect(rec!.count).toBe(1);
		expect(rec!.knownReactors.has(BOB)).toBe(true);
		expect(rec!.knownReactors.has(ALICE)).toBe(false);
	});
	it('REGRESSION: me + other, other removes → reaction kept, me preserved', () => {
		let m = add(emptyMap(), FIRE, ME);
		m = add(m, FIRE, ALICE);
		m = remove(m, FIRE, ALICE);
		const rec = getRecord(m, FIRE);
		expect(rec!.count).toBe(1);
		expect(rec!.me).toBe(true);
	});
	it('REGRESSION: me + other, me removes → reaction kept with other', () => {
		let m = add(emptyMap(), FIRE, ME);
		m = add(m, FIRE, ALICE);
		m = remove(m, FIRE, ME);
		const rec = getRecord(m, FIRE);
		expect(rec!.count).toBe(1);
		expect(rec!.me).toBe(false);
	});
	it('removes reaction entirely when last reactor leaves', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = remove(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)).toBeUndefined();
	});
	it('idempotent: removing same userId twice does not double-decrement', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, BOB);
		m = remove(m, FIRE, ALICE);
		m = remove(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(1);
	});
	it('remove on unknown emoji is a no-op', () => {
		const m = remove(emptyMap(), FIRE, ALICE);
		expect(m.size).toBe(0);
	});
	it('remove unknown user when me=false but isCurrentUser=true is no-op', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = remove(m, FIRE, ME, true);
		expect(getRecord(m, FIRE)!.count).toBe(1);
	});
	it('untracked user remove (not in knownReactors but server says they left)', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = hydrate(m, [{emoji: FIRE, count: 5}]);
		expect(getRecord(m, FIRE)!.count).toBe(5);
		m = remove(m, FIRE, CAROL);
		expect(getRecord(m, FIRE)!.count).toBe(4);
	});
	it('me=true server hydration + me removes drops me flag and count', () => {
		let m = hydrate(emptyMap(), [{emoji: FIRE, count: 3, me: true}]);
		expect(getRecord(m, FIRE)!.me).toBe(true);
		m = remove(m, FIRE, ME);
		expect(getRecord(m, FIRE)!.count).toBe(2);
		expect(getRecord(m, FIRE)!.me).toBe(false);
	});
	it('count cannot go below zero', () => {
		let m = hydrate(emptyMap(), [{emoji: FIRE, count: 1}]);
		m = remove(m, FIRE, ALICE);
		m = remove(m, FIRE, BOB);
		expect(getRecord(m, FIRE)).toBeUndefined();
	});
});

describe('ReactionStateMachine: applyRemoveAll / applyRemoveEmoji', () => {
	it('removeAll clears every reaction on message', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, HEART, BOB);
		m = applyRemoveAll(m);
		expect(m.size).toBe(0);
	});
	it('removeAll on empty map is identity', () => {
		const m = emptyMap();
		expect(applyRemoveAll(m)).toBe(m);
	});
	it('removeEmoji removes only the targeted emoji', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, HEART, BOB);
		m = applyRemoveEmoji(m, FIRE);
		expect(getRecord(m, FIRE)).toBeUndefined();
		expect(getRecord(m, HEART)!.count).toBe(1);
	});
	it('removeEmoji for missing emoji is no-op', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		const before = m;
		m = applyRemoveEmoji(m, HEART);
		expect(m).toBe(before);
	});
});

describe('ReactionStateMachine: hydrate', () => {
	it('hydrates an empty map from wire reactions', () => {
		const m = hydrate(emptyMap(), [{emoji: FIRE, count: 5, me: true}]);
		expect(getRecord(m, FIRE)!.count).toBe(5);
		expect(getRecord(m, FIRE)!.me).toBe(true);
	});
	it('drops zero-count wire reactions', () => {
		const m = hydrate(emptyMap(), [{emoji: FIRE, count: 0}]);
		expect(m.size).toBe(0);
	});
	it('replaces existing reactions but preserves knownReactors', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, BOB);
		m = hydrate(m, [{emoji: FIRE, count: 10}]);
		expect(getRecord(m, FIRE)!.count).toBe(10);
		expect(getRecord(m, FIRE)!.knownReactors.size).toBe(2);
	});
	it('hydrate with smaller server count never drops below known reactor count', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, BOB);
		m = add(m, FIRE, CAROL);
		m = hydrate(m, [{emoji: FIRE, count: 1}]);
		expect(getRecord(m, FIRE)!.count).toBe(3);
	});
	it('hydrate without an emoji drops it from the map', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, HEART, BOB);
		m = hydrate(m, [{emoji: FIRE, count: 1}]);
		expect(getRecord(m, FIRE)).toBeDefined();
		expect(getRecord(m, HEART)).toBeUndefined();
	});
	it('hydrate with null/empty wipes when present', () => {
		const m = add(emptyMap(), FIRE, ALICE);
		expect(hydrate(m, null).size).toBe(0);
		expect(hydrate(m, []).size).toBe(0);
		expect(hydrate(m, undefined).size).toBe(0);
	});
	it('hydrate is identity when nothing changes', () => {
		const m = hydrate(emptyMap(), [{emoji: FIRE, count: 3}]);
		const again = hydrate(m, [{emoji: FIRE, count: 3}]);
		expect(again).toBe(m);
	});
	it('hydrate preserves me flag if server says me=true even if locally false', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = hydrate(m, [{emoji: FIRE, count: 1, me: true}]);
		expect(getRecord(m, FIRE)!.me).toBe(true);
	});
	it('hydrate preserves locally-known me=true even if server omits it', () => {
		let m = add(emptyMap(), FIRE, ME);
		m = hydrate(m, [{emoji: FIRE, count: 5}]);
		expect(getRecord(m, FIRE)!.me).toBe(true);
	});
	it('REGRESSION: stale hydrate does not resurrect a removed current-user reaction', () => {
		let m = hydrate(emptyMap(), [{emoji: FIRE, count: 1, me: true}], ME);
		m = add(m, FIRE, BOB, false);
		m = remove(m, FIRE, ME, true);
		expect(getRecord(m, FIRE)!.count).toBe(1);
		expect(getRecord(m, FIRE)!.me).toBe(false);
		m = hydrate(m, [{emoji: FIRE, count: 2, me: true}], ME);
		expect(getRecord(m, FIRE)!.count).toBe(1);
		expect(getRecord(m, FIRE)!.me).toBe(false);
		expect(getRecord(m, FIRE)!.knownReactors.has(BOB)).toBe(true);
	});
	it('REGRESSION: stale hydrate does not re-count a removed remote reactor', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, BOB);
		m = remove(m, FIRE, ALICE);
		m = hydrate(m, [{emoji: FIRE, count: 2}], ME);
		expect(getRecord(m, FIRE)!.count).toBe(1);
		expect(getRecord(m, FIRE)!.knownReactors.has(BOB)).toBe(true);
		expect(getRecord(m, FIRE)!.knownReactors.has(ALICE)).toBe(false);
	});
});

describe('ReactionStateMachine: mapToReactions', () => {
	it('produces a frozen array', () => {
		const m = add(emptyMap(), FIRE, ALICE);
		const arr = mapToReactions(m);
		expect(Object.isFrozen(arr)).toBe(true);
	});
	it('omits me when me=false', () => {
		const m = add(emptyMap(), FIRE, ALICE);
		const arr = mapToReactions(m);
		expect(arr[0].me).toBeUndefined();
	});
	it('includes me=true when me reacted', () => {
		const m = add(emptyMap(), FIRE, ME);
		const arr = mapToReactions(m);
		expect(arr[0].me).toBe(true);
	});
	it('reactionsEqual identifies count/me changes', () => {
		const a = [{emoji: FIRE, count: 2}] as any;
		const b = [{emoji: FIRE, count: 2}] as any;
		expect(reactionsEqual(a, b)).toBe(true);
		const c = [{emoji: FIRE, count: 3}] as any;
		expect(reactionsEqual(a, c)).toBe(false);
		const d = [{emoji: FIRE, count: 2, me: true}] as any;
		expect(reactionsEqual(a, d)).toBe(false);
	});
});

describe('ReactionStateMachine: trackReactor / untrackReactor', () => {
	it('trackReactor adds known userId without changing count if count already reflects', () => {
		let m = hydrate(emptyMap(), [{emoji: FIRE, count: 3}]);
		m = trackReactor(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(3);
		expect(getRecord(m, FIRE)!.knownReactors.has(ALICE)).toBe(true);
	});
	it('trackReactor bumps count if known reactors exceed reported count', () => {
		let m = hydrate(emptyMap(), [{emoji: FIRE, count: 1}]);
		m = trackReactor(m, FIRE, ALICE);
		m = trackReactor(m, FIRE, BOB);
		m = trackReactor(m, FIRE, CAROL);
		expect(getRecord(m, FIRE)!.count).toBe(3);
	});
	it('trackReactor is idempotent', () => {
		let m = hydrate(emptyMap(), [{emoji: FIRE, count: 3}]);
		m = trackReactor(m, FIRE, ALICE);
		const before = m;
		m = trackReactor(m, FIRE, ALICE);
		expect(m).toBe(before);
	});
	it('untrackReactor removes from knownReactors without changing count', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, BOB);
		m = untrackReactor(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(2);
		expect(getRecord(m, FIRE)!.knownReactors.has(ALICE)).toBe(false);
	});
	it('untrackReactor on unknown user is no-op', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		const before = m;
		m = untrackReactor(m, FIRE, BOB);
		expect(m).toBe(before);
	});
});

describe('ReactionStateMachine: complex scenarios', () => {
	it('full lifecycle: add me, add 2 others, others leave one by one, me leaves', () => {
		let m = emptyMap();
		m = add(m, FIRE, ME);
		expect(getRecord(m, FIRE)!.count).toBe(1);
		m = add(m, FIRE, ALICE);
		m = add(m, FIRE, BOB);
		expect(getRecord(m, FIRE)!.count).toBe(3);
		expect(getRecord(m, FIRE)!.me).toBe(true);
		m = remove(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(2);
		expect(getRecord(m, FIRE)!.me).toBe(true);
		m = remove(m, FIRE, BOB);
		expect(getRecord(m, FIRE)!.count).toBe(1);
		expect(getRecord(m, FIRE)!.me).toBe(true);
		m = remove(m, FIRE, ME);
		expect(getRecord(m, FIRE)).toBeUndefined();
	});
	it('optimistic + gateway echo do not double-count', () => {
		let m = emptyMap();
		m = add(m, FIRE, ME);
		m = add(m, FIRE, ME);
		expect(getRecord(m, FIRE)!.count).toBe(1);
		expect(getRecord(m, FIRE)!.me).toBe(true);
	});
	it('out-of-order remove + add gateway events', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = remove(m, FIRE, ALICE);
		m = add(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(1);
	});
	it('two emojis with overlapping reactors stay independent', () => {
		let m = emptyMap();
		m = add(m, FIRE, ALICE);
		m = add(m, FIRE, BOB);
		m = add(m, HEART, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(2);
		expect(getRecord(m, HEART)!.count).toBe(1);
		m = remove(m, HEART, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(2);
		expect(getRecord(m, HEART)).toBeUndefined();
	});
	it('hydrate after concurrent local adds keeps locally-known reactors', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = add(m, FIRE, BOB);
		m = hydrate(m, [{emoji: FIRE, count: 2}]);
		expect(getRecord(m, FIRE)!.knownReactors.size).toBe(2);
		m = remove(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(1);
	});
	it('reactor leaves then rejoins via gateway', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = remove(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)).toBeUndefined();
		m = add(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(1);
		expect(getRecord(m, FIRE)!.knownReactors.has(ALICE)).toBe(true);
	});
	it('frozen output: original map not mutated by operations', () => {
		const m1 = add(emptyMap(), FIRE, ALICE);
		const snap1 = mapToReactions(m1);
		const m2 = add(m1, FIRE, BOB);
		const snap1AfterMutation = mapToReactions(m1);
		expect(snap1).toEqual(snap1AfterMutation);
		expect(getRecord(m1, FIRE)!.count).toBe(1);
		expect(getRecord(m2, FIRE)!.count).toBe(2);
	});
	it('removeEmoji wipes a reaction that user re-added immediately', () => {
		let m = add(emptyMap(), FIRE, ALICE);
		m = applyRemoveEmoji(m, FIRE);
		expect(m.size).toBe(0);
		m = add(m, FIRE, ALICE);
		expect(getRecord(m, FIRE)!.count).toBe(1);
	});
});

describe('ReactionStateMachine: XState transition surface', () => {
	it('keeps empty snapshots stable for no-op empty-state events', () => {
		const snapshot = createReactionMachineSnapshot(emptyMap(), ME);
		expect(
			transitionReactionSnapshot(snapshot, {
				type: 'reaction.hydrate',
				reactions: [],
				currentUserId: ME,
			}),
		).toBe(snapshot);
		expect(transitionReactionSnapshot(snapshot, {type: 'reaction.removeAll'})).toBe(snapshot);
		expect(
			transitionReactionSnapshot(snapshot, {
				type: 'reaction.removeEmoji',
				emoji: FIRE,
			}),
		).toBe(snapshot);
	});
	it('updates empty snapshot context when a hydrate changes current user', () => {
		const snapshot = createReactionMachineSnapshot(emptyMap(), ME);
		const next = transitionReactionSnapshot(snapshot, {
			type: 'reaction.hydrate',
			reactions: [],
			currentUserId: ALICE,
		});
		expect(next).not.toBe(snapshot);
		expect(next.context.currentUserId).toBe(ALICE);
	});
	it('transitions maps directly without allocating for empty no-op events', () => {
		const map = emptyMap();
		expect(transitionReactionMap(map, {type: 'reaction.removeAll'})).toBe(map);
		expect(transitionReactionMap(map, {type: 'reaction.removeEmoji', emoji: FIRE})).toBe(map);
		expect(
			transitionReactionMap(map, {
				type: 'reaction.trackReactors',
				emoji: FIRE,
				userIds: [ALICE, BOB],
			}),
		).toBe(map);
	});
	it('moves between empty and active states from reaction events', () => {
		let snapshot = createReactionMachineSnapshot(emptyMap(), ME);
		expect(getReactionStateValue(snapshot)).toBe('empty');
		snapshot = transitionReactionSnapshot(snapshot, {
			type: 'reaction.add',
			emoji: FIRE,
			userId: ME,
			isCurrentUser: true,
		});
		expect(getReactionStateValue(snapshot)).toBe('active');
		expect(getRecord(snapshot.context.map, FIRE)!.count).toBe(1);
		snapshot = transitionReactionSnapshot(snapshot, {
			type: 'reaction.remove',
			emoji: FIRE,
			userId: ME,
			isCurrentUser: true,
		});
		expect(getReactionStateValue(snapshot)).toBe('empty');
		expect(getRecord(snapshot.context.map, FIRE)).toBeUndefined();
	});
	it('trackReactors batches fetched users without inflating an already-correct count', () => {
		let m = hydrate(emptyMap(), [{emoji: FIRE, count: 3}]);
		m = trackReactors(m, FIRE, [ALICE, BOB, CAROL]);
		expect(getRecord(m, FIRE)!.count).toBe(3);
		expect([...getRecord(m, FIRE)!.knownReactors].sort()).toEqual([ALICE, BOB, CAROL].sort());
	});
	it('stress: repeated add/remove echoes keep count, me, and known reactors coherent', () => {
		const users = [ME, ALICE, BOB, CAROL];
		const expected = new Set<string>();
		let snapshot = createReactionMachineSnapshot(emptyMap(), ME);
		for (let i = 0; i < 600; i++) {
			const userId = users[(i * 17 + 3) % users.length];
			const shouldAdd = (i * 7) % 5 < 3 || !expected.has(userId);
			if (shouldAdd) {
				expected.add(userId);
				snapshot = transitionReactionSnapshot(snapshot, {
					type: 'reaction.add',
					emoji: FIRE,
					userId,
					isCurrentUser: userId === ME,
				});
				if (i % 11 === 0) {
					snapshot = transitionReactionSnapshot(snapshot, {
						type: 'reaction.add',
						emoji: FIRE,
						userId,
						isCurrentUser: userId === ME,
					});
				}
			} else {
				expected.delete(userId);
				snapshot = transitionReactionSnapshot(snapshot, {
					type: 'reaction.remove',
					emoji: FIRE,
					userId,
					isCurrentUser: userId === ME,
				});
				if (i % 13 === 0) {
					snapshot = transitionReactionSnapshot(snapshot, {
						type: 'reaction.remove',
						emoji: FIRE,
						userId,
						isCurrentUser: userId === ME,
					});
				}
			}
			const record = getRecord(snapshot.context.map, FIRE);
			if (expected.size === 0) {
				expect(record).toBeUndefined();
				expect(getReactionStateValue(snapshot)).toBe('empty');
			} else {
				expect(record!.count).toBe(expected.size);
				expect(record!.me).toBe(expected.has(ME));
				expect([...record!.knownReactors].sort()).toEqual([...expected].sort());
				expect(getReactionStateValue(snapshot)).toBe('active');
			}
		}
	});
});
