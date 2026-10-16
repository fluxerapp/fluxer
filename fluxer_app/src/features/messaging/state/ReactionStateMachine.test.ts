// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	emptyMap,
	getEmojiKey,
	type ReactionMap,
	transitionReactionMap,
} from '@app/features/messaging/state/ReactionStateMachine';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import type {MessageReaction} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {describe, expect, it} from 'vitest';

const ME = 'me-user-id';
const ALICE = 'alice-id';
const BOB = 'bob-id';
const CAROL = 'carol-id';
const FIRE: ReactionEmoji = {name: '🔥'};
const HEART: ReactionEmoji = {name: '❤️'};
const add = (map: ReactionMap, emoji: ReactionEmoji, userId: string, isCurrentUser = userId === ME) =>
	transitionReactionMap(map, {type: 'reaction.add', emoji, userId, isCurrentUser});
const remove = (map: ReactionMap, emoji: ReactionEmoji, userId: string, isCurrentUser = userId === ME) =>
	transitionReactionMap(map, {type: 'reaction.remove', emoji, userId, isCurrentUser});
const getRecord = (map: ReactionMap, emoji: ReactionEmoji) => map.get(getEmojiKey(emoji));
const hydrate = (map: ReactionMap, reactions: ReadonlyArray<MessageReaction>, currentUserId?: string) =>
	transitionReactionMap(map, {type: 'reaction.hydrate', reactions, currentUserId}, currentUserId);

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

describe('ReactionStateMachine: hydrate', () => {
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
