// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {
	answerCountsEqual,
	applyAdd,
	applyRemove,
	createPollVoteMachineSnapshot,
	emptyMap,
	getPollVoteStateValue,
	getRecord,
	hydrate,
	mapToAnswerCounts,
	trackVoter,
	trackVoters,
	transitionPollVoteMap,
	transitionPollVoteSnapshot,
	untrackVoter,
} from './PollVoteStateMachine';

const ME = 'me-user-id';
const ALICE = 'alice-id';
const BOB = 'bob-id';
const CAROL = 'carol-id';

const ONE: number = 1;
const TWO: number = 2;
const THREE: number = 3;
const FOUR: number = 4;

const add = (map: any, answerId: number, userId: string, isMe = userId === ME) => applyAdd(map, answerId, userId, isMe);
const remove = (map: any, answerId: number, userId: string, isMe = userId === ME) =>
	applyRemove(map, answerId, userId, isMe);

describe('PollVoteStateMachine: applyAdd', () => {
	it('creates record on first add', () => {
		const m = add(emptyMap(), ONE, ALICE);
		const rec = getRecord(m, ONE);
		expect(rec).toBeDefined();
		expect(rec!.count).toBe(1);
		expect(rec!.me).toBe(false);
		expect(rec!.knownVoters.has(ALICE)).toBe(true);
	});
	it('increments count for second distinct voter', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, BOB);
		expect(getRecord(m, ONE)!.count).toBe(2);
	});
	it('is idempotent for same userId (no double count)', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, ALICE);
		m = add(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(1);
	});
	it('sets me=true when current user adds', () => {
		const m = add(emptyMap(), ONE, ME);
		expect(getRecord(m, ONE)!.me).toBe(true);
	});
	it('preserves me=true when other user adds after me', () => {
		let m = add(emptyMap(), ONE, ME);
		m = add(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.me).toBe(true);
		expect(getRecord(m, ONE)!.count).toBe(2);
	});
	it('upgrades me=false to me=true if current user re-adds (recovery)', () => {
		let m = add(emptyMap(), ONE, ME, false);
		expect(getRecord(m, ONE)!.me).toBe(false);
		m = add(m, ONE, ME, true);
		expect(getRecord(m, ONE)!.me).toBe(true);
		expect(getRecord(m, ONE)!.count).toBe(1);
	});
	it('different answers tracked independently', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, TWO, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(1);
		expect(getRecord(m, TWO)!.count).toBe(1);
	});
	it('answers with different ids are tracked separately', () => {
		let m = add(emptyMap(), THREE, ALICE);
		m = add(m, FOUR, ALICE);
		expect(getRecord(m, THREE)!.count).toBe(1);
		expect(getRecord(m, FOUR)!.count).toBe(1);
	});
});

describe('PollVoteStateMachine: applyRemove (PRIMARY BUG)', () => {
	it('REGRESSION: 2 voters, one removes → vote is preserved with count 1', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, BOB);
		expect(getRecord(m, ONE)!.count).toBe(2);
		m = remove(m, ONE, ALICE);
		const rec = getRecord(m, ONE);
		expect(rec).toBeDefined();
		expect(rec!.count).toBe(1);
		expect(rec!.knownVoters.has(BOB)).toBe(true);
		expect(rec!.knownVoters.has(ALICE)).toBe(false);
	});
	it('REGRESSION: me + other, other removes → vote kept, me preserved', () => {
		let m = add(emptyMap(), ONE, ME);
		m = add(m, ONE, ALICE);
		m = remove(m, ONE, ALICE);
		const rec = getRecord(m, ONE);
		expect(rec!.count).toBe(1);
		expect(rec!.me).toBe(true);
	});
	it('REGRESSION: me + other, me removes → vote kept with other', () => {
		let m = add(emptyMap(), ONE, ME);
		m = add(m, ONE, ALICE);
		m = remove(m, ONE, ME);
		const rec = getRecord(m, ONE);
		expect(rec!.count).toBe(1);
		expect(rec!.me).toBe(false);
	});
	it('removes vote entirely when last voter leaves', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = remove(m, ONE, ALICE);
		expect(getRecord(m, ONE)).toBeUndefined();
	});
	it('idempotent: removing same userId twice does not double-decrement', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, BOB);
		m = remove(m, ONE, ALICE);
		m = remove(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(1);
	});
	it('remove on unknown answer is a no-op', () => {
		const m = remove(emptyMap(), ONE, ALICE);
		expect(m.size).toBe(0);
	});
	it('remove unknown user when me=false but isCurrentUser=true is no-op', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = remove(m, ONE, ME, true);
		expect(getRecord(m, ONE)!.count).toBe(1);
	});
	it('untracked user remove (not in knownVoters but server says they left)', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = hydrate(m, [{id: ONE, count: 5}]);
		expect(getRecord(m, ONE)!.count).toBe(5);
		m = remove(m, ONE, CAROL);
		expect(getRecord(m, ONE)!.count).toBe(4);
	});
	it('me=true server hydration + me removes drops me flag and count', () => {
		let m = hydrate(emptyMap(), [{id: ONE, count: 3, me_voted: true}]);
		expect(getRecord(m, ONE)!.me).toBe(true);
		m = remove(m, ONE, ME);
		expect(getRecord(m, ONE)!.count).toBe(2);
		expect(getRecord(m, ONE)!.me).toBe(false);
	});
	it('count cannot go below zero', () => {
		let m = hydrate(emptyMap(), [{id: ONE, count: 1}]);
		m = remove(m, ONE, ALICE);
		m = remove(m, ONE, BOB);
		expect(getRecord(m, ONE)).toBeUndefined();
	});
});

describe('PollVoteStateMachine: hydrate', () => {
	it('hydrates an empty map from wire votes', () => {
		const m = hydrate(emptyMap(), [{id: ONE, count: 5, me_voted: true}]);
		expect(getRecord(m, ONE)!.count).toBe(5);
		expect(getRecord(m, ONE)!.me).toBe(true);
	});
	it('drops zero-count wire votes', () => {
		const m = hydrate(emptyMap(), [{id: ONE, count: 0}]);
		expect(m.size).toBe(0);
	});
	it('replaces existing votes but preserves knownVoters', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, BOB);
		m = hydrate(m, [{id: ONE, count: 10}]);
		expect(getRecord(m, ONE)!.count).toBe(10);
		expect(getRecord(m, ONE)!.knownVoters.size).toBe(2);
	});
	it('hydrate with smaller server count never drops below known voter count', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, BOB);
		m = add(m, ONE, CAROL);
		m = hydrate(m, [{id: ONE, count: 1}]);
		expect(getRecord(m, ONE)!.count).toBe(3);
	});
	it('hydrate without an answer drops it from the map', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, TWO, BOB);
		m = hydrate(m, [{id: ONE, count: 1}]);
		expect(getRecord(m, ONE)).toBeDefined();
		expect(getRecord(m, TWO)).toBeUndefined();
	});
	it('hydrate with null/empty wipes when present', () => {
		const m = add(emptyMap(), ONE, ALICE);
		expect(hydrate(m, null).size).toBe(0);
		expect(hydrate(m, []).size).toBe(0);
		expect(hydrate(m, undefined).size).toBe(0);
	});
	it('hydrate is identity when nothing changes', () => {
		const m = hydrate(emptyMap(), [{id: ONE, count: 3}]);
		const again = hydrate(m, [{id: ONE, count: 3}]);
		expect(again).toBe(m);
	});
	it('hydrate preserves me flag if server says me=true even if locally false', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = hydrate(m, [{id: ONE, count: 1, me_voted: true}]);
		expect(getRecord(m, ONE)!.me).toBe(true);
	});
	it('hydrate preserves locally-known me=true even if server omits it', () => {
		let m = add(emptyMap(), ONE, ME);
		m = hydrate(m, [{id: ONE, count: 5}]);
		expect(getRecord(m, ONE)!.me).toBe(true);
	});
	it('REGRESSION: stale hydrate does not resurrect a removed current-user vote', () => {
		let m = hydrate(emptyMap(), [{id: ONE, count: 1, me_voted: true}], ME);
		m = add(m, ONE, BOB, false);
		m = remove(m, ONE, ME, true);
		expect(getRecord(m, ONE)!.count).toBe(1);
		expect(getRecord(m, ONE)!.me).toBe(false);
		m = hydrate(m, [{id: ONE, count: 2, me_voted: true}], ME);
		expect(getRecord(m, ONE)!.count).toBe(1);
		expect(getRecord(m, ONE)!.me).toBe(false);
		expect(getRecord(m, ONE)!.knownVoters.has(BOB)).toBe(true);
	});
	it('REGRESSION: stale hydrate does not re-count a removed remote voter', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, BOB);
		m = remove(m, ONE, ALICE);
		m = hydrate(m, [{id: ONE, count: 2}], ME);
		expect(getRecord(m, ONE)!.count).toBe(1);
		expect(getRecord(m, ONE)!.knownVoters.has(BOB)).toBe(true);
		expect(getRecord(m, ONE)!.knownVoters.has(ALICE)).toBe(false);
	});
});

describe('PollVoteStateMachine: mapToAnswerCounts', () => {
	it('produces a frozen array', () => {
		const m = add(emptyMap(), ONE, ALICE);
		const arr = mapToAnswerCounts(m);
		expect(Object.isFrozen(arr)).toBe(true);
	});
	it('omits me when me_voted=false', () => {
		const m = add(emptyMap(), ONE, ALICE);
		const arr = mapToAnswerCounts(m);
		expect(arr[0].me_voted).toBeUndefined();
	});
	it('includes me_voted=true when me voted', () => {
		const m = add(emptyMap(), ONE, ME);
		const arr = mapToAnswerCounts(m);
		expect(arr[0].me_voted).toBe(true);
	});
	it('answerCountsEqual identifies count/me changes', () => {
		const a = [{id: ONE, count: 2}] as any;
		const b = [{id: ONE, count: 2}] as any;
		expect(answerCountsEqual(a, b)).toBe(true);
		const c = [{id: ONE, count: 3}] as any;
		expect(answerCountsEqual(a, c)).toBe(false);
		const d = [{id: ONE, count: 2, me_voted: true}] as any;
		expect(answerCountsEqual(a, d)).toBe(false);
	});
});

describe('PollVoteStateMachine: trackVoter / untrackVoter', () => {
	it('trackVoter adds known userId without changing count if count already reflects', () => {
		let m = hydrate(emptyMap(), [{id: ONE, count: 3}]);
		m = trackVoter(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(3);
		expect(getRecord(m, ONE)!.knownVoters.has(ALICE)).toBe(true);
	});
	it('trackVoter bumps count if known voters exceed reported count', () => {
		let m = hydrate(emptyMap(), [{id: ONE, count: 1}]);
		m = trackVoter(m, ONE, ALICE);
		m = trackVoter(m, ONE, BOB);
		m = trackVoter(m, ONE, CAROL);
		expect(getRecord(m, ONE)!.count).toBe(3);
	});
	it('trackVoter is idempotent', () => {
		let m = hydrate(emptyMap(), [{id: ONE, count: 3}]);
		m = trackVoter(m, ONE, ALICE);
		const before = m;
		m = trackVoter(m, ONE, ALICE);
		expect(m).toBe(before);
	});
	it('untrackVoter removes from knownVoters without changing count', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, BOB);
		m = untrackVoter(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(2);
		expect(getRecord(m, ONE)!.knownVoters.has(ALICE)).toBe(false);
	});
	it('untrackVoter on unknown user is no-op', () => {
		let m = add(emptyMap(), ONE, ALICE);
		const before = m;
		m = untrackVoter(m, ONE, BOB);
		expect(m).toBe(before);
	});
});

describe('PollVoteStateMachine: complex scenarios', () => {
	it('full lifecycle: add me, add 2 others, others leave one by one, me leaves', () => {
		let m = emptyMap();
		m = add(m, ONE, ME);
		expect(getRecord(m, ONE)!.count).toBe(1);
		m = add(m, ONE, ALICE);
		m = add(m, ONE, BOB);
		expect(getRecord(m, ONE)!.count).toBe(3);
		expect(getRecord(m, ONE)!.me).toBe(true);
		m = remove(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(2);
		expect(getRecord(m, ONE)!.me).toBe(true);
		m = remove(m, ONE, BOB);
		expect(getRecord(m, ONE)!.count).toBe(1);
		expect(getRecord(m, ONE)!.me).toBe(true);
		m = remove(m, ONE, ME);
		expect(getRecord(m, ONE)).toBeUndefined();
	});
	it('optimistic + gateway echo do not double-count', () => {
		let m = emptyMap();
		m = add(m, ONE, ME);
		m = add(m, ONE, ME);
		expect(getRecord(m, ONE)!.count).toBe(1);
		expect(getRecord(m, ONE)!.me).toBe(true);
	});
	it('out-of-order remove + add gateway events', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = remove(m, ONE, ALICE);
		m = add(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(1);
	});
	it('two answers with overlapping voters stay independent', () => {
		let m = emptyMap();
		m = add(m, ONE, ALICE);
		m = add(m, ONE, BOB);
		m = add(m, TWO, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(2);
		expect(getRecord(m, TWO)!.count).toBe(1);
		m = remove(m, TWO, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(2);
		expect(getRecord(m, TWO)).toBeUndefined();
	});
	it('hydrate after concurrent local adds keeps locally-known voters', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = add(m, ONE, BOB);
		m = hydrate(m, [{id: ONE, count: 2}]);
		expect(getRecord(m, ONE)!.knownVoters.size).toBe(2);
		m = remove(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(1);
	});
	it('voter leaves then rejoins via gateway', () => {
		let m = add(emptyMap(), ONE, ALICE);
		m = remove(m, ONE, ALICE);
		expect(getRecord(m, ONE)).toBeUndefined();
		m = add(m, ONE, ALICE);
		expect(getRecord(m, ONE)!.count).toBe(1);
		expect(getRecord(m, ONE)!.knownVoters.has(ALICE)).toBe(true);
	});
	it('frozen output: original map not mutated by operations', () => {
		const m1 = add(emptyMap(), ONE, ALICE);
		const snap1 = mapToAnswerCounts(m1);
		const m2 = add(m1, ONE, BOB);
		const snap1AfterMutation = mapToAnswerCounts(m1);
		expect(snap1).toEqual(snap1AfterMutation);
		expect(getRecord(m1, ONE)!.count).toBe(1);
		expect(getRecord(m2, ONE)!.count).toBe(2);
	});
});

describe('PollVoteStateMachine: XState transition surface', () => {
	it('keeps empty snapshots stable for no-op empty-state events', () => {
		const snapshot = createPollVoteMachineSnapshot(emptyMap(), ME);
		expect(
			transitionPollVoteSnapshot(snapshot, {
				type: 'pollVote.hydrate',
				votes: [],
				currentUserId: ME,
			}),
		).toBe(snapshot);
	});
	it('updates empty snapshot context when a hydrate changes current user', () => {
		const snapshot = createPollVoteMachineSnapshot(emptyMap(), ME);
		const next = transitionPollVoteSnapshot(snapshot, {
			type: 'pollVote.hydrate',
			votes: [],
			currentUserId: ALICE,
		});
		expect(next).not.toBe(snapshot);
		expect(next.context.currentUserId).toBe(ALICE);
	});
	it('transitions maps directly without allocating for empty no-op events', () => {
		const map = emptyMap();
		expect(
			transitionPollVoteMap(map, {
				type: 'pollVote.trackVoters',
				answerId: ONE,
				userIds: [ALICE, BOB],
			}),
		).toBe(map);
	});
	it('moves between empty and active states from vote events', () => {
		let snapshot = createPollVoteMachineSnapshot(emptyMap(), ME);
		expect(getPollVoteStateValue(snapshot)).toBe('empty');
		snapshot = transitionPollVoteSnapshot(snapshot, {
			type: 'pollVote.add',
			answerId: ONE,
			userId: ME,
			isCurrentUser: true,
		});
		expect(getPollVoteStateValue(snapshot)).toBe('active');
		expect(getRecord(snapshot.context.map, ONE)!.count).toBe(1);
		snapshot = transitionPollVoteSnapshot(snapshot, {
			type: 'pollVote.remove',
			answerId: ONE,
			userId: ME,
			isCurrentUser: true,
		});
		expect(getPollVoteStateValue(snapshot)).toBe('empty');
		expect(getRecord(snapshot.context.map, ONE)).toBeUndefined();
	});
	it('trackVoters batches fetched users without inflating an already-correct count', () => {
		let m = hydrate(emptyMap(), [{id: ONE, count: 3}]);
		m = trackVoters(m, ONE, [ALICE, BOB, CAROL]);
		expect(getRecord(m, ONE)!.count).toBe(3);
		expect([...getRecord(m, ONE)!.knownVoters].sort()).toEqual([ALICE, BOB, CAROL].sort());
	});
	it('stress: repeated add/remove echoes keep count, me, and known voters coherent', () => {
		const users = [ME, ALICE, BOB, CAROL];
		const expected = new Set<string>();
		let snapshot = createPollVoteMachineSnapshot(emptyMap(), ME);
		for (let i = 0; i < 600; i++) {
			const userId = users[(i * 17 + 3) % users.length];
			const shouldAdd = (i * 7) % 5 < 3 || !expected.has(userId);
			if (shouldAdd) {
				expected.add(userId);
				snapshot = transitionPollVoteSnapshot(snapshot, {
					type: 'pollVote.add',
					answerId: ONE,
					userId,
					isCurrentUser: userId === ME,
				});
				if (i % 11 === 0) {
					snapshot = transitionPollVoteSnapshot(snapshot, {
						type: 'pollVote.add',
						answerId: ONE,
						userId,
						isCurrentUser: userId === ME,
					});
				}
			} else {
				expected.delete(userId);
				snapshot = transitionPollVoteSnapshot(snapshot, {
					type: 'pollVote.remove',
					answerId: ONE,
					userId,
					isCurrentUser: userId === ME,
				});
				if (i % 13 === 0) {
					snapshot = transitionPollVoteSnapshot(snapshot, {
						type: 'pollVote.remove',
						answerId: ONE,
						userId,
						isCurrentUser: userId === ME,
					});
				}
			}
			const record = getRecord(snapshot.context.map, ONE);
			if (expected.size === 0) {
				expect(record).toBeUndefined();
				expect(getPollVoteStateValue(snapshot)).toBe('empty');
			} else {
				expect(record!.count).toBe(expected.size);
				expect(record!.me).toBe(expected.has(ME));
				expect([...record!.knownVoters].sort()).toEqual([...expected].sort());
				expect(getPollVoteStateValue(snapshot)).toBe('active');
			}
		}
	});
});
