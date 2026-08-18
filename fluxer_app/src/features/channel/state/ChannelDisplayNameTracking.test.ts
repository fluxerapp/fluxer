// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type TrackedRecipientSnapshot,
	type TrackedRecipientUser,
	trackedRecipientNameKey,
} from '@app/features/channel/state/ChannelDisplayNameTracking';
import {autorun, observable, runInAction} from 'mobx';
import {describe, expect, it} from 'vitest';

const snapshots = (entries: Record<string, ReadonlyArray<string>>): ReadonlyMap<string, TrackedRecipientSnapshot> =>
	new Map(Object.entries(entries).map(([id, recipientIds]) => [id, {recipientIds}]));

const userTable = (count: number): Record<string, TrackedRecipientUser> => {
	const users: Record<string, TrackedRecipientUser> = {};
	for (let i = 0; i < count; i++) {
		users[`u${i}`] = {username: `user${i}`, globalName: null};
	}
	return users;
};

/** Wraps a user table so we can count exactly which keys get read. */
const countingUsers = (
	users: Record<string, TrackedRecipientUser | undefined>,
): {proxy: Record<string, TrackedRecipientUser | undefined>; reads: Array<string>} => {
	const reads: Array<string> = [];
	const proxy = new Proxy(users, {
		get(target, key, receiver) {
			if (typeof key === 'string') reads.push(key);
			return Reflect.get(target, key, receiver);
		},
	});
	return {proxy, reads};
};

describe('trackedRecipientNameKey', () => {
	it('reads only the recipients of tracked group DMs, not the whole user cache', () => {
		const {proxy, reads} = countingUsers(userTable(10_000));
		trackedRecipientNameKey(snapshots({c1: ['u1', 'u2'], c2: ['u3']}), proxy);

		// The cost must be a function of tracked recipients, never of cache size.
		expect(reads).toEqual(['u1', 'u2', 'u3']);
	});

	it('cost is independent of how large the user cache grows', () => {
		const small = countingUsers(userTable(10));
		const huge = countingUsers(userTable(100_000));
		const tracked = snapshots({c1: ['u1', 'u2']});

		trackedRecipientNameKey(tracked, small.proxy);
		trackedRecipientNameKey(tracked, huge.proxy);

		expect(small.reads.length).toBe(2);
		expect(huge.reads.length).toBe(2);
	});

	it('changes when a tracked recipient is renamed', () => {
		const users = userTable(3);
		const tracked = snapshots({c1: ['u1']});
		const before = trackedRecipientNameKey(tracked, users);

		users.u1 = {username: 'renamed', globalName: null};

		expect(trackedRecipientNameKey(tracked, users)).not.toBe(before);
	});

	it('does not change when an untracked user is renamed', () => {
		const users = userTable(3);
		const tracked = snapshots({c1: ['u1']});
		const before = trackedRecipientNameKey(tracked, users);

		users.u2 = {username: 'unrelated-change', globalName: 'also changed'};

		expect(trackedRecipientNameKey(tracked, users)).toBe(before);
	});

	it('distinguishes a missing recipient from one that arrives later', () => {
		const users: Record<string, TrackedRecipientUser | undefined> = {};
		const tracked = snapshots({c1: ['u1']});
		const before = trackedRecipientNameKey(tracked, users);

		users.u1 = {username: 'arrived', globalName: null};

		expect(trackedRecipientNameKey(tracked, users)).not.toBe(before);
	});

	it('is not confused by usernames containing the separator characters', () => {
		const tracked = snapshots({c1: ['a', 'b']});
		const collidingLeft: Record<string, TrackedRecipientUser> = {
			a: {username: 'x", "y', globalName: null},
			b: {username: '', globalName: null},
		};
		const collidingRight: Record<string, TrackedRecipientUser> = {
			a: {username: '', globalName: null},
			b: {username: 'x", "y', globalName: null},
		};

		expect(trackedRecipientNameKey(tracked, collidingLeft)).not.toBe(trackedRecipientNameKey(tracked, collidingRight));
	});

	it('drives a MobX reaction on tracked renames but stays quiet on untracked ones', () => {
		const users = observable.object<Record<string, TrackedRecipientUser | undefined>>({
			u1: {username: 'one', globalName: null},
			u2: {username: 'two', globalName: null},
		});
		const tracked = snapshots({c1: ['u1']});

		let runs = 0;
		const dispose = autorun(() => {
			trackedRecipientNameKey(tracked, users);
			runs++;
		});
		expect(runs).toBe(1);

		// An untracked user changing must not wake the reaction.
		runInAction(() => {
			users.u2 = {username: 'two-renamed', globalName: null};
		});
		expect(runs).toBe(1);

		// A tracked user changing must.
		runInAction(() => {
			users.u1 = {username: 'one-renamed', globalName: null};
		});
		expect(runs).toBe(2);

		dispose();
	});
});
