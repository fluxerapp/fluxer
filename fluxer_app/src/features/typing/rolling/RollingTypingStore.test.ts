// SPDX-License-Identifier: AGPL-3.0-or-later

import {autorun, runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const doubles = await vi.hoisted(async () => {
	const {observable} = await import('mobx');
	return {
		auth: observable({currentUserId: 'me' as string | null}),
		developerOptions: observable({showMyselfTyping: false}),
		blockedUserIds: observable.set<string>(),
		users: observable.map<string, {id: string}>(),
	};
});

vi.mock('@app/features/auth/state/Authentication', () => ({default: doubles.auth}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({default: doubles.developerOptions}));
vi.mock('@app/features/relationship/state/Relationships', () => ({
	default: {isBlocked: (userId: string) => doubles.blockedUserIds.has(userId)},
}));
vi.mock('@app/features/user/state/Users', () => ({
	default: {getUser: (userId: string) => doubles.users.get(userId)},
}));

const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');

const CHANNEL = 'channel';

beforeEach(() => {
	vi.useFakeTimers();
	runInAction(() => {
		doubles.auth.currentUserId = 'me';
		doubles.developerOptions.showMyselfTyping = false;
		doubles.blockedUserIds.clear();
		doubles.users.clear();
		for (const id of ['me', 'alice', 'bob', 'carol']) {
			doubles.users.set(id, {id});
		}
	});
});

afterEach(() => {
	RollingTypingStore.reset();
	vi.restoreAllMocks();
	vi.useRealTimers();
});

describe('RollingTypingStore', () => {
	it('arms a 10000 ms expiry for a new typist', () => {
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		expect(vi.getTimerCount()).toBe(1);

		vi.advanceTimersByTime(9999);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(true);

		vi.advanceTimersByTime(1);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(false);
		expect(vi.getTimerCount()).toBe(0);
	});

	it('restarts the window on a repeated typing start', () => {
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		vi.advanceTimersByTime(6000);
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		expect(vi.getTimerCount()).toBe(1);

		vi.advanceTimersByTime(9999);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(true);

		vi.advanceTimersByTime(1);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(false);
	});

	it('keeps insertion order when a typist refreshes', () => {
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');
		RollingTypingStore.start(CHANNEL, 'carol', 'gateway');
		vi.advanceTimersByTime(3000);

		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');

		expect(RollingTypingStore.getTypingUserIds(CHANNEL)).toEqual(['alice', 'bob', 'carol']);
	});

	it('does not notify observers when an existing entry is refreshed', () => {
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		const runs = vi.fn();
		const dispose = autorun(() => {
			runs(
				RollingTypingStore.getTypingUserIds(CHANNEL),
				RollingTypingStore.countTypists(CHANNEL),
				RollingTypingStore.isTyping(CHANNEL, 'alice'),
				RollingTypingStore.isConfirmedTyping(CHANNEL, 'alice'),
			);
		});
		expect(runs).toHaveBeenCalledTimes(1);

		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		RollingTypingStore.start(CHANNEL, 'alice', 'local');
		vi.advanceTimersByTime(9000);
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');

		expect(runs).toHaveBeenCalledTimes(1);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(true);
		dispose();
	});

	it('marks an entry confirmed only from a gateway typing start', () => {
		RollingTypingStore.start(CHANNEL, 'me', 'local');
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(true);
		expect(RollingTypingStore.isConfirmedTyping(CHANNEL, 'me')).toBe(false);

		RollingTypingStore.start(CHANNEL, 'me', 'gateway');

		expect(RollingTypingStore.isConfirmedTyping(CHANNEL, 'me')).toBe(true);
	});

	it('never lowers a confirmed entry on a local write', () => {
		RollingTypingStore.start(CHANNEL, 'me', 'gateway');
		RollingTypingStore.start(CHANNEL, 'me', 'local');

		expect(RollingTypingStore.isConfirmedTyping(CHANNEL, 'me')).toBe(true);
	});

	it('removes the author on message create for any author', () => {
		RollingTypingStore.start(CHANNEL, 'me', 'local');
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');

		RollingTypingStore.remove(CHANNEL, 'alice');
		RollingTypingStore.remove(CHANNEL, 'me');

		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(false);
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
		expect(vi.getTimerCount()).toBe(0);
	});

	it('wipes every entry and clears every timer on reset', () => {
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		RollingTypingStore.start(CHANNEL, 'me', 'local');
		RollingTypingStore.start('other-channel', 'bob', 'gateway');
		expect(RollingTypingStore.getTypingUserIds(CHANNEL)).toEqual(['alice']);
		expect(vi.getTimerCount()).toBe(3);

		RollingTypingStore.reset();

		expect(vi.getTimerCount()).toBe(0);
		expect(RollingTypingStore.countTypists(CHANNEL)).toBe(0);
		expect(RollingTypingStore.countTypists('other-channel')).toBe(0);
		expect(RollingTypingStore.getTypingUserIds(CHANNEL)).toEqual([]);
	});

	it('does not let a timer from before a reset expire a fresh entry', () => {
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		vi.advanceTimersByTime(6000);
		RollingTypingStore.reset();
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');

		vi.advanceTimersByTime(4000);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(true);

		vi.advanceTimersByTime(5999);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(true);

		vi.advanceTimersByTime(1);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(false);
	});

	it('ignores an expiry callback whose handle has been replaced', () => {
		const setTimeoutSpy = vi.spyOn(globalThis, 'setTimeout');
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		const staleExpiry = setTimeoutSpy.mock.calls[0]![0] as () => void;

		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		staleExpiry();

		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(true);
		expect(vi.getTimerCount()).toBe(1);
	});

	it('counts raw entries including self and blocked users', () => {
		runInAction(() => {
			doubles.blockedUserIds.add('bob');
			doubles.users.delete('carol');
		});
		RollingTypingStore.start(CHANNEL, 'me', 'local');
		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');
		RollingTypingStore.start(CHANNEL, 'carol', 'gateway');
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');

		expect(RollingTypingStore.countTypists(CHANNEL)).toBe(4);
		expect(RollingTypingStore.getTypingUserIds(CHANNEL)).toEqual(['alice']);
	});

	it('keeps no cap on tracked typists', () => {
		runInAction(() => {
			for (let index = 0; index < 50; index += 1) {
				doubles.users.set(`typist-${index}`, {id: `typist-${index}`});
			}
		});
		for (let index = 0; index < 50; index += 1) {
			RollingTypingStore.start(CHANNEL, `typist-${index}`, 'gateway');
		}

		expect(RollingTypingStore.countTypists(CHANNEL)).toBe(50);
		expect(RollingTypingStore.getTypingUserIds(CHANNEL)).toHaveLength(50);
	});

	it('drops the channel map when its last typist leaves', () => {
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');

		RollingTypingStore.remove(CHANNEL, 'alice');
		expect(RollingTypingStore['entries'].has(CHANNEL)).toBe(true);

		RollingTypingStore.remove(CHANNEL, 'bob');
		expect(RollingTypingStore['entries'].has(CHANNEL)).toBe(false);

		RollingTypingStore.start(CHANNEL, 'carol', 'gateway');
		vi.advanceTimersByTime(10000);
		expect(RollingTypingStore['entries'].has(CHANNEL)).toBe(false);
	});

	it('notifies a member row reader only for its own user', () => {
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		const aliceRow = vi.fn();
		const bobRow = vi.fn();
		const disposeAliceRow = autorun(() => aliceRow(RollingTypingStore.isTyping(CHANNEL, 'alice')));
		const disposeBobRow = autorun(() => bobRow(RollingTypingStore.isTyping(CHANNEL, 'bob')));

		RollingTypingStore.start(CHANNEL, 'carol', 'gateway');
		RollingTypingStore.start(CHANNEL, 'me', 'local');
		RollingTypingStore.start(CHANNEL, 'me', 'gateway');
		RollingTypingStore.remove(CHANNEL, 'carol');
		expect(aliceRow).toHaveBeenCalledTimes(1);
		expect(bobRow).toHaveBeenCalledTimes(1);

		RollingTypingStore.start(CHANNEL, 'bob', 'gateway');
		expect(bobRow).toHaveBeenCalledTimes(2);
		expect(bobRow).toHaveBeenLastCalledWith(true);
		expect(aliceRow).toHaveBeenCalledTimes(1);

		RollingTypingStore.remove(CHANNEL, 'bob');
		expect(bobRow).toHaveBeenCalledTimes(3);
		expect(bobRow).toHaveBeenLastCalledWith(false);
		expect(aliceRow).toHaveBeenCalledTimes(1);

		disposeAliceRow();
		disposeBobRow();
	});
});
