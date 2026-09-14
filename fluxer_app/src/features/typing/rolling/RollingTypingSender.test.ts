// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const doubles = await vi.hoisted(async () => {
	const {observable} = await import('mobx');
	return {
		auth: observable({currentUserId: 'me' as string | null}),
		post: vi.fn((_path: string) => Promise.resolve({ok: true})),
		logError: vi.fn(),
	};
});

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = doubles.logError;
	},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({http: {post: doubles.post}}));
vi.mock('@app/features/auth/state/Authentication', () => ({default: doubles.auth}));
vi.mock('@app/features/devtools/state/DeveloperOptions', () => ({default: {showMyselfTyping: false}}));
vi.mock('@app/features/relationship/state/Relationships', () => ({default: {isBlocked: () => false}}));
vi.mock('@app/features/user/state/Users', () => ({default: {getUser: () => undefined}}));

const {default: RollingTypingSender} = await import('@app/features/typing/rolling/RollingTypingSender');
const {default: RollingTypingStore} = await import('@app/features/typing/rolling/RollingTypingStore');

const START_TIME = 1_000_000;
const CHANNEL = 'channel';
const OTHER_CHANNEL = 'other-channel';

let postTimes: Array<number> = [];

function elapsed(): number {
	return Date.now() - START_TIME;
}

function typeContinuously(durationMs: number): void {
	const startedAt = Date.now();
	while (Date.now() - startedAt < durationMs) {
		RollingTypingSender.startTyping(CHANNEL);
		vi.advanceTimersByTime(100);
	}
}

function advanceTo(elapsedMs: number): void {
	vi.advanceTimersByTime(elapsedMs - elapsed());
}

function startOtherTypists(count: number): void {
	for (let index = 0; index < count; index += 1) {
		RollingTypingStore.start(CHANNEL, `typist-${index}`, 'gateway');
	}
}

beforeEach(() => {
	vi.useFakeTimers();
	vi.setSystemTime(START_TIME);
	postTimes = [];
	doubles.post.mockImplementation((_path: string) => {
		postTimes.push(elapsed());
		return Promise.resolve({ok: true});
	});
});

afterEach(() => {
	RollingTypingSender.reset();
	RollingTypingStore.reset();
	runInAction(() => {
		doubles.auth.currentUserId = 'me';
	});
	vi.useRealTimers();
	vi.clearAllMocks();
});

describe('RollingTypingSender', () => {
	it('posts once 1500 ms after the first keystroke and not before', () => {
		RollingTypingSender.startTyping(CHANNEL);

		vi.advanceTimersByTime(1499);
		expect(doubles.post).not.toHaveBeenCalled();

		vi.advanceTimersByTime(1);
		expect(doubles.post).toHaveBeenCalledExactlyOnceWith(Endpoints.CHANNEL_TYPING(CHANNEL));

		vi.advanceTimersByTime(20000);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('does not push a pending send back on further keystrokes', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(500);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(1400);
		RollingTypingSender.startTyping(CHANNEL);

		advanceTo(1500);

		expect(postTimes).toEqual([1500]);
	});

	it('writes the self entry at schedule time', () => {
		RollingTypingSender.startTyping(CHANNEL);

		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(true);
		expect(RollingTypingStore.isConfirmedTyping(CHANNEL, 'me')).toBe(false);
		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('writes nothing on a throttled keystroke', () => {
		RollingTypingSender.startTyping(CHANNEL);
		RollingTypingStore.remove(CHANNEL, 'me');
		const timersBefore = vi.getTimerCount();

		advanceTo(700);
		RollingTypingSender.startTyping(CHANNEL);

		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
		expect(vi.getTimerCount()).toBe(timersBefore);
	});

	it('sends every 8000 ms while typing continuously', () => {
		typeContinuously(20000);
		vi.advanceTimersByTime(20000);

		expect(postTimes).toEqual([1500, 9500, 17500]);
	});

	it('posts the second time about 9500 ms after the first keystroke', () => {
		typeContinuously(12000);

		expect(postTimes[1]).toBe(9500);
		expect(postTimes[1]! - postTimes[0]!).toBe(8000);
	});

	it('posts immediately when typing resumes 16000 ms after the last schedule', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(16000);

		RollingTypingSender.startTyping(CHANNEL);
		vi.advanceTimersByTime(0);

		expect(postTimes).toEqual([1500, 16000]);
	});

	it('skips the post when six entries including self are typing', () => {
		startOtherTypists(5);
		RollingTypingSender.startTyping(CHANNEL);

		vi.advanceTimersByTime(1500);

		expect(RollingTypingStore.countTypists(CHANNEL)).toBe(6);
		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('posts with five entries including self typing', () => {
		startOtherTypists(4);
		RollingTypingSender.startTyping(CHANNEL);

		vi.advanceTimersByTime(1500);

		expect(RollingTypingStore.countTypists(CHANNEL)).toBe(5);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('advances the throttle clock when the post is skipped', () => {
		startOtherTypists(5);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(1500);
		expect(doubles.post).not.toHaveBeenCalled();

		advanceTo(5000);
		for (let index = 0; index < 5; index += 1) {
			RollingTypingStore.remove(CHANNEL, `typist-${index}`);
		}
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(7999);
		expect(doubles.post).not.toHaveBeenCalled();

		advanceTo(8000);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(9500);
		expect(postTimes).toEqual([9500]);
	});

	it('does not retry a failed post', async () => {
		doubles.post.mockImplementationOnce(() => Promise.reject(new Error('Too many requests')));
		RollingTypingSender.startTyping(CHANNEL);

		await vi.advanceTimersByTimeAsync(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);
		expect(doubles.logError).toHaveBeenCalledExactlyOnceWith(
			`Failed to send typing indicator to channel ${CHANNEL}:`,
			expect.any(Error),
		);

		await vi.advanceTimersByTimeAsync(30000);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('keeps the throttle after a failed post', async () => {
		doubles.post.mockImplementationOnce(() => Promise.reject(new Error('Network down')));
		RollingTypingSender.startTyping(CHANNEL);
		await vi.advanceTimersByTimeAsync(1500);

		advanceTo(3000);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(7999);
		expect(doubles.post).toHaveBeenCalledTimes(1);

		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(8000);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(9500);
		expect(doubles.post).toHaveBeenCalledTimes(2);
	});

	it('drops the slot and cancels a pending send on a keystroke in another channel', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(500);

		RollingTypingSender.startTyping(OTHER_CHANNEL);
		vi.advanceTimersByTime(20000);

		expect(doubles.post).toHaveBeenCalledExactlyOnceWith(Endpoints.CHANNEL_TYPING(OTHER_CHANNEL));
		expect(postTimes).toEqual([2000]);
	});

	it('leaves the self entry in the previous channel when typing moves on', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(500);

		RollingTypingSender.startTyping(OTHER_CHANNEL);

		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(true);
		expect(RollingTypingStore.isTyping(OTHER_CHANNEL, 'me')).toBe(true);
		advanceTo(10000);
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
		expect(RollingTypingStore.isTyping(OTHER_CHANNEL, 'me')).toBe(true);
	});

	it('keeps a pending send alive across a plain channel switch', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(400);

		RollingTypingSender.stopTyping(OTHER_CHANNEL);
		advanceTo(1500);

		expect(doubles.post).toHaveBeenCalledExactlyOnceWith(Endpoints.CHANNEL_TYPING(CHANNEL));
	});

	it('cancels only a pending send and removes the self entry when the field is emptied', () => {
		RollingTypingSender.startTyping(CHANNEL);
		RollingTypingStore.start(CHANNEL, 'alice', 'gateway');
		advanceTo(500);

		RollingTypingSender.stopTyping(CHANNEL);

		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
		expect(RollingTypingStore.isTyping(CHANNEL, 'alice')).toBe(true);
		expect(vi.getTimerCount()).toBe(1);
		vi.advanceTimersByTime(5000);
		expect(doubles.post).not.toHaveBeenCalled();

		RollingTypingSender.startTyping(CHANNEL);
		vi.advanceTimersByTime(1500);
		expect(postTimes).toEqual([7000]);
	});

	it('leaves the self entry and the throttle alone when emptied after the post fired', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(1500);
		expect(doubles.post).toHaveBeenCalledTimes(1);

		RollingTypingSender.stopTyping(CHANNEL);
		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(true);

		advanceTo(3000);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(7999);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('drops the slot and the self entry on own message sent', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(500);

		RollingTypingSender.handleOwnMessageSent(CHANNEL);

		expect(RollingTypingStore.isTyping(CHANNEL, 'me')).toBe(false);
		expect(vi.getTimerCount()).toBe(0);
		vi.advanceTimersByTime(20000);
		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('schedules a fresh 1500 ms send for a quick follow-up after own message sent', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(2000);
		RollingTypingSender.handleOwnMessageSent(CHANNEL);

		advanceTo(2100);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(3599);
		expect(postTimes).toEqual([1500]);

		advanceTo(3600);
		expect(postTimes).toEqual([1500, 3600]);
	});

	it('bails inside the timer when the signed in account changed', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(1000);

		runInAction(() => {
			doubles.auth.currentUserId = 'someone-else';
		});
		advanceTo(5000);

		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('keeps the slot when the store is reset on reconnect', () => {
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(500);

		RollingTypingStore.reset();
		advanceTo(1500);

		expect(doubles.post).toHaveBeenCalledTimes(1);
		advanceTo(3000);
		RollingTypingSender.startTyping(CHANNEL);
		advanceTo(7999);
		expect(doubles.post).toHaveBeenCalledTimes(1);
	});

	it('sends no trailing post after a burst ends', () => {
		typeContinuously(3000);

		vi.advanceTimersByTime(60000);

		expect(postTimes).toEqual([1500]);
	});

	it('releases the pending timer on reset', () => {
		RollingTypingSender.startTyping(CHANNEL);
		const timersBefore = vi.getTimerCount();

		RollingTypingSender.reset();

		expect(vi.getTimerCount()).toBe(timersBefore - 1);
		vi.advanceTimersByTime(5000);
		expect(doubles.post).not.toHaveBeenCalled();
	});

	it('does nothing without a current user', () => {
		runInAction(() => {
			doubles.auth.currentUserId = null;
		});

		RollingTypingSender.startTyping(CHANNEL);

		expect(vi.getTimerCount()).toBe(0);
		expect(RollingTypingStore.countTypists(CHANNEL)).toBe(0);
		vi.advanceTimersByTime(5000);
		expect(doubles.post).not.toHaveBeenCalled();
	});
});
