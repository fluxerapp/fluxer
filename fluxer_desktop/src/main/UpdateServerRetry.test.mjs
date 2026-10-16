// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {nextUpdateServerRetryDelay, UPDATE_SERVER_RETRY_BASE_MS, UPDATE_SERVER_RETRY_CAP_MS, UpdateServerRetry} =
	await import('@electron/main/UpdateServerRetry');

function fakeClock() {
	const timers = [];
	return {
		schedule: (callback, delayMs) => {
			const timer = {callback, delayMs, cancelled: false};
			timers.push(timer);
			return timer;
		},
		cancel: (timer) => {
			timer.cancelled = true;
		},
		fireLast: () => {
			const timer = timers.at(-1);
			if (!timer.cancelled) timer.callback();
		},
		timers,
	};
}

function settled(promise) {
	let done = false;
	promise.then(() => {
		done = true;
	});
	return async () => {
		await new Promise((resolve) => setImmediate(resolve));
		return done;
	};
}

describe('update server retry delay', () => {
	test('doubles from the base and stops at the cap', () => {
		const delays = [0, 1, 2, 3, 4, 5, 9].map((attempt) => nextUpdateServerRetryDelay(attempt, () => 1));

		assert.deepEqual(delays, [
			UPDATE_SERVER_RETRY_BASE_MS,
			UPDATE_SERVER_RETRY_BASE_MS * 2,
			UPDATE_SERVER_RETRY_BASE_MS * 4,
			UPDATE_SERVER_RETRY_BASE_MS * 8,
			UPDATE_SERVER_RETRY_BASE_MS * 16,
			UPDATE_SERVER_RETRY_CAP_MS,
			UPDATE_SERVER_RETRY_CAP_MS,
		]);
	});

	test('jitters down to half so a fleet behind one outage does not return in step', () => {
		assert.equal(
			nextUpdateServerRetryDelay(0, () => 0),
			UPDATE_SERVER_RETRY_BASE_MS / 2,
		);
	});
});

describe('update server retry', () => {
	test('a blocked splash retries by itself after the backoff, each wait longer than the last', async () => {
		const clock = fakeClock();
		const retry = new UpdateServerRetry({random: () => 1, schedule: clock.schedule, cancel: clock.cancel});

		const first = settled(retry.holdUntilNextAttempt());
		assert.equal(retry.holding, true);
		assert.equal(await first(), false);
		clock.fireLast();
		assert.equal(await first(), true);

		const second = settled(retry.holdUntilNextAttempt());
		clock.fireLast();
		assert.equal(await second(), true);

		assert.deepEqual(
			clock.timers.map((timer) => timer.delayMs),
			[UPDATE_SERVER_RETRY_BASE_MS, UPDATE_SERVER_RETRY_BASE_MS * 2],
		);
	});

	test('the network coming back cuts the wait short and starts the backoff over', async () => {
		const clock = fakeClock();
		const retry = new UpdateServerRetry({random: () => 1, schedule: clock.schedule, cancel: clock.cancel});

		const first = settled(retry.holdUntilNextAttempt());
		clock.fireLast();
		await first();
		const second = settled(retry.holdUntilNextAttempt());
		retry.networkReturned();

		assert.equal(await second(), true);
		assert.equal(clock.timers.at(-1).cancelled, true);

		void retry.holdUntilNextAttempt();
		assert.equal(clock.timers.at(-1).delayMs, UPDATE_SERVER_RETRY_BASE_MS);
	});

	test('the network coming back also cuts short the sleep between the updater’s own attempts', async () => {
		const clock = fakeClock();
		const retry = new UpdateServerRetry({schedule: clock.schedule, cancel: clock.cancel});

		const sleeping = settled(retry.sleep(16000));
		assert.equal(await sleeping(), false);
		retry.networkReturned();

		assert.equal(await sleeping(), true);
		assert.equal(retry.holding, false);
	});

	test('the unreachable screen stays up through the silent checks and yields to the first real progress', async () => {
		const clock = fakeClock();
		const retry = new UpdateServerRetry({schedule: clock.schedule, cancel: clock.cancel});

		assert.equal(retry.endsHold('downloading'), false);

		void retry.holdUntilNextAttempt();
		for (const status of ['checking', 'retry-wait', 'blocked-update-required']) {
			assert.equal(retry.endsHold(status), false, status);
			assert.equal(retry.holding, true, status);
		}

		assert.equal(retry.endsHold('downloading'), true);
		assert.equal(retry.holding, false);
		assert.equal(retry.endsHold('launching'), false);
	});

	test('a server that answered starts the backoff over for the next outage', () => {
		const clock = fakeClock();
		const retry = new UpdateServerRetry({random: () => 1, schedule: clock.schedule, cancel: clock.cancel});

		void retry.holdUntilNextAttempt();
		void retry.holdUntilNextAttempt();
		retry.endsHold('launching');
		void retry.holdUntilNextAttempt();

		assert.deepEqual(
			clock.timers.map((timer) => timer.delayMs),
			[UPDATE_SERVER_RETRY_BASE_MS, UPDATE_SERVER_RETRY_BASE_MS * 2, UPDATE_SERVER_RETRY_BASE_MS],
		);
	});
});
