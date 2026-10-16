// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {DesktopUpdatePoll} = await import('@electron/main/DesktopUpdatePoll');

const PLAN = Object.freeze({intervalMs: 90_000, jitterRatio: 0.15});

function settle() {
	return new Promise((resolve) => setImmediate(resolve));
}

function harness(results) {
	const timers = [];
	const logs = [];
	let checks = 0;
	let release = null;
	const poll = new DesktopUpdatePoll({
		plan: PLAN,
		random: () => 0.5,
		check: () => {
			const result = results[Math.min(checks, results.length - 1)];
			checks += 1;
			if (result === 'hold') {
				return new Promise((resolve) => {
					release = () => resolve({available: false, updating: false});
				});
			}
			return result instanceof Error ? Promise.reject(result) : Promise.resolve(result);
		},
		logger: {
			info: (message) => logs.push(['info', message]),
			warn: (message) => logs.push(['warn', message]),
		},
		setTimer: (callback, delayMs) => {
			const timer = {callback, delayMs, cleared: false};
			timers.push(timer);
			return timer;
		},
		clearTimer: (timer) => {
			timer.cleared = true;
		},
	});
	const fire = async () => {
		const timer = timers.findLast((candidate) => !candidate.cleared);
		timer.cleared = true;
		timer.callback();
		await settle();
	};
	return {
		poll,
		timers,
		logs,
		fire,
		checks: () => checks,
		release: async () => {
			release();
			await settle();
		},
	};
}

const NOTHING = Object.freeze({available: false, updating: false});
const AVAILABLE = Object.freeze({available: true, updating: false});

describe('DesktopUpdatePoll', () => {
	test('a healthy poll checks about every ninety seconds', async () => {
		const {poll, timers, fire, checks} = harness([NOTHING]);

		poll.start(false);
		assert.equal(checks(), 0);
		assert.equal(timers.at(-1).delayMs, 90_000);

		await fire();
		await fire();

		assert.equal(checks(), 2);
		assert.equal(timers.at(-1).delayMs, 90_000);
	});

	test('a failing poll backs off, warns once, and says so when it recovers', async () => {
		const offline = new Error('offline');
		const {poll, timers, logs, fire} = harness([offline, offline, offline, NOTHING, NOTHING]);

		poll.start(true);
		await settle();
		const delays = [timers.at(-1).delayMs];
		await fire();
		delays.push(timers.at(-1).delayMs);
		await fire();
		delays.push(timers.at(-1).delayMs);
		await fire();
		delays.push(timers.at(-1).delayMs);
		await fire();

		assert.deepEqual(delays, [135_000, 270_000, 540_000, 90_000]);
		assert.deepEqual(
			logs.filter(([level]) => level === 'warn').map(([, message]) => message),
			['The desktop update poll failed, backing off until it recovers'],
		);
		assert.equal(logs.filter(([, message]) => message === 'The desktop update poll recovered').length, 1);
	});

	test('the back off never waits longer than fifteen minutes', async () => {
		const {poll, timers, fire} = harness([new Error('offline')]);

		poll.start(true);
		await settle();
		for (let attempt = 0; attempt < 12; attempt += 1) {
			await fire();
		}

		assert.ok(timers.at(-1).delayMs <= 15 * 60 * 1000);
	});

	test('a reopened window or a reload checks at once and restarts the interval', async () => {
		const {poll, timers, checks} = harness([NOTHING]);
		poll.start(false);
		const pending = timers.at(-1);

		poll.wake();
		await settle();

		assert.equal(checks(), 1);
		assert.equal(pending.cleared, true);
		assert.equal(timers.filter((timer) => !timer.cleared).length, 1);
	});

	test('a wake while a check runs never starts a second one', async () => {
		const {poll, checks, release, timers} = harness(['hold', NOTHING]);
		poll.start(true);

		poll.wake();
		poll.wake();
		assert.equal(checks(), 1);

		await release();
		assert.equal(timers.filter((timer) => !timer.cleared).length, 1);
	});

	test('an update that stays available is logged once, not on every tick', async () => {
		const {poll, logs, fire} = harness([AVAILABLE]);
		poll.start(true);
		await settle();
		await fire();
		await fire();

		assert.equal(
			logs.filter(([, message]) => message === 'A desktop update is available, waiting for the user to start it')
				.length,
			1,
		);
	});
});
