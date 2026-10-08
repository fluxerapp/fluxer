// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';

const {MAIN_WINDOW_CONTENT_GRACE_MS, MAIN_WINDOW_HARD_FALLBACK_MS, MainWindowRevealGate, MainWindowRevealReason} =
	await import('./MainWindowReveal.ts');

function harness() {
	const reveals = [];
	const timers = [];
	const gate = new MainWindowRevealGate({
		onReveal: (reason) => reveals.push(reason),
		setTimer: (callback, delayMs) => {
			const timer = {callback, delayMs, cleared: false};
			timers.push(timer);
			return timer;
		},
		clearTimer: (timer) => {
			timer.cleared = true;
		},
	});
	const live = () => timers.filter((timer) => !timer.cleared);
	const fire = (delayMs) => {
		const timer = live().find((entry) => entry.delayMs === delayMs);
		assert.ok(timer, `no live ${delayMs} ms timer`);
		timer.cleared = true;
		timer.callback();
	};
	return {gate, reveals, timers, live, fire};
}

describe('main window reveal gate', () => {
	test('ready-to-show alone never reveals a window whose renderer has not painted content', () => {
		const {gate, reveals} = harness();
		gate.start();
		gate.markReadyToShow();

		assert.deepEqual(reveals, []);
		assert.equal(gate.revealed, false);
	});

	test('the renderer content signal reveals at once and disarms every fallback', () => {
		const {gate, reveals, live} = harness();
		gate.start();
		gate.markReadyToShow();

		gate.markContentPainted();

		assert.deepEqual(reveals, [MainWindowRevealReason.CONTENT_PAINTED]);
		assert.deepEqual(live(), []);
	});

	test('content painted before ready-to-show still reveals exactly once', () => {
		const {gate, reveals, live} = harness();
		gate.start();
		gate.markContentPainted();
		gate.markReadyToShow();
		gate.markContentPainted();

		assert.deepEqual(reveals, [MainWindowRevealReason.CONTENT_PAINTED]);
		assert.deepEqual(live(), []);
	});

	test('a renderer that never signals is revealed after the grace that follows its first paint', () => {
		const {gate, reveals, fire} = harness();
		gate.start();
		gate.markReadyToShow();

		fire(MAIN_WINDOW_CONTENT_GRACE_MS);

		assert.deepEqual(reveals, [MainWindowRevealReason.CONTENT_GRACE_EXPIRED]);
	});

	test('a window that never paints at all is still revealed by the hard fallback', () => {
		const {gate, reveals, fire, live} = harness();
		gate.start();

		fire(MAIN_WINDOW_HARD_FALLBACK_MS);

		assert.deepEqual(reveals, [MainWindowRevealReason.HARD_FALLBACK]);
		assert.deepEqual(live(), []);
	});

	test('the grace never outlasts the hard fallback and both are bounded', () => {
		assert.ok(MAIN_WINDOW_CONTENT_GRACE_MS < MAIN_WINDOW_HARD_FALLBACK_MS);
		assert.ok(MAIN_WINDOW_HARD_FALLBACK_MS <= 10000);
	});

	test('a window shown or closed by someone else disarms the gate without revealing', () => {
		const {gate, reveals, live} = harness();
		gate.start();
		gate.markReadyToShow();

		gate.dispose();
		gate.markContentPainted();

		assert.deepEqual(reveals, []);
		assert.deepEqual(live(), []);
	});

	test('repeated ready-to-show and start calls arm one timer each', () => {
		const {gate, timers} = harness();
		gate.start();
		gate.start();
		gate.markReadyToShow();
		gate.markReadyToShow();

		assert.equal(timers.length, 2);
	});
});
