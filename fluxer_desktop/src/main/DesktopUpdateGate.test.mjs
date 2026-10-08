// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {
	armDesktopUpdate,
	checkDesktopUpdateNow,
	getDesktopUpdateState,
	observeDesktopUpdateState,
	publishDesktopUpdateCheck,
	startDesktopUpdate,
} = await import('@electron/main/DesktopUpdateGate');

function deferred() {
	let resolve;
	const promise = new Promise((resolvePromise) => {
		resolve = resolvePromise;
	});
	return {promise, resolve};
}

describe('DesktopUpdateGate', () => {
	test('nothing is available and nothing starts before the bootstrap arms the gate', async () => {
		assert.deepEqual(getDesktopUpdateState(), {available: false, updating: false});
		assert.deepEqual(await checkDesktopUpdateNow(), {available: false, updating: false});
		assert.equal(startDesktopUpdate(), false);
	});

	test('a check publishes the update and one click runs it exactly once', async () => {
		const states = [];
		const stop = observeDesktopUpdateState((state) => states.push(state));
		const running = deferred();
		let starts = 0;
		armDesktopUpdate({
			check: async () => ({shellNewer: false, modulesChanged: true}),
			start: async () => {
				starts += 1;
				await running.promise;
				publishDesktopUpdateCheck({shellNewer: false, modulesChanged: false});
			},
		});

		assert.deepEqual(await checkDesktopUpdateNow(), {available: true, updating: false});
		assert.equal(startDesktopUpdate(), true);
		assert.equal(startDesktopUpdate(), false, 'a second click while the update runs is a no-op');
		assert.deepEqual(
			getDesktopUpdateState(),
			{available: false, updating: true},
			'the affordance hides the moment it is clicked',
		);
		assert.deepEqual(
			await checkDesktopUpdateNow(),
			{available: false, updating: true},
			'a poll during the update changes nothing',
		);

		running.resolve();
		await new Promise((resolve) => setImmediate(resolve));

		assert.equal(starts, 1);
		assert.deepEqual(getDesktopUpdateState(), {available: false, updating: false});
		assert.deepEqual(states, [
			{available: true, updating: false},
			{available: false, updating: true},
			{available: false, updating: false},
		]);
		stop();
	});

	test('a shell update that could not finish stays available for the next click', async () => {
		armDesktopUpdate({
			check: async () => ({shellNewer: true, modulesChanged: false}),
			start: async () => {
				publishDesktopUpdateCheck({shellNewer: true, modulesChanged: false});
			},
		});
		await checkDesktopUpdateNow();

		assert.equal(startDesktopUpdate(), true);
		await new Promise((resolve) => setImmediate(resolve));

		assert.deepEqual(getDesktopUpdateState(), {available: true, updating: false});
		assert.equal(startDesktopUpdate(), true);
	});
});
