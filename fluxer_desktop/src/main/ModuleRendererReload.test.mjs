// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {EventEmitter} from 'node:events';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {watchModuleRendererReload} = await import('@electron/main/ModuleRendererReload');

function createWatch({timeoutMs = 60_000} = {}) {
	const target = new EventEmitter();
	const loaded = [];
	const abandoned = [];
	let announceAbandon = () => undefined;
	const whenAbandoned = new Promise((resolve) => {
		announceAbandon = resolve;
	});
	const stop = watchModuleRendererReload({
		target,
		observeLaunchConfirmed: (listener) => {
			const handler = () => {
				target.removeListener('launch-confirmed', handler);
				listener();
			};
			target.on('launch-confirmed', handler);
			return () => {
				target.removeListener('launch-confirmed', handler);
			};
		},
		timeoutMs,
		onLoaded: () => {
			loaded.push('loaded');
		},
		onAbandoned: (reason) => {
			abandoned.push(reason);
			announceAbandon(reason);
		},
	});
	return {abandoned, loaded, stop, target, whenAbandoned};
}

function listenerCounts(target) {
	return {
		'launch-confirmed': target.listenerCount('launch-confirmed'),
		destroyed: target.listenerCount('destroyed'),
	};
}

async function settle(milliseconds) {
	await new Promise((resolve) => {
		setTimeout(resolve, milliseconds);
	});
}

async function waitForAbandon(watch) {
	await Promise.race([watch.whenAbandoned, settle(1000)]);
}

describe('watching a renderer reload for an activated module set', () => {
	test('records the launch once the renderer confirms it booted', () => {
		const watch = createWatch();

		watch.target.emit('launch-confirmed');

		assert.deepEqual(watch.loaded, ['loaded']);
		assert.deepEqual(watch.abandoned, []);
		assert.deepEqual(listenerCounts(watch.target), {'launch-confirmed': 0, destroyed: 0});
	});

	test('a renderer destroyed after it loaded never turns the recorded launch into an abandoned one', () => {
		const watch = createWatch();

		watch.target.emit('launch-confirmed');
		watch.target.emit('destroyed');

		assert.deepEqual(watch.loaded, ['loaded']);
		assert.deepEqual(watch.abandoned, []);
	});

	test('releases the pending launch when the renderer is destroyed before it confirms the reload', () => {
		const watch = createWatch();

		watch.target.emit('destroyed');

		assert.equal(watch.abandoned.length, 1);
		assert.match(watch.abandoned[0], /destroyed/u);
		assert.deepEqual(watch.loaded, []);
		assert.deepEqual(listenerCounts(watch.target), {'launch-confirmed': 0, destroyed: 0});
	});

	test('a launch confirmation that arrives from the wreckage of a destroyed renderer records nothing', () => {
		const watch = createWatch();

		watch.target.emit('destroyed');
		watch.target.emit('launch-confirmed');

		assert.deepEqual(watch.loaded, []);
		assert.equal(watch.abandoned.length, 1);
	});

	test('releases the pending launch when neither event ever arrives', async () => {
		const watch = createWatch({timeoutMs: 5});

		await waitForAbandon(watch);

		assert.equal(watch.abandoned.length, 1);
		assert.deepEqual(watch.loaded, []);
	});

	test('a reload that lands after the deadline is still recorded rather than left as a failed boot attempt', async () => {
		const watch = createWatch({timeoutMs: 5});

		await waitForAbandon(watch);
		watch.target.emit('launch-confirmed');

		assert.deepEqual(watch.loaded, ['loaded']);
		assert.equal(watch.abandoned.length, 1);
	});

	test('stopping the watch detaches every listener and disarms the deadline', async () => {
		const watch = createWatch({timeoutMs: 5});

		watch.stop();
		watch.target.emit('launch-confirmed');
		watch.target.emit('destroyed');
		await settle(30);

		assert.deepEqual(watch.loaded, []);
		assert.deepEqual(watch.abandoned, []);
		assert.deepEqual(listenerCounts(watch.target), {'launch-confirmed': 0, destroyed: 0});
	});
});
