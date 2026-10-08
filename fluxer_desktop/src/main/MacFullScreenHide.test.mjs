// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {EventEmitter} from 'node:events';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./MacFullScreenHide.ts', import.meta.url));
const transformedSource = esbuild.transformSync(readFileSync(sourcePath, 'utf8'), {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadMacFullScreenHide() {
	const module = {exports: {}};
	const context = vm.createContext({module, exports: module.exports, process: {platform: 'darwin'}});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports;
}

class FakeWindow extends EventEmitter {
	fullScreen;
	visible = true;
	destroyed = false;
	calls = [];

	constructor(fullScreen) {
		super();
		this.fullScreen = fullScreen;
	}

	isDestroyed() {
		return this.destroyed;
	}

	isFullScreen() {
		return this.fullScreen;
	}

	setFullScreen(flag) {
		this.calls.push(`setFullScreen(${flag})`);
	}

	hide() {
		this.calls.push('hide');
		this.visible = false;
	}

	finishLeavingFullScreen() {
		this.fullScreen = false;
		this.emit('leave-full-screen');
	}
}

describe('MacFullScreenHide', () => {
	test('hides a windowed window at once', () => {
		const {hideWindowLeavingFullScreen} = loadMacFullScreenHide();
		const window = new FakeWindow(false);
		hideWindowLeavingFullScreen(window, 'darwin');
		assert.deepEqual(window.calls, ['hide']);
	});

	test('leaves full screen on macOS and hides only after the transition ends', () => {
		const {hideWindowLeavingFullScreen} = loadMacFullScreenHide();
		const window = new FakeWindow(true);
		hideWindowLeavingFullScreen(window, 'darwin');
		assert.deepEqual(window.calls, ['setFullScreen(false)']);
		assert.equal(window.visible, true);
		window.finishLeavingFullScreen();
		assert.deepEqual(window.calls, ['setFullScreen(false)', 'hide']);
		assert.equal(window.visible, false);
	});

	test('a second close during the transition does not queue another exit', () => {
		const {hideWindowLeavingFullScreen} = loadMacFullScreenHide();
		const window = new FakeWindow(true);
		hideWindowLeavingFullScreen(window, 'darwin');
		hideWindowLeavingFullScreen(window, 'darwin');
		window.finishLeavingFullScreen();
		assert.deepEqual(window.calls, ['setFullScreen(false)', 'hide']);
		assert.equal(window.listenerCount('leave-full-screen'), 0);
	});

	test('showing the window during the transition cancels the pending hide', () => {
		const {hideWindowLeavingFullScreen, cancelPendingFullScreenHide} = loadMacFullScreenHide();
		const window = new FakeWindow(true);
		hideWindowLeavingFullScreen(window, 'darwin');
		cancelPendingFullScreenHide(window);
		window.finishLeavingFullScreen();
		assert.deepEqual(window.calls, ['setFullScreen(false)']);
		assert.equal(window.visible, true);
	});

	test('does not touch a window destroyed before the transition ends', () => {
		const {hideWindowLeavingFullScreen} = loadMacFullScreenHide();
		const window = new FakeWindow(true);
		hideWindowLeavingFullScreen(window, 'darwin');
		window.destroyed = true;
		window.finishLeavingFullScreen();
		assert.deepEqual(window.calls, ['setFullScreen(false)']);
	});

	test('hides directly on other platforms', () => {
		const {hideWindowLeavingFullScreen} = loadMacFullScreenHide();
		const window = new FakeWindow(true);
		hideWindowLeavingFullScreen(window, 'win32');
		assert.deepEqual(window.calls, ['hide']);
	});
});
