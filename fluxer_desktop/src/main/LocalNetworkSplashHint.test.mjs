// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {isLocalNetworkHost, isLocalNetworkUrl} = await import('@electron/common/LocalNetworkHost');
const {LocalNetworkSplashHint, shouldWatchForLocalNetworkPrompt} = await import(
	'@electron/main/LocalNetworkSplashHint'
);

function fakeClock() {
	const pending = [];
	return {
		schedule: (callback, delayMs) => {
			const timer = {callback, delayMs, cancelled: false, unref: () => {}};
			pending.push(timer);
			return timer;
		},
		cancel: (timer) => {
			timer.cancelled = true;
		},
		fire: () => {
			for (const timer of pending.splice(0)) {
				if (!timer.cancelled) timer.callback();
			}
		},
		pending,
	};
}

describe('isLocalNetworkHost', () => {
	test('loopback and link-local names count as local', () => {
		for (const host of ['localhost', 'LOCALHOST', 'fluxer.localhost', 'nas.local', '127.0.0.1', '::1', '[::1]']) {
			assert.equal(isLocalNetworkHost(host), true, host);
		}
	});

	test('private ranges count as local', () => {
		for (const host of ['192.168.1.50', '10.0.0.4', '172.16.3.9', '169.254.1.1', 'fd00::1']) {
			assert.equal(isLocalNetworkHost(host), true, host);
		}
	});

	test('public addresses and ordinary hostnames do not', () => {
		for (const host of ['api.fluxer.app', 'fluxer.app', '1.1.1.1', '2606:4700:4700::1111', '']) {
			assert.equal(isLocalNetworkHost(host), false, host);
		}
	});

	test('a public hostname is never mistaken for a local one', () => {
		assert.equal(isLocalNetworkUrl('https://api.fluxer.app'), false);
		assert.equal(isLocalNetworkUrl('https://api.canary.fluxer.app'), false);
	});

	test('the development endpoint is recognised as local', () => {
		assert.equal(isLocalNetworkUrl('http://localhost:8088/api'), true);
		assert.equal(isLocalNetworkUrl('http://192.168.1.50:8088/api'), true);
	});

	test('an unparseable url is not treated as local', () => {
		assert.equal(isLocalNetworkUrl('not a url'), false);
	});
});

describe('shouldWatchForLocalNetworkPrompt', () => {
	test('only macOS gets the hint, because only macOS shows the prompt', () => {
		assert.equal(shouldWatchForLocalNetworkPrompt('http://localhost:8088/api', 'darwin'), true);
		assert.equal(shouldWatchForLocalNetworkPrompt('http://localhost:8088/api', 'linux'), false);
		assert.equal(shouldWatchForLocalNetworkPrompt('http://localhost:8088/api', 'win32'), false);
	});

	test('a public endpoint never arms the hint', () => {
		assert.equal(shouldWatchForLocalNetworkPrompt('https://api.fluxer.app', 'darwin'), false);
	});
});

describe('LocalNetworkSplashHint', () => {
	test('a slow check against a local host raises the hint', () => {
		const clock = fakeClock();
		let hints = 0;
		const hint = new LocalNetworkSplashHint({
			apiBaseUrl: 'http://localhost:8088/api',
			platform: 'darwin',
			onHint: () => {
				hints += 1;
			},
			schedule: clock.schedule,
			cancel: clock.cancel,
		});
		hint.armWhileChecking(true);
		clock.fire();
		assert.equal(hints, 1);
	});

	test('a check that finishes first never raises the hint', () => {
		const clock = fakeClock();
		let hints = 0;
		const hint = new LocalNetworkSplashHint({
			apiBaseUrl: 'http://localhost:8088/api',
			platform: 'darwin',
			onHint: () => {
				hints += 1;
			},
			schedule: clock.schedule,
			cancel: clock.cancel,
		});
		hint.armWhileChecking(true);
		hint.armWhileChecking(false);
		clock.fire();
		assert.equal(hints, 0);
	});

	test('a public endpoint never raises the hint even on a slow check', () => {
		const clock = fakeClock();
		let hints = 0;
		const hint = new LocalNetworkSplashHint({
			apiBaseUrl: 'https://api.fluxer.app',
			platform: 'darwin',
			onHint: () => {
				hints += 1;
			},
			schedule: clock.schedule,
			cancel: clock.cancel,
		});
		hint.armWhileChecking(true);
		clock.fire();
		assert.equal(hints, 0);
		assert.equal(clock.pending.length, 0);
	});

	test('repeated checking states do not stack timers', () => {
		const clock = fakeClock();
		let hints = 0;
		const hint = new LocalNetworkSplashHint({
			apiBaseUrl: 'http://localhost:8088/api',
			platform: 'darwin',
			onHint: () => {
				hints += 1;
			},
			schedule: clock.schedule,
			cancel: clock.cancel,
		});
		hint.armWhileChecking(true);
		hint.armWhileChecking(true);
		hint.armWhileChecking(true);
		assert.equal(clock.pending.length, 1);
		clock.fire();
		assert.equal(hints, 1);
	});

	test('disarming after the loop stops a pending hint from overwriting the splash', () => {
		const clock = fakeClock();
		let hints = 0;
		const hint = new LocalNetworkSplashHint({
			apiBaseUrl: 'http://localhost:8088/api',
			platform: 'darwin',
			onHint: () => {
				hints += 1;
			},
			schedule: clock.schedule,
			cancel: clock.cancel,
		});
		hint.armWhileChecking(true);
		hint.disarm();
		clock.fire();
		assert.equal(hints, 0);
	});
});
