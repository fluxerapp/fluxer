// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {DesktopRuntimeSecurity} = await import('@electron/main/DesktopRuntimeSecurity');

function createLogger() {
	const errors = [];
	return {
		errors,
		error: (...args) => errors.push(args),
	};
}

function createSession() {
	const session = {
		requestListener: null,
		webRequest: {
			onBeforeSendHeaders: (listener) => {
				session.requestListener = listener;
			},
		},
	};
	return session;
}

function createInstalled() {
	const logger = createLogger();
	const session = createSession();
	const security = new DesktopRuntimeSecurity({logger});
	security.install(session);
	return {security, session, logger};
}

function settleRequest(session, details) {
	let settlement = null;
	let calls = 0;
	session.requestListener(details, (value) => {
		calls += 1;
		settlement = value;
	});
	assert.equal(calls, 1, 'the request callback must be invoked exactly once');
	return settlement;
}

describe('DesktopRuntimeSecurity', () => {
	test('gives the YouTube embed frame the canonical app origin YouTube authorizes', () => {
		const {session} = createInstalled();

		const settlement = settleRequest(session, {
			resourceType: 'subFrame',
			url: 'https://www.youtube.com/embed/dQw4w9WgXcQ?autoplay=1',
			requestHeaders: {Accept: '*/*'},
		});

		assert.deepEqual({...settlement.requestHeaders}, {Accept: '*/*', Referer: 'https://web.fluxer.app/'});
	});

	test('never invents a referrer for anything but the YouTube embed frame', () => {
		const {session} = createInstalled();

		for (const details of [
			{resourceType: 'subFrame', url: 'https://www.youtube.com/watch?v=dQw4w9WgXcQ'},
			{resourceType: 'subFrame', url: 'https://youtube.com/embed/dQw4w9WgXcQ'},
			{resourceType: 'subFrame', url: 'https://www.youtube.com.example.test/embed/dQw4w9WgXcQ'},
			{resourceType: 'subFrame', url: 'http://www.youtube.com/embed/dQw4w9WgXcQ'},
			{resourceType: 'subFrame', url: 'https://challenges.cloudflare.com/embed/widget'},
			{resourceType: 'xhr', url: 'https://www.youtube.com/embed/dQw4w9WgXcQ'},
			{resourceType: 'script', url: 'https://www.youtube.com/embed/dQw4w9WgXcQ'},
			{resourceType: 'mainFrame', url: 'https://www.youtube.com/embed/dQw4w9WgXcQ'},
		]) {
			assert.deepEqual(settleRequest(session, {...details, requestHeaders: {Accept: '*/*'}}), {}, details.url);
		}
	});

	test('replaces the local shell origin the renderer sends with the canonical app origin', () => {
		const {session} = createInstalled();

		const settlement = settleRequest(session, {
			resourceType: 'subFrame',
			url: 'https://www.youtube.com/embed/dQw4w9WgXcQ',
			requestHeaders: {Accept: '*/*', referer: 'fluxer-app://app/'},
		});

		assert.deepEqual({...settlement.requestHeaders}, {Accept: '*/*', Referer: 'https://web.fluxer.app/'});
	});

	test('emits request header values Electron will actually send rather than response-shaped arrays', () => {
		const {session} = createInstalled();

		const settlement = settleRequest(session, {
			resourceType: 'subFrame',
			url: 'https://www.youtube.com/embed/dQw4w9WgXcQ',
			requestHeaders: {Accept: '*/*', 'User-Agent': 'agent'},
		});

		for (const [name, value] of Object.entries(settlement.requestHeaders)) {
			assert.equal(typeof value, 'string', `${name} must be a string, Electron drops array-valued request headers`);
		}
	});

	test('the embed referrer cannot smuggle the local app authorization secret out with it', () => {
		const session = createSession();
		const security = new DesktopRuntimeSecurity({
			logger: createLogger(),
			localAppAuthorization: {
				applyRequestHeaders: (_details, headers) => {
					const next = {};
					for (const [name, value] of Object.entries(headers)) {
						if (name.toLowerCase() !== 'x-fluxer-desktop-protocol-authorization') {
							next[name] = value;
						}
					}
					return next;
				},
			},
		});
		security.install(session);

		const settlement = settleRequest(session, {
			resourceType: 'subFrame',
			url: 'https://www.youtube.com/embed/dQw4w9WgXcQ',
			requestHeaders: {Accept: '*/*', 'X-Fluxer-Desktop-Protocol-Authorization': 'secret'},
		});

		assert.deepEqual({...settlement.requestHeaders}, {Accept: '*/*', Referer: 'https://web.fluxer.app/'});
	});

	test('leaves a request it has no reason to touch alone', () => {
		const {session} = createInstalled();

		assert.deepEqual(
			settleRequest(session, {
				resourceType: 'mainFrame',
				url: 'https://web.fluxer.app/channels/@me',
				requestHeaders: {Accept: '*/*'},
			}),
			{},
		);
	});

	test('installs once per session and stops listening after cleanup', () => {
		const {security, session} = createInstalled();
		const firstRequestListener = session.requestListener;

		security.install(session);
		assert.equal(session.requestListener, firstRequestListener);

		security.cleanup();
		assert.equal(session.requestListener, null);
		assert.throws(() => security.install(session), /closed/);
	});
});
