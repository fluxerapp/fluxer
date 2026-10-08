// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {DesktopBrowserHandoff} = await import('./BrowserHandoff.ts');
const {RendererDocumentOwnerFactory} = await import('./RendererDocumentOwner.ts');
const {createBrowserHandoffPreloadAPI} = await import('../preload/BrowserHandoffPreload.ts');
const {DESKTOP_HANDOFF_CHANNELS} = await import('../../../packages/desktop_ipc/src/BrowserHandoffContract.ts');

const APP_ORIGIN = 'https://web.fluxer.app';
const INSTANCE = Object.freeze({
	apiEndpoint: 'https://self.hosted.example/api',
	apiVersion: 1,
	webAppEndpoint: 'https://self.hosted.example',
});
const CODE = 'ABCDEF-123456';
const COMPLETED_USER = {
	id: '42',
	username: 'ada',
	discriminator: '0001',
	global_name: 'Ada',
	avatar: 'hash',
	email: 'ada@example.test',
};

function futureTimestamp(offsetMs = 300_000) {
	return new Date(Date.now() + offsetMs).toISOString();
}

function createDocument({url = `${APP_ORIGIN}/channels/@me`, privileged = true} = {}) {
	const listeners = new Map();
	const frame = {url, detached: false, parent: null, send: () => {}};
	const sender = {
		privileged,
		destroyed: false,
		isDestroyed: () => sender.destroyed,
		isLoadingMainFrame: () => false,
		mainFrame: frame,
		on: (event, listener) => {
			const list = listeners.get(event) ?? [];
			list.push(listener);
			listeners.set(event, list);
		},
		once: (event, listener) => sender.on(event, listener),
		removeListener: (event, listener) => {
			const list = listeners.get(event) ?? [];
			const index = list.indexOf(listener);
			if (index >= 0) list.splice(index, 1);
			listeners.set(event, list);
		},
		emit: (event, ...args) => {
			for (const listener of [...(listeners.get(event) ?? [])]) listener(...args);
		},
		listenerCount: (event) => (listeners.get(event) ?? []).length,
	};
	frame.top = frame;
	frame.owner = sender;
	return {frame, sender, event: {sender, senderFrame: frame}};
}

function jsonResponse(status, payload) {
	const body = payload == null ? null : Buffer.from(JSON.stringify(payload), 'utf8');
	return {body, headers: {}, ok: status >= 200 && status < 300, status, statusText: 'stub'};
}

function createInstanceClient(routes) {
	const requests = [];
	return {
		requests,
		client: {
			fetch: async (request) => {
				const path = new URL(request.url).pathname;
				let body = null;
				if (request.body != null) {
					body = JSON.parse(Buffer.from(request.body).toString('utf8'));
				}
				requests.push({
					expectedOrigin: request.expectedOrigin,
					headers: request.headers,
					method: request.method,
					path,
					body,
					url: request.url,
				});
				const route = routes[`${request.method} ${path}`];
				if (route == null) {
					return jsonResponse(404, {message: 'unknown route'});
				}
				return route(requests.length);
			},
		},
	};
}

function createHandoff(routes, {privileged = true, returnUri = null} = {}) {
	const document = createDocument({privileged});
	const instance = createInstanceClient(routes);
	const warnings = [];
	const handoff = new DesktopBrowserHandoff({
		logger: {warn: (message, ...args) => warnings.push({message, args})},
		rendererDocumentOwners: new RendererDocumentOwnerFactory({
			policy: {isPrivilegedRendererDocument: ({sender}) => sender.privileged === true},
			onWatcherFailure: (error, reason) => warnings.push({message: reason, args: [error]}),
		}),
		selectedInstanceClient: instance.client,
		returnUri: () => returnUri,
	});
	return {document, handoff, requests: instance.requests, routes: handoff.ipcRoutes(), warnings};
}

const INITIATE_PATH = '/api/v1/auth/handoff/initiate';
const STATUS_PATH = `/api/v1/auth/handoff/${encodeURIComponent(CODE)}/status`;

function initiateRoute({pollSecret = 'poll-secret-value', returnMethod} = {}) {
	return () =>
		jsonResponse(200, {
			code: CODE,
			expires_at: futureTimestamp(),
			poll_secret: pollSecret,
			...(returnMethod == null ? {} : {return_method: returnMethod}),
		});
}

const RETURN_URI = 'fluxer://handoff';
const GRANT = 'grant-value_42';

function completedStatus() {
	return jsonResponse(200, {status: 'completed', token: 'token-42', user_id: '42', user: COMPLETED_USER});
}

describe('the main-process browser handoff transport', () => {
	test('falls back to GET status against a server that only implements the deployed route', async () => {
		const {document, routes, requests} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => jsonResponse(404, {message: 'unknown route'}),
			[`GET ${STATUS_PATH}`]: () => completedStatus(),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		const result = await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);

		assert.equal(session.code, CODE);
		assert.deepEqual(session.instance, INSTANCE);
		assert.deepEqual(
			requests.map((request) => `${request.method} ${request.path}`),
			[`POST ${INITIATE_PATH}`, `POST ${STATUS_PATH}`, `GET ${STATUS_PATH}`],
		);
		assert.deepEqual(result, {
			status: 'completed',
			token: 'token-42',
			userId: '42',
			user: {
				username: 'ada',
				discriminator: '0001',
				global_name: 'Ada',
				avatar: 'hash',
				email: 'ada@example.test',
			},
		});
	});

	test('stops retrying POST status once the instance answered 405 for this session', async () => {
		let pending = true;
		const {document, routes, requests} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => jsonResponse(405, {message: 'method not allowed'}),
			[`GET ${STATUS_PATH}`]: () => {
				if (pending) {
					pending = false;
					return jsonResponse(200, {status: 'pending'});
				}
				return completedStatus();
			},
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);
		await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);

		assert.deepEqual(
			requests.map((request) => `${request.method} ${request.path}`),
			[`POST ${INITIATE_PATH}`, `POST ${STATUS_PATH}`, `GET ${STATUS_PATH}`, `GET ${STATUS_PATH}`],
		);
	});

	test('presents the poll secret to an upgraded instance and never leaks it to the renderer', async () => {
		const {document, routes, requests} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => completedStatus(),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		const result = await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);

		assert.equal(result.status, 'completed');
		assert.deepEqual(
			requests.map((request) => `${request.method} ${request.path}`),
			[`POST ${INITIATE_PATH}`, `POST ${STATUS_PATH}`],
		);
		assert.deepEqual(requests[1].body, {poll_secret: 'poll-secret-value'});
		assert.equal(JSON.stringify(session).includes('poll-secret-value'), false);
		assert.equal(requests[0].expectedOrigin, 'https://self.hosted.example');
	});

	test('polls with GET only when the instance returned no poll secret', async () => {
		const {document, routes, requests} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute({pollSecret: null}),
			[`GET ${STATUS_PATH}`]: () => jsonResponse(200, {status: 'pending'}),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		const result = await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);

		assert.deepEqual(result, {status: 'pending'});
		assert.deepEqual(
			requests.map((request) => `${request.method} ${request.path}`),
			[`POST ${INITIATE_PATH}`, `GET ${STATUS_PATH}`],
		);
	});

	test('refuses a status request for a code this renderer document never initiated', async () => {
		const {document, routes} = createHandoff({[`POST ${INITIATE_PATH}`]: initiateRoute()});

		await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);

		await assert.rejects(
			() => routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, 'ZZZZZZ-999999'),
			/not active for this renderer document/,
		);
	});

	test('drops the session when the renderer document navigates away', async () => {
		const {document, routes} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => jsonResponse(200, {status: 'pending'}),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		document.sender.emit('did-start-navigation', {isMainFrame: true, isSameDocument: false});

		await assert.rejects(
			() => routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code),
			/not active for this renderer document/,
		);
		assert.equal(document.sender.listenerCount('did-start-navigation'), 0);
	});

	test('releases the session once the instance reports a terminal status', async () => {
		const {document, routes} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => jsonResponse(200, {status: 'expired'}),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		assert.deepEqual(await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code), {status: 'expired'});

		await assert.rejects(
			() => routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code),
			/not active for this renderer document/,
		);
	});

	test('keeps the newest session when a stale poll from a replaced session finishes late', async () => {
		let release = () => {};
		const gate = new Promise((resolve) => {
			release = resolve;
		});
		const {document, routes} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: async (call) => {
				if (call === 2) {
					await gate;
					return jsonResponse(200, {status: 'expired'});
				}
				return jsonResponse(200, {status: 'pending'});
			},
		});

		const first = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		const stalePoll = routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, first.code);
		const second = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		release();
		assert.deepEqual(await stalePoll, {status: 'expired'});

		assert.deepEqual(await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, second.code), {status: 'pending'});
	});

	test('refuses an instance descriptor that is not an absolute HTTP endpoint', async () => {
		const {document, routes} = createHandoff({});

		await assert.rejects(
			() => routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, {...INSTANCE, apiEndpoint: '/api'}),
			/instance.apiEndpoint is invalid/,
		);
		await assert.rejects(
			() => routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, {...INSTANCE, apiVersion: 0}),
			/instance.apiVersion is invalid/,
		);
	});

	test('refuses to serve a renderer document that is not the privileged main frame', async () => {
		const {document, routes} = createHandoff({[`POST ${INITIATE_PATH}`]: initiateRoute()}, {privileged: false});

		await assert.rejects(
			() => routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE),
			/requires the current privileged main-frame renderer document/,
		);
	});

	test('cleanup drops every session and its renderer watcher', async () => {
		const {document, handoff, routes} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => jsonResponse(200, {status: 'pending'}),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		handoff.cleanup();

		assert.equal(document.sender.listenerCount('did-start-navigation'), 0);
		await assert.rejects(
			() => routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code),
			/not active for this renderer document/,
		);
	});
});

describe('the main-process handoff return through the deep link', () => {
	test('asks for a deep link return only when the app can receive one', async () => {
		const registered = createHandoff(
			{[`POST ${INITIATE_PATH}`]: initiateRoute({returnMethod: 'deep_link'})},
			{returnUri: RETURN_URI},
		);
		const unregistered = createHandoff({[`POST ${INITIATE_PATH}`]: initiateRoute({returnMethod: 'code'})});

		const linked = await registered.routes[DESKTOP_HANDOFF_CHANNELS.initiate](registered.document.event, INSTANCE);
		const typed = await unregistered.routes[DESKTOP_HANDOFF_CHANNELS.initiate](unregistered.document.event, INSTANCE);

		assert.deepEqual(registered.requests[0].body, {return_uri: RETURN_URI});
		assert.equal(linked.returnMethod, 'deep_link');
		assert.equal(unregistered.requests[0].body, null);
		assert.equal(typed.returnMethod, 'code');
	});

	test('falls back to the code when the instance does not confirm the deep link return', async () => {
		const {document, routes} = createHandoff({[`POST ${INITIATE_PATH}`]: initiateRoute()}, {returnUri: RETURN_URI});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);

		assert.equal(session.returnMethod, 'code');
	});

	test('presents the grant from the return link with the poll secret and never hands it to the renderer', async () => {
		const {document, handoff, routes, requests} = createHandoff(
			{
				[`POST ${INITIATE_PATH}`]: initiateRoute({returnMethod: 'deep_link'}),
				[`POST ${STATUS_PATH}`]: (call) => (call === 2 ? jsonResponse(200, {status: 'pending'}) : completedStatus()),
			},
			{returnUri: RETURN_URI},
		);

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		assert.deepEqual(await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code), {status: 'pending'});
		handoff.acceptReturnLink(new URL(`${RETURN_URI}?code=${CODE.toLowerCase()}&grant=${GRANT}`));
		const result = await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);

		assert.equal(result.status, 'completed');
		assert.deepEqual(requests[1].body, {poll_secret: 'poll-secret-value'});
		assert.deepEqual(requests[2].body, {poll_secret: 'poll-secret-value', grant: GRANT});
		assert.equal(JSON.stringify(result).includes(GRANT), false);
	});

	test('ignores a return link for a request this app is not waiting on', async () => {
		const {document, handoff, routes, requests, warnings} = createHandoff(
			{
				[`POST ${INITIATE_PATH}`]: initiateRoute({returnMethod: 'deep_link'}),
				[`POST ${STATUS_PATH}`]: () => jsonResponse(200, {status: 'pending'}),
			},
			{returnUri: RETURN_URI},
		);

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		handoff.acceptReturnLink(new URL(`${RETURN_URI}?code=ZZZZZZ-999999&grant=${GRANT}`));
		handoff.acceptReturnLink(new URL(`${RETURN_URI}?code=${CODE}&grant=not%20valid`));
		await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);

		assert.deepEqual(requests[1].body, {poll_secret: 'poll-secret-value'});
		assert.equal(warnings.length, 2);
	});

	test('reports a request the browser declined and releases the session', async () => {
		const {document, routes} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => jsonResponse(200, {status: 'denied'}),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, INSTANCE);
		assert.deepEqual(await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code), {status: 'denied'});

		await assert.rejects(
			() => routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code),
			/not active for this renderer document/,
		);
	});
});

describe('the browser handoff preload bridge', () => {
	test('reaches the two registered main-process channels and nothing else', async () => {
		const {document, routes} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => completedStatus(),
		});
		const invoked = [];
		const api = createBrowserHandoffPreloadAPI({
			invoke: (channel, ...args) => {
				invoked.push(channel);
				const handler = routes[channel];
				if (handler == null) {
					throw new Error(`No main-process handler is registered for ${channel}`);
				}
				return handler(document.event, ...args);
			},
		});

		assert.deepEqual(Object.keys(api), ['desktopHandoff']);
		assert.ok(Object.isFrozen(api.desktopHandoff));
		const session = await api.desktopHandoff.initiate(INSTANCE);
		const result = await api.desktopHandoff.status(session.code);

		assert.equal(result.status, 'completed');
		assert.deepEqual(invoked, [DESKTOP_HANDOFF_CHANNELS.initiate, DESKTOP_HANDOFF_CHANNELS.status]);
	});
});

describe('the main-process handoff sends the browser Origin the instance demands', () => {
	const ORIGIN_INSTANCE = Object.freeze({
		apiEndpoint: 'https://web.canary.fluxer.app/api',
		apiVersion: 1,
		webAppEndpoint: 'https://web.canary.fluxer.app',
	});

	test('a POST carries the origin of the web app endpoint, and a GET carries none', async () => {
		const {document, routes, requests} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute({pollSecret: null}),
			[`GET ${STATUS_PATH}`]: () => jsonResponse(200, {status: 'pending'}),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, ORIGIN_INSTANCE);
		await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);

		assert.deepEqual(
			requests.map((request) => [request.method, request.headers.Origin ?? null]),
			[
				['POST', 'https://web.canary.fluxer.app'],
				['GET', null],
			],
		);
		assert.equal(Object.hasOwn(requests[1].headers, 'Origin'), false);
	});

	test('the polled POST status carries the same origin as the initiation', async () => {
		const {document, routes, requests} = createHandoff({
			[`POST ${INITIATE_PATH}`]: initiateRoute(),
			[`POST ${STATUS_PATH}`]: () => jsonResponse(200, {status: 'pending'}),
		});

		const session = await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, ORIGIN_INSTANCE);
		await routes[DESKTOP_HANDOFF_CHANNELS.status](document.event, session.code);

		assert.deepEqual(
			requests.map((request) => request.headers.Origin),
			['https://web.canary.fluxer.app', 'https://web.canary.fluxer.app'],
		);
	});

	test('the origin is the web app endpoint, not the API endpoint it posts to', async () => {
		const {document, routes, requests} = createHandoff({[`POST ${INITIATE_PATH}`]: initiateRoute()});

		await routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, {
			...ORIGIN_INSTANCE,
			apiEndpoint: 'https://api.canary.fluxer.app/api',
			webAppEndpoint: 'https://web.canary.fluxer.app/app',
		});

		assert.equal(requests[0].expectedOrigin, 'https://api.canary.fluxer.app');
		assert.equal(requests[0].headers.Origin, 'https://web.canary.fluxer.app');
	});

	test('a web app endpoint that has no usable origin is refused before any request is sent', async () => {
		const {document, routes, requests} = createHandoff({[`POST ${INITIATE_PATH}`]: initiateRoute()});

		for (const webAppEndpoint of ['/app', 'not a url', 'javascript:alert(1)', 'https://user:pw@web.fluxer.app']) {
			await assert.rejects(
				() => routes[DESKTOP_HANDOFF_CHANNELS.initiate](document.event, {...ORIGIN_INSTANCE, webAppEndpoint}),
				/instance.webAppEndpoint is invalid/,
				webAppEndpoint,
			);
		}
		assert.deepEqual(requests, []);
	});
});
