// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {mkdirSync, mkdtempSync, writeFileSync} from 'node:fs';
import {rm} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {after, beforeEach, describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();
const {DesktopLocalAppRequestHandler} = await import('@electron/main/LocalAppRequestHandler');
const {DesktopLocalAppFiles} = await import('@electron/main/LocalAppFileResolver');

const rendererRoot = mkdtempSync(path.join(os.tmpdir(), 'local-app-handler-'));
mkdirSync(path.join(rendererRoot, 'assets'));
writeFileSync(
	path.join(rendererRoot, 'index.html'),
	'<html lang="en"><head><script nonce="{{CSP_NONCE_PLACEHOLDER}}"></script></head><body></body></html>',
);
writeFileSync(path.join(rendererRoot, 'assets', 'deadbeefdeadbeef.js'), 'console.log(1)');

const INSTANCE_KEY = 'https://api.fluxer.app';
const ENCODED_KEY = encodeURIComponent(INSTANCE_KEY);
const SECRET = 'test-secret';
const REACTION_BURST_SIZE = 300;

const PLAN = {
	instanceKey: INSTANCE_KEY,
	document: {},
	selfHosted: false,
	desktopModulesEnabled: null,
	endpoints: {
		apiEndpoint: 'https://api.fluxer.app',
		apiPublicEndpoint: 'https://api.fluxer.app',
		webAppEndpoint: 'https://web.fluxer.app',
		mediaEndpoint: 'https://media.fluxer.app',
		staticCdnEndpoint: null,
		uploadRelayEndpoint: null,
		gatewayEndpoint: null,
		inviteEndpoint: null,
		giftEndpoint: null,
	},
};

let proxyCalls;

function createProxyClient() {
	return {
		fetch(request) {
			proxyCalls.push(request);
			return Promise.resolve(new Response('upstream', {status: 200, headers: {'Content-Type': 'text/plain'}}));
		},
		responseBodyForMethod({method, response}) {
			return Promise.resolve(method === 'HEAD' ? null : response.body);
		},
		settleFailure({failure}) {
			return Promise.resolve({bodyCancellationFailed: false, error: failure});
		},
	};
}

function createHandler(options = {}) {
	return new DesktopLocalAppRequestHandler({
		authorization: {
			hasValidRequestAuthorization: (request) =>
				request.headers.get('X-Fluxer-Desktop-Protocol-Authorization') === SECRET,
		},
		files: new DesktopLocalAppFiles({rendererRoot}),
		indexContext: () => ({prebootTheme: null}),
		proxyClient: createProxyClient(),
		runtimePlans: {findPlanForRoute: (key) => (key === INSTANCE_KEY ? PLAN : null)},
		shutdownSignal: new AbortController().signal,
		...options,
	});
}

function authorized(url, init = {}) {
	const headers = new Headers(init.headers ?? {});
	headers.set('X-Fluxer-Desktop-Protocol-Authorization', SECRET);
	return new Request(url, {...init, headers});
}

beforeEach(() => {
	proxyCalls = [];
});

after(async () => {
	await rm(rendererRoot, {recursive: true, force: true});
});

describe('an unauthorized request is indistinguishable from nothing being there', () => {
	test('no header at all yields a bare 404', async () => {
		const response = await createHandler().handle(new Request('fluxer-app://app/'));
		assert.equal(response.status, 404);
		assert.equal(await response.text(), 'Not found');
	});

	test('a wrong header value yields the same 404 on every privileged route', async () => {
		const handler = createHandler();
		for (const url of [
			'fluxer-app://app/',
			'fluxer-app://app/channels/@me',
			`fluxer-app://app/api/${ENCODED_KEY}/v1/users/@me`,
			`fluxer-app://app/proxy/${ENCODED_KEY}?url=https%3A%2F%2Fmedia.fluxer.app%2Fx.png`,
		]) {
			const response = await handler.handle(
				new Request(url, {headers: {'X-Fluxer-Desktop-Protocol-Authorization': 'wrong'}}),
			);
			assert.equal(response.status, 404, url);
			assert.equal(await response.text(), 'Not found', url);
		}
		assert.deepEqual(proxyCalls, [], 'an unauthorized request must never reach the network');
	});

	test('static asset reads are exempt because Electron cannot stamp browser-initiated loads', async () => {
		const handler = createHandler();
		const response = await handler.handle(
			new Request('fluxer-app://app/assets/deadbeefdeadbeef.js', {
				headers: {'X-Fluxer-Desktop-Protocol-Authorization': 'wrong'},
			}),
		);
		assert.equal(response.status, 200);
		assert.deepEqual(proxyCalls, [], 'an asset read must never reach the network');
	});

	test('the asset exemption is confined to GET/HEAD reads of served asset paths', async () => {
		const handler = createHandler();
		const cases = [
			new Request('fluxer-app://app/assets/deadbeefdeadbeef.js', {
				method: 'POST',
				headers: {'X-Fluxer-Desktop-Protocol-Authorization': 'wrong'},
			}),
			new Request('fluxer-app://app/not-assets/deadbeefdeadbeef.js', {
				headers: {'X-Fluxer-Desktop-Protocol-Authorization': 'wrong'},
			}),
			new Request('fluxer-app://app/assets/index.html', {
				headers: {'X-Fluxer-Desktop-Protocol-Authorization': 'wrong'},
			}),
		];
		for (const request of cases) {
			const response = await handler.handle(request);
			assert.equal(response.status, 404, request.url);
		}
	});
});

describe('the document gate keeps a hostile instance out of the privileged origin', () => {
	test('a framed /api request is 404ed before any outbound fetch is attempted', async () => {
		const response = await createHandler().handle(
			authorized(`fluxer-app://app/api/${ENCODED_KEY}/v1/users/@me`, {
				headers: {'Upgrade-Insecure-Requests': '1'},
			}),
		);
		assert.equal(response.status, 404);
		assert.deepEqual(proxyCalls, [], 'the proxy client must not be called for a document-class request');
	});

	test('a framed /proxy request is 404ed before any outbound fetch is attempted', async () => {
		const response = await createHandler().handle(
			authorized(`fluxer-app://app/proxy/${ENCODED_KEY}?url=https%3A%2F%2Fmedia.fluxer.app%2Fx.png`, {
				headers: {'Upgrade-Insecure-Requests': '1'},
			}),
		);
		assert.equal(response.status, 404);
		assert.deepEqual(proxyCalls, []);
	});

	test('a Sec-Fetch-Dest document load is 404ed even without a navigation preference header', async () => {
		const handler = createHandler();
		for (const url of [
			`fluxer-app://app/api/${ENCODED_KEY}/v1/users/@me`,
			`fluxer-app://app/proxy/${ENCODED_KEY}?url=https%3A%2F%2Fmedia.fluxer.app%2Fx.png`,
		]) {
			const response = await handler.handle(authorized(url, {headers: {'Sec-Fetch-Dest': 'document'}}));
			assert.equal(response.status, 404, url);
		}
		assert.deepEqual(proxyCalls, [], 'the proxy client must not be called for a document-class request');
	});

	test('every document-class destination is refused, not just a top-level document', async () => {
		const handler = createHandler();
		for (const destination of ['document', 'embed', 'frame', 'iframe', 'object']) {
			const response = await handler.handle(
				authorized(`fluxer-app://app/api/${ENCODED_KEY}/v1/users/@me`, {
					headers: {'Sec-Fetch-Dest': destination},
				}),
			);
			assert.equal(response.status, 404, destination);
		}
		assert.deepEqual(proxyCalls, [], 'the proxy client must not be called for a document-class request');
	});

	test('a data-class destination on the same route is still served', async () => {
		const response = await createHandler().handle(
			authorized(`fluxer-app://app/api/${ENCODED_KEY}/v1/users/@me`, {headers: {'Sec-Fetch-Dest': 'empty'}}),
		);
		assert.equal(response.status, 200);
		assert.equal(proxyCalls.length, 1);
	});

	test('the same URL fetched as data still works', async () => {
		const response = await createHandler().handle(authorized(`fluxer-app://app/api/${ENCODED_KEY}/v1/users/@me`));
		assert.equal(response.status, 200);
		assert.equal(proxyCalls.length, 1);
		assert.equal(proxyCalls[0].targetURL, 'https://api.fluxer.app/v1/users/@me');
	});
});

describe('runtime key routing', () => {
	test('an unknown runtime key is 404ed and never reaches the network', async () => {
		const handler = createHandler();
		for (const url of [
			'fluxer-app://app/api/https%3A%2F%2Fevil.example/v1/users/@me',
			'fluxer-app://app/proxy/https%3A%2F%2Fevil.example?url=https%3A%2F%2Fmedia.fluxer.app%2Fx.png',
		]) {
			const response = await handler.handle(authorized(url));
			assert.equal(response.status, 404, url);
		}
		assert.deepEqual(proxyCalls, []);
	});

	test('a remote target outside the active plan is refused with 403', async () => {
		const response = await createHandler().handle(
			authorized(`fluxer-app://app/proxy/${ENCODED_KEY}?url=https%3A%2F%2Fevil.example%2Fx.png`),
		);
		assert.equal(response.status, 403);
		assert.deepEqual(proxyCalls, []);
	});

	test('a remote target inside the active plan is proxied', async () => {
		const response = await createHandler().handle(
			authorized(`fluxer-app://app/proxy/${ENCODED_KEY}?url=https%3A%2F%2Fmedia.fluxer.app%2Fx.png`),
		);
		assert.equal(response.status, 200);
		assert.equal(proxyCalls[0].targetURL, 'https://media.fluxer.app/x.png');
	});

	test('a POST on the remote resource route is refused', async () => {
		const response = await createHandler().handle(
			authorized(`fluxer-app://app/proxy/${ENCODED_KEY}?url=https%3A%2F%2Fmedia.fluxer.app%2Fx.png`, {
				method: 'POST',
			}),
		);
		assert.equal(response.status, 405);
		assert.equal(response.headers.get('Allow'), 'GET, HEAD, PUT');
		assert.deepEqual(proxyCalls, []);
	});
});

describe('everything that is not a runtime route falls through to the bundle', () => {
	test('a bare /api or /proxy with no key renders the SPA shell', async () => {
		const handler = createHandler();
		for (const url of ['fluxer-app://app/api', 'fluxer-app://app/api/', 'fluxer-app://app/proxy']) {
			const response = await handler.handle(authorized(url));
			assert.equal(response.status, 200, url);
			assert.equal(response.headers.get('Content-Type'), 'text/html; charset=utf-8', url);
		}
		assert.deepEqual(proxyCalls, []);
	});

	test('the index response carries a CSP whose nonce is the one in the document', async () => {
		const response = await createHandler().handle(authorized('fluxer-app://app/channels/@me'));
		assert.equal(response.status, 200);
		assert.equal(response.headers.get('Cache-Control'), 'no-store');
		const body = await response.text();
		const nonce = /nonce="([0-9a-f]{32})"/u.exec(body)?.[1];
		assert.ok(nonce != null, 'no nonce in the served document');
		assert.ok(
			response.headers.get('Content-Security-Policy').includes(`'nonce-${nonce}'`),
			'the nonce must round-trip',
		);
	});

	test('the index Content-Length describes the rewritten body, not the file on disk', async () => {
		const handler = createHandler();
		const body = await (await handler.handle(authorized('fluxer-app://app/'))).text();
		const head = await handler.handle(authorized('fluxer-app://app/', {method: 'HEAD'}));
		assert.equal(await head.text(), '');
		assert.ok(Number(head.headers.get('Content-Length')) > 35, 'HEAD advertised the pre-rewrite length');
		assert.ok(body.length > 35);
	});

	test('a missing asset is 404 and a bad method is 405', async () => {
		const handler = createHandler();
		assert.equal((await handler.handle(authorized('fluxer-app://app/assets/nope.js'))).status, 404);
		const badMethod = await handler.handle(authorized('fluxer-app://app/', {method: 'POST'}));
		assert.equal(badMethod.status, 405);
		assert.equal(badMethod.headers.get('Allow'), 'GET, HEAD');
	});

	test('a message full of reaction emoji is served in full, never turned away for capacity', async () => {
		const handler = createHandler();
		const responses = await Promise.all(
			Array.from({length: REACTION_BURST_SIZE}, () =>
				handler.handle(authorized('fluxer-app://app/assets/deadbeefdeadbeef.js')),
			),
		);
		assert.deepEqual(new Set(responses.map((response) => response.status)), new Set([200]));
		const bodies = await Promise.all(responses.map((response) => response.text()));
		assert.deepEqual(bodies, new Array(REACTION_BURST_SIZE).fill('console.log(1)'));
	});
});
