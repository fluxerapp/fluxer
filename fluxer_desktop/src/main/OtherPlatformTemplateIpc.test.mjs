// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {afterEach, describe, test} from 'node:test';
import {installElectronStub, installWindowStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();
installWindowStub(null);

const {DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS, OtherPlatformTemplateFetchFailure, isOtherPlatformTemplateCode} =
	await import('../../../packages/desktop_ipc/src/OtherPlatformTemplateContract.ts');
const {
	MAX_TEMPLATE_RESPONSE_BYTES,
	cleanupOtherPlatformTemplateHandlers,
	fetchOtherPlatformTemplate,
	registerOtherPlatformTemplateHandlers,
} = await import('./OtherPlatformTemplateIpc.ts');
const {createOtherPlatformTemplatePreloadAPI} = await import('../preload/OtherPlatformTemplatePreload.ts');

const TEMPLATE_BODY = {code: 'hgM48av5Q69A', name: 'Starter', serialized_source_guild: {name: 'Starter'}};

function installFetch(respond) {
	const calls = [];
	const handlers = new Map();
	installElectronStub({
		net: {
			fetch: async (url, init) => {
				calls.push({url, init});
				return respond(url, init);
			},
		},
		ipcMain: {
			handle: (channel, listener) => handlers.set(channel, listener),
			removeHandler: (channel) => handlers.delete(channel),
		},
	});
	return {calls, handlers};
}

function jsonResponse(body, init = {}) {
	return new Response(JSON.stringify(body), {
		status: 200,
		headers: {'content-type': 'application/json'},
		...init,
	});
}

function topLevelSender() {
	const frame = {detached: false, parent: null, url: 'fluxer-app://app/'};
	return {sender: {}, senderFrame: frame};
}

function subframeSender() {
	const parent = {detached: false, parent: null, url: 'fluxer-app://app/'};
	return {sender: {}, senderFrame: {detached: false, parent, url: 'fluxer-app://app/'}};
}

afterEach(() => {
	cleanupOtherPlatformTemplateHandlers();
});

describe('template code validation', () => {
	test('accepts alphanumeric codes and rejects anything that could reshape the URL', () => {
		assert.equal(isOtherPlatformTemplateCode('hgM48av5Q69A'), true);
		assert.equal(isOtherPlatformTemplateCode('a'.repeat(64)), true);
		for (const value of [
			'',
			'a'.repeat(65),
			'abc/def',
			'../users/@me',
			'abc?x=1',
			'abc#frag',
			'abc def',
			'abc%2F',
			'https://discord.new/abc',
			42,
			null,
			undefined,
		]) {
			assert.equal(isOtherPlatformTemplateCode(value), false, String(value));
		}
	});

	test('rejects an invalid code before any request is made', async () => {
		const {calls} = installFetch(() => jsonResponse(TEMPLATE_BODY));
		for (const code of ['abc/../x', '', 'a'.repeat(65), {code: 'abc'}]) {
			const result = await fetchOtherPlatformTemplate(code);
			assert.deepEqual(result, {
				ok: false,
				failure: OtherPlatformTemplateFetchFailure.INVALID_CODE,
			});
		}
		assert.equal(calls.length, 0);
	});
});

describe('template fetch', () => {
	test('requests only the fixed template endpoint with a generic browser user agent and no credentials', async () => {
		const {calls} = installFetch(() => jsonResponse(TEMPLATE_BODY));
		const result = await fetchOtherPlatformTemplate('hgM48av5Q69A');
		assert.deepEqual(result, {ok: true, template: TEMPLATE_BODY});
		assert.equal(calls.length, 1);
		const [{url, init}] = calls;
		assert.equal(url, 'https://discord.com/api/v10/guilds/templates/hgM48av5Q69A');
		assert.equal(init.method, 'GET');
		assert.equal(init.redirect, 'error');
		assert.equal(init.credentials, 'omit');
		const userAgent = init.headers['User-Agent'];
		assert.match(userAgent, /^Mozilla\/5\.0 \(/);
		assert.doesNotMatch(userAgent, /fluxer|electron/i);
		assert.equal(init.headers.Accept, 'application/json');
		assert.ok(init.signal instanceof AbortSignal);
	});

	test('maps a 404 to not found', async () => {
		installFetch(() => jsonResponse({message: 'Unknown Guild Template', code: 10057}, {status: 404}));
		assert.deepEqual(await fetchOtherPlatformTemplate('missing'), {
			ok: false,
			failure: OtherPlatformTemplateFetchFailure.NOT_FOUND,
		});
	});

	test('maps a 429 to rate limited', async () => {
		installFetch(() =>
			jsonResponse({message: 'You are being rate limited.'}, {status: 429, headers: {'retry-after': '2.4'}}),
		);
		assert.deepEqual(await fetchOtherPlatformTemplate('busy'), {
			ok: false,
			failure: OtherPlatformTemplateFetchFailure.RATE_LIMITED,
		});
	});

	test('maps server errors and transport failures to network', async () => {
		installFetch(() => new Response('oops', {status: 503}));
		assert.equal((await fetchOtherPlatformTemplate('abc')).failure, OtherPlatformTemplateFetchFailure.NETWORK);
		installFetch(() => {
			throw new TypeError('net::ERR_INTERNET_DISCONNECTED');
		});
		assert.equal((await fetchOtherPlatformTemplate('abc')).failure, OtherPlatformTemplateFetchFailure.NETWORK);
		installFetch(() => {
			throw new TypeError('redirect mode is set to error');
		});
		assert.equal((await fetchOtherPlatformTemplate('abc')).failure, OtherPlatformTemplateFetchFailure.NETWORK);
	});

	test('maps unreadable, non-object and oversized bodies to invalid response', async () => {
		installFetch(() => new Response('<html>not json</html>', {status: 200}));
		assert.equal((await fetchOtherPlatformTemplate('abc')).failure, OtherPlatformTemplateFetchFailure.INVALID_RESPONSE);
		installFetch(() => jsonResponse([TEMPLATE_BODY]));
		assert.equal((await fetchOtherPlatformTemplate('abc')).failure, OtherPlatformTemplateFetchFailure.INVALID_RESPONSE);
		installFetch(
			() => new Response('x', {status: 200, headers: {'content-length': String(MAX_TEMPLATE_RESPONSE_BYTES + 1)}}),
		);
		assert.equal((await fetchOtherPlatformTemplate('abc')).failure, OtherPlatformTemplateFetchFailure.INVALID_RESPONSE);
		const chunk = new Uint8Array(1024 * 1024).fill(0x20);
		let sent = 0;
		const stream = new ReadableStream({
			pull(controller) {
				if (sent > MAX_TEMPLATE_RESPONSE_BYTES) {
					controller.close();
					return;
				}
				sent += chunk.byteLength;
				controller.enqueue(chunk);
			},
		});
		installFetch(() => new Response(stream, {status: 200}));
		assert.equal((await fetchOtherPlatformTemplate('abc')).failure, OtherPlatformTemplateFetchFailure.INVALID_RESPONSE);
		assert.ok(sent <= MAX_TEMPLATE_RESPONSE_BYTES + chunk.byteLength);
	});
});

describe('template IPC', () => {
	test('the preload call reaches the registered handler for a trusted top-level document', async () => {
		const {handlers} = installFetch(() => jsonResponse(TEMPLATE_BODY));
		registerOtherPlatformTemplateHandlers();
		const handler = handlers.get(DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS.fetch);
		assert.equal(typeof handler, 'function');
		const api = createOtherPlatformTemplatePreloadAPI({
			invoke: (channel, ...args) => handlers.get(channel)(topLevelSender(), ...args),
		});
		assert.deepEqual(await api.otherPlatformTemplate.fetch('hgM48av5Q69A'), {ok: true, template: TEMPLATE_BODY});
	});

	test('refuses subframes and detached frames without fetching', async () => {
		const {calls, handlers} = installFetch(() => jsonResponse(TEMPLATE_BODY));
		registerOtherPlatformTemplateHandlers();
		const handler = handlers.get(DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS.fetch);
		assert.throws(() => handler(subframeSender(), 'hgM48av5Q69A'), /trusted top-level renderer document/);
		const detached = topLevelSender();
		detached.senderFrame.detached = true;
		assert.throws(() => handler(detached, 'hgM48av5Q69A'), /trusted top-level renderer document/);
		assert.throws(
			() => handler({sender: {}, senderFrame: null}, 'hgM48av5Q69A'),
			/trusted top-level renderer document/,
		);
		assert.equal(calls.length, 0);
	});

	test('cleanup removes the handler', () => {
		const {handlers} = installFetch(() => jsonResponse(TEMPLATE_BODY));
		registerOtherPlatformTemplateHandlers();
		cleanupOtherPlatformTemplateHandlers();
		assert.equal(handlers.has(DESKTOP_OTHER_PLATFORM_TEMPLATE_CHANNELS.fetch), false);
	});
});
