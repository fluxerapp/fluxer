// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {Readable} from 'node:stream';
import {describe, mock, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {DesktopLocalAppProxyClient, LocalAppProxyTraffic} = await import('./LocalAppProxyClient.ts');

const PROXY_MAX_CONCURRENT_REQUESTS = 32;
const PROXY_MAX_QUEUED_REQUESTS = 4096;
const OUTBOUND_MAX_IN_FLIGHT_PER_SERVICE = 64;
const RENDERED_AVATAR_COUNT = 120;

class DesktopOutboundHTTPCapacityError extends Error {
	constructor(limit) {
		super(`Desktop outbound HTTP exceeded its ${limit} limit`);
		this.name = 'DesktopOutboundHTTPCapacityError';
	}
}

function tick() {
	return new Promise((resolve) => {
		setImmediate(resolve);
	});
}

function createOutboundHTTP() {
	const state = {inFlight: 0, peakInFlight: 0, started: 0, pending: []};
	const outboundHTTP = {
		request: (request) => {
			if (state.inFlight >= OUTBOUND_MAX_IN_FLIGHT_PER_SERVICE) {
				throw new DesktopOutboundHTTPCapacityError(`in-flight request for ${request.serviceName}`);
			}
			state.inFlight += 1;
			state.started += 1;
			state.peakInFlight = Math.max(state.peakInFlight, state.inFlight);
			return new Promise((resolve) => {
				state.pending.push(() => {
					state.inFlight -= 1;
					resolve({
						headers: {'content-type': 'image/webp'},
						message: Readable.from([Buffer.from('avatar-bytes')]),
						status: 200,
						statusText: 'OK',
						url: new URL(request.url),
					});
				});
			});
		},
	};
	return {outboundHTTP, state};
}

function avatarRequest(index, signal) {
	return {
		targetURL: `https://media.fluxer.app/avatars/${index}.webp`,
		method: 'GET',
		headers: new Headers(),
		body: null,
		signal,
		acceptEncoding: null,
		uploadId: null,
		uploadTotalBytes: null,
	};
}

async function pump(state, work) {
	let finished = false;
	const result = work.finally(() => {
		finished = true;
	});
	result.catch(() => {});
	while (!finished) {
		for (const settle of state.pending.splice(0)) {
			settle();
		}
		await tick();
	}
	return await result;
}

function readAvatar(client, index, signal) {
	return client.fetch(avatarRequest(index, signal)).then(async (response) => ({
		status: response.status,
		text: await response.text(),
	}));
}

describe('the local app proxy bounds its own concurrency instead of overrunning outbound HTTP', () => {
	test('every avatar of a fully rendered chat resolves rather than tripping the outbound capacity cap', async () => {
		const {outboundHTTP, state} = createOutboundHTTP();
		const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
		const controller = new AbortController();

		const fetches = [];
		for (let index = 0; index < RENDERED_AVATAR_COUNT; index += 1) {
			fetches.push(readAvatar(client, index, controller.signal));
		}
		const responses = await pump(state, Promise.all(fetches));

		assert.equal(responses.length, RENDERED_AVATAR_COUNT);
		assert.deepEqual(new Set(responses.map((response) => response.status)), new Set([200]));
		assert.equal(state.started, RENDERED_AVATAR_COUNT);
		assert.deepEqual(
			responses.map((response) => response.text),
			new Array(RENDERED_AVATAR_COUNT).fill('avatar-bytes'),
		);
	});

	test('no more requests are in flight at any instant than the proxy concurrency limit', async () => {
		const {outboundHTTP, state} = createOutboundHTTP();
		const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
		const controller = new AbortController();

		const fetches = [];
		for (let index = 0; index < RENDERED_AVATAR_COUNT; index += 1) {
			fetches.push(readAvatar(client, index, controller.signal));
		}
		await tick();
		const peakBeforeAnyCompletion = state.peakInFlight;
		await pump(state, Promise.all(fetches));

		assert.equal(peakBeforeAnyCompletion, PROXY_MAX_CONCURRENT_REQUESTS);
		assert.equal(state.peakInFlight, PROXY_MAX_CONCURRENT_REQUESTS);
	});

	test('an aborted waiter rejects with the abort reason and leaves its queue slot to the next waiter', async () => {
		const {outboundHTTP, state} = createOutboundHTTP();
		const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
		const held = new AbortController();

		const holding = [];
		for (let index = 0; index < PROXY_MAX_CONCURRENT_REQUESTS; index += 1) {
			holding.push(readAvatar(client, index, held.signal));
		}
		await tick();
		assert.equal(state.started, PROXY_MAX_CONCURRENT_REQUESTS);

		const abandoned = new AbortController();
		const abandonedFetch = client.fetch(avatarRequest(998, abandoned.signal));
		const survivor = readAvatar(client, 999, held.signal);
		const reason = new Error('the renderer navigated away');
		abandoned.abort(reason);

		await assert.rejects(abandonedFetch, (error) => error === reason);
		assert.equal(state.started, PROXY_MAX_CONCURRENT_REQUESTS);

		state.pending.shift()();
		await tick();
		await tick();
		assert.equal(state.started, PROXY_MAX_CONCURRENT_REQUESTS + 1);

		const settled = await pump(state, Promise.all([survivor, ...holding]));
		assert.equal(settled[0].status, 200);
		assert.equal(settled.length, PROXY_MAX_CONCURRENT_REQUESTS + 1);
	});

	test('the queue refuses a request past its bound rather than growing without limit', async () => {
		const {outboundHTTP, state} = createOutboundHTTP();
		const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
		const controller = new AbortController();

		const accepted = [];
		for (let index = 0; index < PROXY_MAX_CONCURRENT_REQUESTS + PROXY_MAX_QUEUED_REQUESTS; index += 1) {
			accepted.push(readAvatar(client, index, controller.signal));
		}

		await assert.rejects(
			client.fetch(avatarRequest(Number.MAX_SAFE_INTEGER, controller.signal)),
			(error) => error.name === 'LocalAppProxyQueueOverflowError',
		);

		const settled = await pump(state, Promise.all(accepted));
		assert.equal(settled.length, PROXY_MAX_CONCURRENT_REQUESTS + PROXY_MAX_QUEUED_REQUESTS);
	});
});

const RESOURCE_HEADERS_DEADLINE_MS = 30_000;

function createSilentUpstream() {
	const state = {started: [], aborted: 0};
	const outboundHTTP = {
		request: (request) =>
			new Promise((resolve, reject) => {
				const entry = {
					url: request.url,
					serviceName: request.serviceName,
					answer: () =>
						resolve({
							headers: {'content-type': 'image/webp'},
							message: Readable.from([Buffer.from('avatar-bytes')]),
							status: 200,
							statusText: 'OK',
							url: new URL(request.url),
						}),
				};
				state.started.push(entry);
				request.signal.addEventListener(
					'abort',
					() => {
						state.aborted += 1;
						reject(request.signal.reason);
					},
					{once: true},
				);
			}),
	};
	return {outboundHTTP, state};
}

function apiRequest(signal) {
	return {...avatarRequest(0, signal), targetURL: 'https://api.fluxer.app/v1/users/@me'};
}

describe('media that a slow or silent self-host never answers cannot wedge the local app proxy', () => {
	test('avatars dropped while their upstream sends no headers free their permits at the headers deadline', async () => {
		mock.timers.enable({apis: ['setTimeout']});
		try {
			const {outboundHTTP, state} = createSilentUpstream();
			const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
			const renderer = new AbortController();
			const dropped = [];
			for (let index = 0; index < PROXY_MAX_CONCURRENT_REQUESTS; index += 1) {
				const fetch = client.fetch(avatarRequest(index, renderer.signal));
				fetch.catch(() => {});
				dropped.push(fetch);
			}
			await tick();
			const fresh = client.fetch(avatarRequest(1000, renderer.signal));
			await tick();
			assert.equal(state.started.length, PROXY_MAX_CONCURRENT_REQUESTS);

			mock.timers.tick(RESOURCE_HEADERS_DEADLINE_MS);
			await Promise.allSettled(dropped);
			await tick();

			assert.equal(state.aborted, PROXY_MAX_CONCURRENT_REQUESTS);
			assert.equal(state.started.length, PROXY_MAX_CONCURRENT_REQUESTS + 1);
			state.started.at(-1).answer();
			const response = await fresh;
			assert.equal(response.status, 200);
			assert.equal(await response.text(), 'avatar-bytes');
		} finally {
			mock.timers.reset();
		}
	});

	test('API requests keep flowing while every media permit is held by a silent upstream', async () => {
		const {outboundHTTP, state} = createSilentUpstream();
		const media = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
		const api = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.API});
		const renderer = new AbortController();
		for (let index = 0; index < PROXY_MAX_CONCURRENT_REQUESTS * 2; index += 1) {
			media.fetch(avatarRequest(index, renderer.signal)).catch(() => {});
		}
		await tick();
		const apiCall = api.fetch(apiRequest(renderer.signal));
		await tick();
		const started = state.started.at(-1);
		assert.equal(started.url, 'https://api.fluxer.app/v1/users/@me');
		assert.equal(started.serviceName, LocalAppProxyTraffic.API);
		started.answer();
		assert.equal((await apiCall).status, 200);
		renderer.abort();
	});

	test('the newest queued media request is admitted first so what is on screen loads before what scrolled away', async () => {
		const {outboundHTTP, state} = createSilentUpstream();
		const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
		const renderer = new AbortController();
		const fetches = [];
		for (let index = 0; index < PROXY_MAX_CONCURRENT_REQUESTS + 3; index += 1) {
			const fetch = client.fetch(avatarRequest(index, renderer.signal));
			fetch.catch(() => {});
			fetches.push(fetch);
		}
		await tick();
		state.started[0].answer();
		await (await fetches[0]).text();
		await tick();
		assert.equal(
			state.started.at(-1).url,
			`https://media.fluxer.app/avatars/${PROXY_MAX_CONCURRENT_REQUESTS + 2}.webp`,
		);
		renderer.abort();
	});
});
