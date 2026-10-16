// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Buffer} from 'node:buffer';
import {PassThrough, Readable} from 'node:stream';
import {describe, mock, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {DesktopLocalAppProxyClient, LocalAppProxyTraffic} = await import('./LocalAppProxyClient.ts');

const RENDERED_AVATAR_COUNT = 120;
const PARKED_MEDIA_BODY_COUNT = 200;
const RESOURCE_HEADERS_DEADLINE_MS = 30_000;

function tick() {
	return new Promise((resolve) => {
		setImmediate(resolve);
	});
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

function createUpstream({streamBodies}) {
	const state = {started: [], aborted: 0};
	const outboundHTTP = {
		request: (request) =>
			new Promise((resolve, reject) => {
				const entry = {
					url: request.url,
					answer: () =>
						resolve({
							headers: {'content-type': 'image/webp'},
							message: streamBodies ? new PassThrough() : Readable.from([Buffer.from('avatar-bytes')]),
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

describe('the local app proxy never queues a request behind another', () => {
	test('every avatar of a fully rendered chat starts at once', async () => {
		const {outboundHTTP, state} = createUpstream({streamBodies: false});
		const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
		const renderer = new AbortController();

		const fetches = [];
		for (let index = 0; index < RENDERED_AVATAR_COUNT; index += 1) {
			fetches.push(client.fetch(avatarRequest(index, renderer.signal)));
		}
		await tick();
		assert.equal(state.started.length, RENDERED_AVATAR_COUNT);

		for (const entry of state.started) {
			entry.answer();
		}
		const texts = await Promise.all(fetches.map(async (fetch) => await (await fetch).text()));
		assert.deepEqual(texts, new Array(RENDERED_AVATAR_COUNT).fill('avatar-bytes'));
	});

	test('a media request starts at once while earlier response bodies sit unread', async () => {
		const {outboundHTTP, state} = createUpstream({streamBodies: true});
		const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
		const renderer = new AbortController();

		for (let index = 0; index < PARKED_MEDIA_BODY_COUNT; index += 1) {
			const fetch = client.fetch(avatarRequest(index, renderer.signal));
			await tick();
			state.started.at(-1).answer();
			assert.equal((await fetch).status, 200);
		}

		const next = client.fetch(avatarRequest(PARKED_MEDIA_BODY_COUNT, renderer.signal));
		await tick();
		assert.equal(state.started.length, PARKED_MEDIA_BODY_COUNT + 1);
		state.started.at(-1).answer();
		assert.equal((await next).status, 200);
		renderer.abort();
	});
});

describe('media that a slow or silent self-host never answers is cut off', () => {
	test('a silent upstream is aborted at the headers deadline', async () => {
		mock.timers.enable({apis: ['setTimeout']});
		try {
			const {outboundHTTP, state} = createUpstream({streamBodies: false});
			const client = new DesktopLocalAppProxyClient({outboundHTTP, traffic: LocalAppProxyTraffic.REMOTE_RESOURCE});
			const renderer = new AbortController();
			const silent = client.fetch(avatarRequest(0, renderer.signal));
			silent.catch(() => {});
			await tick();

			mock.timers.tick(RESOURCE_HEADERS_DEADLINE_MS);

			await assert.rejects(silent, (error) => error.name === 'LocalAppProxyResponseHeadersDeadlineError');
			assert.equal(state.aborted, 1);
		} finally {
			mock.timers.reset();
		}
	});
});
