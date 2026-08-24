// SPDX-License-Identifier: AGPL-3.0-or-later

import {getWorkerFetchRoute} from '@app/features/platform/service_worker/WorkerFetchRouting';
import {describe, expect, it} from 'vitest';

const WORKER_ORIGIN = 'https://app.fluxer.test';

function request(url: string, init?: RequestInit): Request {
	return new Request(url, init);
}

describe('WorkerFetchRouting', () => {
	it('does not intercept expression media URLs', () => {
		expect(getWorkerFetchRoute(request('https://fluxerusercontent.com/emojis/123.webp'), WORKER_ORIGIN)).toBe('ignore');
		expect(getWorkerFetchRoute(request('https://fluxerusercontent.com/stickers/456.webp'), WORKER_ORIGIN)).toBe(
			'ignore',
		);
		expect(getWorkerFetchRoute(request(`${WORKER_ORIGIN}/emojis/123.webp`), WORKER_ORIGIN)).toBe('ignore');
		expect(getWorkerFetchRoute(request(`${WORKER_ORIGIN}/stickers/456.webp`), WORKER_ORIGIN)).toBe('ignore');
	});

	it('keeps app-owned runtime routes explicit', () => {
		expect(
			getWorkerFetchRoute(request(`${WORKER_ORIGIN}/channels/@me`, {headers: {accept: 'text/html'}}), WORKER_ORIGIN),
		).toBe('navigation');
		expect(
			getWorkerFetchRoute(request(`${WORKER_ORIGIN}/admin`, {headers: {accept: 'text/html'}}), WORKER_ORIGIN),
		).toBe('ignore');
		expect(getWorkerFetchRoute(request(`${WORKER_ORIGIN}/manifest.json`), WORKER_ORIGIN)).toBe('metadata');
		expect(getWorkerFetchRoute(request(`${WORKER_ORIGIN}/version.json`), WORKER_ORIGIN)).toBe('metadata');
	});

	it('leaves every build output under /assets/ to the immutable HTTP cache', () => {
		const buildOutputs = [
			'/assets/16bf14551996ba83.js',
			'/assets/2d715e4730758083.worker.js',
			'/assets/38cf6fb33e42e52d.css',
			'/assets/a79f1c3119cd700d.woff2',
			'/assets/0f50c815cd5e74ce.wasm',
			'/assets/ff9b1f835d5aa8cb.png',
			'/assets/22e569554c3be0e5.webm',
			'/assets/63a33cf1048e51c7.mp3',
			'/assets/fonts-NOTICE.txt',
			'/assets/chunk',
			'/assets/fonts/inter',
		];
		for (const pathname of buildOutputs) {
			expect(getWorkerFetchRoute(request(`${WORKER_ORIGIN}${pathname}`), WORKER_ORIGIN)).toBe('ignore');
		}
	});

	it('only recognises metadata served from the worker origin', () => {
		expect(getWorkerFetchRoute(request('https://cdn.fluxer.test/manifest.json'), WORKER_ORIGIN)).toBe('ignore');
		expect(getWorkerFetchRoute(request('https://cdn.fluxer.test/version.json'), WORKER_ORIGIN)).toBe('ignore');
	});

	it('ignores non-GET requests', () => {
		expect(getWorkerFetchRoute(request(`${WORKER_ORIGIN}/version.json`, {method: 'POST'}), WORKER_ORIGIN)).toBe(
			'ignore',
		);
	});
});
