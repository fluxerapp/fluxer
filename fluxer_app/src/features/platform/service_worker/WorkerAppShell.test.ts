// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AppShellRuntime,
	fetchAppShellNavigation,
	type PrecacheEntry,
	precacheAssets,
	seedAppShell,
} from '@app/features/platform/service_worker/WorkerAppShell';
import {beforeEach, describe, expect, it} from 'vitest';

const WORKER_ORIGIN = 'https://self-hosted.fluxer.test';
const PRECACHE_NAME = 'fluxer-precache-test';
const NAVIGATION_CACHE_NAME = 'fluxer-navigation-test';
const NETWORK_TIMEOUT_MS = 10;
const SLOW_NETWORK_MS = 80;

const UNRENDERED_INDEX_TEMPLATE = [
	'<!doctype html><html lang="en"><head>',
	'<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">',
	'<link rel="apple-touch-icon" href="{{STATIC_CDN_ENDPOINT}}/web/apple-touch-icon.png">',
	'<script nonce="{{CSP_NONCE_PLACEHOLDER}}"></script>',
	'</head><body><div id="root"></div></body></html>',
].join('');

const RENDERED_INDEX_DOCUMENT = [
	'<!doctype html><html lang="en"><head>',
	'<link rel="preconnect" href="https://cdn.fluxer.test">',
	'<script nonce="abc123">window.__FLUXER_BOOTSTRAP__={"instance":{}};</script>',
	'</head><body><div id="root"></div></body></html>',
].join('');

const DEPLOYED_PRECACHE_MANIFEST: ReadonlyArray<PrecacheEntry> = [
	{url: '/index.html', revision: '2757:1'},
	{url: '/', revision: '2757:1'},
	{url: '/assets/app.js', revision: '10:1'},
];

class FakeCache {
	private readonly entries = new Map<string, Response>();

	async put(request: Request | string, response: Response): Promise<void> {
		this.entries.set(cacheKey(request), response);
	}

	async match(request: Request | string): Promise<Response | undefined> {
		return this.entries.get(cacheKey(request))?.clone();
	}

	async delete(request: Request | string): Promise<boolean> {
		return this.entries.delete(cacheKey(request));
	}
}

class FakeCacheStorage {
	private readonly caches = new Map<string, FakeCache>();

	async open(name: string): Promise<FakeCache> {
		const existing = this.caches.get(name);
		if (existing) {
			return existing;
		}
		const created = new FakeCache();
		this.caches.set(name, created);
		return created;
	}

	async match(request: Request | string): Promise<Response | undefined> {
		for (const cache of this.caches.values()) {
			const hit = await cache.match(request);
			if (hit) {
				return hit;
			}
		}
		return undefined;
	}

	async keys(): Promise<Array<string>> {
		return Array.from(this.caches.keys());
	}
}

function cacheKey(request: Request | string): string {
	const url = typeof request === 'string' ? request : request.url;
	return new URL(url, WORKER_ORIGIN).toString();
}

function delay(milliseconds: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, milliseconds);
	});
}

function createAppProxyFetch(navigationDelayMs: number): (request: Request) => Promise<Response> {
	return async (request: Request) => {
		const {pathname} = new URL(request.url);
		if (pathname === '/index.html') {
			return new Response(UNRENDERED_INDEX_TEMPLATE, {headers: {'content-type': 'text/html; charset=utf-8'}});
		}
		if (pathname === '/assets/app.js') {
			return new Response('console.log(1)', {headers: {'content-type': 'text/javascript'}});
		}
		await delay(navigationDelayMs);
		return new Response(RENDERED_INDEX_DOCUMENT, {headers: {'content-type': 'text/html; charset=utf-8'}});
	};
}

function navigationRequest(pathname: string): Request {
	return new Request(`${WORKER_ORIGIN}${pathname}`, {headers: {accept: 'text/html'}});
}

describe('WorkerAppShell', () => {
	let cacheStorage: FakeCacheStorage;

	function createRuntime(navigationDelayMs: number): AppShellRuntime {
		return {
			caches: cacheStorage as unknown as CacheStorage,
			fetch: createAppProxyFetch(navigationDelayMs),
			origin: WORKER_ORIGIN,
			precacheName: PRECACHE_NAME,
			navigationCacheName: NAVIGATION_CACHE_NAME,
			networkTimeoutMs: NETWORK_TIMEOUT_MS,
			onCacheWriteError: (error) => {
				throw error;
			},
		};
	}

	beforeEach(() => {
		cacheStorage = new FakeCacheStorage();
	});

	it('never serves the app-proxy index template that was fetched outside the bootstrap render', async () => {
		const runtime = createRuntime(SLOW_NETWORK_MS);
		await precacheAssets(runtime, DEPLOYED_PRECACHE_MANIFEST);

		const response = await fetchAppShellNavigation(runtime, navigationRequest('/channels/1234567890/9876543210'));
		const html = await response.text();

		expect(html).toContain('window.__FLUXER_BOOTSTRAP__');
		expect(html).not.toContain('{{STATIC_CDN_ENDPOINT}}');
	});

	it('replays the previously rendered document when the network loses the navigation race', async () => {
		const runtime = createRuntime(0);
		await precacheAssets(runtime, DEPLOYED_PRECACHE_MANIFEST);
		await fetchAppShellNavigation(runtime, navigationRequest('/channels/@me'));

		const slowRuntime = createRuntime(SLOW_NETWORK_MS);
		const response = await fetchAppShellNavigation(slowRuntime, navigationRequest('/channels/1234567890/9876543210'));
		const html = await response.text();

		expect(html).toBe(RENDERED_INDEX_DOCUMENT);
	});

	it('keeps documents out of the precache', async () => {
		const runtime = createRuntime(0);
		await precacheAssets(runtime, DEPLOYED_PRECACHE_MANIFEST);

		const precache = await cacheStorage.open(PRECACHE_NAME);

		expect(await precache.match('/index.html')).toBeUndefined();
		expect(await precache.match('/')).toBeUndefined();
		expect(await precache.match('/assets/app.js')).toBeDefined();
	});
	it('serves the seeded app shell when the network is unavailable', async () => {
		await seedAppShell(createRuntime(0));

		const offlineRuntime: AppShellRuntime = {
			...createRuntime(0),
			fetch: () => Promise.reject(new Error('Failed to fetch')),
		};
		const response = await fetchAppShellNavigation(offlineRuntime, navigationRequest('/channels/@me'));

		expect(await response.text()).toBe(RENDERED_INDEX_DOCUMENT);
	});

	it('ignores an unrendered template left in the precache by an older worker', async () => {
		const precache = await cacheStorage.open(PRECACHE_NAME);
		await precache.put(
			'/index.html',
			new Response(UNRENDERED_INDEX_TEMPLATE, {headers: {'content-type': 'text/html; charset=utf-8'}}),
		);
		await precache.put(
			'/',
			new Response(UNRENDERED_INDEX_TEMPLATE, {headers: {'content-type': 'text/html; charset=utf-8'}}),
		);
		await seedAppShell(createRuntime(0));

		const response = await fetchAppShellNavigation(createRuntime(SLOW_NETWORK_MS), navigationRequest('/channels/1/2'));
		const html = await response.text();

		expect(html).toContain('window.__FLUXER_BOOTSTRAP__');
		expect(html).not.toContain('{{STATIC_CDN_ENDPOINT}}');
	});
});
