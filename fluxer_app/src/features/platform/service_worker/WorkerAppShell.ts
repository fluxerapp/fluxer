// SPDX-License-Identifier: AGPL-3.0-or-later

import {APP_SHELL_BUILD_META_NAME, WORKER_CACHE_PREFIX} from '@app/features/platform/service_worker/WorkerCacheCleanup';
import {servesSignedInShell} from '@app/features/platform/service_worker/WorkerNavigation';

const APP_SHELL_CACHE_KEY = '/app-shell';
const APP_SHELL_SEED_PATH = '/app';
const APP_SHELL_MARKER_HEADER = 'x-fluxer-app-shell';

export interface PrecacheEntry {
	readonly url: string;
	readonly integrity: string | null;
}

export interface AppShellRuntime {
	readonly caches: CacheStorage;
	readonly fetch: (request: Request) => Promise<Response>;
	readonly origin: string;
	readonly version: string;
	readonly precacheName: string;
	readonly navigationCacheName: string;
	readonly networkTimeoutMs: number;
	readonly onBuildMismatch: () => Promise<unknown>;
	readonly onCacheWriteError: (error: unknown) => void;
}

export function readAppShellBuildId(html: string): string | null {
	for (const [tag] of html.matchAll(/<meta\b[^>]*>/giu)) {
		if (tag.match(/\bname=["']([^"']+)["']/iu)?.[1] === APP_SHELL_BUILD_META_NAME) {
			return tag.match(/\bcontent=["']([^"']+)["']/iu)?.[1] ?? null;
		}
	}
	return null;
}

async function appShellBuildId(response: Response): Promise<string | null> {
	if (!response.ok || response.headers.get(APP_SHELL_MARKER_HEADER) !== '1') return null;
	return readAppShellBuildId(await response.clone().text());
}

async function findCachedAsset(runtime: AppShellRuntime, request: Request): Promise<Response | undefined> {
	const names = (await runtime.caches.keys()).filter((name) => name.startsWith(`${WORKER_CACHE_PREFIX}-`));
	for (const cacheName of names) {
		const cached = await runtime.caches.match(request, {cacheName});
		if (cached?.ok && cached.type !== 'opaque') {
			return cached;
		}
	}
	return undefined;
}

async function precacheEntry(runtime: AppShellRuntime, cache: Cache, entry: PrecacheEntry): Promise<void> {
	const request = new Request(new URL(entry.url, runtime.origin), {
		mode: 'cors',
		credentials: 'omit',
		cache: entry.integrity ? 'default' : 'reload',
		integrity: entry.integrity ?? '',
	});
	const response =
		(entry.integrity ? await findCachedAsset(runtime, request) : undefined) ?? (await runtime.fetch(request));
	if (!response.ok || response.type === 'opaque') {
		throw new Error(`Precache failed: ${entry.url}`);
	}
	await cache.put(request, response);
}

export async function precacheAssets(runtime: AppShellRuntime, manifest: ReadonlyArray<PrecacheEntry>): Promise<void> {
	const cache = await runtime.caches.open(runtime.precacheName);
	const assets = manifest.filter((entry) => entry.integrity !== null);
	const metadata = manifest.filter((entry) => entry.integrity === null);
	await Promise.all([
		...assets.map((entry) => precacheEntry(runtime, cache, entry)),
		...metadata.map((entry) => precacheEntry(runtime, cache, entry).catch(() => undefined)),
	]);
}

async function readAppShell(runtime: AppShellRuntime): Promise<Response | undefined> {
	return (await runtime.caches.match(APP_SHELL_CACHE_KEY, {cacheName: runtime.navigationCacheName})) ?? undefined;
}

async function storeAppShell(runtime: AppShellRuntime, response: Response): Promise<void> {
	const buildId = await appShellBuildId(response);
	if (buildId !== runtime.version) {
		if (buildId !== null) await runtime.onBuildMismatch();
		return;
	}
	try {
		const cache = await runtime.caches.open(runtime.navigationCacheName);
		await cache.put(APP_SHELL_CACHE_KEY, response);
	} catch (error) {
		runtime.onCacheWriteError(error);
	}
}

function shellRequest(runtime: AppShellRuntime, url: string, cache: RequestCache, priority: RequestPriority): Request {
	return new Request(new URL(url, runtime.origin), {mode: 'cors', credentials: 'omit', cache, priority});
}

export async function seedAppShell(runtime: AppShellRuntime): Promise<void> {
	const response = await runtime.fetch(shellRequest(runtime, APP_SHELL_SEED_PATH, 'reload', 'auto'));
	if ((await appShellBuildId(response)) !== runtime.version) throw new Error('Precache shell build mismatch');
	const cache = await runtime.caches.open(runtime.navigationCacheName);
	await cache.put(APP_SHELL_CACHE_KEY, response);
}

export async function fetchAppShellNavigation(
	runtime: AppShellRuntime,
	request: Request,
	keepAlive: (work: Promise<unknown>) => void,
): Promise<Response> {
	const cached = servesSignedInShell(new URL(request.url).pathname)
		? await readAppShell(runtime).catch(() => undefined)
		: undefined;
	if (cached) {
		keepAlive(
			runtime
				.fetch(shellRequest(runtime, APP_SHELL_SEED_PATH, 'no-cache', 'low'))
				.then((response) => storeAppShell(runtime, response))
				.catch(() => undefined),
		);
		return cached;
	}
	return fetchNetworkShell(runtime, request, keepAlive);
}

async function fetchNetworkShell(
	runtime: AppShellRuntime,
	request: Request,
	keepAlive: (work: Promise<unknown>) => void,
): Promise<Response> {
	let timeoutId: ReturnType<typeof setTimeout> | undefined;
	const timeout = new Promise<undefined>((resolve) => {
		timeoutId = setTimeout(() => resolve(undefined), runtime.networkTimeoutMs);
	});
	const network = runtime.fetch(request);
	if (servesSignedInShell(new URL(request.url).pathname)) {
		keepAlive(network.then((response) => storeAppShell(runtime, response.clone())).catch(() => undefined));
	}
	try {
		let networkResponse: Response | undefined;
		try {
			networkResponse = await Promise.race([network, timeout]);
			if (networkResponse?.ok) return networkResponse;
		} catch {}
		const cached = await readAppShell(runtime);
		if (cached) return cached;
		return networkResponse ?? network;
	} finally {
		clearTimeout(timeoutId);
	}
}
