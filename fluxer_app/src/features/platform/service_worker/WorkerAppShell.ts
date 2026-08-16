// SPDX-License-Identifier: AGPL-3.0-or-later

const APP_SHELL_CACHE_KEY = '/app-shell';
const APP_SHELL_SEED_PATH = '/';

export interface PrecacheEntry {
	readonly url: string;
	readonly revision: string;
}

export interface AppShellRuntime {
	readonly caches: CacheStorage | undefined;
	readonly fetch: (request: Request) => Promise<Response>;
	readonly origin: string;
	readonly precacheName: string;
	readonly navigationCacheName: string;
	readonly networkTimeoutMs: number;
	readonly onCacheWriteError: (error: unknown) => void;
}

export function isCacheableResponse(response: Response): boolean {
	return response.ok || response.type === 'opaque';
}

export function isPrecacheableAssetUrl(url: string): boolean {
	const pathname = url.split(/[?#]/, 1)[0].toLowerCase();
	return pathname !== '/' && !pathname.endsWith('.html');
}

export async function precacheAssets(runtime: AppShellRuntime, manifest: ReadonlyArray<PrecacheEntry>): Promise<void> {
	if (!runtime.caches) {
		return;
	}
	const cache = await runtime.caches.open(runtime.precacheName);
	await Promise.allSettled(
		manifest
			.filter((entry) => isPrecacheableAssetUrl(entry.url))
			.map(async (entry) => {
				const request = new Request(new URL(entry.url, runtime.origin).toString(), {cache: 'reload'});
				const response = await runtime.fetch(request);
				if (isCacheableResponse(response)) {
					await cache.put(entry.url, response);
				}
			}),
	);
}

async function readAppShell(runtime: AppShellRuntime): Promise<Response | undefined> {
	if (!runtime.caches) {
		return undefined;
	}
	const cache = await runtime.caches.open(runtime.navigationCacheName);
	return (await cache.match(APP_SHELL_CACHE_KEY)) ?? undefined;
}

async function storeAppShell(runtime: AppShellRuntime, response: Response): Promise<void> {
	if (!runtime.caches || !isCacheableResponse(response)) {
		return;
	}
	try {
		const cache = await runtime.caches.open(runtime.navigationCacheName);
		await cache.put(APP_SHELL_CACHE_KEY, response.clone());
	} catch (error) {
		runtime.onCacheWriteError(error);
	}
}

export async function seedAppShell(runtime: AppShellRuntime): Promise<void> {
	if (!runtime.caches) {
		return;
	}
	const request = new Request(new URL(APP_SHELL_SEED_PATH, runtime.origin).toString(), {cache: 'reload'});
	const response = await runtime.fetch(request);
	await storeAppShell(runtime, response);
}

export async function fetchAppShellNavigation(runtime: AppShellRuntime, request: Request): Promise<Response> {
	const timeout = new Promise<Response | undefined>((resolve) => {
		setTimeout(() => resolve(undefined), runtime.networkTimeoutMs);
	});
	const network = runtime.fetch(request).then(async (response) => {
		await storeAppShell(runtime, response);
		return response;
	});
	let networkResponse: Response | undefined;
	try {
		networkResponse = await Promise.race([network, timeout]);
		if (networkResponse && isCacheableResponse(networkResponse)) {
			return networkResponse;
		}
	} catch {}
	const cached = await readAppShell(runtime);
	if (cached) {
		network.catch(() => undefined);
		return cached;
	}
	if (networkResponse) {
		return networkResponse;
	}
	return network;
}
