// SPDX-License-Identifier: AGPL-3.0-or-later

export const WORKER_CACHE_PREFIX = 'fluxer';
export const WORKER_NAVIGATION_CACHE_PREFIX = `${WORKER_CACHE_PREFIX}-navigation-`;

export function shouldDeleteWorkerCache(cacheName: string, expectedCaches: ReadonlySet<string>): boolean {
	return cacheName.startsWith(`${WORKER_CACHE_PREFIX}-`) && !expectedCaches.has(cacheName);
}
