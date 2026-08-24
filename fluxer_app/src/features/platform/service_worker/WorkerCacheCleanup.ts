// SPDX-License-Identifier: AGPL-3.0-or-later

export const WORKER_CACHE_PREFIX = 'fluxer';

export function shouldDeleteWorkerCache(cacheName: string, expectedCaches: ReadonlySet<string>): boolean {
	return cacheName.startsWith(`${WORKER_CACHE_PREFIX}-`) && !expectedCaches.has(cacheName);
}
