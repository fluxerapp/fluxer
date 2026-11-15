// SPDX-License-Identifier: AGPL-3.0-or-later

import {isAppNavigationPath} from '@app/features/platform/service_worker/WorkerNavigation';

export type WorkerFetchRoute = 'ignore' | 'asset' | 'metadata' | 'navigation';

interface WorkerUrlPattern {
	readonly protocol: string;
	readonly hostname: string;
	readonly port: string;
	readonly pathname: string;
}

export interface WorkerStaticRoute {
	readonly condition: {readonly urlPattern: WorkerUrlPattern};
	readonly source: 'network' | {cacheName: string};
}

function urlPattern(url: URL, pathname: string): WorkerUrlPattern {
	return {protocol: url.protocol.slice(0, -1), hostname: url.hostname, port: url.port, pathname};
}

export function getWorkerStaticRoutes(
	assetUrls: ReadonlySet<string>,
	workerOrigin: string,
	cacheName: string,
): Array<WorkerStaticRoute> {
	const assetBases = new Set(
		Array.from(assetUrls, (href) => {
			const url = new URL(href);
			return `${url.origin}${url.pathname.slice(0, url.pathname.lastIndexOf('/assets/'))}/assets/*`;
		}),
	);
	return [
		{condition: {urlPattern: urlPattern(new URL(workerOrigin), '/api/*')}, source: 'network'},
		...Array.from(assetBases, (base): WorkerStaticRoute => {
			const url = new URL(base);
			return {condition: {urlPattern: urlPattern(url, url.pathname)}, source: {cacheName}};
		}),
	];
}

function isNavigationRequest(request: Request): boolean {
	if (request.mode === 'navigate') return true;
	const accept = request.headers.get('accept') ?? '';
	return request.destination === 'document' || accept.includes('text/html');
}

export function getWorkerFetchRoute(
	request: Request,
	workerOrigin: string,
	assetUrls: ReadonlySet<string>,
): WorkerFetchRoute {
	if (request.method !== 'GET') {
		return 'ignore';
	}
	const url = new URL(request.url);
	if (assetUrls.has(url.href) && request.cache !== 'reload') {
		return 'asset';
	}
	if (url.origin !== workerOrigin) {
		return 'ignore';
	}
	if (isNavigationRequest(request)) {
		return isAppNavigationPath(url.pathname) ? 'navigation' : 'ignore';
	}
	if (url.pathname === '/manifest.json' || url.pathname === '/version.json') {
		return 'metadata';
	}
	return 'ignore';
}
