// SPDX-License-Identifier: AGPL-3.0-or-later

import {isAppNavigationPath} from '@app/features/platform/service_worker/WorkerNavigation';

export type WorkerFetchRoute = 'ignore' | 'metadata' | 'navigation';

export function isNavigationRequest(request: Request): boolean {
	if (request.mode === 'navigate') return true;
	const accept = request.headers.get('accept') ?? '';
	return request.destination === 'document' || accept.includes('text/html');
}

export function getWorkerFetchRoute(request: Request, workerOrigin: string): WorkerFetchRoute {
	if (request.method !== 'GET') {
		return 'ignore';
	}
	const url = new URL(request.url);
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
