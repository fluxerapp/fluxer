// SPDX-License-Identifier: AGPL-3.0-or-later

import Config from '@app/features/app/config/Config';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {getProtectedCacheStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {resolveDocumentURLFromRoot} from '@app/features/platform/URLOriginUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {hasUnavailableElectronNativeContext, isElectron} from '@app/features/ui/utils/NativeUtils';

const logger = new Logger('ServiceWorkerRegister');
const FLUXER_SERVICE_WORKER_CACHE_PREFIX = 'fluxer-';

function isNativeDesktopClient(): boolean {
	return isElectron() || hasUnavailableElectronNativeContext();
}

async function cleanupNativeDesktopPushState(reason: string): Promise<void> {
	try {
		const {cleanupNativeDesktopWebPushSubscriptions} = await import(
			'@app/features/platform/push/PushSubscriptionService'
		);
		await cleanupNativeDesktopWebPushSubscriptions(reason);
	} catch (error) {
		logger.error('Failed to clean up native desktop Web Push state before disabling service workers', error);
	}
}

async function clearFluxerServiceWorkerCaches(): Promise<Array<string>> {
	const browserCaches = getProtectedCacheStorage();
	if (!browserCaches) return [];
	const cacheNames = await browserCaches.keys();
	const deleted: Array<string> = [];
	await Promise.all(
		cacheNames.map(async (cacheName) => {
			if (!cacheName.startsWith(FLUXER_SERVICE_WORKER_CACHE_PREFIX)) return;
			if (await browserCaches.delete(cacheName)) {
				deleted.push(cacheName);
			}
		}),
	);
	return deleted;
}

export function shouldDisableServiceWorkersForLocalDev(): boolean {
	return (
		import.meta.env.MODE === 'development' || (Config.PUBLIC_BUILD_VERSION ?? 'dev').trim().toLowerCase() === 'dev'
	);
}

async function unregisterFluxerServiceWorkers({
	cleanupPushState,
	logContext,
	reason,
}: {
	cleanupPushState: boolean;
	logContext: string;
	reason: string;
}): Promise<void> {
	if (isDesktopLocalAppDocument()) return;
	if (!('serviceWorker' in navigator)) return;
	if (cleanupPushState) {
		await cleanupNativeDesktopPushState(reason);
	}
	try {
		const registrations = await navigator.serviceWorker.getRegistrations();
		const unregistered = await Promise.all(
			registrations.map(async (registration) => ({
				scope: registration.scope,
				ok: await registration.unregister(),
			})),
		);
		const deletedCaches = await clearFluxerServiceWorkerCaches();
		logger.debug(`Disabled service workers for ${logContext}`, {
			reason,
			registrations: unregistered,
			deletedCaches,
		});
	} catch (error) {
		logger.error(`Failed to unregister service workers for ${logContext}`, error);
	}
}

export async function unregisterServiceWorkersForNativeDesktop(reason = 'native-desktop'): Promise<void> {
	await unregisterFluxerServiceWorkers({
		cleanupPushState: true,
		logContext: 'native desktop client',
		reason,
	});
}

export async function registerServiceWorker(): Promise<ServiceWorkerRegistration | undefined> {
	if (isDesktopLocalAppDocument()) {
		return;
	}
	if (!('serviceWorker' in navigator)) {
		return;
	}
	if (isNativeDesktopClient()) {
		await unregisterServiceWorkersForNativeDesktop('native-desktop-service-worker-disabled');
		return;
	}
	if (shouldDisableServiceWorkersForLocalDev()) {
		await unregisterFluxerServiceWorkers({
			cleanupPushState: false,
			logContext: 'local development',
			reason: 'local-dev-service-worker-disabled',
		});
		return;
	}
	try {
		const versionParam = Config.PUBLIC_BUILD_VERSION || 'dev';
		const swUrl = resolveDocumentURLFromRoot('/sw.js');
		swUrl.searchParams.set('v', String(versionParam));
		return await navigator.serviceWorker.register(`${swUrl.pathname}${swUrl.search}`);
	} catch (error) {
		logger.error('Registration failed', error);
		return;
	}
}
