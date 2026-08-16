// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/ProductConstants';
import {
	getNotificationAlertOptions,
	isMobileOrTabletUserAgent,
} from '@app/features/platform/notifications/NotificationAlertOptions';
import {
	type AppShellRuntime,
	fetchAppShellNavigation,
	isCacheableResponse,
	type PrecacheEntry,
	precacheAssets,
	seedAppShell,
} from '@app/features/platform/service_worker/WorkerAppShell';
import {shouldDeleteWorkerCache, WORKER_CACHE_PREFIX} from '@app/features/platform/service_worker/WorkerCacheCleanup';
import {getWorkerFetchRoute} from '@app/features/platform/service_worker/WorkerFetchRouting';
import {
	getBadgeCount,
	getPushNotificationClientState,
	isNotificationClearPayload,
	matchesPushChannelNotification,
	normalizePushPayload,
	resolvePushChannelId,
	resolvePushNotificationTag,
	shouldSilenceNonMobilePushNotification,
} from '@app/features/platform/service_worker/WorkerPushPayload';

type SwLogLevel = 'debug' | 'info' | 'warn' | 'error';

declare const self: ServiceWorkerGlobalScope &
	typeof globalThis & {
		skipWaiting(): void;
		__WB_MANIFEST: unknown;
		console: Console;
	};

declare const __FLUXER_PRECACHE_MANIFEST__: ReadonlyArray<PrecacheEntry>;
declare const __FLUXER_SW_VERSION__: string;
const workerNavigator = self.navigator as {readonly userAgent: string; readonly maxTouchPoints?: number};
const ensureServiceWorkerReady: Promise<void> = Promise.resolve();
const SERVICE_WORKER_VERSION = typeof __FLUXER_SW_VERSION__ === 'string' ? __FLUXER_SW_VERSION__ : 'dev';
const PRECACHE_MANIFEST = typeof __FLUXER_PRECACHE_MANIFEST__ === 'undefined' ? [] : __FLUXER_PRECACHE_MANIFEST__;
const PRECACHE_CACHE = `${WORKER_CACHE_PREFIX}-precache-${SERVICE_WORKER_VERSION}`;
const ASSET_CACHE = `${WORKER_CACHE_PREFIX}-assets-${SERVICE_WORKER_VERSION}`;
const NAVIGATION_CACHE = `${WORKER_CACHE_PREFIX}-navigation-${SERVICE_WORKER_VERSION}`;
const EXPECTED_CACHES = new Set([PRECACHE_CACHE, ASSET_CACHE, NAVIGATION_CACHE]);
const NAVIGATION_NETWORK_TIMEOUT_MS = 650;
const serviceWorkerCaches = self.caches;
const isNativeDesktopUserAgent = (userAgent: string): boolean => /\bElectron\/\d+(?:\.\d+)*/.test(userAgent);

try {
	delete (self as Partial<ServiceWorkerGlobalScope> & {caches?: CacheStorage}).caches;
} catch {}

const log = async (level: SwLogLevel, message: string, data?: unknown): Promise<void> => {
	try {
		if (typeof console !== 'undefined') {
			const fn =
				level === 'error'
					? console.error
					: level === 'warn'
						? console.warn
						: level === 'info'
							? console.info
							: console.debug;
			if (data !== undefined) fn?.call(console, '[SW]', message, data);
			else fn?.call(console, '[SW]', message);
		}
	} catch {}
};
const appShellRuntime: AppShellRuntime = {
	caches: serviceWorkerCaches,
	fetch: (request) => fetch(request),
	origin: self.location.origin,
	precacheName: PRECACHE_CACHE,
	navigationCacheName: NAVIGATION_CACHE,
	networkTimeoutMs: NAVIGATION_NETWORK_TIMEOUT_MS,
	onCacheWriteError: (error) => {
		void log('warn', 'app shell cache put failed', {error: describeError(error)});
	},
};
const pruneCacheEntries = async (cache: Cache, maxEntries: number): Promise<void> => {
	const keys = await cache.keys();
	const overflow = keys.length - maxEntries;
	if (overflow <= 0) {
		return;
	}
	await Promise.all(keys.slice(0, overflow).map((request) => cache.delete(request)));
};
const cacheRequest = async (
	cacheName: string,
	request: Request | string,
	response: Response,
	maxEntries?: number,
): Promise<void> => {
	if (!isCacheableResponse(response)) {
		return;
	}
	try {
		if (!serviceWorkerCaches) {
			return;
		}
		const cache = await serviceWorkerCaches.open(cacheName);
		await cache.put(request, response.clone());
		if (maxEntries != null) {
			await pruneCacheEntries(cache, maxEntries);
		}
	} catch (error) {
		await log('warn', 'cache put failed', {cacheName, error: describeError(error)});
	}
};
const cleanupOldCaches = async (): Promise<void> => {
	if (!serviceWorkerCaches) {
		return;
	}
	const names = await serviceWorkerCaches.keys();
	await Promise.all(
		names.map((name) => {
			if (!shouldDeleteWorkerCache(name, EXPECTED_CACHES)) {
				return undefined;
			}
			return serviceWorkerCaches.delete(name);
		}),
	);
};
const fetchCacheFirst = async (request: Request): Promise<Response> => {
	const cached = await serviceWorkerCaches?.match(request);
	if (cached) {
		return cached;
	}
	const response = await fetch(request);
	await cacheRequest(ASSET_CACHE, request, response);
	return response;
};
const fetchNetworkFirst = async (request: Request): Promise<Response> => {
	try {
		const response = await fetch(request);
		await cacheRequest(PRECACHE_CACHE, request, response);
		return response;
	} catch (error) {
		const cached = await serviceWorkerCaches?.match(request);
		if (cached) {
			return cached;
		}
		throw error;
	}
};

self.addEventListener('install', (event: ExtendableEvent) => {
	event.waitUntil(
		(async () => {
			await ensureServiceWorkerReady;
			await Promise.allSettled([precacheAssets(appShellRuntime, PRECACHE_MANIFEST), seedAppShell(appShellRuntime)]);
			await log('info', 'install');
			self.skipWaiting();
		})(),
	);
});

self.addEventListener('activate', (event: ExtendableEvent) => {
	event.waitUntil(
		(async () => {
			await ensureServiceWorkerReady;
			await log('info', 'activate');
			await cleanupOldCaches();
			await self.clients.claim();
			await log('debug', 'clients claimed');
		})(),
	);
});

self.addEventListener('fetch', (event: FetchEvent) => {
	const request = event.request;
	const route = getWorkerFetchRoute(request, self.location.origin);
	if (route === 'navigation') {
		event.respondWith(fetchAppShellNavigation(appShellRuntime, request));
		return;
	}
	if (route === 'static-asset') {
		event.respondWith(fetchCacheFirst(request));
		return;
	}
	if (route === 'metadata') {
		event.respondWith(fetchNetworkFirst(request));
	}
});

self.addEventListener('message', (event: ExtendableMessageEvent) => {
	const data = isRecord(event.data) ? event.data : {};
	const type = data?.type;
	if (type === 'SKIP_WAITING') {
		event.waitUntil(log('info', 'message: SKIP_WAITING'));
		self.skipWaiting();
		return;
	}
	if (type === 'APP_UPDATE_BADGE') {
		const rawCount = data?.count;
		let badgeCount: number | null = null;
		if (typeof rawCount === 'number' && Number.isFinite(rawCount)) {
			badgeCount = rawCount;
		} else if (typeof rawCount === 'string' && rawCount.length > 0) {
			const parsed = Number(rawCount);
			badgeCount = Number.isFinite(parsed) ? parsed : null;
		}
		event.waitUntil(updateAppBadge(badgeCount));
		return;
	}
	event.waitUntil(log('debug', 'message: unknown', {type}));
});

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null;
}

const updateAppBadge = async (count: number | null): Promise<void> => {
	if (typeof navigator.setAppBadge !== 'function' && typeof navigator.clearAppBadge !== 'function') {
		return;
	}
	try {
		if (count !== null && count > 0) {
			if (typeof navigator.setAppBadge === 'function') {
				await navigator.setAppBadge(count);
			}
		} else if (typeof navigator.clearAppBadge === 'function') {
			await navigator.clearAppBadge();
		}
	} catch (error) {
		await log('error', 'Failed to update app badge', {error: describeError(error)});
	}
};
const closePushNotifications = async (tag: string | undefined): Promise<number> => {
	if (!tag || typeof self.registration.getNotifications !== 'function') {
		return 0;
	}
	try {
		const notifications = await self.registration.getNotifications({tag});
		for (const notification of notifications) {
			notification.close();
		}
		return notifications.length;
	} catch (error) {
		await log('error', 'push: failed to close notifications', {tag, error: describeError(error)});
		return 0;
	}
};
const closePushNotificationsForChannel = async (channelId: string): Promise<number> => {
	if (typeof self.registration.getNotifications !== 'function') {
		return 0;
	}
	try {
		const notifications = await self.registration.getNotifications();
		let closedCount = 0;
		for (const notification of notifications) {
			if (matchesPushChannelNotification(notification, channelId)) {
				notification.close();
				closedCount++;
			}
		}
		return closedCount;
	} catch (error) {
		await log('error', 'push: failed to close channel notifications', {
			channelId,
			error: describeError(error),
		});
		return 0;
	}
};
const resolveTargetUrl = (url?: string): string | null => {
	if (!url) return null;
	try {
		const resolved = new URL(url, self.location.origin);
		if (resolved.protocol !== 'http:' && resolved.protocol !== 'https:') return null;
		return resolved.toString();
	} catch {
		return null;
	}
};
const postMessageToClients = async (message: Record<string, unknown>): Promise<ReadonlyArray<WindowClient>> => {
	try {
		const clientList = (await self.clients.matchAll({
			type: 'window',
			includeUncontrolled: true,
		})) as ReadonlyArray<WindowClient>;
		for (const client of clientList) {
			client.postMessage(message);
		}
		return clientList;
	} catch (error) {
		await log('error', 'Unable to broadcast to clients', {error: describeError(error)});
		return [];
	}
};
const focusOrOpenClient = async (targetUrl: string, targetUserId?: string): Promise<void> => {
	const message: Record<string, unknown> = {
		type: 'NOTIFICATION_CLICK_NAVIGATE',
		url: targetUrl,
	};
	if (targetUserId) {
		message.targetUserId = targetUserId;
	}
	const clientList = await postMessageToClients(message);
	const exact = clientList.find((c) => c.url === targetUrl);
	if (exact) {
		await log('debug', 'focus exact client', {url: targetUrl});
		await exact.focus();
		return;
	}
	const sameOrigin = clientList.find((c) => {
		try {
			return new URL(c.url).origin === self.location.origin;
		} catch {
			return false;
		}
	});
	if (sameOrigin) {
		await log('debug', 'focus same-origin client', {url: sameOrigin.url});
		await sameOrigin.focus();
		return;
	}
	if (self.clients.openWindow) {
		await log('debug', 'openWindow', {url: targetUrl});
		await self.clients.openWindow(targetUrl);
	}
};

function describeError(error: unknown): Record<string, unknown> {
	if (error instanceof Error) {
		return {name: error.name, message: error.message, stack: error.stack};
	}
	return {value: String(error)};
}

self.addEventListener('push', (event: PushEvent) => {
	event.waitUntil(
		(async () => {
			await ensureServiceWorkerReady;
			let raw: unknown;
			let rawText: string | null = null;
			try {
				rawText = event.data?.text() ?? null;
			} catch {
				rawText = null;
			}
			try {
				raw = event.data?.json?.();
			} catch (error) {
				await log('error', 'push: JSON parse failed', {
					error: describeError(error),
					rawPreview: rawText?.slice(0, 512) ?? null,
				});
			}
			const payload = normalizePushPayload(raw ?? {title: PRODUCT_NAME});
			const title = payload.title ?? PRODUCT_NAME;
			const tag = resolvePushNotificationTag(payload);
			const badgeCount = getBadgeCount(payload);
			if (isNotificationClearPayload(payload)) {
				const channelId = resolvePushChannelId(payload);
				const closedCount = channelId
					? await closePushNotificationsForChannel(channelId)
					: await closePushNotifications(tag);
				await updateAppBadge(badgeCount);
				await log('info', 'push clear received', {tag, channelId, closedCount, badgeCount});
				return;
			}
			if (isNativeDesktopUserAgent(workerNavigator.userAgent)) {
				await updateAppBadge(badgeCount);
				await log('warn', 'push ignored in native desktop service worker', {badgeCount});
				return;
			}
			const body = payload.body ?? '';
			let clientState = getPushNotificationClientState([]);
			try {
				const clientList = (await self.clients.matchAll({
					type: 'window',
					includeUncontrolled: true,
				})) as ReadonlyArray<WindowClient>;
				clientState = getPushNotificationClientState(clientList);
			} catch {}
			const options: NotificationOptions & {
				renotify?: boolean;
			} = {
				body,
				icon: payload.icon ?? undefined,
				badge: payload.badge ?? undefined,
				data: payload.data ?? undefined,
				tag,
				renotify: tag !== undefined,
				...getNotificationAlertOptions({
					mobileOrTablet: isMobileOrTabletUserAgent(workerNavigator.userAgent, workerNavigator.maxTouchPoints ?? 0),
					silentOnNonMobile: shouldSilenceNonMobilePushNotification(clientState),
				}),
			};
			await log('info', 'push received', {
				title,
				hasBody: Boolean(payload.body),
				tag,
				hasData: payload.data !== undefined,
				badgeCount,
				hasWindowClient: clientState.hasWindowClient,
				hasVisibleClient: clientState.hasVisibleClient,
				webPush: payload.web_push ?? null,
			});
			try {
				await Promise.all([self.registration.showNotification(title, options), updateAppBadge(badgeCount)]);
			} catch (error) {
				await log('error', 'push: showNotification failed, showing fallback', {error: describeError(error)});
				try {
					await self.registration.showNotification(PRODUCT_NAME, {body: ''});
				} catch (fallbackError) {
					await log('error', 'push: fallback showNotification also failed', {
						error: describeError(fallbackError),
					});
				}
			}
		})(),
	);
});

self.addEventListener('notificationclick', (event: NotificationEvent) => {
	event.notification.close();
	const targetUrl = resolveTargetUrl(event.notification.data?.url as string | undefined);
	const targetUserId = event.notification.data?.target_user_id as string | undefined;
	event.waitUntil(
		(async () => {
			await ensureServiceWorkerReady;
			await log('info', 'notificationclick', {targetUrl, targetUserId, tag: event.notification.tag});
			if (!targetUrl) return;
			try {
				await focusOrOpenClient(targetUrl, targetUserId);
			} catch (error) {
				await log('error', 'notificationclick: focusOrOpen failed', {error: describeError(error)});
			}
		})(),
	);
});

self.addEventListener('notificationclose', (event: NotificationEvent) => {
	event.waitUntil(log('debug', 'notificationclose', {tag: event.notification.tag}));
});

interface PushSubscriptionChangeEvent extends ExtendableEvent {
	oldSubscription?: PushSubscription | null;
	newSubscription?: PushSubscription | null;
}

const arrayBufferToBase64Url = (buffer: ArrayBuffer | null): string | null => {
	if (!buffer) return null;
	const bytes = new Uint8Array(buffer);
	let binary = '';
	for (let i = 0; i < bytes.length; i += 1) binary += String.fromCharCode(bytes[i]);
	return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
};
const urlBase64ToUint8Array = (base64: string): Uint8Array => {
	const padding = '='.repeat((4 - (base64.length % 4)) % 4);
	const raw = atob((base64 + padding).replace(/-/g, '+').replace(/_/g, '/'));
	const out = new Uint8Array(raw.length);
	for (let i = 0; i < raw.length; i += 1) out[i] = raw.charCodeAt(i);
	return out;
};
const fetchInstanceConfig = async (): Promise<{
	apiClient: string;
	vapidKey: string | null;
} | null> => {
	try {
		const res = await fetch('/.well-known/fluxer', {credentials: 'include'});
		if (!res.ok) return null;
		const data = (await res.json()) as {
			endpoints?: {
				api_client?: string;
				api?: string;
			};
			push?: {
				public_vapid_key?: string | null;
			};
		};
		const apiClient = data.endpoints?.api_client ?? data.endpoints?.api;
		if (!apiClient) return null;
		return {apiClient, vapidKey: data.push?.public_vapid_key ?? null};
	} catch (error) {
		await log('error', 'Failed to fetch instance config in SW', {error: describeError(error)});
		return null;
	}
};
const SW_API_VERSION = 1;
const rotateSubscriptionOnServer = async (
	oldEndpoint: string,
	newSub: PushSubscription,
	apiClient: string,
): Promise<void> => {
	const p256dh = arrayBufferToBase64Url(newSub.getKey('p256dh'));
	const auth = arrayBufferToBase64Url(newSub.getKey('auth'));
	if (!p256dh || !auth) {
		await log('error', 'rotate: new subscription missing keys');
		return;
	}
	const url = `${apiClient}/v${SW_API_VERSION}/users/@me/push/rotate`;
	try {
		const res = await fetch(url, {
			method: 'POST',
			credentials: 'include',
			headers: {'Content-Type': 'application/json'},
			body: JSON.stringify({
				old_endpoint: oldEndpoint,
				endpoint: newSub.endpoint,
				keys: {p256dh, auth},
				user_agent: self.navigator.userAgent,
			}),
		});
		await log(res.ok ? 'info' : 'error', 'rotate: server response', {status: res.status, ok: res.ok});
	} catch (error) {
		await log('error', 'rotate: fetch failed', {error: describeError(error)});
	}
};

self.addEventListener('pushsubscriptionchange', (event: PushSubscriptionChangeEvent) => {
	event.waitUntil(
		(async () => {
			await ensureServiceWorkerReady;
			await log('info', 'pushsubscriptionchange', {
				hasOld: Boolean(event.oldSubscription),
				hasNew: Boolean(event.newSubscription),
				oldEndpoint: event.oldSubscription?.endpoint ?? null,
				newEndpoint: event.newSubscription?.endpoint ?? null,
			});
			try {
				const oldEndpoint = event.oldSubscription?.endpoint ?? null;
				let appServerKey = event.oldSubscription?.options?.applicationServerKey ?? null;
				const config = await fetchInstanceConfig();
				if (!appServerKey && config?.vapidKey) {
					appServerKey = urlBase64ToUint8Array(config.vapidKey).buffer as ArrayBuffer;
				}
				if (!appServerKey) {
					await log('error', 'pushsubscriptionchange: no VAPID key available');
					await postMessageToClients({type: 'PUSH_SUBSCRIPTION_CHANGE'});
					return;
				}
				const newSub =
					event.newSubscription ??
					(await self.registration.pushManager.subscribe({
						userVisibleOnly: true,
						applicationServerKey: appServerKey,
					}));
				if (oldEndpoint && config) {
					await rotateSubscriptionOnServer(oldEndpoint, newSub, config.apiClient);
				} else {
					await log('warn', 'pushsubscriptionchange: unable to rotate on server', {
						hasOldEndpoint: Boolean(oldEndpoint),
						hasConfig: Boolean(config),
					});
				}
				await postMessageToClients({type: 'PUSH_SUBSCRIPTION_CHANGE'});
			} catch (error) {
				await log('error', 'pushsubscriptionchange handler failed', {error: describeError(error)});
				await postMessageToClients({type: 'PUSH_SUBSCRIPTION_CHANGE'}).catch(() => undefined);
			}
		})(),
	);
});
