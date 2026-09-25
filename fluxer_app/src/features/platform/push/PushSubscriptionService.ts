// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {detectDomainMigrationInstallKind} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {hasUnavailableElectronNativeContext, isElectron} from '@app/features/ui/utils/NativeUtils';
import {isInstalledPwa} from '@app/features/ui/utils/PwaUtils';

interface PushSubscriptionListResponse {
	subscriptions: Array<{
		subscription_id: string;
		user_agent: string | null;
	}>;
}

const logger = new Logger('PushSubscriptionService');
const LAST_PUSH_ENDPOINT_KEY = 'fluxer.lastPushEndpoint';
const readLastEndpoint = (): string | null => {
	try {
		return AppStorage.getItem(LAST_PUSH_ENDPOINT_KEY);
	} catch {
		return null;
	}
};
const writeLastEndpoint = (endpoint: string | null): void => {
	try {
		if (endpoint) AppStorage.setItem(LAST_PUSH_ENDPOINT_KEY, endpoint);
		else AppStorage.removeItem(LAST_PUSH_ENDPOINT_KEY);
	} catch {}
};

let registerPromise: Promise<string | null> | null = null;
let unregisterPromise: Promise<void> | null = null;
let nativeDesktopCleanupPromise: Promise<void> | null = null;

export function isWebPushBlockedForNativeDesktop(): boolean {
	return isElectron() || hasUnavailableElectronNativeContext();
}

const getPublicVapidKey = async (): Promise<string | null> => {
	await RuntimeConfig.waitForInit();
	return RuntimeConfig.publicPushVapidKey;
};
const isWebPushSupported = (): boolean => {
	return 'serviceWorker' in navigator && 'PushManager' in window && typeof Notification !== 'undefined';
};
const logWebPushUnavailable = (): void => {
	logger.debug('Web push not supported in this environment', {
		installedPwa: isInstalledPwa(),
		nativeDesktop: isWebPushBlockedForNativeDesktop(),
	});
};
const arrayBufferToBase64Url = (buffer: ArrayBuffer | null): string | null => {
	if (!buffer) return null;
	const bytes = new Uint8Array(buffer);
	let binary = '';
	for (let i = 0; i < bytes.length; i += 1) {
		binary += String.fromCharCode(bytes[i]);
	}
	const base64 = btoa(binary);
	return base64.replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
};
const buffersEqual = (a: ArrayBuffer | Uint8Array, b: ArrayBuffer | Uint8Array): boolean => {
	const viewA = a instanceof Uint8Array ? a : new Uint8Array(a);
	const viewB = b instanceof Uint8Array ? b : new Uint8Array(b);
	if (viewA.byteLength !== viewB.byteLength) return false;
	for (let i = 0; i < viewA.byteLength; i += 1) {
		if (viewA[i] !== viewB[i]) return false;
	}
	return true;
};
const urlBase64ToUint8Array = (base64String: string): Uint8Array => {
	const padding = '='.repeat((4 - (base64String.length % 4)) % 4);
	const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/');
	const rawData = atob(base64);
	const outputArray = new Uint8Array(rawData.length);
	for (let i = 0; i < rawData.length; i += 1) {
		outputArray[i] = rawData.charCodeAt(i);
	}
	return outputArray;
};
const getServiceWorkerRegistration = async ({
	waitUntilReady = true,
}: {
	waitUntilReady?: boolean;
} = {}): Promise<ServiceWorkerRegistration | undefined> => {
	if (!isWebPushSupported()) {
		return undefined;
	}
	try {
		const existingRegistration = await navigator.serviceWorker.getRegistration();
		if (existingRegistration) return existingRegistration;
		if (!waitUntilReady) return undefined;
		return await navigator.serviceWorker.ready;
	} catch (error) {
		logger.error('Failed to get service worker registration', {error});
		return undefined;
	}
};

async function getLocalBrowserPushSubscription(reason: string): Promise<PushSubscription | null> {
	const registration = await getServiceWorkerRegistration({waitUntilReady: false});
	if (!registration) return null;
	try {
		return await registration.pushManager.getSubscription();
	} catch (error) {
		logger.error('Failed to read local Web Push registration for native desktop client', {reason, error});
		return null;
	}
}

async function unsubscribeLocalBrowserPushRegistration(
	reason: string,
	subscription?: PushSubscription | null,
): Promise<string | null> {
	const existingSubscription =
		subscription === undefined ? await getLocalBrowserPushSubscription(reason) : subscription;
	if (!existingSubscription) return null;
	const endpoint = existingSubscription.endpoint;
	try {
		await existingSubscription.unsubscribe();
		writeLastEndpoint(null);
		logger.warn('Unsubscribed local Web Push registration because this client uses native desktop notifications', {
			reason,
			endpoint,
		});
		return endpoint;
	} catch (error) {
		logger.error('Failed to unsubscribe local Web Push registration for native desktop client', {reason, error});
		return endpoint;
	}
}

export async function cleanupNativeDesktopWebPushSubscriptions(reason = 'native-desktop'): Promise<void> {
	if (!isWebPushBlockedForNativeDesktop()) return;
	if (nativeDesktopCleanupPromise) return nativeDesktopCleanupPromise;
	const promise = (async () => {
		try {
			const existingSubscription = await getLocalBrowserPushSubscription(reason);
			const lastEndpoint = readLastEndpoint();
			const endpointToDelete = existingSubscription?.endpoint ?? lastEndpoint;
			if (endpointToDelete) {
				const localSubscriptionId = await createWebPushSubscriptionId(endpointToDelete);
				try {
					await http.delete(Endpoints.USER_PUSH_SUBSCRIPTION(localSubscriptionId));
					logger.warn('Deleted legacy desktop Web Push subscription on backend', {
						reason,
						subscriptionId: localSubscriptionId,
					});
				} catch (error) {
					logger.warn('Failed to delete legacy desktop Web Push subscription on backend', {
						reason,
						subscriptionId: localSubscriptionId,
						error,
					});
				}
			}
			await unsubscribeLocalBrowserPushRegistration(reason, existingSubscription);
			writeLastEndpoint(null);
		} finally {
			nativeDesktopCleanupPromise = null;
		}
	})();
	nativeDesktopCleanupPromise = promise;
	return promise;
}

async function createWebPushSubscriptionId(endpoint: string): Promise<string> {
	const encoder = new TextEncoder();
	const digest = await crypto.subtle.digest('SHA-256', encoder.encode(endpoint));
	const bytes = new Uint8Array(digest);
	let hex = '';
	for (const byte of bytes) {
		hex += byte.toString(16).padStart(2, '0');
	}
	return hex.slice(0, 32);
}

export async function registerPushSubscription(): Promise<string | null> {
	if (isWebPushBlockedForNativeDesktop()) {
		logger.error('Blocked Web Push registration in native desktop client; native notifications must be used', {
			installedPwa: isInstalledPwa(),
			userAgent: navigator.userAgent,
		});
		await cleanupNativeDesktopWebPushSubscriptions('register-blocked-native-desktop');
		return null;
	}
	if (!isWebPushSupported()) {
		logWebPushUnavailable();
		return null;
	}
	let publicVapidKey: string | null;
	try {
		publicVapidKey = await getPublicVapidKey();
	} catch (error) {
		logger.error('Failed to resolve runtime configuration before push registration', {error});
		return null;
	}
	if (!publicVapidKey) {
		logger.debug('VAPID public key is not configured');
		return null;
	}
	if (Notification.permission !== 'granted') {
		logger.debug('Notification permission is not granted; skipping push subscription');
		return null;
	}
	if (registerPromise) return registerPromise;
	const promise = (async () => {
		try {
			const registration = await getServiceWorkerRegistration();
			if (!registration) {
				logger.debug('No active service worker registration');
				return null;
			}
			const existingSubscription = await registration.pushManager.getSubscription();
			const applicationServerKey = urlBase64ToUint8Array(publicVapidKey) as BufferSource;
			const existingKey = existingSubscription?.options?.applicationServerKey ?? null;
			const expectedKeyBytes = urlBase64ToUint8Array(publicVapidKey);
			const keyMatches = existingKey ? buffersEqual(existingKey, expectedKeyBytes) : false;
			if (existingSubscription && !keyMatches) {
				logger.info('VAPID key mismatch; resubscribing push');
				try {
					await existingSubscription.unsubscribe();
				} catch (error) {
					logger.warn('Failed to unsubscribe stale push subscription', {error});
				}
			}
			const subscription =
				existingSubscription && keyMatches
					? existingSubscription
					: await registration.pushManager.subscribe({
							userVisibleOnly: true,
							applicationServerKey,
						});
			const p256dh = arrayBufferToBase64Url(subscription.getKey('p256dh'));
			const auth = arrayBufferToBase64Url(subscription.getKey('auth'));
			if (!subscription.endpoint || !p256dh || !auth) {
				logger.error('Push subscription did not contain expected data', {
					endpoint: subscription.endpoint,
					p256dh,
					auth,
				});
				return null;
			}
			const installedApp = detectDomainMigrationInstallKind() !== 'none';
			const lastEndpoint = readLastEndpoint();
			const isRotation = lastEndpoint !== null && lastEndpoint !== subscription.endpoint;
			const response = isRotation
				? await http.post<{
						subscription_id: string;
					}>(Endpoints.USER_PUSH_ROTATE, {
						body: {
							old_endpoint: lastEndpoint,
							endpoint: subscription.endpoint,
							keys: {p256dh, auth},
							user_agent: navigator.userAgent,
							installed_app: installedApp,
						},
					})
				: await http.post<{
						subscription_id: string;
					}>(Endpoints.USER_PUSH_SUBSCRIBE, {
						body: {
							endpoint: subscription.endpoint,
							keys: {p256dh, auth},
							user_agent: navigator.userAgent,
							installed_app: installedApp,
						},
					});
			writeLastEndpoint(subscription.endpoint);
			logger.info('Registered push subscription', {
				subscriptionId: response.body.subscription_id,
				rotated: isRotation,
			});
			return response.body.subscription_id;
		} catch (error) {
			logger.error('Failed to register push subscription', {error});
			return null;
		} finally {
			registerPromise = null;
		}
	})();
	registerPromise = promise;
	return promise;
}

export async function unregisterAllPushSubscriptions(): Promise<void> {
	if (isWebPushBlockedForNativeDesktop()) {
		await cleanupNativeDesktopWebPushSubscriptions('unregister-all-blocked-native-desktop');
		return;
	}
	if (!isWebPushSupported()) {
		logWebPushUnavailable();
		return;
	}
	if (unregisterPromise) return unregisterPromise;
	const promise = (async () => {
		try {
			const response = await http.get<PushSubscriptionListResponse>(Endpoints.USER_PUSH_SUBSCRIPTIONS);
			const subscriptions = response.body.subscriptions ?? [];
			await Promise.all(
				subscriptions.map(async (subscription) => {
					try {
						await http.delete(Endpoints.USER_PUSH_SUBSCRIPTION(subscription.subscription_id));
					} catch (error) {
						logger.warn('Failed to delete push subscription on backend', {
							subscriptionId: subscription.subscription_id,
							error,
						});
					}
				}),
			);
			const registration = await getServiceWorkerRegistration();
			if (!registration) return;
			const existingSubscription = await registration.pushManager.getSubscription();
			if (existingSubscription) {
				await existingSubscription.unsubscribe();
			}
			writeLastEndpoint(null);
		} catch (error) {
			logger.error('Failed to unregister push subscriptions', {error});
		} finally {
			unregisterPromise = null;
		}
	})();
	unregisterPromise = promise;
	return promise;
}
