/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import {isInstalledPwa} from '~/utils/PwaUtils';

interface PushSubscriptionListResponse {
	subscriptions: Array<{subscription_id: string; user_agent: string | null}>;
}

const logger = new Logger('PushSubscriptionService');

let registerPromise: Promise<string | null> | null = null;
let unregisterPromise: Promise<void> | null = null;

const getPublicVapidKey = async (): Promise<string | null> => {
	await RuntimeConfigStore.waitForInit();
	return RuntimeConfigStore.publicPushVapidKey;
};

const isWebPushSupported = (): boolean => {
	return (
		isInstalledPwa() && 'serviceWorker' in navigator && 'PushManager' in window && typeof Notification !== 'undefined'
	);
};

const logWebPushUnavailable = (): void => {
	if (!isInstalledPwa()) {
		logger.debug('Skipping push handling because the app is not running as an installed PWA');
	} else {
		logger.debug('Web push not supported in this environment');
	}
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

const getServiceWorkerRegistration = async (): Promise<ServiceWorkerRegistration | undefined> => {
	if (!isWebPushSupported()) {
		return undefined;
	}

	try {
		return await navigator.serviceWorker.ready;
	} catch (error) {
		logger.error('Failed to get service worker registration', {error});
		return undefined;
	}
};

export async function registerPushSubscription(): Promise<string | null> {
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
			const subscription =
				existingSubscription ??
				(await registration.pushManager.subscribe({
					userVisibleOnly: true,
					applicationServerKey,
				}));

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

			const response = await http.post<{subscription_id: string}>({
				url: Endpoints.USER_PUSH_SUBSCRIBE,
				body: {
					endpoint: subscription.endpoint,
					keys: {
						p256dh,
						auth,
					},
					user_agent: navigator.userAgent,
				},
			});

			logger.info('Registered push subscription', {subscriptionId: response.body.subscription_id});
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
	if (!isWebPushSupported()) {
		logWebPushUnavailable();
		return;
	}

	if (unregisterPromise) return unregisterPromise;

	const promise = (async () => {
		try {
			const response = await http.get<PushSubscriptionListResponse>({
				url: Endpoints.USER_PUSH_SUBSCRIPTIONS,
			});

			const subscriptions = response.body.subscriptions ?? [];

			await Promise.all(
				subscriptions.map(async (subscription) => {
					try {
						await http.delete({
							url: Endpoints.USER_PUSH_SUBSCRIPTION(subscription.subscription_id),
						});
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
		} catch (error) {
			logger.error('Failed to unregister push subscriptions', {error});
		} finally {
			unregisterPromise = null;
		}
	})();

	unregisterPromise = promise;
	return promise;
}
