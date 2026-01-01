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

/// <reference lib="webworker" />

declare const self: ServiceWorkerGlobalScope &
	typeof globalThis & {
		skipWaiting(): void;
		__WB_MANIFEST: unknown;
	};

self.addEventListener('install', () => {
	self.skipWaiting();
});

self.addEventListener('activate', (event: ExtendableEvent) => {
	event.waitUntil(self.clients.claim());
});

self.addEventListener('message', (event: ExtendableMessageEvent) => {
	if (event.data?.type === 'SKIP_WAITING') {
		self.skipWaiting();
	} else if (event.data?.type === 'APP_UPDATE_BADGE') {
		const rawCount = event.data?.count;
		let badgeCount: number | null = null;
		if (typeof rawCount === 'number' && Number.isFinite(rawCount)) {
			badgeCount = rawCount;
		} else if (typeof rawCount === 'string' && rawCount.length > 0) {
			const parsed = Number(rawCount);
			badgeCount = Number.isFinite(parsed) ? parsed : null;
		}
		event.waitUntil(updateAppBadge(badgeCount));
	}
});

interface PushPayload {
	title?: string;
	body?: string;
	icon?: string;
	badge?: string;
	data?: Record<string, unknown>;
}

declare global {
	interface Navigator {
		setAppBadge?: (value?: number) => Promise<void>;
		clearAppBadge?: () => Promise<void>;
	}
}

const getBadgeCount = (payload: PushPayload): number | null => {
	const badgeValue = payload.data?.badge_count;
	if (typeof badgeValue === 'number' && Number.isFinite(badgeValue)) {
		return badgeValue;
	}
	if (typeof badgeValue === 'string' && badgeValue.length > 0) {
		const parsed = Number(badgeValue);
		return Number.isFinite(parsed) ? parsed : null;
	}
	return null;
};

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
		console.error('[SW] Failed to update app badge', error);
	}
};

const resolveTargetUrl = (url?: string): string | null => {
	if (!url) return null;
	if (url.startsWith('http://') || url.startsWith('https://')) {
		return url;
	}
	try {
		return new URL(url, self.location.origin).toString();
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
		console.error('[SW] Unable to broadcast to clients', error);
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
	for (const client of clientList) {
		if (client.url === targetUrl) {
			await client.focus();
			return;
		}
	}

	if (self.clients.openWindow) {
		await self.clients.openWindow(targetUrl);
	}
};

self.addEventListener('push', (event: PushEvent) => {
	const payload: PushPayload = event.data?.json?.() ?? {
		title: 'Fluxer',
	};

	const title = payload.title ?? 'Fluxer';
	const options: NotificationOptions = {
		body: payload.body ?? undefined,
		icon: payload.icon ?? undefined,
		badge: payload.badge ?? undefined,
		data: payload.data ?? undefined,
	};

	const badgeCount = getBadgeCount(payload);

	event.waitUntil(
		(async () => {
			await Promise.all([self.registration.showNotification(title, options), updateAppBadge(badgeCount)]);
		})(),
	);
});

self.addEventListener('notificationclick', (event: NotificationEvent) => {
	event.notification.close();

	const targetUrl = resolveTargetUrl(event.notification.data?.url as string | undefined);
	if (!targetUrl) return;

	const targetUserId = event.notification.data?.target_user_id as string | undefined;

	event.waitUntil(
		(async () => {
			await focusOrOpenClient(targetUrl, targetUserId);
		})(),
	);
});

self.addEventListener('pushsubscriptionchange', (event: ExtendableEvent) => {
	event.waitUntil(
		postMessageToClients({type: 'PUSH_SUBSCRIPTION_CHANGE'})
			.then(() => undefined)
			.catch(() => undefined),
	);
});
