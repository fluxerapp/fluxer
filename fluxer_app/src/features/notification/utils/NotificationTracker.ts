// SPDX-License-Identifier: AGPL-3.0-or-later

import * as NotificationUtils from '@app/features/notification/utils/NotificationUtils';

const MAX_PER_CHANNEL = 5;

export interface TrackedNotification {
	readonly browserNotification: Notification | null;
	readonly nativeId: string | null;
}

const trackedByChannel = new Map<string, Array<TrackedNotification>>();

function trackerKey(accountKey: string | null, channelId: string): string {
	return `${accountKey ?? ''}\u0000${channelId}`;
}

function closeTracked(notifications: ReadonlyArray<TrackedNotification>): void {
	for (const notification of notifications) {
		notification.browserNotification?.close();
	}
	const nativeIds = notifications.map((n) => n.nativeId).filter((id): id is string => id != null);
	NotificationUtils.closeNativeNotifications(nativeIds);
}

export function trackNotification(
	accountKey: string | null,
	channelId: string,
	notification: TrackedNotification,
): void {
	if (notification.browserNotification === null && notification.nativeId === null) return;
	const key = trackerKey(accountKey, channelId);
	const notifications = trackedByChannel.get(key) ?? [];
	notifications.push(notification);
	trackedByChannel.set(key, notifications);
	if (notifications.length > MAX_PER_CHANNEL) {
		closeTracked(notifications.splice(0, notifications.length - MAX_PER_CHANNEL));
	}
}

export function clearChannelNotifications(accountKey: string | null, channelId: string): void {
	const key = trackerKey(accountKey, channelId);
	const notifications = trackedByChannel.get(key);
	if (notifications === undefined) return;
	trackedByChannel.delete(key);
	closeTracked(notifications);
}
