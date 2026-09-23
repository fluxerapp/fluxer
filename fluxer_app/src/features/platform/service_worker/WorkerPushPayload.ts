// SPDX-License-Identifier: AGPL-3.0-or-later

export interface PushPayload {
	type?: string;
	action?: string;
	title?: string;
	body?: string;
	icon?: string;
	badge?: string;
	tag?: string;
	data?: Record<string, unknown>;
	notification?: PushPayload;
	web_push?: number;
}

export interface PushNotificationClientState {
	hasWindowClient: boolean;
	hasVisibleClient: boolean;
}

const isRecord = (value: unknown): value is Record<string, unknown> => typeof value === 'object' && value !== null;

export const normalizePushPayload = (raw: unknown): PushPayload => {
	const payload = (raw ?? {}) as PushPayload;
	if (payload.web_push === 8030 && payload.notification && typeof payload.notification === 'object') {
		const inner = payload.notification;
		const navigateUrl = (inner as Record<string, unknown>).navigate;
		const data: Record<string, unknown> = {...(inner.data ?? {})};
		if (typeof navigateUrl === 'string' && !data.url) data.url = navigateUrl;
		return {...inner, data};
	}
	return payload;
};
export const getBadgeCount = (payload: PushPayload): number | null => {
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
export const resolvePushNotificationTag = (payload: PushPayload): string | undefined => {
	const explicitTag = payload.tag ?? payload.data?.tag ?? payload.data?.notification_tag;
	if (typeof explicitTag === 'string' && explicitTag.length > 0) {
		return explicitTag;
	}
	const channelId = payload.data?.channel_id;
	if (typeof channelId === 'string' && channelId.length > 0) {
		return `channel:${channelId}`;
	}
	return undefined;
};
export const resolvePushMessageId = (payload: PushPayload): string | undefined => {
	const messageId = payload.data?.message_id;
	if (typeof messageId === 'string' && messageId.length > 0) {
		return messageId;
	}
	return undefined;
};
export const shouldRenotifyPushNotification = (
	messageId: string | undefined,
	shownWithSameTag: ReadonlyArray<{readonly data?: unknown}>,
): boolean =>
	messageId === undefined ||
	!shownWithSameTag.some((notification) => isRecord(notification.data) && notification.data.message_id === messageId);
export const resolvePushChannelId = (payload: PushPayload): string | undefined => {
	const channelId = payload.data?.channel_id;
	if (typeof channelId === 'string' && channelId.length > 0) {
		return channelId;
	}
	return undefined;
};
export const matchesPushChannelNotification = (notification: Notification, channelId: string): boolean => {
	if (channelId.length === 0) {
		return false;
	}
	const data = isRecord(notification.data) ? notification.data : undefined;
	const dataChannelId = data?.channel_id;
	if (typeof dataChannelId === 'string' && dataChannelId === channelId) {
		return true;
	}
	const channelTag = `channel:${channelId}`;
	const tag = notification.tag;
	if (tag === channelTag) {
		return true;
	}
	return typeof tag === 'string' && tag.startsWith(`${channelTag}:`);
};
export const getPushNotificationClientState = (
	clients: ReadonlyArray<{readonly visibilityState?: string}>,
): PushNotificationClientState => ({
	hasWindowClient: clients.length > 0,
	hasVisibleClient: clients.some((client) => client.visibilityState === 'visible'),
});
export const shouldSilenceNonMobilePushNotification = (clientState: PushNotificationClientState): boolean =>
	clientState.hasWindowClient;
export const isNotificationClearPayload = (payload: PushPayload): boolean => {
	const record = payload as Record<string, unknown>;
	const data = payload.data ?? {};
	return (
		record.type === 'notification_clear' ||
		record.action === 'clear_channel' ||
		data.type === 'notification_clear' ||
		data.action === 'clear_channel' ||
		payload.notification?.data?.type === 'notification_clear' ||
		payload.notification?.data?.action === 'clear_channel'
	);
};
