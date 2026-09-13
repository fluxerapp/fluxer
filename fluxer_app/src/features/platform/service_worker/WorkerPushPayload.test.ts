// SPDX-License-Identifier: AGPL-3.0-or-later

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
import {describe, expect, it} from 'vitest';

describe('WorkerPushPayload', () => {
	it('normalizes encrypted Web Push payloads without losing navigation data', () => {
		const payload = normalizePushPayload({
			web_push: 8030,
			notification: {
				title: 'New message',
				tag: 'channel:123',
				navigate: '/channels/@me/123/456',
				data: {
					channel_id: '123',
				},
			},
		});
		expect(payload.title).toBe('New message');
		expect(payload.tag).toBe('channel:123');
		expect(payload.data).toMatchObject({
			channel_id: '123',
			url: '/channels/@me/123/456',
		});
	});
	it('detects clear packets from the root payload and data payload', () => {
		expect(
			isNotificationClearPayload({
				type: 'notification_clear',
				data: {
					channel_id: '123',
				},
			}),
		).toBe(true);
		expect(
			isNotificationClearPayload({
				data: {
					action: 'clear_channel',
				},
			}),
		).toBe(true);
		expect(isNotificationClearPayload({title: 'Visible notification'})).toBe(false);
	});
	it('resolves notification tags from explicit tags, notification tags, and channel ids', () => {
		expect(resolvePushNotificationTag({tag: 'channel:from-root'})).toBe('channel:from-root');
		expect(resolvePushNotificationTag({data: {tag: 'channel:123:456'}})).toBe('channel:123:456');
		expect(resolvePushNotificationTag({data: {notification_tag: 'channel:from-data'}})).toBe('channel:from-data');
		expect(resolvePushNotificationTag({data: {channel_id: '987'}})).toBe('channel:987');
		expect(resolvePushNotificationTag({data: {channel_id: ''}})).toBeUndefined();
	});
	it('resolves channel ids and matches channel notifications for clear-on-read', () => {
		expect(resolvePushChannelId({data: {channel_id: '123'}})).toBe('123');
		expect(resolvePushChannelId({})).toBeUndefined();
		const channelNotification = {
			tag: 'channel:123:456',
			data: {channel_id: '123'},
		} as Notification;
		const legacyChannelNotification = {tag: 'channel:123'} as Notification;
		const otherChannelNotification = {
			tag: 'channel:999:456',
			data: {channel_id: '999'},
		} as Notification;
		const samePrefixOtherChannelNotification = {tag: 'channel:1234:456'} as Notification;
		const malformedDataNotification = {tag: 'channel:123:789', data: 'not-record'} as unknown as Notification;
		expect(matchesPushChannelNotification(channelNotification, '123')).toBe(true);
		expect(matchesPushChannelNotification(legacyChannelNotification, '123')).toBe(true);
		expect(matchesPushChannelNotification(otherChannelNotification, '123')).toBe(false);
		expect(matchesPushChannelNotification(samePrefixOtherChannelNotification, '123')).toBe(false);
		expect(matchesPushChannelNotification(malformedDataNotification, '123')).toBe(true);
		expect(matchesPushChannelNotification(channelNotification, '')).toBe(false);
	});
	it('parses badge counts from numeric and string data values', () => {
		expect(getBadgeCount({data: {badge_count: 5}})).toBe(5);
		expect(getBadgeCount({data: {badge_count: '0'}})).toBe(0);
		expect(getBadgeCount({data: {badge_count: 'not-a-number'}})).toBeNull();
		expect(getBadgeCount({})).toBeNull();
	});
	it('silences non-mobile push notifications when any app client is open', () => {
		const hiddenClientState = getPushNotificationClientState([{visibilityState: 'hidden'}]);
		expect(hiddenClientState).toEqual({hasWindowClient: true, hasVisibleClient: false});
		expect(shouldSilenceNonMobilePushNotification(hiddenClientState)).toBe(true);
		const noClientState = getPushNotificationClientState([]);
		expect(noClientState).toEqual({hasWindowClient: false, hasVisibleClient: false});
		expect(shouldSilenceNonMobilePushNotification(noClientState)).toBe(false);
	});
});
