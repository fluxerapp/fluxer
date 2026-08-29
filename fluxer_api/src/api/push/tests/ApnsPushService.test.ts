// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {ApnsPushServiceTestHooks} from '../ApnsPushService';

describe('ApnsPushService', () => {
	it('builds modern APNs alert payloads for chat messages', () => {
		const payload = ApnsPushServiceTestHooks.buildApnsPayload({
			tag: 'channel:123:456',
			image_url: 'https://cdn.example/image.png',
			data: {
				channel_id: '123',
				message_id: '456',
				notification_tag: 'channel:123',
				badge_count: 7,
				url: '/channels/@me/123/456',
			},
			notification: {
				title: 'Alice',
				body: 'Hello',
			},
		});
		expect(payload).toMatchObject({
			channel_id: '123',
			message_id: '456',
			title: 'Alice',
			body: 'Hello',
			url: '/channels/@me/123/456',
			image_url: 'https://cdn.example/image.png',
			aps: {
				alert: {title: 'Alice', body: 'Hello'},
				sound: 'default',
				badge: 7,
				'thread-id': 'channel:123',
				category: 'FLUXER_MESSAGE',
				'interruption-level': 'active',
				'mutable-content': 1,
			},
		});
	});
	it('builds silent APNs clear payloads as pure background pushes', () => {
		const payload = ApnsPushServiceTestHooks.buildApnsPayload({
			type: 'notification_clear',
			action: 'clear_channel',
			data: {
				channel_id: '123',
				message_id: '456',
				badge_count: 0,
			},
		});
		expect(payload).toMatchObject({
			type: 'notification_clear',
			action: 'clear_channel',
			channel_id: '123',
			message_id: '456',
			badge_count: 0,
			aps: {
				'content-available': 1,
			},
		});
		expect(payload.aps).not.toHaveProperty('alert');
		expect(payload.aps).not.toHaveProperty('sound');
		expect(payload.aps).not.toHaveProperty('badge');
	});
	it('uses APNs push-type and priority headers that match alert versus background delivery', () => {
		const alertHeaders = ApnsPushServiceTestHooks.buildApnsHeaders({
			providerToken: 'provider-token',
			topic: 'com.fluxer',
			payload: {
				tag: 'channel:123:456',
				data: {
					message_id: '456',
				},
			},
		});
		const clearHeaders = ApnsPushServiceTestHooks.buildApnsHeaders({
			providerToken: 'provider-token',
			topic: 'com.fluxer',
			payload: {
				type: 'notification_clear',
				tag: 'channel:123',
				data: {},
			},
		});
		expect(alertHeaders).toMatchObject({
			authorization: 'bearer provider-token',
			'apns-topic': 'com.fluxer',
			'apns-push-type': 'alert',
			'apns-priority': '10',
			'apns-collapse-id': 'channel:123:456',
		});
		expect(clearHeaders).toMatchObject({
			'apns-push-type': 'background',
			'apns-priority': '5',
			'apns-collapse-id': 'channel:123',
		});
		const now = Math.floor(Date.now() / 1000);
		expect(Number(clearHeaders['apns-expiration']) - now).toBeGreaterThan(1800);
	});

	it('omits the badge entirely when no usable badge count is supplied', () => {
		const payload = ApnsPushServiceTestHooks.buildApnsPayload({
			tag: 'channel:123:456',
			data: {channel_id: '123', message_id: '456'},
			notification: {title: 'Alice', body: 'Hello'},
		});
		expect(payload.aps).not.toHaveProperty('badge');
		const unparseable = ApnsPushServiceTestHooks.buildApnsPayload({
			tag: 'channel:123:456',
			data: {channel_id: '123', message_id: '456', badge_count: 'not-a-number'},
			notification: {title: 'Alice', body: 'Hello'},
		});
		expect(unparseable.aps).not.toHaveProperty('badge');
	});

	it('keeps a genuine zero badge so reading the last message clears the app icon', () => {
		const payload = ApnsPushServiceTestHooks.buildApnsPayload({
			tag: 'channel:123:456',
			data: {channel_id: '123', message_id: '456', badge_count: 0},
			notification: {title: 'Alice', body: 'Hello'},
		});
		expect((payload.aps as Record<string, unknown>).badge).toBe(0);
	});

	it('does not use the sender avatar as the notification media image', () => {
		const payload = ApnsPushServiceTestHooks.buildApnsPayload({
			tag: 'channel:123:456',
			data: {channel_id: '123', message_id: '456', author_avatar_url: 'https://cdn.example/avatar.png'},
			notification: {title: 'Alice', body: 'Hello', icon: 'https://cdn.example/avatar.png'},
		});
		expect(payload.image_url).toBeUndefined();
		expect(payload.aps).not.toHaveProperty('mutable-content');
		expect(payload.author_avatar_url).toBe('https://cdn.example/avatar.png');
	});
	it('marks only permanent APNs token failures as subscription deletion signals', () => {
		expect(ApnsPushServiceTestHooks.isPermanentApnsFailure(410, 'Unregistered')).toBe(true);
		expect(ApnsPushServiceTestHooks.isPermanentApnsFailure(400, 'BadDeviceToken')).toBe(true);
		expect(ApnsPushServiceTestHooks.isPermanentApnsFailure(400, 'DeviceTokenNotForTopic')).toBe(true);
		expect(ApnsPushServiceTestHooks.isPermanentApnsFailure(403, 'ExpiredProviderToken')).toBe(false);
		expect(ApnsPushServiceTestHooks.isPermanentApnsFailure(500, 'InternalServerError')).toBe(false);
	});
});
