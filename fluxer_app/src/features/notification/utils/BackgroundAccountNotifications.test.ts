// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BackgroundMessageNotification} from '@app/features/gateway/transport/BackgroundSnapshotSink';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {beforeEach, describe, expect, test, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	currentAccountKey: 'active::1' as string | null,
	accounts: new Map<string, unknown>(),
	storage: new Map<string, string>(),
	shown: [] as Array<Record<string, unknown>>,
}));

vi.mock('@lingui/core/macro', () => ({msg: (value: unknown) => value}));

vi.mock('@app/app/I18n', () => ({
	default: {
		_: (descriptor: {message: string}, values?: Record<string, string>) =>
			descriptor.message.replace(/\{(\w+)\}/g, (_token, name: string) => values?.[name] ?? ''),
	},
}));

vi.mock('@app/features/notification/utils/MessageNotificationPreview', () => ({
	buildWireMessageNotificationBody: (message: WireMessage) => message.content,
}));

vi.mock('@app/features/notification/utils/NotificationIconURL', () => ({NATIVE_NOTIFICATION_ICON_CSS_SIZE: 128}));

vi.mock('@app/features/notification/utils/NotificationUtils', () => ({
	isMacOSDesktopNotification: () => true,
	playDirectMessageNotificationSoundIfEnabled: vi.fn(),
	playNotificationSoundIfEnabled: vi.fn(),
	showNotification: async (options: Record<string, unknown>) => {
		mocks.shown.push(options);
		return {browserNotification: null, nativeNotificationId: String(options.id)};
	},
}));

vi.mock('@app/features/platform/state/AuthSession', () => ({
	default: {
		get currentAccountKey() {
			return mocks.currentAccountKey;
		},
		getAccount: (accountKey: string) => mocks.accounts.get(accountKey) ?? null,
	},
}));

vi.mock('@app/features/platform/state/PersistentStorageBackend', () => ({
	getPersistentStorageBackend: () => ({
		get: async (scope: string, key: string) => {
			const value = mocks.storage.get(`${scope}/${key}`);
			return value === undefined ? null : {value};
		},
	}),
}));

vi.mock('@app/features/streamer_mode/state/StreamerMode', () => ({default: {shouldDisableNotifications: false}}));

vi.mock('@app/features/user/utils/AvatarUtils', () => ({
	getDefaultAvatarURL: () => 'default-avatar',
	getUserNotificationAvatarURLForEndpoint: (user: {id: string}, endpoint: string) => `${endpoint}/avatars/${user.id}`,
}));

const {showBackgroundAccountNotification} = await import(
	'@app/features/notification/utils/BackgroundAccountNotifications'
);

const ACCOUNT_KEY = 'other::2';
let messageSequence = 0;

function backgroundAccount(overrides: Record<string, unknown> = {}): unknown {
	return {
		storageKey: ACCOUNT_KEY,
		userId: '2',
		token: 'token',
		lastActive: 0,
		isValid: true,
		userData: {username: 'second', discriminator: '0000'},
		instance: {mediaEndpoint: 'https://media.other.test'},
		presenceIntent: null,
		...overrides,
	};
}

function guildNotification(): BackgroundMessageNotification {
	messageSequence += 1;
	return {
		message: {
			id: `message-${messageSequence}`,
			channel_id: 'channel-1',
			guild_id: 'guild-1',
			author: {id: '999', username: 'sender', global_name: 'Sender', avatar: 'abc'},
			content: 'hello',
			mentions: [],
		} as unknown as WireMessage,
		guildName: 'Guild One',
		channelName: 'general',
		channelType: ChannelTypes.GUILD_TEXT,
	};
}

beforeEach(() => {
	mocks.currentAccountKey = 'active::1';
	mocks.accounts.clear();
	mocks.accounts.set(ACCOUNT_KEY, backgroundAccount());
	mocks.storage.clear();
	mocks.storage.set(`${ACCOUNT_KEY}/Notification`, JSON.stringify({browserNotificationsEnabled: true}));
	mocks.shown.length = 0;
});

describe('background account notifications', () => {
	test('a background account with notifications enabled shows one tagged with that account', async () => {
		const notification = guildNotification();

		await showBackgroundAccountNotification(ACCOUNT_KEY, notification);

		expect(mocks.shown).toEqual([
			expect.objectContaining({
				id: `${ACCOUNT_KEY}:${notification.message.id}`,
				title: 'Sender',
				subtitle: 'Guild One #general · second',
				body: 'hello',
				icon: 'https://media.other.test/avatars/999',
				url: `/channels/guild-1/channel-1/${notification.message.id}`,
				accountKey: ACCOUNT_KEY,
			}),
		]);
	});

	test('the same message is shown once', async () => {
		const notification = guildNotification();

		await showBackgroundAccountNotification(ACCOUNT_KEY, notification);
		await showBackgroundAccountNotification(ACCOUNT_KEY, notification);

		expect(mocks.shown).toHaveLength(1);
	});

	test("the account's own notification setting is respected", async () => {
		mocks.storage.set(`${ACCOUNT_KEY}/Notification`, JSON.stringify({browserNotificationsEnabled: false}));

		await showBackgroundAccountNotification(ACCOUNT_KEY, guildNotification());

		expect(mocks.shown).toHaveLength(0);
	});

	test('an account that never enabled notifications stays silent', async () => {
		mocks.storage.clear();

		await showBackgroundAccountNotification(ACCOUNT_KEY, guildNotification());

		expect(mocks.shown).toHaveLength(0);
	});

	test('do not disturb on the background account suppresses it', async () => {
		mocks.accounts.set(
			ACCOUNT_KEY,
			backgroundAccount({
				presenceIntent: {
					status: StatusTypes.DND,
					statusResetsAt: null,
					statusResetsTo: null,
					customStatus: null,
					capturedAt: 0,
				},
			}),
		);

		await showBackgroundAccountNotification(ACCOUNT_KEY, guildNotification());

		expect(mocks.shown).toHaveLength(0);
	});

	test('an account that became active is left to the foreground notifier', async () => {
		mocks.currentAccountKey = ACCOUNT_KEY;

		await showBackgroundAccountNotification(ACCOUNT_KEY, guildNotification());

		expect(mocks.shown).toHaveLength(0);
	});
});
