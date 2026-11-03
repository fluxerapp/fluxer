// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {Routes} from '@app/app/Routes';
import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import {SENSITIVE_MESSAGE_PREVIEW_DESCRIPTOR} from '@app/features/account_engine/state/AccountEngineMessages';
import {resolveAccountInstanceLabel} from '@app/features/auth/AccountDisplayUtils';
import {parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {GROUP_DM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {
	type AccountNotificationPreferences,
	accountSoundEnabled,
	shouldShowAccountNotification,
} from '@app/features/notification/utils/AccountNotificationGate';
import {buildWireMessageNotificationBody} from '@app/features/notification/utils/MessageNotificationPreview';
import {NATIVE_NOTIFICATION_ICON_CSS_SIZE} from '@app/features/notification/utils/NotificationIconURL';
import {clearChannelNotifications, trackNotification} from '@app/features/notification/utils/NotificationTracker';
import * as NotificationUtils from '@app/features/notification/utils/NotificationUtils';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import SessionManager, {type Account} from '@app/features/platform/state/AuthSession';
import {getPersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {AccountEngineNotification} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {LRUCache} from 'lru-cache';

const ACCOUNT_NOTIFICATION_CONTEXT_DESCRIPTOR = msg({
	message: '{context} · {accountName}',
	comment:
		'Desktop notification title or subtitle for a message received by a signed-in account that is not the active one. context is the sender or channel label, accountName is the receiving account name.',
});
const ACCOUNT_ON_HOST_DESCRIPTOR = msg({
	message: '{accountName} on {host}',
	comment:
		'Names the receiving account in a desktop notification when the signed-in accounts are on more than one instance. host is the instance address, such as fluxer.example.com.',
});

const SYNCED_PREFERENCES_LOCAL_KEY = 'UserSettings:syncedPreferencesLocal';

const GUILD_CHANNEL_TYPES: ReadonlySet<number> = new Set([
	ChannelTypes.GUILD_TEXT,
	ChannelTypes.GUILD_ANNOUNCEMENT,
	ChannelTypes.GUILD_VOICE,
]);

const logger = new Logger('AccountEngineNotifications');
const notifiedMessages = new LRUCache<string, true>({max: 500});

function parseNotificationsEnabled(value: string): boolean {
	try {
		const parsed: unknown = JSON.parse(value);
		return (
			typeof parsed === 'object' &&
			parsed !== null &&
			(parsed as {browserNotificationsEnabled?: unknown}).browserNotificationsEnabled === true
		);
	} catch {
		return false;
	}
}

interface StoredAccountNotificationPreferences {
	readonly notification: string | null;
	readonly syncedPreferences: string | null;
}

const PREFERENCES_CACHE_TTL_MS = 5_000;
const storedPreferences = new LRUCache<string, Promise<StoredAccountNotificationPreferences>>({
	max: 256,
	ttl: PREFERENCES_CACHE_TTL_MS,
});

function readStoredPreferences(accountKey: string): Promise<StoredAccountNotificationPreferences> {
	const cached = storedPreferences.get(accountKey);
	if (cached !== undefined) return cached;
	const backend = getPersistentStorageBackend();
	const read = Promise.all([
		backend.get(accountKey, AppStorageKey.NOTIFICATION),
		backend.get(accountKey, SYNCED_PREFERENCES_LOCAL_KEY),
	]).then(([notification, syncedPreferences]) => ({
		notification: notification?.value ?? null,
		syncedPreferences: syncedPreferences?.value ?? null,
	}));
	storedPreferences.set(accountKey, read);
	read.catch(() => storedPreferences.delete(accountKey));
	return read;
}

async function readAccountNotificationPreferences(
	accountKey: string,
	soundType: SoundType,
): Promise<AccountNotificationPreferences> {
	const stored = await readStoredPreferences(accountKey);
	return {
		notificationsEnabled: stored.notification === null ? false : parseNotificationsEnabled(stored.notification),
		soundEnabled: accountSoundEnabled(stored.syncedPreferences, soundType),
	};
}

function parseWireMessage(value: unknown): WireMessage | null {
	if (typeof value !== 'object' || value === null) return null;
	const message = value as Partial<WireMessage>;
	const author = message.author as Partial<WireMessage['author']> | undefined;
	if (typeof message.id !== 'string' || typeof message.channel_id !== 'string') return null;
	if (typeof author !== 'object' || author === null) return null;
	if (typeof author.id !== 'string' || typeof author.username !== 'string') return null;
	return {
		...message,
		content: typeof message.content === 'string' ? message.content : '',
		mentions: Array.isArray(message.mentions) ? message.mentions : [],
		attachments: Array.isArray(message.attachments) ? message.attachments : [],
		embeds: Array.isArray(message.embeds) ? message.embeds : [],
		stickers: Array.isArray(message.stickers) ? message.stickers : [],
	} as WireMessage;
}

function accountsSpanSeveralInstances(): boolean {
	const instanceKeys = new Set<string>();
	for (const account of SessionManager.accounts) {
		instanceKeys.add(parseAccountStorageKey(account.storageKey)?.instanceKey ?? account.storageKey);
		if (instanceKeys.size > 1) return true;
	}
	return false;
}

function accountNotificationLabel(account: Account, includeHost: boolean): string {
	const accountName = account.userData?.globalName || account.userData?.username || account.userId;
	const host = includeHost ? resolveAccountInstanceLabel(account) : null;
	return host === null ? accountName : i18n._(ACCOUNT_ON_HOST_DESCRIPTOR, {accountName, host});
}

function notificationIcon(account: Account, message: WireMessage): string {
	const author = message.author;
	const endpoint = account.instance?.mediaEndpoint;
	if (typeof endpoint !== 'string' || endpoint.length === 0) {
		return AvatarUtils.getDefaultAvatarURL(author.id);
	}
	return AvatarUtils.getUserNotificationAvatarURLForEndpoint(
		{id: author.id, avatar: author.avatar ?? null},
		endpoint,
		NATIVE_NOTIFICATION_ICON_CSS_SIZE,
	);
}

interface AccountNotificationPresentation {
	readonly title: string;
	readonly subtitle: string | undefined;
}

function presentAccountNotification(
	notification: Pick<AccountEngineNotification, 'guildName' | 'channelName' | 'channelType'>,
	message: Pick<WireMessage, 'author' | 'member'>,
	accountLabel: string,
	macOSPresentation: boolean,
): AccountNotificationPresentation {
	const {guildName, channelName, channelType} = notification;
	const author = message.author;
	let title = message.member?.nick || author.global_name || author.username;
	let context: string | null = null;
	if (channelType !== null && GUILD_CHANNEL_TYPES.has(channelType) && channelName !== null) {
		const channelLabel = `${channelType === ChannelTypes.GUILD_VOICE ? '' : '#'}${channelName}`;
		context = macOSPresentation && guildName !== null ? `${guildName} ${channelLabel}` : channelLabel;
	} else if (channelType === ChannelTypes.GROUP_DM) {
		context = channelName || i18n._(GROUP_DM_DESCRIPTOR);
	}
	if (macOSPresentation) {
		const subtitle =
			context === null
				? accountLabel
				: i18n._(ACCOUNT_NOTIFICATION_CONTEXT_DESCRIPTOR, {context, accountName: accountLabel});
		return {title, subtitle};
	}
	if (context !== null) {
		title = `${title} (${context})`;
	}
	return {
		title: i18n._(ACCOUNT_NOTIFICATION_CONTEXT_DESCRIPTOR, {context: title, accountName: accountLabel}),
		subtitle: undefined,
	};
}

function gate(
	accountKey: string,
	notificationKey: string,
	preferences: AccountNotificationPreferences | null,
): Account | null {
	const account = SessionManager.getAccount(accountKey);
	const allowed = shouldShowAccountNotification({
		accountKey,
		currentAccountKey: SessionManager.currentAccountKey,
		account,
		streamerModeBlocksNotifications: StreamerMode.shouldDisableNotifications,
		alreadyNotified: notifiedMessages.has(notificationKey),
		preferences,
		now: Date.now(),
	});
	return allowed ? account : null;
}

async function showAccountEngineNotification(notification: AccountEngineNotification): Promise<void> {
	const {accountKey} = notification;
	const message = parseWireMessage(notification.message);
	if (message === null) return;
	const notificationKey = `${accountKey}:${message.id}`;
	if (gate(accountKey, notificationKey, null) === null) return;
	const guildId = message.guild_id ?? null;
	const soundType = guildId === null ? SoundType.DirectMessage : SoundType.Message;
	const preferences = await readAccountNotificationPreferences(accountKey, soundType);
	const account = gate(accountKey, notificationKey, preferences);
	if (account === null) return;
	notifiedMessages.set(notificationKey, true);
	const body = notification.gated
		? i18n._(SENSITIVE_MESSAGE_PREVIEW_DESCRIPTOR)
		: buildWireMessageNotificationBody(message, i18n);
	if (!body) return;
	if (preferences.soundEnabled) {
		if (guildId === null) {
			NotificationUtils.playDirectMessageNotificationSoundIfEnabled();
		} else {
			NotificationUtils.playNotificationSoundIfEnabled();
		}
	}
	const {title, subtitle} = presentAccountNotification(
		notification,
		message,
		accountNotificationLabel(account, accountsSpanSeveralInstances()),
		NotificationUtils.isMacOSDesktopNotification(),
	);
	try {
		const result = await NotificationUtils.showNotification({
			id: notificationKey,
			title,
			subtitle,
			body,
			icon: notificationIcon(account, message),
			url:
				guildId === null
					? Routes.dmChannelMessage(message.channel_id, message.id)
					: Routes.channelMessage(guildId, message.channel_id, message.id),
			playSound: false,
			accountKey,
		});
		trackNotification(accountKey, message.channel_id, {
			browserNotification: result.browserNotification,
			nativeId: result.nativeNotificationId,
		});
	} catch (error) {
		logger.error('Failed to show a notification for another account', {accountKey, messageId: message.id}, error);
	}
}

let stopListening: (() => void) | null = null;

export function startAccountEngineNotifications(): () => void {
	if (stopListening !== null) return stopListening;
	const disposeNotification = AccountEngine.onNotification((notification) => {
		void showAccountEngineNotification(notification);
	});
	const disposeChannelRead = AccountEngine.onChannelRead((accountKey, channelId) => {
		clearChannelNotifications(accountKey, channelId);
	});
	stopListening = () => {
		disposeNotification();
		disposeChannelRead();
		stopListening = null;
	};
	return stopListening;
}
