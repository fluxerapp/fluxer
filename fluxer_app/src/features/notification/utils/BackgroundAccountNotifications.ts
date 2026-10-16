// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {Routes} from '@app/app/Routes';
import type {AccountPresenceIntent} from '@app/features/auth/state/AccountStorageContract';
import type {BackgroundMessageNotification} from '@app/features/gateway/transport/BackgroundSnapshotSink';
import {GROUP_DM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {buildWireMessageNotificationBody} from '@app/features/notification/utils/MessageNotificationPreview';
import {NATIVE_NOTIFICATION_ICON_CSS_SIZE} from '@app/features/notification/utils/NotificationIconURL';
import * as NotificationUtils from '@app/features/notification/utils/NotificationUtils';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import SessionManager, {type Account} from '@app/features/platform/state/AuthSession';
import {getPersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {msg} from '@lingui/core/macro';
import {LRUCache} from 'lru-cache';

const ACCOUNT_NOTIFICATION_CONTEXT_DESCRIPTOR = msg({
	message: '{context} · {accountName}',
	comment:
		'Desktop notification title or subtitle for a message received by a signed-in account that is not the active one. context is the sender or channel label, accountName is the receiving account name.',
});

const GUILD_CHANNEL_TYPES: ReadonlySet<number> = new Set([
	ChannelTypes.GUILD_TEXT,
	ChannelTypes.GUILD_ANNOUNCEMENT,
	ChannelTypes.GUILD_VOICE,
]);

const logger = new Logger('BackgroundAccountNotifications');
const notifiedMessages = new LRUCache<string, true>({max: 500});

function effectivePresenceStatus(intent: AccountPresenceIntent | null | undefined, now: number): string | null {
	if (intent == null) return null;
	if (intent.statusResetsAt != null && intent.statusResetsTo != null) {
		const resetsAt = Date.parse(intent.statusResetsAt);
		if (!Number.isNaN(resetsAt) && resetsAt <= now) {
			return intent.statusResetsTo;
		}
	}
	return intent.status;
}

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

export async function readAccountNotificationsEnabled(accountKey: string): Promise<boolean> {
	const entry = await getPersistentStorageBackend().get(accountKey, AppStorageKey.NOTIFICATION);
	return entry === null ? false : parseNotificationsEnabled(entry.value);
}

function accountDisplayName(account: Account): string {
	return account.userData?.globalName || account.userData?.username || account.userId;
}

function notificationIcon(account: Account, notification: BackgroundMessageNotification): string {
	const author = notification.message.author;
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

export interface BackgroundNotificationPresentation {
	readonly title: string;
	readonly subtitle: string | undefined;
}

export function presentBackgroundNotification(
	notification: BackgroundMessageNotification,
	accountName: string,
	macOSPresentation: boolean,
): BackgroundNotificationPresentation {
	const {message, guildName, channelName, channelType} = notification;
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
			context === null ? accountName : i18n._(ACCOUNT_NOTIFICATION_CONTEXT_DESCRIPTOR, {context, accountName});
		return {title, subtitle};
	}
	if (context !== null) {
		title = `${title} (${context})`;
	}
	return {title: i18n._(ACCOUNT_NOTIFICATION_CONTEXT_DESCRIPTOR, {context: title, accountName}), subtitle: undefined};
}

export async function showBackgroundAccountNotification(
	accountKey: string,
	notification: BackgroundMessageNotification,
): Promise<void> {
	if (StreamerMode.shouldDisableNotifications) return;
	if (accountKey === SessionManager.currentAccountKey) return;
	const account = SessionManager.getAccount(accountKey);
	if (account === null || !account.isValid) return;
	if (effectivePresenceStatus(account.presenceIntent, Date.now()) === StatusTypes.DND) return;
	const {message} = notification;
	const notificationKey = `${accountKey}:${message.id}`;
	if (notifiedMessages.has(notificationKey)) return;
	if (!(await readAccountNotificationsEnabled(accountKey))) return;
	if (notifiedMessages.has(notificationKey) || accountKey === SessionManager.currentAccountKey) return;
	notifiedMessages.set(notificationKey, true);
	const body = buildWireMessageNotificationBody(message, i18n);
	if (!body) return;
	const guildId = message.guild_id ?? null;
	if (guildId === null) {
		NotificationUtils.playDirectMessageNotificationSoundIfEnabled();
	} else {
		NotificationUtils.playNotificationSoundIfEnabled();
	}
	const {title, subtitle} = presentBackgroundNotification(
		notification,
		accountDisplayName(account),
		NotificationUtils.isMacOSDesktopNotification(),
	);
	try {
		await NotificationUtils.showNotification({
			id: notificationKey,
			title,
			subtitle,
			body,
			icon: notificationIcon(account, notification),
			url:
				guildId === null
					? Routes.dmChannelMessage(message.channel_id, message.id)
					: Routes.channelMessage(guildId, message.channel_id, message.id),
			playSound: false,
			accountKey,
		});
	} catch (error) {
		logger.error('Failed to show a background account notification', {accountKey, messageId: message.id}, error);
	}
}
