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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {LRUCache} from 'lru-cache';
import {makeAutoObservable, reaction, runInAction} from 'mobx';
import {
	ChannelTypes,
	ME,
	MessageFlags,
	MessageNotifications,
	MessageTypes,
	RelationshipTypes,
	StatusTypes,
} from '~/Constants';
import {IS_DEV} from '~/lib/env';
import {Logger} from '~/lib/Logger';
import {makePersistent, stopPersistent} from '~/lib/MobXPersistence';
import {parseAndRenderToPlaintext} from '~/lib/markdown/plaintext';
import {getParserFlagsForContext, MarkdownContext} from '~/lib/markdown/renderers';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {Message} from '~/records/MessageRecord';
import {MessageRecord} from '~/records/MessageRecord';
import type {Relationship} from '~/records/RelationshipRecord';
import type {UserRecord} from '~/records/UserRecord';
import * as PushSubscriptionService from '~/services/push/PushSubscriptionService';
import AccountManager from '~/stores/AccountManager';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import LocalPresenceStore from '~/stores/LocalPresenceStore';
import RelationshipStore from '~/stores/RelationshipStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import UserStore from '~/stores/UserStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import * as MessageUtils from '~/utils/MessageUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as NotificationUtils from '~/utils/NotificationUtils';
import {isInstalledPwa} from '~/utils/PwaUtils';
import {SystemMessageUtils} from '~/utils/SystemMessageUtils';
import FriendsTabStore from './FriendsTabStore';
import GuildNSFWAgreeStore from './GuildNSFWAgreeStore';

const logger = new Logger('NotificationStore');
const shouldManagePushSubscriptions = (): boolean => isInstalledPwa();

export enum TTSNotificationMode {
	FOR_ALL_CHANNELS = 0,
	FOR_CURRENT_CHANNEL = 1,
	NEVER = 2,
}

const MAX_PER_CHANNEL = 5;
const CACHE_SIZE = 500;

interface TrackedNotification {
	browserNotification: Notification | null;
	nativeId: string | null;
}

const notificationTracker = new (class {
	private channels: Record<string, Array<TrackedNotification>> = {};

	track(channelId: string, notification: TrackedNotification): void {
		let notifications = this.channels[channelId];
		if (notifications == null) {
			notifications = [];
			this.channels[channelId] = notifications;
		}

		notifications.push(notification);

		while (notifications.length > MAX_PER_CHANNEL) {
			const old = notifications.shift();
			if (old) {
				old.browserNotification?.close();
				if (old.nativeId) {
					NotificationUtils.closeNativeNotification(old.nativeId);
				}
			}
		}
	}

	clearChannel(channelId: string): void {
		const notifications = this.channels[channelId];
		if (notifications == null) return;

		delete this.channels[channelId];

		const browserNotifications = notifications
			.map((n) => n.browserNotification)
			.filter((n): n is Notification => n != null);
		browserNotifications.forEach((notification) => notification.close());

		const nativeIds = notifications.map((n) => n.nativeId).filter((id): id is string => id != null);
		if (nativeIds.length > 0) {
			NotificationUtils.closeNativeNotifications(nativeIds);
		}
	}
})();

type NotificationData = Readonly<{
	message: Message;
	user: UserRecord;
	channel: ChannelRecord;
}>;

class NotificationStore {
	browserNotificationsEnabled = false;
	unreadMessageBadgeEnabled = true;
	ttsNotificationMode: TTSNotificationMode = TTSNotificationMode.NEVER;
	focused = true;
	notifiedMessageIds = new LRUCache<string, boolean>({max: CACHE_SIZE});
	private isPersisting = false;
	private accountReactionDisposer: (() => void) | null = null;
	private i18n: I18n | null = null;

	constructor() {
		makeAutoObservable(
			this,
			{
				notifiedMessageIds: false,
			},
			{autoBind: true},
		);
		this.initPersistence();
		queueMicrotask(() => {
			this.refreshPermission();
		});

		queueMicrotask(() => {
			NotificationUtils.ensureDesktopNotificationClickHandler();
		});

		queueMicrotask(() => {
			this.accountReactionDisposer = reaction(
				() => {
					try {
						return AccountManager?.currentUserId;
					} catch {
						return undefined;
					}
				},
				() => {
					if (!shouldManagePushSubscriptions()) return;
					if (!this.browserNotificationsEnabled) return;
					void PushSubscriptionService.registerPushSubscription();
				},
			);
		});

		if (IS_DEV) {
			window.__notificationStoreCleanup = () => this.cleanup();
		}
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
	}

	private async initPersistence(): Promise<void> {
		if (this.isPersisting) return;
		this.isPersisting = true;
		await makePersistent(this, 'NotificationStore', [
			'browserNotificationsEnabled',
			'unreadMessageBadgeEnabled',
			'ttsNotificationMode',
		]);
	}

	private cleanup(): void {
		if (!this.isPersisting) return;
		stopPersistent('NotificationStore', this);
		this.isPersisting = false;
		this.accountReactionDisposer?.();
		this.accountReactionDisposer = null;
	}

	getUnreadMessageBadgeEnabled(): boolean {
		return this.unreadMessageBadgeEnabled;
	}

	getBrowserNotificationsEnabled(): boolean {
		return this.browserNotificationsEnabled;
	}

	getTTSNotificationMode(): TTSNotificationMode {
		return this.ttsNotificationMode;
	}

	setTTSNotificationMode(mode: TTSNotificationMode): void {
		this.ttsNotificationMode = mode;
	}

	isFocused(): boolean {
		return this.focused;
	}

	private isMessageMentionLike(channel: ChannelRecord, message: MessageRecord, currentUser: UserRecord): boolean {
		if (MessageUtils.isMentioned(currentUser, message)) {
			return true;
		}

		if (channel.isPrivate()) {
			return !UserGuildSettingsStore.isGuildOrChannelMuted(null, channel.id);
		}

		return false;
	}

	private shouldNotifyBasedOnSettings(
		channel: ChannelRecord,
		messageRecord: MessageRecord,
		currentUser: UserRecord,
	): boolean {
		const level = UserGuildSettingsStore.resolvedMessageNotifications({
			id: channel.id,
			guildId: channel.guildId,
			parentId: channel.parentId ?? undefined,
			type: channel.type,
		});

		if (level === MessageNotifications.NO_MESSAGES) {
			return false;
		}

		if (level === MessageNotifications.ALL_MESSAGES) {
			return true;
		}

		return this.isMessageMentionLike(channel, messageRecord, currentUser);
	}

	private validateNotificationData(message: Message): NotificationData | null {
		const channel = ChannelStore.getChannel(message.channel_id);
		if (!channel) return null;

		const user = UserStore.getUser(message.author.id);
		if (!user) return null;

		if (message.author.id === AuthenticationStore.currentUserId) return null;
		if (RelationshipStore.isBlocked(user.id)) return null;
		if (LocalPresenceStore.getStatus() === StatusTypes.DND) return null;

		if ((message.flags & MessageFlags.SUPPRESS_NOTIFICATIONS) === MessageFlags.SUPPRESS_NOTIFICATIONS) {
			return null;
		}

		if (
			UserGuildSettingsStore.allowNoMessages({
				id: channel.id,
				guildId: channel.guildId,
				parentId: channel.parentId ?? undefined,
				type: channel.type,
			})
		) {
			return null;
		}

		if (this.notifiedMessageIds.has(message.id)) {
			return null;
		}

		if (channel.isNSFW() && GuildNSFWAgreeStore.shouldShowGate(channel.id)) {
			return null;
		}

		const currentUser = UserStore.getCurrentUser();
		if (!currentUser) return null;

		const messageRecord = new MessageRecord(message, {skipUserCache: false});
		if (!this.shouldNotifyBasedOnSettings(channel, messageRecord, currentUser)) {
			return null;
		}

		return {message, user, channel};
	}

	private markNotified(key: string): void {
		const newCache = new LRUCache<string, boolean>({max: CACHE_SIZE});
		this.notifiedMessageIds.forEach((value, k) => newCache.set(k, value));
		newCache.set(key, true);
		this.notifiedMessageIds = newCache;
	}

	private async showNotification(data: NotificationData): Promise<void> {
		if (!this.i18n) {
			throw new Error('NotificationStore: i18n not initialized');
		}
		const {message, user, channel} = data;

		const shouldPlaySound = !this.focused || channel.id !== SelectedChannelStore.currentChannelId;
		if (shouldPlaySound) {
			NotificationUtils.playNotificationSoundIfEnabled();
		}

		if (!this.browserNotificationsEnabled) {
			this.markNotified(message.id);
			return;
		}

		if (this.focused && channel.id === SelectedChannelStore.currentChannelId) {
			this.markNotified(message.id);
			return;
		}

		let title = NicknameUtils.getNickname(user, channel.guildId);
		switch (channel.type) {
			case ChannelTypes.GUILD_TEXT:
				if (message.type === MessageTypes.DEFAULT) {
					title = `${title} (#${channel.name})`;
				} else {
					const guild = channel.guildId ? GuildStore.getGuild(channel.guildId) : null;
					if (guild) {
						title = `${guild.name} (#${channel.name})`;
					}
				}
				break;
			case ChannelTypes.GROUP_DM:
				title = `${title} (${channel.name || 'Group DM'})`;
				break;
		}

		let body = '';
		const isUserMessage =
			message.type === MessageTypes.DEFAULT ||
			message.type === MessageTypes.REPLY ||
			message.type === MessageTypes.CLIENT_SYSTEM;

		if (!isUserMessage) {
			body = SystemMessageUtils.stringify(message, this.i18n) || '';
		} else {
			body = parseAndRenderToPlaintext(
				message.content,
				getParserFlagsForContext(MarkdownContext.STANDARD_WITHOUT_JUMBO),
				{
					channelId: channel.id,
					preserveMarkdown: true,
					includeEmojiNames: true,
				},
			);
		}

		if (!body && message.attachments?.length) {
			body = this.i18n._(msg`Attachment: ${message.attachments[0].filename}`);
		}

		if (!body && message.embeds?.length) {
			const embed = message.embeds[0];
			if (embed.description) {
				body = embed.title ? `${embed.title}: ${embed.description}` : embed.description;
			} else if (embed.title) {
				body = embed.title;
			} else if (embed.fields?.length) {
				const field = embed.fields[0];
				body = `${field.name}: ${field.value}`;
			}
		}

		const notificationUrl =
			channel.guildId && channel.guildId !== ME
				? Routes.channelMessage(channel.guildId, channel.id, message.id)
				: Routes.dmChannelMessage(channel.id, message.id);

		try {
			const result = await NotificationUtils.showNotification({
				title,
				body,
				icon: AvatarUtils.getUserAvatarURL(user),
				url: notificationUrl,
				playSound: false,
			});

			notificationTracker.track(channel.id, {
				browserNotification: result.browserNotification,
				nativeId: result.nativeNotificationId,
			});

			this.markNotified(message.id);
		} catch (error) {
			logger.error('Failed to show notification', {messageId: message.id, channelId: channel.id}, error);
			this.markNotified(message.id);
		}
	}

	handleMessageCreate({message}: {message: Message}): boolean {
		const notificationData = this.validateNotificationData(message);
		if (!notificationData) {
			return false;
		}

		void this.showNotification(notificationData);
		return true;
	}

	handleNotificationPermissionGranted(): void {
		this.browserNotificationsEnabled = true;
		if (shouldManagePushSubscriptions()) {
			void PushSubscriptionService.registerPushSubscription();
		}
	}

	handleNotificationPermissionDenied(): void {
		this.browserNotificationsEnabled = false;
		if (shouldManagePushSubscriptions()) {
			void PushSubscriptionService.unregisterAllPushSubscriptions();
		}
	}

	async refreshPermission(): Promise<void> {
		try {
			const granted = await NotificationUtils.isGranted();
			runInAction(() => {
				this.browserNotificationsEnabled = granted;
			});
			if (granted) {
				if (shouldManagePushSubscriptions()) {
					void PushSubscriptionService.registerPushSubscription();
				}
			}
		} catch (error) {
			logger.error('Failed to refresh notification permission', error);
		}
	}

	handleNotificationSoundToggle(enabled: boolean): void {
		this.unreadMessageBadgeEnabled = enabled;
	}

	handleWindowFocus({focused}: {focused: boolean}): void {
		this.focused = focused;
		if (focused) {
			const channelId = SelectedChannelStore.currentChannelId;
			if (channelId) {
				notificationTracker.clearChannel(channelId);
			}
		}
	}

	handleChannelSelect({channelId}: {channelId?: string | null}): void {
		if (channelId) {
			notificationTracker.clearChannel(channelId);
		}
	}

	handleMessageAck({channelId}: {channelId: string}): void {
		notificationTracker.clearChannel(channelId);
	}

	handleMessageDelete({channelId}: {channelId: string}): void {
		notificationTracker.clearChannel(channelId);
	}

	handleRelationshipNotification(relationship: Relationship): void {
		if (!this.i18n) {
			throw new Error('NotificationStore: i18n not initialized');
		}
		if (!this.browserNotificationsEnabled) {
			return;
		}

		if (LocalPresenceStore.getStatus() === StatusTypes.DND) {
			return;
		}

		const user = UserStore.getUser(relationship.user?.id ?? relationship.id);
		if (!user) {
			return;
		}

		const cacheKey = `relationship_${relationship.type}_${user.id}`;
		if (this.notifiedMessageIds.has(cacheKey)) {
			return;
		}

		let title: string;
		let body: string;

		if (relationship.type === RelationshipTypes.INCOMING_REQUEST) {
			title = this.i18n._(msg`Friend Request`);
			body = this.i18n._(msg`${user.displayName} sent you a friend request`);
			FriendsTabStore.setTab('pending');
		} else if (relationship.type === RelationshipTypes.FRIEND) {
			title = this.i18n._(msg`Friend Added`);
			body = this.i18n._(msg`${user.displayName} is now your friend!`);
		} else {
			return;
		}

		void NotificationUtils.showNotification({
			title,
			body,
			icon: AvatarUtils.getUserAvatarURL(user),
			url: Routes.ME,
		}).catch((error) => {
			logger.error('Failed to show relationship notification', {cacheKey}, error);
		});

		this.markNotified(cacheKey);
	}
}

export default new NotificationStore();
