// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import AccountManager from '@app/features/auth/state/AccountManager';
import Authentication from '@app/features/auth/state/Authentication';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import Guilds from '@app/features/guild/state/Guilds';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import * as MessageUtils from '@app/features/messaging/utils/MessageUtils';
import Navigation from '@app/features/navigation/state/Navigation';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {buildMessageNotificationBody} from '@app/features/notification/utils/MessageNotificationPreview';
import {getNotificationIconURL} from '@app/features/notification/utils/NotificationIconURL';
import * as NotificationUtils from '@app/features/notification/utils/NotificationUtils';
import * as PushSubscriptionService from '@app/features/platform/push/PushSubscriptionService';
import {IS_DEV} from '@app/features/platform/types/Env';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent, stopPersistent} from '@app/features/platform/utils/MobXPersistence';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import type {RelationshipWire} from '@app/features/relationship/models/Relationship';
import FriendsTab from '@app/features/relationship/state/FriendsTab';
import Relationships from '@app/features/relationship/state/Relationships';
import {FRIEND_ADDED_DESCRIPTOR} from '@app/features/relationship/utils/RelationshipMessageDescriptors';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import Modal from '@app/features/ui/state/Modal';
import {isInstalledPwa} from '@app/features/ui/utils/PwaUtils';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {FAVORITES_GUILD_ID as ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, MessageFlags, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {LRUCache} from 'lru-cache';
import {makeAutoObservable, reaction, runInAction} from 'mobx';

const FRIEND_REQUEST_DESCRIPTOR = msg({
	message: 'Friend request',
	comment: 'Section or status label for a friend request item.',
});
const SENT_YOU_A_FRIEND_REQUEST_DESCRIPTOR = msg({
	message: '{displayName} sent you a friend request',
	comment: 'Toast title announcing an incoming friend request.',
});
const IS_NOW_YOUR_FRIEND_DESCRIPTOR = msg({
	message: '{displayName} is now your friend!',
	comment: 'Toast title announcing a newly accepted friend request.',
});
const logger = new Logger('Notification');
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
	message: WireMessage;
	messageRecord: Message;
	currentUser: User;
	user: User;
	channel: Channel;
}>;

const isDocumentFocused = (): boolean => {
	if (typeof document === 'undefined') {
		return false;
	}
	if (document.hidden) {
		return false;
	}
	return typeof document.hasFocus === 'function' && document.hasFocus();
};

class NotificationState {
	browserNotificationsEnabled = false;
	unreadMessageBadgeEnabled = true;
	ttsNotificationMode: TTSNotificationMode = TTSNotificationMode.NEVER;
	focused = isDocumentFocused();
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
		void this.initPersistence().then(() => {
			void this.refreshPermission();
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
			window.__notificationCleanup = () => this.cleanup();
		}
	}

	setI18n(i18n: I18n): void {
		this.i18n = i18n;
	}

	private async initPersistence(): Promise<void> {
		if (this.isPersisting) return;
		this.isPersisting = true;
		await makePersistent(this, 'Notification', [
			'browserNotificationsEnabled',
			'unreadMessageBadgeEnabled',
			'ttsNotificationMode',
		]);
	}

	private cleanup(): void {
		if (!this.isPersisting) return;
		stopPersistent('Notification', this);
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

	private isMessageMentionLike(channel: Channel, message: Message, currentUser: User): boolean {
		if (MessageUtils.isMentioned(currentUser, message)) {
			return true;
		}
		if (channel.isPrivate()) {
			return !UserGuildSettings.isGuildOrChannelMuted(null, channel.id);
		}
		return false;
	}

	private shouldNotifyBasedOnSettings(channel: Channel, messageRecord: Message, currentUser: User): boolean {
		const level = UserGuildSettings.resolveEffectiveMessageNotifications({
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

	private isFocusedForNotifications(): boolean {
		return isDocumentFocused() || this.focused;
	}

	private getVisibleChannelId(): string | null {
		return Navigation.channelId ?? SelectedChannel.currentChannelId;
	}

	private isViewingChannel(channelId: string): boolean {
		return this.getVisibleChannelId() === channelId;
	}

	private validateNotificationData(message: WireMessage): NotificationData | null {
		if (StreamerMode.shouldDisableNotifications) return null;
		const channel = Channels.getChannel(message.channel_id);
		if (!channel) return null;
		if (message.author.id === Authentication.currentUserId) return null;
		if (Relationships.isBlocked(message.author.id)) return null;
		if (LocalPresence.getStatus() === StatusTypes.DND) return null;
		if ((message.flags & MessageFlags.SUPPRESS_NOTIFICATIONS) === MessageFlags.SUPPRESS_NOTIFICATIONS) {
			return null;
		}
		if (
			UserGuildSettings.resolvesToNoMessages({
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
		if (GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null})) {
			return null;
		}
		const currentUser = Users.getCurrentUser();
		if (!currentUser) return null;
		const messageRecord = new Message(message, {skipUserCache: false, missingReactions: 'preserve'});
		return {message, messageRecord, currentUser, user: messageRecord.author, channel};
	}

	private markNotified(key: string): void {
		this.notifiedMessageIds.set(key, true);
	}

	private claimNotification(key: string): boolean {
		if (this.notifiedMessageIds.has(key)) return false;
		this.markNotified(key);
		return true;
	}

	private async showBackgroundNotification(data: NotificationData): Promise<void> {
		if (!this.i18n) {
			throw new Error('Notification: i18n not initialized');
		}
		const i18n = this.i18n;
		const {message, user, channel} = data;
		if (channel.isPrivate()) {
			NotificationUtils.playDirectMessageNotificationSoundIfEnabled();
		} else {
			NotificationUtils.playNotificationSoundIfEnabled();
		}
		if (!this.browserNotificationsEnabled) {
			return;
		}
		const useMacOSNotificationPresentation = NotificationUtils.isMacOSDesktopNotification();
		let title = NicknameUtils.getNickname(user, channel.guildId, channel.id);
		let subtitle: string | undefined;
		switch (channel.type) {
			case ChannelTypes.GUILD_TEXT:
			case ChannelTypes.GUILD_VOICE:
				if (message.type === MessageTypes.DEFAULT) {
					if (useMacOSNotificationPresentation) {
						const guild = channel.guildId ? Guilds.getGuild(channel.guildId) : null;
						const channelPrefix = channel.type === ChannelTypes.GUILD_TEXT ? '#' : '';
						subtitle = guild ? `${guild.name} ${channelPrefix}${channel.name}` : `${channelPrefix}${channel.name}`;
					} else {
						const channelPrefix = channel.type === ChannelTypes.GUILD_TEXT ? '#' : '';
						title = `${title} (${channelPrefix}${channel.name})`;
					}
				} else {
					const guild = channel.guildId ? Guilds.getGuild(channel.guildId) : null;
					if (guild) {
						if (useMacOSNotificationPresentation) {
							title = guild.name;
							const channelPrefix = channel.type === ChannelTypes.GUILD_TEXT ? '#' : '';
							subtitle = `${channelPrefix}${channel.name}`;
						} else {
							const channelPrefix = channel.type === ChannelTypes.GUILD_TEXT ? '#' : '';
							title = `${guild.name} (${channelPrefix}${channel.name})`;
						}
					}
				}
				break;
			case ChannelTypes.GROUP_DM:
				if (useMacOSNotificationPresentation) {
					subtitle = channel.name || 'Group DM';
				} else {
					title = `${title} (${channel.name || 'Group DM'})`;
				}
				break;
		}
		const body = buildMessageNotificationBody(data.messageRecord, i18n);
		const notificationUrl =
			channel.guildId && channel.guildId !== ME
				? Routes.channelMessage(channel.guildId, channel.id, message.id)
				: Routes.dmChannelMessage(channel.id, message.id);
		try {
			const result = await NotificationUtils.showNotification({
				id: message.id,
				title,
				subtitle,
				body,
				icon: getNotificationIconURL(user, channel.guildId),
				url: notificationUrl,
				playSound: false,
			});
			notificationTracker.track(channel.id, {
				browserNotification: result.browserNotification,
				nativeId: result.nativeNotificationId,
			});
		} catch (error) {
			logger.error('Failed to show notification', {messageId: message.id, channelId: channel.id}, error);
		}
	}

	private playForegroundObscuredNotificationSound(channel: Channel): void {
		if (channel.isPrivate()) {
			NotificationUtils.playDirectMessageNotificationSoundIfEnabled();
		} else {
			NotificationUtils.playNotificationSoundIfEnabled();
		}
	}

	handleMessageCreate({message}: {message: WireMessage}): boolean {
		if (StreamerMode.shouldDisableNotifications) {
			return false;
		}
		const isFocusedViewingChannel =
			message.author.id !== Authentication.currentUserId &&
			!Relationships.isBlocked(message.author.id) &&
			Channels.getChannel(message.channel_id) != null &&
			this.isViewingChannel(message.channel_id) &&
			this.isFocusedForNotifications();
		if (isFocusedViewingChannel && !Modal.hasModalOpen()) {
			NotificationUtils.playSameChannelNotificationSoundIfEnabled();
			this.markNotified(message.id);
			return true;
		}
		const notificationData = this.validateNotificationData(message);
		if (!notificationData) {
			return false;
		}
		const {channel, messageRecord, currentUser} = notificationData;
		if (!this.shouldNotifyBasedOnSettings(channel, messageRecord, currentUser)) {
			return false;
		}
		if (!this.claimNotification(message.id)) {
			return false;
		}
		if (isFocusedViewingChannel && Modal.hasModalOpen()) {
			this.playForegroundObscuredNotificationSound(channel);
			return true;
		}
		void this.showBackgroundNotification(notificationData);
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
			if (!granted) {
				runInAction(() => {
					this.browserNotificationsEnabled = false;
				});
				return;
			}
			if (this.browserNotificationsEnabled && shouldManagePushSubscriptions()) {
				void PushSubscriptionService.registerPushSubscription();
			}
		} catch (error) {
			logger.error('Failed to refresh notification permission', error);
		}
	}

	handleNotificationSoundToggle(enabled: boolean): void {
		this.unreadMessageBadgeEnabled = enabled;
	}

	handleWindowFocused({focused}: {focused: boolean}): void {
		this.focused = focused;
		if (focused) {
			const channelId = SelectedChannel.currentChannelId;
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

	handleRelationshipNotification(
		relationship: RelationshipWire,
		options?: {
			event?: 'add' | 'update';
		},
	): void {
		if (RuntimeConfig.directMessagesDisabled) {
			return;
		}
		if (StreamerMode.shouldDisableNotifications) {
			return;
		}
		if (!this.i18n) {
			throw new Error('Notification: i18n not initialized');
		}
		if (!this.browserNotificationsEnabled) {
			return;
		}
		if (LocalPresence.getStatus() === StatusTypes.DND) {
			return;
		}
		const user = Users.getUser(relationship.user?.id ?? relationship.id);
		if (!user) {
			return;
		}
		const cacheKey = `relationship_${relationship.type}_${user.id}`;
		if (this.notifiedMessageIds.has(cacheKey)) {
			return;
		}
		if (options?.event === 'update') {
			return;
		}
		let title: string;
		let body: string;
		const displayName = NicknameUtils.getDisplayName(user);
		if (relationship.type === RelationshipTypes.INCOMING_REQUEST) {
			title = this.i18n._(FRIEND_REQUEST_DESCRIPTOR);
			body = this.i18n._(SENT_YOU_A_FRIEND_REQUEST_DESCRIPTOR, {displayName});
			FriendsTab.setTab('pending');
		} else if (relationship.type === RelationshipTypes.FRIEND) {
			title = this.i18n._(FRIEND_ADDED_DESCRIPTOR);
			body = this.i18n._(IS_NOW_YOUR_FRIEND_DESCRIPTOR, {displayName});
		} else {
			return;
		}
		if (this.focused && NotificationUtils.isMacOSDesktopNotification()) {
			this.markNotified(cacheKey);
			return;
		}
		void NotificationUtils.showNotification({
			id: cacheKey,
			title,
			body,
			icon: getNotificationIconURL(user),
			url: Routes.ME,
		}).catch((error) => {
			logger.error('Failed to show relationship notification', {cacheKey}, error);
		});
		this.markNotified(cacheKey);
	}
}

export default new NotificationState();
