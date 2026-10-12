// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMuteConfig, getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import type {Channel} from '@app/features/channel/models/Channel';
import {
	MUTE_CHANNEL_DESCRIPTOR,
	UNMUTE_CHANNEL_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {
	COMMUNITY_DEFAULT_DESCRIPTOR,
	NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR,
	NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR,
	NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ContextMenuCloseProvider, SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {MuteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import itemStyles from '@app/features/ui/action_menu/items/MenuItems.module.css';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import menuItemStyles from '@app/features/ui/action_menu/MenuItem.module.css';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {getMutedText, getNotificationSettingsLabel} from '@app/lib/overlay/OverlayContextMenu';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const USE_CATEGORY_DEFAULT_DESCRIPTOR = msg({
	message: 'Category default',
	comment: 'Short label in the channel notification settings dropdown. Keep it concise.',
});
const UNREAD_BADGES_DESCRIPTOR = msg({
	message: 'Unread badges',
	comment: 'Short label in the channel notification settings dropdown. Keep it concise.',
});

interface Props {
	channel: Channel;
	onClose: () => void;
}

export const ChannelNotificationSettingsDropdown: React.FC<Props> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();
	const muteDurations = useMemo(() => getMuteDurationOptions(i18n), [i18n.locale]);
	const guildId = channel.guildId;
	const isGuildChannel = guildId != null;
	const channelOverride = UserGuildSettings.getChannelOverride(guildId ?? null, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const channelNotifications = channelOverride?.message_notifications;
	const currentNotificationLevel =
		channelNotifications ?? (isGuildChannel ? MessageNotifications.INHERIT : MessageNotifications.ALL_MESSAGES);
	const channelUnreadBadges = channelOverride?.unread_badges;
	const currentUnreadBadgesLevel =
		channelUnreadBadges == null
			? isGuildChannel
				? MessageNotifications.INHERIT
				: MessageNotifications.ALL_MESSAGES
			: channelUnreadBadges;
	const showUnreadBadgeCustomization = AdvancedSettings.unreadBadgeCustomizationEnabled;
	const categoryId = channel.parentId;
	const effectiveDefaultLevel = guildId
		? UserGuildSettings.resolveEffectiveMessageNotifications({
				id: channel.parentId ?? '',
				guildId: channel.guildId,
				type: ChannelTypes.GUILD_CATEGORY,
			})
		: MessageNotifications.ALL_MESSAGES;
	const hasCategory = categoryId != null;
	const handleMute = useCallback(
		(duration: number | null) => {
			const muteConfigValue = createMuteConfig(duration);
			UserGuildSettingsCommands.updateChannelOverride(
				guildId ?? null,
				channel.id,
				{
					muted: true,
					mute_config: muteConfigValue,
				},
				{persistImmediately: true},
			);
			onClose();
		},
		[guildId, channel.id, onClose],
	);
	const handleUnmute = useCallback(() => {
		UserGuildSettingsCommands.updateChannelOverride(
			guildId ?? null,
			channel.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
		onClose();
	}, [guildId, channel.id, onClose]);
	const handleUnreadBadgesChange = useCallback(
		(level: number) => {
			const value = level === MessageNotifications.INHERIT ? null : level;
			UserGuildSettingsCommands.updateUnreadBadgesLevel(guildId ?? null, value, channel.id, {
				persistImmediately: true,
			});
		},
		[guildId, channel.id],
	);
	const handleNotificationLevelChange = useCallback(
		(level: number) => {
			UserGuildSettingsCommands.updateMessageNotifications(guildId ?? null, level, channel.id, {
				persistImmediately: true,
			});
		},
		[guildId, channel.id],
	);
	const defaultLabelParts = useMemo(
		() => ({
			main: hasCategory ? i18n._(USE_CATEGORY_DEFAULT_DESCRIPTOR) : i18n._(COMMUNITY_DEFAULT_DESCRIPTOR),
			sub: getNotificationSettingsLabel(effectiveDefaultLevel) ?? null,
		}),
		[effectiveDefaultLevel, hasCategory, i18n.locale],
	);
	return (
		<ContextMenuCloseProvider
			value={onClose}
			data-flx="channel.channel-header-components.channel-notification-settings-dropdown.context-menu-close-provider"
		>
			<MenuGroup data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-group">
				{isMuted ? (
					<MenuItem
						icon={
							<MuteIcon data-flx="channel.channel-header-components.channel-notification-settings-dropdown.mute-icon" />
						}
						onClick={handleUnmute}
						hint={mutedText ?? undefined}
						data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item.unmute"
					>
						{i18n._(UNMUTE_CHANNEL_DESCRIPTOR)}
					</MenuItem>
				) : (
					<MenuItemSubmenu
						label={i18n._(MUTE_CHANNEL_DESCRIPTOR)}
						onTriggerSelect={() => handleMute(null)}
						render={() => (
							<MenuGroup data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-group--2">
								{muteDurations.map((duration) => (
									<MenuItem
										key={duration.value ?? 'until'}
										onClick={() => handleMute(duration.value)}
										data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item.mute"
									>
										{duration.label}
									</MenuItem>
								))}
							</MenuGroup>
						)}
						data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-submenu"
					/>
				)}
			</MenuGroup>
			{isGuildChannel && (
				<MenuGroup data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-group--3">
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.INHERIT}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.INHERIT)}
						data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-radio.notification-level-change"
					>
						<div
							className={itemStyles.flexColumn}
							data-flx="channel.channel-header-components.channel-notification-settings-dropdown.div"
						>
							<span data-flx="channel.channel-header-components.channel-notification-settings-dropdown.span">
								{defaultLabelParts.main}
							</span>
							{defaultLabelParts.sub && (
								<div
									className={menuItemStyles.subtext}
									data-flx="channel.channel-header-components.channel-notification-settings-dropdown.div--2"
								>
									{defaultLabelParts.sub}
								</div>
							)}
						</div>
					</MenuItemRadio>
				</MenuGroup>
			)}
			<MenuGroup data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-group--4">
				<MenuItemRadio
					selected={currentNotificationLevel === MessageNotifications.ALL_MESSAGES}
					onSelect={() => handleNotificationLevelChange(MessageNotifications.ALL_MESSAGES)}
					data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-radio.notification-level-change--2"
				>
					{i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
				</MenuItemRadio>
				<MenuItemRadio
					selected={currentNotificationLevel === MessageNotifications.ONLY_MENTIONS}
					onSelect={() => handleNotificationLevelChange(MessageNotifications.ONLY_MENTIONS)}
					data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-radio.notification-level-change--3"
				>
					{i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
				</MenuItemRadio>
				<MenuItemRadio
					selected={currentNotificationLevel === MessageNotifications.NO_MESSAGES}
					onSelect={() => handleNotificationLevelChange(MessageNotifications.NO_MESSAGES)}
					data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-radio.notification-level-change--4"
				>
					{i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
				</MenuItemRadio>
			</MenuGroup>
			{showUnreadBadgeCustomization && (
				<MenuGroup data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-group--5">
					<MenuItemSubmenu
						label={i18n._(UNREAD_BADGES_DESCRIPTOR)}
						hint={getNotificationSettingsLabel(currentUnreadBadgesLevel) ?? undefined}
						render={() => (
							<MenuGroup data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-group--6">
								{isGuildChannel && (
									<MenuItemRadio
										selected={currentUnreadBadgesLevel === MessageNotifications.INHERIT}
										onSelect={() => handleUnreadBadgesChange(MessageNotifications.INHERIT)}
										data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-radio.unread-badges-change"
									>
										{hasCategory ? i18n._(USE_CATEGORY_DEFAULT_DESCRIPTOR) : i18n._(COMMUNITY_DEFAULT_DESCRIPTOR)}
									</MenuItemRadio>
								)}
								<MenuItemRadio
									selected={currentUnreadBadgesLevel === MessageNotifications.ALL_MESSAGES}
									onSelect={() => handleUnreadBadgesChange(MessageNotifications.ALL_MESSAGES)}
									data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-radio.unread-badges-change--2"
								>
									{i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
								</MenuItemRadio>
								<MenuItemRadio
									selected={currentUnreadBadgesLevel === MessageNotifications.ONLY_MENTIONS}
									onSelect={() => handleUnreadBadgesChange(MessageNotifications.ONLY_MENTIONS)}
									data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-radio.unread-badges-change--3"
								>
									{i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
								</MenuItemRadio>
								<MenuItemRadio
									selected={currentUnreadBadgesLevel === MessageNotifications.NO_MESSAGES}
									onSelect={() => handleUnreadBadgesChange(MessageNotifications.NO_MESSAGES)}
									data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-radio.unread-badges-change--4"
								>
									{i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
								</MenuItemRadio>
							</MenuGroup>
						)}
						data-flx="channel.channel-header-components.channel-notification-settings-dropdown.menu-item-submenu--2"
					/>
				</MenuGroup>
			)}
		</ContextMenuCloseProvider>
	);
});
