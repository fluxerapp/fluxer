// SPDX-License-Identifier: AGPL-3.0-or-later

import {EVERYONE_MENTION, HERE_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {createMuteConfig, getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Guild} from '@app/features/guild/models/Guild';
import {
	COMMUNITY_NOTIFICATION_SETTINGS_DESCRIPTOR,
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
	NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR,
	NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR,
	NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {SubMenu as MenuItemSubmenu} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import type {
	ChannelOverrideUpdate,
	UserGuildSettingsUpdate,
} from '@app/features/user/commands/UserGuildSettingsCommands';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import type {UserSettingsPatch} from '@app/features/user/commands/UserSettingsCommands';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {GroupDmAddPermissionFlags, IncomingCallFlags} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const MUTE_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Mute communities',
	comment: 'Action that mutes notifications for every joined community.',
});
const UNMUTE_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Unmute communities',
	comment: 'Action that re-enables notifications for every joined community.',
});
const SHOW_MUTED_CHANNELS_DESCRIPTOR = msg({
	message: 'Show muted channels',
	comment: 'Toggle label that shows muted channels in the channel list.',
});
const SUPPRESS_AND_DESCRIPTOR = msg({
	message: 'Suppress {everyoneMention} and {hereMention}',
	comment: 'Toggle label that suppresses @everyone and @here mentions; placeholders are the localized mention tags.',
});
const ALLOW_AND_DESCRIPTOR = msg({
	message: 'Allow {everyoneMention} and {hereMention}',
	comment: 'Toggle label that allows @everyone and @here mentions; placeholders are the localized mention tags.',
});
const SUPPRESS_ROLE_MENTIONS_DESCRIPTOR = msg({
	message: 'Suppress role mentions',
	comment: 'Toggle label that suppresses role mentions in notifications.',
});
const ALLOW_ROLE_MENTIONS_DESCRIPTOR = msg({
	message: 'Allow role mentions',
	comment: 'Toggle label that allows role mentions in notifications.',
});
const ENABLE_MOBILE_PUSH_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Enable mobile push notifications',
	comment: 'Action that enables push notifications on the current mobile device.',
});
const DISABLE_MOBILE_PUSH_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Disable mobile push notifications',
	comment: 'Action that disables push notifications on the current mobile device.',
});
const COMMUNITY_PRIVACY_SETTINGS_DESCRIPTOR = msg({
	message: 'Community privacy settings',
	comment: 'Submenu label that groups privacy options for the community.',
});
const ALLOW_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Allow direct messages',
	comment: 'Toggle label that allows DMs from members of this community.',
});
const BLOCK_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Block direct messages',
	comment: 'Toggle label that blocks DMs from members of this community.',
});
const ALLOW_BOT_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Allow bot direct messages',
	comment: 'Toggle label that allows DMs from bots in this community.',
});
const BLOCK_BOT_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Block bot direct messages',
	comment: 'Toggle label that blocks DMs from bots in this community.',
});
const MUTE_DMS_DESCRIPTOR = msg({
	message: 'Mute DMs',
	comment: 'Action that mutes notifications for all direct messages.',
});
const UNMUTE_DMS_DESCRIPTOR = msg({
	message: 'Unmute DMs',
	comment: 'Action that re-enables notifications for all direct messages.',
});
const DM_NOTIFICATION_SETTINGS_DESCRIPTOR = msg({
	message: 'DM notification settings',
	comment: 'Submenu label that groups DM notification options.',
});
const DM_PRIVACY_SETTINGS_DESCRIPTOR = msg({
	message: 'DM privacy settings',
	comment: 'Submenu label that groups DM privacy options.',
});
const INCOMING_CALLS_NOBODY_DESCRIPTOR = msg({
	message: 'Incoming calls: nobody',
	comment: 'Option label for an incoming call privacy setting that allows nobody.',
});
const INCOMING_CALLS_FRIENDS_ONLY_DESCRIPTOR = msg({
	message: 'Incoming calls: friends only',
	comment: 'Option label for an incoming call privacy setting that allows friends only.',
});
const INCOMING_CALLS_EVERYONE_DESCRIPTOR = msg({
	message: 'Incoming calls: everyone',
	comment: 'Option label for an incoming call privacy setting that allows everyone.',
});
const GROUP_CHAT_ADDS_NOBODY_DESCRIPTOR = msg({
	message: 'Group chat adds: nobody',
	comment: 'Option label for who can add the current user to group DMs (nobody).',
});
const GROUP_CHAT_ADDS_FRIENDS_ONLY_DESCRIPTOR = msg({
	message: 'Group chat adds: friends only',
	comment: 'Option label for who can add the current user to group DMs (friends only).',
});
const GROUP_CHAT_ADDS_EVERYONE_DESCRIPTOR = msg({
	message: 'Group chat adds: everyone',
	comment: 'Option label for who can add the current user to group DMs (everyone).',
});

interface BulkGuildSettingsMenuItemsProps {
	guilds: ReadonlyArray<Guild>;
	onClose: () => void;
}

interface BulkDMSettingsMenuItemsProps {
	channels: ReadonlyArray<Channel>;
	onClose: () => void;
}

export const BulkGuildSettingsMenuItems: React.FC<BulkGuildSettingsMenuItemsProps> = observer(({guilds, onClose}) => {
	const {i18n} = useLingui();
	const muteDurations = useMemo(() => getMuteDurationOptions(i18n), [i18n.locale]);
	const guildIds = useMemo(() => Array.from(new Set(guilds.map((guild) => guild.id))), [guilds]);
	const hasGuilds = guildIds.length > 0;
	const handleUpdateGuildSettings = useCallback(
		(updates: UserGuildSettingsUpdate) => {
			UserGuildSettingsCommands.bulkUpdateGuildSettings(guildIds, updates);
			onClose();
		},
		[guildIds, onClose],
	);
	const handleMute = useCallback(
		(duration: number | null) => {
			handleUpdateGuildSettings({
				muted: true,
				mute_config: createMuteConfig(duration),
			});
		},
		[handleUpdateGuildSettings],
	);
	const handleUnmute = useCallback(() => {
		handleUpdateGuildSettings({
			muted: false,
			mute_config: null,
		});
	}, [handleUpdateGuildSettings]);
	const handleNotificationLevel = useCallback(
		(level: number) => {
			handleUpdateGuildSettings({message_notifications: level});
		},
		[handleUpdateGuildSettings],
	);
	const handleGuildPrivacy = useCallback(
		(kind: 'user' | 'bot', allow: boolean) => {
			const currentRestrictedGuilds = kind === 'bot' ? UserSettings.botRestrictedGuilds : UserSettings.restrictedGuilds;
			const nextRestrictedGuilds = new Set(currentRestrictedGuilds);
			for (const guildId of guildIds) {
				if (allow) {
					nextRestrictedGuilds.delete(guildId);
				} else {
					nextRestrictedGuilds.add(guildId);
				}
			}
			if (kind === 'bot') {
				void UserSettingsCommands.update({botRestrictedGuilds: Array.from(nextRestrictedGuilds)});
			} else {
				void UserSettingsCommands.update({restrictedGuilds: Array.from(nextRestrictedGuilds)});
			}
			onClose();
		},
		[guildIds, onClose],
	);
	return (
		<>
			<MenuItemSubmenu
				label={i18n._(MUTE_COMMUNITIES_DESCRIPTOR)}
				disabled={!hasGuilds}
				render={() => (
					<>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-group">
							{muteDurations.map((duration) => (
								<MenuItem
									key={duration.value ?? 'until'}
									onClick={() => handleMute(duration.value)}
									data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.mute"
								>
									{duration.label}
								</MenuItem>
							))}
						</MenuGroup>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-group--2">
							<MenuItem
								onClick={handleUnmute}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.unmute"
							>
								{i18n._(UNMUTE_COMMUNITIES_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-group--3">
							<MenuItem
								onClick={() => handleUpdateGuildSettings({hide_muted_channels: true})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.update-guild-settings"
							>
								{i18n._(HIDE_MUTED_CHANNELS_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() => handleUpdateGuildSettings({hide_muted_channels: false})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.update-guild-settings--2"
							>
								{i18n._(SHOW_MUTED_CHANNELS_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					</>
				)}
				data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item-submenu"
			/>
			<MenuItemSubmenu
				label={i18n._(COMMUNITY_NOTIFICATION_SETTINGS_DESCRIPTOR)}
				disabled={!hasGuilds}
				render={() => (
					<>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-group--4">
							<MenuItem
								onClick={() => handleNotificationLevel(MessageNotifications.ALL_MESSAGES)}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.notification-level"
							>
								{i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() => handleNotificationLevel(MessageNotifications.ONLY_MENTIONS)}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.notification-level--2"
							>
								{i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() => handleNotificationLevel(MessageNotifications.NO_MESSAGES)}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.notification-level--3"
							>
								{i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-group--5">
							<MenuItem
								onClick={() => handleUpdateGuildSettings({suppress_everyone: true})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.update-guild-settings--3"
							>
								{i18n._(SUPPRESS_AND_DESCRIPTOR, {everyoneMention: EVERYONE_MENTION, hereMention: HERE_MENTION})}
							</MenuItem>
							<MenuItem
								onClick={() => handleUpdateGuildSettings({suppress_everyone: false})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.update-guild-settings--4"
							>
								{i18n._(ALLOW_AND_DESCRIPTOR, {everyoneMention: EVERYONE_MENTION, hereMention: HERE_MENTION})}
							</MenuItem>
							<MenuItem
								onClick={() => handleUpdateGuildSettings({suppress_roles: true})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.update-guild-settings--5"
							>
								{i18n._(SUPPRESS_ROLE_MENTIONS_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() => handleUpdateGuildSettings({suppress_roles: false})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.update-guild-settings--6"
							>
								{i18n._(ALLOW_ROLE_MENTIONS_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-group--6">
							<MenuItem
								onClick={() => handleUpdateGuildSettings({mobile_push: true})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.update-guild-settings--7"
							>
								{i18n._(ENABLE_MOBILE_PUSH_NOTIFICATIONS_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() => handleUpdateGuildSettings({mobile_push: false})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.update-guild-settings--8"
							>
								{i18n._(DISABLE_MOBILE_PUSH_NOTIFICATIONS_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					</>
				)}
				data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item-submenu--2"
			/>
			{!RuntimeConfig.directMessagesDisabled && (
				<MenuItemSubmenu
					label={i18n._(COMMUNITY_PRIVACY_SETTINGS_DESCRIPTOR)}
					disabled={!hasGuilds}
					render={() => (
						<>
							<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-group--7">
								<MenuItem
									onClick={() => handleGuildPrivacy('user', true)}
									data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.guild-privacy"
								>
									{i18n._(ALLOW_DIRECT_MESSAGES_DESCRIPTOR)}
								</MenuItem>
								<MenuItem
									onClick={() => handleGuildPrivacy('user', false)}
									data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.guild-privacy--2"
								>
									{i18n._(BLOCK_DIRECT_MESSAGES_DESCRIPTOR)}
								</MenuItem>
							</MenuGroup>
							<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-group--8">
								<MenuItem
									onClick={() => handleGuildPrivacy('bot', true)}
									data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.guild-privacy--3"
								>
									{i18n._(ALLOW_BOT_DIRECT_MESSAGES_DESCRIPTOR)}
								</MenuItem>
								<MenuItem
									onClick={() => handleGuildPrivacy('bot', false)}
									data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item.guild-privacy--4"
								>
									{i18n._(BLOCK_BOT_DIRECT_MESSAGES_DESCRIPTOR)}
								</MenuItem>
							</MenuGroup>
						</>
					)}
					data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-guild-settings-menu-items.menu-item-submenu--3"
				/>
			)}
		</>
	);
});
export const BulkDMSettingsMenuItems: React.FC<BulkDMSettingsMenuItemsProps> = observer(({channels, onClose}) => {
	const {i18n} = useLingui();
	const muteDurations = useMemo(() => getMuteDurationOptions(i18n), [i18n.locale]);
	const channelIds = useMemo(() => Array.from(new Set(channels.map((channel) => channel.id))), [channels]);
	const hasChannels = channelIds.length > 0;
	const handleUpdateDMOverrides = useCallback(
		(updates: ChannelOverrideUpdate) => {
			UserGuildSettingsCommands.bulkUpdateChannelOverrides(null, channelIds, updates, {persistImmediately: true});
			onClose();
		},
		[channelIds, onClose],
	);
	const handleMute = useCallback(
		(duration: number | null) => {
			handleUpdateDMOverrides({
				muted: true,
				mute_config: createMuteConfig(duration),
			});
		},
		[handleUpdateDMOverrides],
	);
	const handleUnmute = useCallback(() => {
		handleUpdateDMOverrides({
			muted: false,
			mute_config: null,
		});
	}, [handleUpdateDMOverrides]);
	const handleNotificationLevel = useCallback(
		(level: number) => {
			handleUpdateDMOverrides({message_notifications: level});
		},
		[handleUpdateDMOverrides],
	);
	const handlePrivacySetting = useCallback(
		(updates: UserSettingsPatch) => {
			void UserSettingsCommands.update(updates);
			onClose();
		},
		[onClose],
	);
	return (
		<>
			<MenuItemSubmenu
				label={i18n._(MUTE_DMS_DESCRIPTOR)}
				disabled={!hasChannels}
				render={() => (
					<>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-group">
							{muteDurations.map((duration) => (
								<MenuItem
									key={duration.value ?? 'until'}
									onClick={() => handleMute(duration.value)}
									data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.mute"
								>
									{duration.label}
								</MenuItem>
							))}
						</MenuGroup>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-group--2">
							<MenuItem
								onClick={handleUnmute}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.unmute"
							>
								{i18n._(UNMUTE_DMS_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					</>
				)}
				data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item-submenu"
			/>
			<MenuItemSubmenu
				label={i18n._(DM_NOTIFICATION_SETTINGS_DESCRIPTOR)}
				disabled={!hasChannels}
				render={() => (
					<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-group--3">
						<MenuItem
							onClick={() => handleNotificationLevel(MessageNotifications.ALL_MESSAGES)}
							data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.notification-level"
						>
							{i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
						</MenuItem>
						<MenuItem
							onClick={() => handleNotificationLevel(MessageNotifications.ONLY_MENTIONS)}
							data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.notification-level--2"
						>
							{i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
						</MenuItem>
						<MenuItem
							onClick={() => handleNotificationLevel(MessageNotifications.NO_MESSAGES)}
							data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.notification-level--3"
						>
							{i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				)}
				data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item-submenu--2"
			/>
			<MenuItemSubmenu
				label={i18n._(DM_PRIVACY_SETTINGS_DESCRIPTOR)}
				render={() => (
					<>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-group--4">
							<MenuItem
								onClick={() => handlePrivacySetting({incomingCallFlags: IncomingCallFlags.NOBODY})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.privacy-setting"
							>
								{i18n._(INCOMING_CALLS_NOBODY_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() => handlePrivacySetting({incomingCallFlags: IncomingCallFlags.FRIENDS_ONLY})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.privacy-setting--2"
							>
								{i18n._(INCOMING_CALLS_FRIENDS_ONLY_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() => handlePrivacySetting({incomingCallFlags: IncomingCallFlags.EVERYONE})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.privacy-setting--3"
							>
								{i18n._(INCOMING_CALLS_EVERYONE_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
						<MenuGroup data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-group--5">
							<MenuItem
								onClick={() => handlePrivacySetting({groupDmAddPermissionFlags: GroupDmAddPermissionFlags.NOBODY})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.privacy-setting--4"
							>
								{i18n._(GROUP_CHAT_ADDS_NOBODY_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() =>
									handlePrivacySetting({groupDmAddPermissionFlags: GroupDmAddPermissionFlags.FRIENDS_ONLY})
								}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.privacy-setting--5"
							>
								{i18n._(GROUP_CHAT_ADDS_FRIENDS_ONLY_DESCRIPTOR)}
							</MenuItem>
							<MenuItem
								onClick={() => handlePrivacySetting({groupDmAddPermissionFlags: GroupDmAddPermissionFlags.EVERYONE})}
								data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item.privacy-setting--6"
							>
								{i18n._(GROUP_CHAT_ADDS_EVERYONE_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					</>
				)}
				data-flx="ui.action-menu.items.bulk-settings-menu-items.bulk-dm-settings-menu-items.menu-item-submenu--3"
			/>
		</>
	);
});
