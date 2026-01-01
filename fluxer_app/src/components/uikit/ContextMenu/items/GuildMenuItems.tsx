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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {MessageNotifications, Permissions} from '~/Constants';
import {CategoryCreateModal} from '~/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '~/components/modals/ChannelCreateModal';
import {GuildNotificationSettingsModal} from '~/components/modals/GuildNotificationSettingsModal';
import {GuildPrivacySettingsModal} from '~/components/modals/GuildPrivacySettingsModal';
import {GuildSettingsModal} from '~/components/modals/GuildSettingsModal';
import {InviteModal} from '~/components/modals/InviteModal';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import {type GuildSettingsTab, getGuildSettingsTabs} from '~/components/modals/utils/guildSettingsConstants';
import {useLeaveGuild} from '~/hooks/useLeaveGuild';
import type {GuildRecord} from '~/records/GuildRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import PermissionStore from '~/stores/PermissionStore';
import ReadStateStore from '~/stores/ReadStateStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import {getMutedText, getNotificationSettingsLabel} from '~/utils/ContextMenuUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import {
	CopyIdIcon,
	CreateCategoryIcon,
	CreateChannelIcon,
	EditProfileIcon,
	InviteIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MuteIcon,
	NotificationSettingsIcon,
	PrivacySettingsIcon,
	SettingsIcon,
} from '../ContextMenuIcons';
import {MenuGroup} from '../MenuGroup';
import {MenuItem} from '../MenuItem';
import {MenuItemCheckbox} from '../MenuItemCheckbox';
import {MenuItemRadio} from '../MenuItemRadio';
import {MenuItemSubmenu} from '../MenuItemSubmenu';

interface GuildMenuItemProps {
	guild: GuildRecord;
	onClose: () => void;
}

export const MarkAsReadMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const channels = ChannelStore.getGuildChannels(guild.id);

	const hasUnread = React.useMemo(() => {
		return channels.some((channel) => ReadStateStore.hasUnread(channel.id));
	}, [channels]);

	const handleMarkAsRead = React.useCallback(() => {
		const channelIds = channels
			.filter((channel) => ReadStateStore.getUnreadCount(channel.id) > 0)
			.map((channel) => channel.id);

		if (channelIds.length > 0) {
			void ReadStateActionCreators.bulkAckChannels(channelIds);
		}
		onClose();
	}, [channels, onClose]);

	return (
		<MenuItem icon={<MarkAsReadIcon />} onClick={handleMarkAsRead} disabled={!hasUnread}>
			{t(msg`Mark as Read`)}
		</MenuItem>
	);
});

export const InvitePeopleMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const channelId = InviteUtils.getInvitableChannelId(guild.id);
	const canInvite = InviteUtils.canInviteToChannel(channelId, guild.id);

	const handleInvite = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <InviteModal channelId={channelId ?? ''} />));
		onClose();
	}, [channelId, onClose]);

	if (!canInvite) return null;

	return (
		<MenuItem icon={<InviteIcon />} onClick={handleInvite}>
			{t(msg`Invite People`)}
		</MenuItem>
	);
});

interface MuteDuration {
	label: string;
	value: number | null;
}

const getMuteDurations = (t: (message: MessageDescriptor) => string): Array<MuteDuration> => {
	return [
		{label: t(msg`For 15 Minutes`), value: 15 * 60 * 1000},
		{label: t(msg`For 1 Hour`), value: 60 * 60 * 1000},
		{label: t(msg`For 3 Hours`), value: 3 * 60 * 60 * 1000},
		{label: t(msg`For 8 Hours`), value: 8 * 60 * 60 * 1000},
		{label: t(msg`For 24 Hours`), value: 24 * 60 * 60 * 1000},
		{label: t(msg`Until I turn it back on`), value: null},
	];
};

export const MuteCommunityMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const settings = UserGuildSettingsStore.getSettings(guild.id);
	const isMuted = settings?.muted ?? false;
	const muteConfig = settings?.mute_config;

	const mutedText = getMutedText(isMuted, muteConfig);
	const MUTE_DURATIONS = React.useMemo(() => getMuteDurations(t), [t]);

	const handleMute = React.useCallback(
		(duration: number | null) => {
			const computedMuteConfig = duration
				? {
						selected_time_window: duration,
						end_time: new Date(Date.now() + duration).toISOString(),
					}
				: null;

			UserGuildSettingsActionCreators.updateGuildSettings(
				guild.id,
				{
					muted: true,
					mute_config: computedMuteConfig,
				},
				{persistImmediately: true},
			);
			onClose();
		},
		[guild.id, onClose],
	);

	const handleUnmute = React.useCallback(() => {
		UserGuildSettingsActionCreators.updateGuildSettings(
			guild.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
		onClose();
	}, [guild.id, onClose]);

	if (isMuted) {
		return (
			<MenuItem icon={<MuteIcon />} onClick={handleUnmute} hint={mutedText ?? undefined}>
				{t(msg`Unmute Community`)}
			</MenuItem>
		);
	}

	return (
		<MenuItemSubmenu
			label={t(msg`Mute Community`)}
			icon={<MuteIcon />}
			onTriggerSelect={() => handleMute(null)}
			render={() => (
				<MenuGroup>
					{MUTE_DURATIONS.map((duration) => (
						<MenuItem key={duration.label} onClick={() => handleMute(duration.value)}>
							{duration.label}
						</MenuItem>
					))}
				</MenuGroup>
			)}
		/>
	);
});

export const NotificationSettingsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const settings = UserGuildSettingsStore.getSettings(guild.id);
	const suppressEveryone = settings?.suppress_everyone ?? false;
	const suppressRoles = settings?.suppress_roles ?? false;
	const mobilePush = settings?.mobile_push ?? true;

	const effectiveNotificationLevel = UserGuildSettingsStore.getGuildMessageNotifications(guild.id);
	const currentStateText = getNotificationSettingsLabel(effectiveNotificationLevel);

	const handleNotificationLevelChange = React.useCallback(
		(level: number) => {
			UserGuildSettingsActionCreators.updateMessageNotifications(guild.id, level, undefined, {
				persistImmediately: true,
			});
		},
		[guild.id],
	);

	const handleToggleSuppressEveryone = React.useCallback(
		(checked: boolean) => {
			UserGuildSettingsActionCreators.updateGuildSettings(
				guild.id,
				{suppress_everyone: checked},
				{persistImmediately: true},
			);
		},
		[guild.id],
	);

	const handleToggleSuppressRoles = React.useCallback(
		(checked: boolean) => {
			UserGuildSettingsActionCreators.updateGuildSettings(
				guild.id,
				{suppress_roles: checked},
				{persistImmediately: true},
			);
		},
		[guild.id],
	);

	const handleToggleMobilePush = React.useCallback(
		(checked: boolean) => {
			UserGuildSettingsActionCreators.updateGuildSettings(guild.id, {mobile_push: checked}, {persistImmediately: true});
		},
		[guild.id],
	);

	const handleOpenModal = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <GuildNotificationSettingsModal guildId={guild.id} />));
		onClose();
	}, [guild.id, onClose]);

	return (
		<MenuItemSubmenu
			label={t(msg`Notification Settings`)}
			icon={<NotificationSettingsIcon />}
			hint={currentStateText}
			onTriggerSelect={handleOpenModal}
			render={() => (
				<>
					<MenuGroup>
						<MenuItemRadio
							selected={effectiveNotificationLevel === MessageNotifications.ALL_MESSAGES}
							onSelect={() => handleNotificationLevelChange(MessageNotifications.ALL_MESSAGES)}
						>
							{t(msg`All Messages`)}
						</MenuItemRadio>
						<MenuItemRadio
							selected={effectiveNotificationLevel === MessageNotifications.ONLY_MENTIONS}
							onSelect={() => handleNotificationLevelChange(MessageNotifications.ONLY_MENTIONS)}
						>
							{t(msg`Only @mentions`)}
						</MenuItemRadio>
						<MenuItemRadio
							selected={effectiveNotificationLevel === MessageNotifications.NO_MESSAGES}
							onSelect={() => handleNotificationLevelChange(MessageNotifications.NO_MESSAGES)}
						>
							{t(msg`Nothing`)}
						</MenuItemRadio>
					</MenuGroup>

					<MenuGroup>
						<MenuItemCheckbox checked={suppressEveryone} onChange={handleToggleSuppressEveryone}>
							{t(msg`Suppress @everyone and @here`)}
						</MenuItemCheckbox>
						<MenuItemCheckbox checked={suppressRoles} onChange={handleToggleSuppressRoles}>
							{t(msg`Suppress All Role @mentions`)}
						</MenuItemCheckbox>
						<MenuItemCheckbox checked={mobilePush} onChange={handleToggleMobilePush}>
							{t(msg`Mobile Push Notifications`)}
						</MenuItemCheckbox>
					</MenuGroup>
				</>
			)}
		/>
	);
});

export const HideMutedChannelsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild}) => {
	const {t} = useLingui();
	const settings = UserGuildSettingsStore.getSettings(guild.id);
	const hideMutedChannels = settings?.hide_muted_channels ?? false;

	const handleToggle = React.useCallback(
		(checked: boolean) => {
			const currentSettings = UserGuildSettingsStore.getSettings(guild.id);
			const currentValue = currentSettings?.hide_muted_channels ?? false;
			if (checked === currentValue) return;
			UserGuildSettingsActionCreators.toggleHideMutedChannels(guild.id);
		},
		[guild.id],
	);

	return (
		<MenuItemCheckbox checked={hideMutedChannels} onChange={handleToggle}>
			{t(msg`Hide Muted Channels`)}
		</MenuItemCheckbox>
	);
});

export const CommunitySettingsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const accessibleTabs = React.useMemo(() => {
		const guildTabs = getGuildSettingsTabs(t);
		return guildTabs.filter((tab) => {
			if (tab.permission && !PermissionStore.can(tab.permission, {guildId: guild.id})) {
				return false;
			}
			if (tab.requireFeature && !guild.features.has(tab.requireFeature)) {
				return false;
			}
			return true;
		});
	}, [guild, t]);

	const defaultTab = React.useMemo(() => {
		const overviewTab = accessibleTabs.find((tab) => tab.type === 'overview');
		return overviewTab ?? accessibleTabs[0] ?? null;
	}, [accessibleTabs]);

	const handleOpenSettings = React.useCallback(
		(tab: GuildSettingsTab) => {
			ModalActionCreators.push(modal(() => <GuildSettingsModal guildId={guild.id} initialTab={tab.type} />));
			onClose();
		},
		[guild.id, onClose],
	);

	const handleOpenDefaultTab = React.useCallback(() => {
		if (!defaultTab) return;
		handleOpenSettings(defaultTab);
	}, [defaultTab, handleOpenSettings]);

	if (accessibleTabs.length === 0) return null;

	return (
		<MenuItemSubmenu
			label={t(msg`Community Settings`)}
			icon={<SettingsIcon />}
			onTriggerSelect={handleOpenDefaultTab}
			render={() => (
				<>
					{accessibleTabs.map((tab) => {
						const IconComponent = tab.icon;
						return (
							<MenuItem
								key={tab.type}
								icon={<IconComponent size={16} weight={tab.iconWeight ?? 'fill'} />}
								onClick={() => handleOpenSettings(tab)}
							>
								{tab.label}
							</MenuItem>
						);
					})}
				</>
			)}
		/>
	);
});

export const PrivacySettingsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const handleOpenPrivacySettings = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <GuildPrivacySettingsModal guildId={guild.id} />));
		onClose();
	}, [guild.id, onClose]);

	return (
		<MenuItem icon={<PrivacySettingsIcon />} onClick={handleOpenPrivacySettings}>
			{t(msg`Privacy Settings`)}
		</MenuItem>
	);
});

export const EditCommunityProfileMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const handleEditProfile = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <UserSettingsModal initialGuildId={guild.id} initialTab="my_profile" />));
		onClose();
	}, [guild.id, onClose]);

	return (
		<MenuItem icon={<EditProfileIcon />} onClick={handleEditProfile}>
			{t(msg`Edit Community Profile`)}
		</MenuItem>
	);
});

export const CreateChannelMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {guildId: guild.id});

	const handleCreateChannel = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <ChannelCreateModal guildId={guild.id} />));
		onClose();
	}, [guild.id, onClose]);

	if (!canManageChannels) return null;

	return (
		<MenuItem icon={<CreateChannelIcon />} onClick={handleCreateChannel}>
			{t(msg`Create Channel`)}
		</MenuItem>
	);
});

export const CreateCategoryMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {guildId: guild.id});

	const handleCreateCategory = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <CategoryCreateModal guildId={guild.id} />));
		onClose();
	}, [guild.id, onClose]);

	if (!canManageChannels) return null;

	return (
		<MenuItem icon={<CreateCategoryIcon />} onClick={handleCreateCategory}>
			{t(msg`Create Category`)}
		</MenuItem>
	);
});

export const LeaveCommunityMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const isOwner = guild.isOwner(AuthenticationStore.currentUserId);
	const leaveGuild = useLeaveGuild();

	const handleLeave = React.useCallback(() => {
		leaveGuild(guild.id);
		onClose();
	}, [guild.id, onClose, leaveGuild]);

	if (isOwner) return null;

	return (
		<MenuItem icon={<LeaveIcon />} onClick={handleLeave} danger>
			{t(msg`Leave Community`)}
		</MenuItem>
	);
});

export const CopyGuildIdMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t, i18n} = useLingui();
	const handleCopyId = React.useCallback(() => {
		TextCopyActionCreators.copy(i18n, guild.id);
		onClose();
	}, [guild.id, onClose, i18n]);

	return (
		<MenuItem icon={<CopyIdIcon />} onClick={handleCopyId}>
			{t(msg`Copy Guild ID`)}
		</MenuItem>
	);
});
