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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ReadStateActionCreators from '@app/actions/ReadStateActionCreators';
import * as TextCopyActionCreators from '@app/actions/TextCopyActionCreators';
import * as UserGuildSettingsActionCreators from '@app/actions/UserGuildSettingsActionCreators';
import {getMuteDurationOptions} from '@app/components/channel/MuteOptions';
import {CategoryCreateModal} from '@app/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '@app/components/modals/ChannelCreateModal';
import {GuildNotificationSettingsModal} from '@app/components/modals/GuildNotificationSettingsModal';
import {GuildPrivacySettingsModal} from '@app/components/modals/GuildPrivacySettingsModal';
import {GuildSettingsModal} from '@app/components/modals/GuildSettingsModal';
import {InviteModal} from '@app/components/modals/InviteModal';
import {UserSettingsModal} from '@app/components/modals/UserSettingsModal';
import {type GuildSettingsTab, getGuildSettingsTabs} from '@app/components/modals/utils/GuildSettingsConstants';
import {CheckboxItem} from '@app/components/uikit/context_menu/ContextMenu';
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
} from '@app/components/uikit/context_menu/ContextMenuIcons';
import {MenuGroup} from '@app/components/uikit/context_menu/MenuGroup';
import {MenuItem} from '@app/components/uikit/context_menu/MenuItem';
import {MenuItemRadio} from '@app/components/uikit/context_menu/MenuItemRadio';
import {MenuItemSubmenu} from '@app/components/uikit/context_menu/MenuItemSubmenu';
import {useLeaveGuild} from '@app/hooks/useLeaveGuild';
import type {GuildRecord} from '@app/records/GuildRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import ChannelStore from '@app/stores/ChannelStore';
import PermissionStore from '@app/stores/PermissionStore';
import ReadStateStore from '@app/stores/ReadStateStore';
import UserGuildSettingsStore from '@app/stores/UserGuildSettingsStore';
import {getMutedText, getNotificationSettingsLabel} from '@app/utils/ContextMenuUtils';
import * as InviteUtils from '@app/utils/InviteUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

interface GuildMenuItemProps {
	guild: GuildRecord;
	onClose: () => void;
}

export const MarkAsReadMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const channels = ChannelStore.getGuildChannels(guild.id);

	const hasUnread = useMemo(() => {
		return channels.some((channel) => ReadStateStore.hasUnread(channel.id));
	}, [channels]);

	const handleMarkAsRead = useCallback(() => {
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

	const handleInvite = useCallback(() => {
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

export const MuteCommunityMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t, i18n} = useLingui();
	const settings = UserGuildSettingsStore.getSettings(guild.id);
	const isMuted = settings?.muted ?? false;
	const muteConfig = settings?.mute_config;

	const mutedText = getMutedText(isMuted, muteConfig);
	const muteDurations = useMemo(() => getMuteDurationOptions(i18n), [i18n]);

	const handleMute = useCallback(
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

	const handleUnmute = useCallback(() => {
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
					{muteDurations.map((duration) => (
						<MenuItem key={duration.value ?? 'until'} onClick={() => handleMute(duration.value)}>
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

	const handleNotificationLevelChange = useCallback(
		(level: number) => {
			UserGuildSettingsActionCreators.updateMessageNotifications(guild.id, level, undefined, {
				persistImmediately: true,
			});
		},
		[guild.id],
	);

	const handleToggleSuppressEveryone = useCallback(
		(checked: boolean) => {
			UserGuildSettingsActionCreators.updateGuildSettings(
				guild.id,
				{suppress_everyone: checked},
				{persistImmediately: true},
			);
		},
		[guild.id],
	);

	const handleToggleSuppressRoles = useCallback(
		(checked: boolean) => {
			UserGuildSettingsActionCreators.updateGuildSettings(
				guild.id,
				{suppress_roles: checked},
				{persistImmediately: true},
			);
		},
		[guild.id],
	);

	const handleToggleMobilePush = useCallback(
		(checked: boolean) => {
			UserGuildSettingsActionCreators.updateGuildSettings(guild.id, {mobile_push: checked}, {persistImmediately: true});
		},
		[guild.id],
	);

	const handleOpenModal = useCallback(() => {
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
						<CheckboxItem checked={suppressEveryone} onCheckedChange={handleToggleSuppressEveryone}>
							{t(msg`Suppress @everyone and @here`)}
						</CheckboxItem>
						<CheckboxItem checked={suppressRoles} onCheckedChange={handleToggleSuppressRoles}>
							{t(msg`Suppress All Role @mentions`)}
						</CheckboxItem>
						<CheckboxItem checked={mobilePush} onCheckedChange={handleToggleMobilePush}>
							{t(msg`Mobile Push Notifications`)}
						</CheckboxItem>
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

	const handleToggle = useCallback(
		(checked: boolean) => {
			const currentSettings = UserGuildSettingsStore.getSettings(guild.id);
			const currentValue = currentSettings?.hide_muted_channels ?? false;
			if (checked === currentValue) return;
			UserGuildSettingsActionCreators.toggleHideMutedChannels(guild.id);
		},
		[guild.id],
	);

	return (
		<CheckboxItem checked={hideMutedChannels} onCheckedChange={handleToggle}>
			{t(msg`Hide Muted Channels`)}
		</CheckboxItem>
	);
});

export const CommunitySettingsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {t, i18n} = useLingui();
	const accessibleTabs = useMemo(() => {
		const guildTabs = getGuildSettingsTabs(i18n);
		return guildTabs.filter((tab) => {
			if (tab.permission) {
				const perms = Array.isArray(tab.permission) ? tab.permission : [tab.permission];
				if (!perms.some((p) => PermissionStore.can(p, {guildId: guild.id}))) {
					return false;
				}
			}
			if (tab.requireFeature && !guild.features.has(tab.requireFeature)) {
				return false;
			}
			return true;
		});
	}, [guild, i18n]);

	const defaultTab = useMemo(() => {
		const overviewTab = accessibleTabs.find((tab) => tab.type === 'overview');
		return overviewTab ?? accessibleTabs[0] ?? null;
	}, [accessibleTabs]);

	const handleOpenSettings = useCallback(
		(tab: GuildSettingsTab) => {
			ModalActionCreators.push(modal(() => <GuildSettingsModal guildId={guild.id} initialTab={tab.type} />));
			onClose();
		},
		[guild.id, onClose],
	);

	const handleOpenDefaultTab = useCallback(() => {
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
	const handleOpenPrivacySettings = useCallback(() => {
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
	const handleEditProfile = useCallback(() => {
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

	const handleCreateChannel = useCallback(() => {
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

	const handleCreateCategory = useCallback(() => {
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

	const handleLeave = useCallback(() => {
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
	const handleCopyId = useCallback(() => {
		TextCopyActionCreators.copy(i18n, guild.id);
		onClose();
	}, [guild.id, onClose, i18n]);

	return (
		<MenuItem icon={<CopyIdIcon />} onClick={handleCopyId}>
			{t(msg`Copy Guild ID`)}
		</MenuItem>
	);
});
