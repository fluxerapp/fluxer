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
import {GuildDebugModal} from '@app/components/debug/GuildDebugModal';
import {CategoryCreateModal} from '@app/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '@app/components/modals/ChannelCreateModal';
import {GuildNotificationSettingsModal} from '@app/components/modals/GuildNotificationSettingsModal';
import {GuildPrivacySettingsModal} from '@app/components/modals/GuildPrivacySettingsModal';
import {GuildSettingsModal} from '@app/components/modals/GuildSettingsModal';
import type {IARContext} from '@app/components/modals/IARModal';
import {IARModal} from '@app/components/modals/IARModal';
import {InviteModal} from '@app/components/modals/InviteModal';
import {UserSettingsModal} from '@app/components/modals/UserSettingsModal';
import {getGuildSettingsTabs} from '@app/components/modals/utils/GuildSettingsConstants';
import {
	CopyIdIcon,
	CreateCategoryIcon,
	CreateChannelIcon,
	DebugChannelIcon,
	EditProfileIcon,
	InviteIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MuteIcon,
	NotificationSettingsIcon,
	PrivacySettingsIcon,
	ReportUserIcon,
	SettingsIcon,
} from '@app/components/uikit/context_menu/ContextMenuIcons';
import type {
	MenuCheckboxType,
	MenuGroupType,
	MenuItemType,
	MenuSubmenuItemType,
} from '@app/components/uikit/menu_bottom_sheet/MenuBottomSheet';
import {useLeaveGuild} from '@app/hooks/useLeaveGuild';
import {Routes} from '@app/Routes';
import type {GuildRecord} from '@app/records/GuildRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import ChannelStore from '@app/stores/ChannelStore';
import PermissionStore from '@app/stores/PermissionStore';
import ReadStateStore from '@app/stores/ReadStateStore';
import UserGuildSettingsStore from '@app/stores/UserGuildSettingsStore';
import UserSettingsStore from '@app/stores/UserSettingsStore';
import {getMutedText} from '@app/utils/ContextMenuUtils';
import * as InviteUtils from '@app/utils/InviteUtils';
import * as RouterUtils from '@app/utils/RouterUtils';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

interface UseGuildMenuDataOptions {
	onClose: () => void;
	onOpenMuteSheet?: () => void;
}

export interface GuildMenuHandlers {
	handleMarkAsRead: () => void;
	handleInviteMembers: () => void;
	handleCommunitySettings: () => void;
	handleCreateChannel: () => void;
	handleCreateCategory: () => void;
	handleNotificationSettings: () => void;
	handlePrivacySettings: () => void;
	handleEditCommunityProfile: () => void;
	handleLeaveCommunity: () => void;
	handleCopyGuildId: () => void;
	handleDebugGuild: () => void;
	handleReportGuild: () => void;
	handleToggleHideMutedChannels: (checked: boolean) => void;
}

export interface GuildMenuPermissions {
	canManageGuild: boolean;
	canManageChannels: boolean;
	canInvite: boolean;
	canAccessGuildSettings: boolean;
	isOwner: boolean;
	hasGuildUnread: boolean;
	developerMode: boolean;
}

export interface GuildMenuData {
	groups: Array<MenuGroupType>;
	handlers: GuildMenuHandlers;
	permissions: GuildMenuPermissions;
	isMuted: boolean;
	mutedText: string | null;
	hideMutedChannels: boolean;
}

export function useGuildMenuData(guild: GuildRecord, options: UseGuildMenuDataOptions): GuildMenuData {
	const {t, i18n} = useLingui();
	const {onClose, onOpenMuteSheet} = options;
	const leaveGuild = useLeaveGuild();

	const channels = ChannelStore.getGuildChannels(guild.id);

	const canManageGuild = PermissionStore.can(Permissions.MANAGE_GUILD, {guildId: guild.id});
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {guildId: guild.id});
	const invitableChannelId = InviteUtils.getInvitableChannelId(guild.id);
	const canInvite = InviteUtils.canInviteToChannel(invitableChannelId, guild.id);
	const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId: guild.id});
	const canViewAuditLog = PermissionStore.can(Permissions.VIEW_AUDIT_LOG, {guildId: guild.id});
	const canManageWebhooks = PermissionStore.can(Permissions.MANAGE_WEBHOOKS, {guildId: guild.id});
	const canManageEmojis = PermissionStore.can(Permissions.MANAGE_EXPRESSIONS, {guildId: guild.id});
	const canBanMembers = PermissionStore.can(Permissions.BAN_MEMBERS, {guildId: guild.id});

	const canAccessGuildSettings =
		canManageGuild || canManageRoles || canViewAuditLog || canManageWebhooks || canManageEmojis || canBanMembers;

	const isOwner = guild.isOwner(AuthenticationStore.currentUserId);
	const developerMode = UserSettingsStore.developerMode;

	const settings = UserGuildSettingsStore.getSettings(guild.id);
	const hideMutedChannels = settings?.hide_muted_channels ?? false;
	const isMuted = settings?.muted ?? false;
	const muteConfig = settings?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);

	const hasGuildUnread = useMemo(() => channels.some((channel) => ReadStateStore.hasUnread(channel.id)), [channels]);

	const handlers = useMemo(
		() => ({
			handleMarkAsRead: () => {
				const channelIds = channels
					.filter((channel) => ReadStateStore.getUnreadCount(channel.id) > 0)
					.map((channel) => channel.id);

				if (channelIds.length > 0) {
					void ReadStateActionCreators.bulkAckChannels(channelIds);
				}
				onClose();
			},
			handleInviteMembers: () => {
				const invitableChannelId = InviteUtils.getInvitableChannelId(guild.id);
				onClose();
				ModalActionCreators.push(modal(() => <InviteModal channelId={invitableChannelId ?? ''} />));
			},
			handleCommunitySettings: () => {
				onClose();
				ModalActionCreators.push(modal(() => <GuildSettingsModal guildId={guild.id} />));
			},
			handleCreateChannel: () => {
				onClose();
				ModalActionCreators.push(modal(() => <ChannelCreateModal guildId={guild.id} />));
			},
			handleCreateCategory: () => {
				onClose();
				ModalActionCreators.push(modal(() => <CategoryCreateModal guildId={guild.id} />));
			},
			handleNotificationSettings: () => {
				onClose();
				ModalActionCreators.push(modal(() => <GuildNotificationSettingsModal guildId={guild.id} />));
			},
			handlePrivacySettings: () => {
				onClose();
				ModalActionCreators.push(modal(() => <GuildPrivacySettingsModal guildId={guild.id} />));
			},
			handleEditCommunityProfile: () => {
				onClose();
				ModalActionCreators.push(modal(() => <UserSettingsModal initialGuildId={guild.id} initialTab="my_profile" />));
			},
			handleLeaveCommunity: () => {
				onClose();
				leaveGuild(guild.id);
			},
			handleCopyGuildId: () => {
				void TextCopyActionCreators.copy(i18n, guild.id);
				onClose();
			},
			handleDebugGuild: () => {
				ModalActionCreators.push(modal(() => <GuildDebugModal title={t`Community Debug`} guild={guild} />));
				onClose();
			},
			handleReportGuild: () => {
				onClose();
				const context: IARContext = {
					type: 'guild',
					guild,
				};
				ModalActionCreators.push(modal(() => <IARModal context={context} />));
			},
			handleToggleHideMutedChannels: (checked: boolean) => {
				const currentSettings = UserGuildSettingsStore.getSettings(guild.id);
				const currentValue = currentSettings?.hide_muted_channels ?? false;
				if (checked === currentValue) return;
				UserGuildSettingsActionCreators.toggleHideMutedChannels(guild.id);
			},
		}),
		[channels, guild, i18n, leaveGuild, onClose, t],
	);

	const permissions: GuildMenuPermissions = useMemo(
		() => ({
			canManageGuild,
			canManageChannels,
			canInvite,
			canAccessGuildSettings,
			isOwner,
			hasGuildUnread,
			developerMode,
		}),
		[canManageGuild, canManageChannels, canInvite, canAccessGuildSettings, isOwner, hasGuildUnread, developerMode],
	);

	const availableSettingsTabs = useMemo(() => {
		const allTabs = getGuildSettingsTabs(i18n);
		return allTabs.filter((tab) => {
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
	}, [guild.features, guild.id, i18n]);

	const groups = useMemo(() => {
		const menuGroups: Array<MenuGroupType> = [];

		const quickActions: Array<MenuItemType | MenuSubmenuItemType> = [];

		if (hasGuildUnread) {
			quickActions.push({
				icon: <MarkAsReadIcon size={20} />,
				label: t`Mark as Read`,
				onClick: handlers.handleMarkAsRead,
			});
		}

		if (canInvite) {
			quickActions.push({
				icon: <InviteIcon size={20} />,
				label: t`Invite Members`,
				onClick: handlers.handleInviteMembers,
			});
		}

		if (canAccessGuildSettings) {
			const settingsSubItems: Array<MenuItemType> = availableSettingsTabs.map((tab) => ({
				id: tab.type,
				label: tab.label,
				onClick: () => {
					onClose();
					if (tab.type === 'members') {
						RouterUtils.transitionTo(Routes.guildMembers(guild.id));
						return;
					}
					ModalActionCreators.push(modal(() => <GuildSettingsModal guildId={guild.id} initialTab={tab.type} />));
				},
			}));

			quickActions.push({
				icon: <SettingsIcon size={20} />,
				label: t`Community Settings`,
				items: settingsSubItems,
				onTriggerSelect: handlers.handleCommunitySettings,
			});
		}

		if (canManageChannels) {
			quickActions.push({
				icon: <CreateChannelIcon size={20} />,
				label: t`Create Channel`,
				onClick: handlers.handleCreateChannel,
			});
			quickActions.push({
				icon: <CreateCategoryIcon size={20} />,
				label: t`Create Category`,
				onClick: handlers.handleCreateCategory,
			});
		}

		if (quickActions.length > 0) {
			menuGroups.push({items: quickActions});
		}

		const settingsItems: Array<MenuItemType> = [
			{
				icon: <NotificationSettingsIcon size={20} />,
				label: t`Notification Settings`,
				onClick: handlers.handleNotificationSettings,
			},
			{
				icon: <PrivacySettingsIcon size={20} />,
				label: t`Privacy Settings`,
				onClick: handlers.handlePrivacySettings,
			},
			{
				icon: <EditProfileIcon size={20} />,
				label: t`Edit Community Profile`,
				onClick: handlers.handleEditCommunityProfile,
			},
		];

		menuGroups.push({items: settingsItems});

		const muteItems: Array<MenuItemType | MenuCheckboxType> = [];

		if (onOpenMuteSheet) {
			muteItems.push({
				icon: <MuteIcon size={20} />,
				label: isMuted ? t`Unmute Community` : t`Mute Community`,
				onClick: onOpenMuteSheet,
			});
		}

		const hideMutedChannelsItem: MenuCheckboxType = {
			label: t`Hide Muted Channels`,
			checked: hideMutedChannels,
			onChange: handlers.handleToggleHideMutedChannels,
		};
		muteItems.push(hideMutedChannelsItem);

		menuGroups.push({items: muteItems});

		if (!isOwner) {
			const dangerActions: Array<MenuItemType> = [
				{
					icon: <LeaveIcon size={20} />,
					label: t`Leave Community`,
					onClick: handlers.handleLeaveCommunity,
					danger: true,
				},
				{
					icon: <ReportUserIcon size={20} />,
					label: t`Report Community`,
					onClick: handlers.handleReportGuild,
					danger: true,
				},
			];
			menuGroups.push({items: dangerActions});
		}

		if (developerMode) {
			const debugItems: Array<MenuItemType> = [
				{
					icon: <DebugChannelIcon size={20} />,
					label: t`Debug Community`,
					onClick: handlers.handleDebugGuild,
				},
			];
			menuGroups.push({items: debugItems});
		}

		const utilityItems: Array<MenuItemType> = [
			{
				icon: <CopyIdIcon size={20} />,
				label: t`Copy Community ID`,
				onClick: handlers.handleCopyGuildId,
			},
		];

		menuGroups.push({items: utilityItems});

		return menuGroups;
	}, [
		hasGuildUnread,
		canInvite,
		canAccessGuildSettings,
		canManageChannels,
		availableSettingsTabs,
		guild.id,
		isMuted,
		hideMutedChannels,
		isOwner,
		developerMode,
		handlers,
		onClose,
		onOpenMuteSheet,
		t,
	]);

	return {
		groups,
		handlers,
		permissions,
		isMuted,
		mutedText: mutedText ?? null,
		hideMutedChannels,
	};
}
