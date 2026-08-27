// SPDX-License-Identifier: AGPL-3.0-or-later

import {EVERYONE_MENTION, HERE_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import Authentication from '@app/features/auth/state/Authentication';
import {getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import {CategoryCreateModal} from '@app/features/channel/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '@app/features/channel/components/modals/ChannelCreateModal';
import Channels from '@app/features/channel/state/Channels';
import {GuildNotificationSettingsModal} from '@app/features/guild/components/modals/GuildNotificationSettingsModal';
import {GuildPrivacySettingsModal} from '@app/features/guild/components/modals/GuildPrivacySettingsModal';
import {GuildSettingsModal} from '@app/features/guild/components/modals/GuildSettingsModal';
import {useLeaveGuild} from '@app/features/guild/hooks/useLeaveGuild';
import type {Guild} from '@app/features/guild/models/Guild';
import {isStockCommunityGuild} from '@app/features/guild/utils/GuildCommunityUtils';
import {
	COPY_COMMUNITY_ID_DESCRIPTOR,
	CREATE_CATEGORY_DESCRIPTOR,
	CREATE_CHANNEL_DESCRIPTOR,
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
	INVITE_PEOPLE_DESCRIPTOR,
	LEAVE_COMMUNITY_DESCRIPTOR,
	MARK_AS_READ_DESCRIPTOR,
	MUTE_COMMUNITY_DESCRIPTOR,
	NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR,
	NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR,
	NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR,
	NOTIFICATION_SETTINGS_DESCRIPTOR,
	PRIVACY_SETTINGS_DESCRIPTOR,
	UNMUTE_COMMUNITY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import Permission from '@app/features/permissions/state/Permission';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {
	CopyIdIcon,
	CreateCategoryIcon,
	CreateChannelIcon,
	EditProfileIcon,
	InviteIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MuteIcon,
	PrivacySettingsIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {
	GUILD_SETTINGS_LABEL_DESCRIPTOR,
	type GuildSettingsTab,
	getGuildSettingsTabs,
} from '@app/features/user/components/settings_utils/GuildSettingsConstants';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import {getMutedText, getNotificationSettingsLabel} from '@app/lib/overlay/OverlayContextMenu';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const SUPPRESS_AND_DESCRIPTOR = msg({
	message: 'Suppress {everyoneMention} and {hereMention}',
	comment: 'Toggle label that suppresses @everyone and @here mentions; placeholders are the localized mention tags.',
});
const SUPPRESS_ALL_ROLE_MENTIONS_DESCRIPTOR = msg({
	message: 'Suppress all role mentions',
	comment: 'Toggle label that suppresses all role mentions.',
});
const MOBILE_PUSH_NOTIFICATIONS_DESCRIPTOR = msg({
	message: 'Mobile push notifications',
	comment: 'Section label for mobile push notification settings.',
});
const EDIT_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Edit community profile',
	comment: 'Action that opens the community profile editor for the current member.',
});

interface GuildMenuItemProps {
	guild: Guild;
	onClose: () => void;
}

export const MarkAsReadMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const channels = Channels.getGuildChannels(guild.id);
	const hasUnread = useMemo(() => {
		return channels.some((channel) => ReadStates.hasUnread(channel.id));
	}, [channels]);
	const handleMarkAsRead = useCallback(() => {
		const channelIds = channels
			.filter((channel) => ReadStates.isUnreadOrMentioned(channel.id))
			.map((channel) => channel.id);
		if (channelIds.length > 0) {
			void ReadStateCommands.bulkAckChannels(channelIds);
		}
		onClose();
	}, [channels, onClose]);
	return (
		<MenuItem
			icon={
				<MarkAsReadIcon data-flx="ui.action-menu.items.guild-menu-items.mark-as-read-menu-item.mark-as-read-icon" />
			}
			onClick={handleMarkAsRead}
			disabled={!hasUnread}
			data-flx="ui.action-menu.items.guild-menu-items.mark-as-read-menu-item.menu-item.mark-as-read"
		>
			{i18n._(MARK_AS_READ_DESCRIPTOR)}
		</MenuItem>
	);
});
export const InvitePeopleMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const channelId = InviteUtils.getInvitableChannelId(guild.id);
	const canInvite = InviteUtils.canInviteToChannel(channelId, guild.id);
	const handleInvite = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<InviteModal
					channelId={channelId ?? ''}
					data-flx="ui.action-menu.items.guild-menu-items.handle-invite.invite-modal"
				/>
			)),
		);
		onClose();
	}, [channelId, onClose]);
	if (!canInvite) return null;
	return (
		<MenuItem
			icon={<InviteIcon data-flx="ui.action-menu.items.guild-menu-items.invite-people-menu-item.invite-icon" />}
			onClick={handleInvite}
			data-flx="ui.action-menu.items.guild-menu-items.invite-people-menu-item.menu-item.invite"
		>
			{i18n._(INVITE_PEOPLE_DESCRIPTOR)}
		</MenuItem>
	);
});
export const MuteCommunityMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const settings = UserGuildSettings.getSettingsForScope(guild.id);
	const isMuted = settings?.muted ?? false;
	const muteConfig = settings?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const muteDurations = useMemo(() => getMuteDurationOptions(i18n), [i18n.locale]);
	const handleMute = useCallback(
		(duration: number | null) => {
			const computedMuteConfig = duration
				? {
						selected_time_window: duration,
						end_time: new Date(Date.now() + duration).toISOString(),
					}
				: null;
			UserGuildSettingsCommands.updateGuildSettings(
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
		UserGuildSettingsCommands.updateGuildSettings(
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
			<MenuItem
				icon={<MuteIcon data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.mute-icon" />}
				onClick={handleUnmute}
				hint={mutedText ?? undefined}
				data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.menu-item.unmute"
			>
				{i18n._(UNMUTE_COMMUNITY_DESCRIPTOR)}
			</MenuItem>
		);
	}
	return (
		<MenuItemSubmenu
			label={i18n._(MUTE_COMMUNITY_DESCRIPTOR)}
			onTriggerSelect={() => handleMute(null)}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.menu-group">
					{muteDurations.map((duration) => (
						<MenuItem
							key={duration.value ?? 'until'}
							onClick={() => handleMute(duration.value)}
							data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.menu-item.mute"
						>
							{duration.label}
						</MenuItem>
					))}
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.menu-item-submenu"
		/>
	);
});
export const NotificationSettingsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const settings = UserGuildSettings.getSettingsForScope(guild.id);
	const suppressEveryone = settings?.suppress_everyone ?? false;
	const suppressRoles = settings?.suppress_roles ?? false;
	const mobilePush = settings?.mobile_push ?? true;
	const effectiveNotificationLevel = UserGuildSettings.getGuildMessageNotifications(guild.id);
	const currentStateText = getNotificationSettingsLabel(effectiveNotificationLevel);
	const handleNotificationLevelChange = useCallback(
		(level: number) => {
			UserGuildSettingsCommands.updateMessageNotifications(guild.id, level, undefined, {
				persistImmediately: true,
			});
		},
		[guild.id],
	);
	const handleToggleSuppressEveryone = useCallback(
		(checked: boolean) => {
			UserGuildSettingsCommands.updateGuildSettings(guild.id, {suppress_everyone: checked}, {persistImmediately: true});
		},
		[guild.id],
	);
	const handleToggleSuppressRoles = useCallback(
		(checked: boolean) => {
			UserGuildSettingsCommands.updateGuildSettings(guild.id, {suppress_roles: checked}, {persistImmediately: true});
		},
		[guild.id],
	);
	const handleToggleMobilePush = useCallback(
		(checked: boolean) => {
			UserGuildSettingsCommands.updateGuildSettings(guild.id, {mobile_push: checked}, {persistImmediately: true});
		},
		[guild.id],
	);
	const handleOpenModal = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<GuildNotificationSettingsModal
					guildId={guild.id}
					data-flx="ui.action-menu.items.guild-menu-items.handle-open-modal.guild-notification-settings-modal"
				/>
			)),
		);
		onClose();
	}, [guild.id, onClose]);
	return (
		<MenuItemSubmenu
			label={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
			hint={currentStateText}
			onTriggerSelect={handleOpenModal}
			render={() => (
				<>
					<MenuGroup data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.menu-group">
						<MenuItemRadio
							selected={effectiveNotificationLevel === MessageNotifications.ALL_MESSAGES}
							onSelect={() => handleNotificationLevelChange(MessageNotifications.ALL_MESSAGES)}
							data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.menu-item-radio.notification-level-change"
						>
							{i18n._(NOTIFICATION_LEVEL_ALL_MESSAGES_DESCRIPTOR)}
						</MenuItemRadio>
						<MenuItemRadio
							selected={effectiveNotificationLevel === MessageNotifications.ONLY_MENTIONS}
							onSelect={() => handleNotificationLevelChange(MessageNotifications.ONLY_MENTIONS)}
							data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.menu-item-radio.notification-level-change--2"
						>
							{i18n._(NOTIFICATION_LEVEL_ONLY_MENTIONS_DESCRIPTOR)}
						</MenuItemRadio>
						<MenuItemRadio
							selected={effectiveNotificationLevel === MessageNotifications.NO_MESSAGES}
							onSelect={() => handleNotificationLevelChange(MessageNotifications.NO_MESSAGES)}
							data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.menu-item-radio.notification-level-change--3"
						>
							{i18n._(NOTIFICATION_LEVEL_NOTHING_DESCRIPTOR)}
						</MenuItemRadio>
					</MenuGroup>
					<MenuGroup data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.menu-group--2">
						<CheckboxItem
							checked={suppressEveryone}
							onCheckedChange={handleToggleSuppressEveryone}
							data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.checkbox-item"
						>
							{i18n._(SUPPRESS_AND_DESCRIPTOR, {everyoneMention: EVERYONE_MENTION, hereMention: HERE_MENTION})}
						</CheckboxItem>
						<CheckboxItem
							checked={suppressRoles}
							onCheckedChange={handleToggleSuppressRoles}
							data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.checkbox-item--2"
						>
							{i18n._(SUPPRESS_ALL_ROLE_MENTIONS_DESCRIPTOR)}
						</CheckboxItem>
						<CheckboxItem
							checked={mobilePush}
							onCheckedChange={handleToggleMobilePush}
							data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.checkbox-item--3"
						>
							{i18n._(MOBILE_PUSH_NOTIFICATIONS_DESCRIPTOR)}
						</CheckboxItem>
					</MenuGroup>
				</>
			)}
			data-flx="ui.action-menu.items.guild-menu-items.notification-settings-menu-item.menu-item-submenu"
		/>
	);
});
export const HideMutedChannelsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild}) => {
	const {i18n} = useLingui();
	const settings = UserGuildSettings.getSettingsForScope(guild.id);
	const hideMutedChannels = settings?.hide_muted_channels ?? false;
	const handleToggle = useCallback(
		(checked: boolean) => {
			const currentSettings = UserGuildSettings.getSettingsForScope(guild.id);
			const currentValue = currentSettings?.hide_muted_channels ?? false;
			if (checked === currentValue) return;
			UserGuildSettingsCommands.toggleHideMutedChannels(guild.id);
		},
		[guild.id],
	);
	return (
		<CheckboxItem
			checked={hideMutedChannels}
			onCheckedChange={handleToggle}
			data-flx="ui.action-menu.items.guild-menu-items.hide-muted-channels-menu-item.checkbox-item"
		>
			{i18n._(HIDE_MUTED_CHANNELS_DESCRIPTOR)}
		</CheckboxItem>
	);
});
export const CommunitySettingsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const accessibleTabs = useMemo(() => {
		const guildTabs = getGuildSettingsTabs(i18n);
		return guildTabs.filter((tab) => {
			if (tab.permission) {
				const perms = Array.isArray(tab.permission) ? tab.permission : [tab.permission];
				if (!perms.some((p) => Permission.can(p, {guildId: guild.id}))) {
					return false;
				}
			}
			if (tab.requireFeature && !guild.features.has(tab.requireFeature)) {
				return false;
			}
			return true;
		});
	}, [guild, i18n.locale]);
	const defaultTab = useMemo(() => {
		const overviewTab = accessibleTabs.find((tab) => tab.type === 'overview');
		return overviewTab ?? accessibleTabs[0] ?? null;
	}, [accessibleTabs]);
	const handleOpenSettings = useCallback(
		(tab: GuildSettingsTab) => {
			ModalCommands.push(
				modal(() => (
					<GuildSettingsModal
						guildId={guild.id}
						initialTab={tab.type}
						data-flx="ui.action-menu.items.guild-menu-items.handle-open-settings.guild-settings-modal"
					/>
				)),
			);
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
			label={i18n._(GUILD_SETTINGS_LABEL_DESCRIPTOR)}
			onTriggerSelect={handleOpenDefaultTab}
			render={() => (
				<>
					{accessibleTabs.map((tab) => {
						const IconComponent = tab.icon;
						return (
							<MenuItem
								key={tab.type}
								icon={
									<IconComponent
										size={16}
										weight={tab.iconWeight ?? 'fill'}
										data-flx="ui.action-menu.items.guild-menu-items.community-settings-menu-item.icon-component"
									/>
								}
								onClick={() => handleOpenSettings(tab)}
								data-flx="ui.action-menu.items.guild-menu-items.community-settings-menu-item.menu-item.open-settings"
							>
								{tab.label}
							</MenuItem>
						);
					})}
				</>
			)}
			data-flx="ui.action-menu.items.guild-menu-items.community-settings-menu-item.menu-item-submenu"
		/>
	);
});
export const PrivacySettingsMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const handleOpenPrivacySettings = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<GuildPrivacySettingsModal
					guildId={guild.id}
					data-flx="ui.action-menu.items.guild-menu-items.handle-open-privacy-settings.guild-privacy-settings-modal"
				/>
			)),
		);
		onClose();
	}, [guild.id, onClose]);
	return (
		<MenuItem
			icon={
				<PrivacySettingsIcon data-flx="ui.action-menu.items.guild-menu-items.privacy-settings-menu-item.privacy-settings-icon" />
			}
			onClick={handleOpenPrivacySettings}
			data-flx="ui.action-menu.items.guild-menu-items.privacy-settings-menu-item.menu-item.open-privacy-settings"
		>
			{i18n._(PRIVACY_SETTINGS_DESCRIPTOR)}
		</MenuItem>
	);
});
export const EditCommunityProfileMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const currentUser = Users.getCurrentUser();
	const handleEditProfile = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialGuildId={guild.id}
					initialTab="my_profile"
					data-flx="ui.action-menu.items.guild-menu-items.handle-edit-profile.user-settings-modal"
				/>
			)),
		);
		onClose();
	}, [guild.id, onClose]);
	if (!currentUser?.isClaimed()) return null;
	return (
		<MenuItem
			icon={
				<EditProfileIcon data-flx="ui.action-menu.items.guild-menu-items.edit-community-profile-menu-item.edit-profile-icon" />
			}
			onClick={handleEditProfile}
			data-flx="ui.action-menu.items.guild-menu-items.edit-community-profile-menu-item.menu-item.edit-profile"
		>
			{i18n._(EDIT_COMMUNITY_PROFILE_DESCRIPTOR)}
		</MenuItem>
	);
});
export const CreateChannelMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {guildId: guild.id});
	const handleCreateChannel = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ChannelCreateModal
					guildId={guild.id}
					data-flx="ui.action-menu.items.guild-menu-items.handle-create-channel.channel-create-modal"
				/>
			)),
		);
		onClose();
	}, [guild.id, onClose]);
	if (!canManageChannels) return null;
	return (
		<MenuItem
			icon={
				<CreateChannelIcon data-flx="ui.action-menu.items.guild-menu-items.create-channel-menu-item.create-channel-icon" />
			}
			onClick={handleCreateChannel}
			data-flx="ui.action-menu.items.guild-menu-items.create-channel-menu-item.menu-item.create-channel"
		>
			{i18n._(CREATE_CHANNEL_DESCRIPTOR)}
		</MenuItem>
	);
});
export const CreateCategoryMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {guildId: guild.id});
	const handleCreateCategory = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<CategoryCreateModal
					guildId={guild.id}
					data-flx="ui.action-menu.items.guild-menu-items.handle-create-category.category-create-modal"
				/>
			)),
		);
		onClose();
	}, [guild.id, onClose]);
	if (!canManageChannels) return null;
	return (
		<MenuItem
			icon={
				<CreateCategoryIcon data-flx="ui.action-menu.items.guild-menu-items.create-category-menu-item.create-category-icon" />
			}
			onClick={handleCreateCategory}
			data-flx="ui.action-menu.items.guild-menu-items.create-category-menu-item.menu-item.create-category"
		>
			{i18n._(CREATE_CATEGORY_DESCRIPTOR)}
		</MenuItem>
	);
});
export const LeaveCommunityMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const isOwner = guild.isOwner(Authentication.currentUserId);
	const leaveGuild = useLeaveGuild();
	const handleLeave = useCallback(() => {
		leaveGuild(guild.id);
		onClose();
	}, [guild.id, onClose, leaveGuild]);
	if (isOwner || isStockCommunityGuild(guild.id)) return null;
	return (
		<MenuItem
			icon={<LeaveIcon data-flx="ui.action-menu.items.guild-menu-items.leave-community-menu-item.leave-icon" />}
			onClick={handleLeave}
			danger
			data-flx="ui.action-menu.items.guild-menu-items.leave-community-menu-item.menu-item.leave"
		>
			{i18n._(LEAVE_COMMUNITY_DESCRIPTOR)}
		</MenuItem>
	);
});
export const CopyGuildIdMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const handleCopyId = useCallback(() => {
		TextCopyCommands.copy(i18n, guild.id);
		onClose();
	}, [guild.id, onClose, i18n]);
	return (
		<MenuItem
			icon={<CopyIdIcon data-flx="ui.action-menu.items.guild-menu-items.copy-guild-id-menu-item.copy-id-icon" />}
			onClick={handleCopyId}
			data-flx="ui.action-menu.items.guild-menu-items.copy-guild-id-menu-item.menu-item.copy-id"
		>
			{i18n._(COPY_COMMUNITY_ID_DESCRIPTOR)}
		</MenuItem>
	);
});
